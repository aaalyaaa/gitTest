# ml_analysis.R (исправленный)
library(dplyr)
library(forecast)
library(cluster)
library(ggplot2)

git_error <- function(class, message, ...) {
  structure(list(message = message, ...), class = c(class, "error", "condition"))
}

optimal_clusters <- function(data, max_clusters = 10) {
  if (nrow(data) < 3) return(2)
  wss <- sapply(1:min(max_clusters, nrow(data)-1), function(k) {
    kmeans(data, centers = k, nstart = 10)$tot.withinss
  })
  if (length(wss) < 2) return(2)
  diffs <- diff(wss)
  elbow <- which.min(diffs[2:length(diffs)]) + 1
  return(min(max(elbow, 2), max_clusters))
}

prepare_clustering_data <- function(conn) {
  query <- "
    WITH developer_metrics AS (
      SELECT 
          c.author_name,
          COUNT(DISTINCT c.commit) as total_commits,
          COUNT(DISTINCT CAST(c.date AS DATE)) as active_days,
          AVG(EXTRACT(HOUR FROM c.date)) as avg_commit_hour,
          SUM(CASE WHEN EXTRACT(HOUR FROM c.date) BETWEEN 0 AND 5 THEN 1 ELSE 0 END) as night_commits,
          SUM(d.count_add) as total_added,
          SUM(d.count_del) as total_deleted,
          AVG(d.count_add) as avg_add_per_commit,
          AVG(d.count_del) as avg_del_per_commit,
          COUNT(DISTINCT d.src_file) as unique_files,
          SUM(CASE WHEN d.src_file LIKE '%.env%' OR d.src_file LIKE '%.key%' OR d.src_file LIKE '%secret%' THEN 1 ELSE 0 END) as sensitive_changes
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      GROUP BY c.author_name
    )
    SELECT * FROM developer_metrics
  "
  df <- DBI::dbGetQuery(conn, query)
  if (nrow(df) == 0) return(git_error("no_data_error", "Нет данных для ML анализа"))
  
  df_scaled <- df
  numeric_cols <- c("total_commits", "active_days", "avg_commit_hour", "night_commits",
                    "total_added", "total_deleted", "avg_add_per_commit", "avg_del_per_commit",
                    "unique_files", "sensitive_changes")
  for (col in numeric_cols) {
    if (sd(df[[col]], na.rm = TRUE) > 0) {
      df_scaled[[col]] <- scale(df[[col]])
    } else {
      df_scaled[[col]] <- 0
    }
  }
  list(data = df, scaled = df_scaled, features = numeric_cols)
}

cluster_developers <- function(conn, n_clusters = NULL) {
  tryCatch({
    ml_data <- prepare_clustering_data(conn)
    if (inherits(ml_data, "error")) return(ml_data)
    dev_count <- nrow(ml_data$data)
    if (dev_count < 3) return(git_error("insufficient_data_error", "Разработчиков меньше 3"))
    
    df_scaled <- ml_data$scaled[, ml_data$features, drop = FALSE]
    df_scaled <- df_scaled[complete.cases(df_scaled), ]
    if (nrow(df_scaled) < 3) return(git_error("insufficient_data_error", "Недостаточно данных после очистки"))
    
    if (is.null(n_clusters)) {
      n_clusters <- optimal_clusters(df_scaled, max_clusters = min(5, dev_count - 1))
    }
    n_clusters <- min(n_clusters, nrow(df_scaled) - 1)
    if (n_clusters < 2) n_clusters <- 2
    
    set.seed(123)
    kmeans_result <- kmeans(df_scaled, centers = n_clusters, nstart = 25)
    ml_data$data$cluster <- kmeans_result$cluster
    
    cluster_profiles <- aggregate(ml_data$data[, ml_data$features], 
                                  by = list(cluster = ml_data$data$cluster), FUN = mean)
    cluster_names <- c()
    for (i in 1:n_clusters) {
      profile <- cluster_profiles[i, ]
      if (profile$total_commits > median(ml_data$data$total_commits)) {
        if (profile$avg_commit_hour < 8 || profile$avg_commit_hour > 22) {
          cluster_names <- c(cluster_names, "Ночной трудоголик")
        } else {
          cluster_names <- c(cluster_names, "Активный разработчик")
        }
      } else {
        if (profile$unique_files > median(ml_data$data$unique_files)) {
          cluster_names <- c(cluster_names, "Многофайловый")
        } else if (profile$night_commits > 3) {
          cluster_names <- c(cluster_names, "Совенок")
        } else {
          cluster_names <- c(cluster_names, "Специалист")
        }
      }
    }
    ml_data$data$cluster_type <- cluster_names[kmeans_result$cluster]
    cluster_profiles$cluster_type <- cluster_names
    cluster_stats <- as.data.frame(table(ml_data$data$cluster_type))
    names(cluster_stats) <- c("cluster_type", "developers_count")
    
    list(clustering = kmeans_result, data = ml_data$data,
         cluster_profiles = cluster_profiles, cluster_stats = cluster_stats,
         features = ml_data$features, n_clusters = n_clusters)
  }, error = function(e) git_error("clustering_error", paste("Ошибка кластеризации:", e$message)))
}

forecast_developer_activity <- function(conn, author_name, forecast_days = 7) {
  tryCatch({
    query <- sprintf("
      SELECT CAST(date AS DATE) as commit_date, COUNT(*) as commits
      FROM git_commit_history
      WHERE author_name = '%s'
      GROUP BY commit_date ORDER BY commit_date
    ", author_name)
    df <- DBI::dbGetQuery(conn, query)
    if (nrow(df) < 3) {
      return(git_error("insufficient_data_error", paste("Недостаточно данных для", author_name)))
    }
    ts_data <- ts(df$commits, frequency = 1)
    fit <- tryCatch(auto.arima(ts_data, seasonal = FALSE), error = function(e) NULL)
    if (is.null(fit)) {
      # Fallback: простая экспоненциальная модель
      fit <- tryCatch(ets(ts_data), error = function(e) NULL)
    }
    if (is.null(fit)) {
      # Простейший прогноз – среднее
      forecast_mean <- rep(mean(df$commits), forecast_days)
      forecast_obj <- list(mean = forecast_mean, lower = matrix(forecast_mean - sd(df$commits), ncol=2),
                           upper = matrix(forecast_mean + sd(df$commits), ncol=2))
      class(forecast_obj) <- "forecast"
    } else {
      forecast_obj <- forecast(fit, h = forecast_days)
    }
    plot_data <- data.frame(
      day = 1:forecast_days,
      forecast = as.numeric(forecast_obj$mean),
      lower = as.numeric(forecast_obj$lower[, 2]),
      upper = as.numeric(forecast_obj$upper[, 2])
    )
    expected <- round(sum(forecast_obj$mean, na.rm = TRUE), 1)
    list(author = author_name, historical = df, forecast = forecast_obj,
         expected_commits_next_week = expected, plot_data = plot_data)
  }, error = function(e) git_error("forecast_error", paste("Ошибка прогнозирования:", e$message)))
}

plot_forecast <- function(forecast_result) {
  if (inherits(forecast_result, "error")) {
    cat("⚠️", forecast_result$message, "\n"); return(invisible(NULL))
  }
  plot_data <- forecast_result$plot_data
  if (all(is.na(plot_data$forecast))) {
    cat("Нет данных для прогноза\n"); return(invisible(NULL))
  }
  y_max <- max(plot_data$upper, na.rm = TRUE) + 1
  plot(plot_data$day, plot_data$forecast, type = "b", col = "blue", pch = 19,
       main = paste("Прогноз для", forecast_result$author),
       xlab = "Дни", ylab = "Коммиты", ylim = c(0, y_max))
  lines(plot_data$day, plot_data$lower, col = "red", lty = 2)
  lines(plot_data$day, plot_data$upper, col = "red", lty = 2)
  legend("topright", legend = c("Прогноз", "95% ДИ"), col = c("blue", "red"), lty = c(1,2))
  mtext(paste("Ожидается:", forecast_result$expected_commits_next_week, "коммитов"), side = 3)
}

get_activity_seasonality <- function(conn, author_name = NULL) {
  tryCatch({
    where <- if (!is.null(author_name)) sprintf("WHERE author_name = '%s'", author_name) else ""
    query <- sprintf("
      SELECT EXTRACT(HOUR FROM date) as hour, COUNT(*) as commits
      FROM git_commit_history %s
      GROUP BY hour ORDER BY hour
    ", where)
    hour_data <- DBI::dbGetQuery(conn, query)
    if (nrow(hour_data) == 0) return(git_error("no_data_error", "Нет данных"))
    peak_hours <- hour_data[order(-hour_data$commits), ][1:3, ]
    list(by_hour = hour_data, peak_hours = peak_hours)
  }, error = function(e) git_error("seasonality_error", e$message))
}

compare_with_team <- function(conn, author_name) {
  tryCatch({
    team <- DBI::dbGetQuery(conn, "
      SELECT AVG(total_commits) as avg_commits, AVG(avg_commit_size) as avg_size, AVG(active_days) as avg_days
      FROM developer_metrics
    ")
    user <- DBI::dbGetQuery(conn, sprintf("
      SELECT total_commits, avg_commit_size, active_days FROM developer_metrics WHERE author_name = '%s'
    ", author_name))
    if (nrow(user) == 0) return(git_error("no_developer_error", "Разработчик не найден"))
    data.frame(
      metric = c("Коммитов", "Средний размер", "Активных дней"),
      developer = as.numeric(user[1, ]),
      team_avg = as.numeric(team[1, ]),
      diff_percent = round(100 * (as.numeric(user[1, ]) - as.numeric(team[1, ])) / as.numeric(team[1, ]), 1),
      status = ifelse(abs(round(100 * (as.numeric(user[1, ]) - as.numeric(team[1, ])) / as.numeric(team[1, ]), 1)) > 20,
                      ifelse(round(100 * (as.numeric(user[1, ]) - as.numeric(team[1, ])) / as.numeric(team[1, ]), 1) > 0,
                             "выше среднего", "ниже среднего"), "в норме")
    )
  }, error = function(e) git_error("comparison_error", e$message))
}

get_activity_trends <- function(conn, author_name = NULL) {
  tryCatch({
    where <- if (!is.null(author_name)) sprintf("WHERE author_name = '%s'", author_name) else ""
    query <- sprintf("
      SELECT DATE_TRUNC('month', date) as month, COUNT(*) as commits
      FROM git_commit_history %s
      GROUP BY month ORDER BY month
    ", where)
    df <- DBI::dbGetQuery(conn, query)
    if (nrow(df) < 2) return(git_error("insufficient_data_error", "Недостаточно месяцев"))
    df$trend <- c(NA, diff(df$commits))
    df$direction <- ifelse(df$trend > 0, "рост", ifelse(df$trend < 0, "падение", "стабильно"))
    overall <- ifelse(coef(lm(commits ~ seq_len(nrow(df)), data = df))[2] > 0, "общий тренд: РОСТ", "общий тренд: ПАДЕНИЕ")
    list(monthly_data = df, overall_trend = overall,
         best_month = df[which.max(df$commits), ],
         worst_month = df[which.min(df$commits), ])
  }, error = function(e) git_error("trend_error", e$message))
}

# ============================================================================
# 6. ОБНАРУЖЕНИЕ АНОМАЛИЙ С ПОМОЩЬЮ ISOLATION FOREST
# ============================================================================

prepare_anomaly_features <- function(conn, author_name = NULL) {
  where_clause <- if (!is.null(author_name)) {
    sprintf("WHERE c.author_name = '%s'", author_name)
  } else ""
  
  query <- sprintf("
    SELECT 
        c.commit,
        c.author_name,
        EXTRACT(HOUR FROM c.date) AS hour,
        EXTRACT(DOW FROM c.date) AS dow,
        COUNT(DISTINCT d.src_file) AS n_files,
        SUM(d.count_add + d.count_del) AS commit_size,
        SUM(d.count_add) / NULLIF(SUM(d.count_del), 0) AS add_del_ratio
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    %s
    GROUP BY c.commit, c.author_name, c.date
  ", where_clause)
  
  df <- DBI::dbGetQuery(conn, query)
  df$add_del_ratio[is.infinite(df$add_del_ratio)] <- 999
  df$add_del_ratio[is.na(df$add_del_ratio)] <- 1
  df
}

get_ml_anomalies <- function(conn, author_name = NULL, threshold = 0.95) {
  if (!requireNamespace("solitude", quietly = TRUE)) {
    stop("Пакет 'solitude' не установлен. Установите: install.packages('solitude')")
  }
  
  features_df <- prepare_anomaly_features(conn, author_name)
  if (nrow(features_df) == 0) {
    message("Нет данных для анализа аномалий")
    return(data.frame())
  }
  
  X <- features_df[, c("hour", "dow", "n_files", "commit_size", "add_del_ratio")]
  iso <- solitude::isolationForest$new(sample_size = min(nrow(X), 10000), num_trees = 100)
  iso$fit(X)
  scores <- iso$predict(X)
  features_df$anomaly_score <- scores$anomaly_score
  
  quantile_thresh <- quantile(features_df$anomaly_score, probs = threshold, na.rm = TRUE)
  anomalies <- features_df[features_df$anomaly_score >= quantile_thresh, ]
  
  anomalies$anomaly_type <- "ml_anomaly"
  anomalies$description <- paste(
    "ML-аномалия: оценка", round(anomalies$anomaly_score, 3),
    "(порог", round(quantile_thresh, 3), ")"
  )
  
  result <- data.frame(
    author_name = anomalies$author_name,
    date = NA,
    anomaly_type = anomalies$anomaly_type,
    description = anomalies$description,
    anomaly_score = anomalies$anomaly_score,
    commit = anomalies$commit,
    stringsAsFactors = FALSE
  )
  
  cat(sprintf("Найдено %d ML-аномалий (порог: %.2f%%)\n", nrow(result), threshold * 100))
  return(result)
}

summary_ml_anomalies <- function(anomalies) {
  if (nrow(anomalies) == 0) return(data.frame(author_name = character(), ml_anomaly_count = numeric()))
  agg <- aggregate(anomaly_score ~ author_name, data = anomalies, FUN = length)
  names(agg) <- c("author_name", "ml_anomaly_count")
  agg[order(-agg$ml_anomaly_count), ]
}