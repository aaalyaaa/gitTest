# ml_analysis.R
library(dplyr)
library(forecast)
library(cluster)
library(ggplot2)

# Исправленный оптимальный выбор числа кластеров (метод локтя + силуэт)
optimal_clusters <- function(data, max_clusters = 10) {
  if (missing(data)) {
    return(git_error("invalid_argument", "data не может быть пропущен"))
  }
  if (is_git_error(data)) return(data)
  
  if (nrow(data) < 3) return(2)
  max_k <- min(max_clusters, nrow(data) - 1)
  if (max_k < 2) return(2)
  
  wss <- sapply(1:max_k, function(k) {
    kmeans(data, centers = k, nstart = 10)$tot.withinss
  })
  
  # Метод локтя
  if (length(wss) >= 3) {
    diffs <- diff(wss)
    diffs2 <- diff(diffs)
    elbow <- which.max(diffs2) + 1
    if (!is.na(elbow) && elbow >= 2 && elbow <= max_k) return(elbow)
  }
  
  # Силуэт
  sil_scores <- sapply(2:max_k, function(k) {
    km <- kmeans(data, centers = k, nstart = 10)
    ss <- silhouette(km$cluster, dist(data))
    mean(ss[, 3])
  })
  best_k <- which.max(sil_scores) + 1
  return(best_k)
}

prepare_clustering_data <- function(conn) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  query <- "
    SELECT 
        author_name,
        total_commits,
        active_days,
        avg_commit_hour,
        night_commits,
        total_added,
        total_deleted,
        avg_add_per_commit,
        avg_del_per_commit,
        unique_files,
        sensitive_commits_count as sensitive_changes
    FROM developer_metrics
  "
  df <- tryCatch(
    DBI::dbGetQuery(conn, query),
    error = function(e) git_error("db_error", paste("Ошибка подготовки данных:", e$message))
  )
  if (is_git_error(df)) return(df)
  if (nrow(df) == 0) {
    return(git_error("no_data_error", "Нет данных для ML анализа"))
  }
  
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
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  ml_data <- prepare_clustering_data(conn)
  if (is_git_error(ml_data)) return(ml_data)
  
  dev_count <- nrow(ml_data$data)
  if (dev_count < 3) {
    return(git_error("insufficient_data_error", "Разработчиков меньше 3 для кластеризации"))
  }
  
  df_scaled <- ml_data$scaled[, ml_data$features, drop = FALSE]
  df_scaled <- df_scaled[complete.cases(df_scaled), ]
  if (nrow(df_scaled) < 3) {
    return(git_error("insufficient_data_error", "Недостаточно данных после очистки"))
  }
  
  if (is.null(n_clusters)) {
    n_clusters <- optimal_clusters(df_scaled, max_clusters = min(5, dev_count - 1))
  }
  n_clusters <- min(n_clusters, nrow(df_scaled) - 1)
  if (n_clusters < 2) n_clusters <- 2
  
  set.seed(123)
  kmeans_result <- tryCatch(
    kmeans(df_scaled, centers = n_clusters, nstart = 25),
    error = function(e) git_error("clustering_error", paste("Ошибка k-means:", e$message))
  )
  if (is_git_error(kmeans_result)) return(kmeans_result)
  
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
}

forecast_developer_activity <- function(conn, author_name, forecast_days = 7) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  if (missing(author_name) || is.null(author_name) || author_name == "") {
    return(git_error("invalid_argument", "author_name обязателен"))
  }
  
  query <- sprintf("
    SELECT CAST(date AS DATE) as commit_date, COUNT(*) as commits
    FROM git_commit_history
    WHERE author_name = '%s'
    GROUP BY commit_date ORDER BY commit_date
  ", author_name)
  df <- tryCatch(
    DBI::dbGetQuery(conn, query),
    error = function(e) git_error("db_error", paste("Ошибка запроса истории:", e$message))
  )
  if (is_git_error(df)) return(df)
  if (nrow(df) < 3) {
    return(git_error("insufficient_data_error", paste("Недостаточно данных для", author_name)))
  }
  
  ts_data <- ts(df$commits, frequency = 7)
  fit <- tryCatch(auto.arima(ts_data, seasonal = TRUE), error = function(e) NULL)
  if (is.null(fit)) {
    fit <- tryCatch(ets(ts_data), error = function(e) NULL)
  }
  if (is.null(fit)) {
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
}

plot_forecast <- function(forecast_result) {
  if (missing(forecast_result)) {
    cat("Ошибка: forecast_result не передан\n")
    return(invisible(NULL))
  }
  if (is_git_error(forecast_result)) {
    cat("⚠️", forecast_result$message, "\n")
    return(invisible(NULL))
  }
  plot_data <- forecast_result$plot_data
  if (all(is.na(plot_data$forecast))) {
    cat("Нет данных для прогноза\n")
    return(invisible(NULL))
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
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  where <- if (!is.null(author_name)) sprintf("WHERE author_name = '%s'", author_name) else ""
  query <- sprintf("
    SELECT EXTRACT(HOUR FROM date) as hour, COUNT(*) as commits
    FROM git_commit_history %s
    GROUP BY hour ORDER BY hour
  ", where)
  hour_data <- tryCatch(
    DBI::dbGetQuery(conn, query),
    error = function(e) git_error("db_error", paste("Ошибка сезонности:", e$message))
  )
  if (is_git_error(hour_data)) return(hour_data)
  if (nrow(hour_data) == 0) {
    return(git_error("no_data_error", "Нет данных для анализа сезонности"))
  }
  peak_hours <- hour_data[order(-hour_data$commits), ][1:3, ]
  list(by_hour = hour_data, peak_hours = peak_hours)
}

compare_with_team <- function(conn, author_name) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  if (missing(author_name) || is.null(author_name) || author_name == "") {
    return(git_error("invalid_argument", "author_name обязателен"))
  }
  team <- tryCatch(
    DBI::dbGetQuery(conn, "
      SELECT AVG(total_commits) as avg_commits, AVG(avg_commit_size) as avg_size, AVG(active_days) as avg_days
      FROM developer_metrics
    "),
    error = function(e) git_error("db_error", paste("Ошибка получения команды:", e$message))
  )
  if (is_git_error(team)) return(team)
  
  user <- tryCatch(
    DBI::dbGetQuery(conn, sprintf("
      SELECT total_commits, avg_commit_size, active_days FROM developer_metrics WHERE author_name = '%s'
    ", author_name)),
    error = function(e) git_error("db_error", paste("Ошибка получения пользователя:", e$message))
  )
  if (is_git_error(user)) return(user)
  if (nrow(user) == 0) {
    return(git_error("no_developer_error", paste("Разработчик", author_name, "не найден")))
  }
  
  data.frame(
    metric = c("Коммитов", "Средний размер", "Активных дней"),
    developer = as.numeric(user[1, ]),
    team_avg = as.numeric(team[1, ]),
    diff_percent = round(100 * (as.numeric(user[1, ]) - as.numeric(team[1, ])) / as.numeric(team[1, ]), 1),
    status = ifelse(abs(round(100 * (as.numeric(user[1, ]) - as.numeric(team[1, ])) / as.numeric(team[1, ]), 1)) > 20,
                    ifelse(round(100 * (as.numeric(user[1, ]) - as.numeric(team[1, ])) / as.numeric(team[1, ]), 1) > 0,
                           "выше среднего", "ниже среднего"), "в норме")
  )
}

get_activity_trends <- function(conn, author_name = NULL) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  where <- if (!is.null(author_name)) sprintf("WHERE author_name = '%s'", author_name) else ""
  query <- sprintf("
    SELECT DATE_TRUNC('month', date) as month, COUNT(*) as commits
    FROM git_commit_history %s
    GROUP BY month ORDER BY month
  ", where)
  df <- tryCatch(
    DBI::dbGetQuery(conn, query),
    error = function(e) git_error("db_error", paste("Ошибка трендов:", e$message))
  )
  if (is_git_error(df)) return(df)
  if (nrow(df) < 2) {
    return(git_error("insufficient_data_error", "Недостаточно месяцев для анализа тренда"))
  }
  df$trend <- c(NA, diff(df$commits))
  df$direction <- ifelse(df$trend > 0, "рост", ifelse(df$trend < 0, "падение", "стабильно"))
  overall <- ifelse(coef(lm(commits ~ seq_len(nrow(df)), data = df))[2] > 0, "общий тренд: РОСТ", "общий тренд: ПАДЕНИЕ")
  list(monthly_data = df, overall_trend = overall,
       best_month = df[which.max(df$commits), ],
       worst_month = df[which.min(df$commits), ])
}

#' Обнаружение ML-аномалий с возвратом признаков и объяснением
#' @param conn Подключение к DuckDB
#' @param author_name Опционально фильтр по автору
#' @param threshold Порог аномальности (по умолчанию 0.95)
#' @param return_features Возвращать ли признаки (hour, dow, n_files, commit_size, add_del_ratio)
#' @return data.frame с аномалиями (включая признаки и explanation) или git_error
get_ml_anomalies <- function(conn, author_name = NULL, threshold = 0.95, return_features = TRUE) {
  if (!requireNamespace("solitude", quietly = TRUE)) {
    return(git_error("missing_package", "Пакет 'solitude' не установлен. Установите: install.packages('solitude')"))
  }
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  
  # Получаем данные с признаками
  features_df <- prepare_anomaly_features(conn, author_name)
  if (is_git_error(features_df)) return(features_df)
  if (nrow(features_df) == 0) {
    return(data.frame())  # нет данных – не ошибка
  }
  
  # Обучаем Isolation Forest
  X <- features_df[, c("hour", "dow", "n_files", "commit_size", "add_del_ratio")]
  iso <- solitude::isolationForest$new(sample_size = min(nrow(X), 10000), num_trees = 100)
  iso$fit(X)
  scores <- iso$predict(X)
  features_df$anomaly_score <- scores$anomaly_score
  
  # Отбираем аномалии по порогу
  quantile_thresh <- quantile(features_df$anomaly_score, probs = threshold, na.rm = TRUE)
  anomalies <- features_df[features_df$anomaly_score >= quantile_thresh, ]
  
  if (nrow(anomalies) == 0) {
    cat(sprintf("ML-аномалий не найдено (порог: %.2f%%)\n", threshold * 100))
    return(data.frame())
  }
  
  # Формируем человеко-читаемое объяснение
  anomalies$explanation <- apply(anomalies, 1, function(row) {
    reasons <- c()
    hour <- as.numeric(row["hour"])
    dow <- as.numeric(row["dow"])
    n_files <- as.numeric(row["n_files"])
    commit_size <- as.numeric(row["commit_size"])
    add_del_ratio <- as.numeric(row["add_del_ratio"])
    
    if (hour < 6 || hour > 22) reasons <- c(reasons, "ночное время")
    if (dow %in% c(0, 6)) reasons <- c(reasons, "выходной день")
    if (commit_size > 500) reasons <- c(reasons, paste0("очень большой коммит (", commit_size, " строк)"))
    if (commit_size < 10 && commit_size > 0) reasons <- c(reasons, paste0("очень маленький коммит (", commit_size, " строк)"))
    if (n_files > 5) reasons <- c(reasons, paste0("много файлов (", n_files, ")"))
    if (add_del_ratio > 3) reasons <- c(reasons, paste0("сильный перекос в добавлениях (", round(add_del_ratio, 1), ")"))
    if (add_del_ratio < 0.33) reasons <- c(reasons, paste0("сильный перекос в удалениях (", round(add_del_ratio, 1), ")"))
    if (add_del_ratio == 999) reasons <- c(reasons, "только удаления, без добавлений")
    
    if (length(reasons) == 0) {
      paste("Аномальная комбинация признаков (оценка", round(as.numeric(row["anomaly_score"]), 3), ")")
    } else {
      paste("Необычно:", paste(reasons, collapse = ", "), 
            "(оценка", round(as.numeric(row["anomaly_score"]), 3), ")")
    }
  })
  
  anomalies$anomaly_type <- "ml_anomaly"
  anomalies$description <- paste(
    "ML-аномалия: оценка", round(anomalies$anomaly_score, 3),
    "(порог", round(quantile_thresh, 3), ")"
  )
  
  # Базовый результат
  result <- data.frame(
    author_name = anomalies$author_name,
    date = anomalies$date,
    anomaly_type = anomalies$anomaly_type,
    description = anomalies$description,
    explanation = anomalies$explanation,
    anomaly_score = anomalies$anomaly_score,
    commit = anomalies$commit,
    stringsAsFactors = FALSE
  )
  
  # Добавляем признаки по желанию
  if (return_features && nrow(anomalies) > 0) {
    result$hour <- anomalies$hour
    result$dow <- anomalies$dow
    result$n_files <- anomalies$n_files
    result$commit_size <- anomalies$commit_size
    result$add_del_ratio <- anomalies$add_del_ratio
  }
  
  cat(sprintf("Найдено %d ML-аномалий (порог: %.2f%%)\n", nrow(result), threshold * 100))
  return(result)
}

prepare_anomaly_features <- function(conn, author_name = NULL) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  where_clause <- if (!is.null(author_name)) {
    sprintf("WHERE c.author_name = '%s'", author_name)
  } else ""
  
  query <- sprintf("
    SELECT 
        c.commit,
        c.author_name,
        c.date,
        EXTRACT(HOUR FROM c.date) AS hour,
        EXTRACT(DOW FROM c.date) AS dow,
        COUNT(DISTINCT COALESCE(d.src_file, d.dst_file)) AS n_files,
        SUM(d.count_add + d.count_del) AS commit_size,
        SUM(d.count_add) / NULLIF(SUM(d.count_del), 0) AS add_del_ratio
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    %s
    GROUP BY c.commit, c.author_name, c.date
  ", where_clause)
  
  df <- tryCatch(
    DBI::dbGetQuery(conn, query),
    error = function(e) git_error("db_error", paste("Ошибка подготовки признаков:", e$message))
  )
  if (is_git_error(df)) return(df)
  df$add_del_ratio[is.infinite(df$add_del_ratio)] <- 999
  df$add_del_ratio[is.na(df$add_del_ratio)] <- 1
  df
}

summary_ml_anomalies <- function(anomalies) {
  if (missing(anomalies)) {
    return(git_error("invalid_argument", "anomalies не может быть пропущен"))
  }
  if (is_git_error(anomalies)) return(anomalies)
  if (nrow(anomalies) == 0) {
    return(data.frame(author_name = character(), ml_anomaly_count = numeric()))
  }
  agg <- aggregate(anomaly_score ~ author_name, data = anomalies, FUN = length)
  names(agg) <- c("author_name", "ml_anomaly_count")
  agg[order(-agg$ml_anomaly_count), ]
}