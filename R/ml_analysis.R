library(dplyr)
library(tidyr)
library(forecast)
library(cluster)
library(randomForest)
library(factoextra)
library(ggplot2)

git_error <- function(class, message, ...) {
  structure(
    list(message = message, ...),
    class = c(class, "error", "condition")
  )
}

optimal_clusters <- function(data, max_clusters = 10) {
  if (nrow(data) < 3) return(2)
  wss <- sapply(1:min(max_clusters, nrow(data)-1), function(k) {
    kmeans(data, centers = k, nstart = 10, iter.max = 50)$tot.withinss
  })
  if (length(wss) < 2) return(2)
  diffs <- diff(wss)
  if (length(diffs) < 2) return(2)
  elbow <- which.min(diffs[2:length(diffs)]) + 1
  return(min(max(elbow, 2), max_clusters))
}

prepare_clustering_data <- function(conn) {
  query <- "
    WITH developer_metrics AS (
      SELECT 
          c.author_name,
          COUNT(DISTINCT c.commit) as total_commits,
          COUNT(DISTINCT SUBSTR(CAST(c.date AS VARCHAR), 1, 10)) as active_days,
          AVG(CAST(SUBSTR(CAST(c.date AS VARCHAR), 12, 2) AS INTEGER)) as avg_commit_hour,
          SUM(CASE WHEN CAST(SUBSTR(CAST(c.date AS VARCHAR), 12, 2) AS INTEGER) BETWEEN 0 AND 5 
               THEN 1 ELSE 0 END) as night_commits,
          SUM(d.count_add) as total_added,
          SUM(d.count_del) as total_deleted,
          AVG(d.count_add) as avg_add_per_commit,
          AVG(d.count_del) as avg_del_per_commit,
          COUNT(DISTINCT d.src_file) as unique_files,
          SUM(CASE WHEN d.src_file LIKE '%.env%' OR d.src_file LIKE '%.key%' OR d.src_file LIKE '%secret%' OR d.src_file LIKE '%password%' THEN 1 ELSE 0 END) as sensitive_changes
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      GROUP BY c.author_name
    )
    SELECT * FROM developer_metrics
  "
  
  df <- DBI::dbGetQuery(conn, query)
  
  if (nrow(df) == 0) {
    return(git_error("no_data_error", "Нет данных для ML анализа"))
  }
  
  df_scaled <- df
  numeric_cols <- c("total_commits", "active_days", "avg_commit_hour", 
                    "night_commits", "total_added", "total_deleted", 
                    "avg_add_per_commit", "avg_del_per_commit",
                    "unique_files", "sensitive_changes")
  
  for (col in numeric_cols) {
    if (sd(df[[col]], na.rm = TRUE) > 0 && !is.na(sd(df[[col]], na.rm = TRUE))) {
      df_scaled[[col]] <- scale(df[[col]])
    } else {
      df_scaled[[col]] <- 0
    }
  }
  
  return(list(
    data = df,
    scaled = df_scaled,
    features = numeric_cols
  ))
}

cluster_developers <- function(conn, n_clusters = NULL) {
  tryCatch({
    ml_data <- prepare_clustering_data(conn)
    if (inherits(ml_data, "error")) {
      return(ml_data)
    }
    
    dev_count <- nrow(ml_data$data)
    if (dev_count < 3) {
      return(git_error("insufficient_data_error", 
                       sprintf("Невозможно выполнить кластеризацию: разработчиков (%d) меньше 3.", dev_count)))
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
    kmeans_result <- kmeans(df_scaled, centers = n_clusters, nstart = 25)
    
    ml_data$data$cluster <- kmeans_result$cluster
    
    cluster_profiles <- aggregate(ml_data$data[, ml_data$features], 
                                  by = list(cluster = ml_data$data$cluster), 
                                  FUN = mean)
    
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
    
    return(list(
      clustering = kmeans_result,
      data = ml_data$data,
      cluster_profiles = cluster_profiles,
      cluster_stats = cluster_stats,
      features = ml_data$features,
      n_clusters = n_clusters
    ))
    
  }, error = function(e) {
    return(git_error("clustering_error", paste("Ошибка кластеризации:", e$message)))
  })
}

cluster_commits <- function(conn, n_clusters = NULL) {
  tryCatch({
    query <- "
      SELECT 
          c.commit,
          c.author_name,
          COUNT(*) as files_changed,
          SUM(d.count_add) as lines_added,
          SUM(d.count_del) as lines_deleted,
          SUM(d.count_add + d.count_del) as total_changes
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      GROUP BY c.commit, c.author_name
    "
    
    df <- DBI::dbGetQuery(conn, query)
    
    if (nrow(df) < 5) {
      return(git_error("insufficient_data_error", 
                       sprintf("Невозможно выполнить кластеризацию коммитов: коммитов (%d) меньше 5.", nrow(df))))
    }
    
    features <- df[, c("files_changed", "lines_added", "lines_deleted", "total_changes")]
    features_scaled <- scale(features)
    features_scaled <- features_scaled[complete.cases(features_scaled), ]
    
    if (is.null(n_clusters)) {
      n_clusters <- optimal_clusters(features_scaled, max_clusters = min(5, nrow(df) - 1))
    }
    
    n_clusters <- min(n_clusters, nrow(df) - 1)
    if (n_clusters < 2) n_clusters <- 2
    
    set.seed(123)
    clusters <- kmeans(features_scaled, centers = n_clusters, nstart = 10)
    
    df$commit_cluster <- clusters$cluster
    
    cluster_summary <- df %>%
      group_by(commit_cluster) %>%
      summarise(
        avg_files = mean(files_changed),
        avg_lines = mean(total_changes),
        тип = case_when(
          mean(total_changes) < 50 ~ "маленький коммит",
          mean(total_changes) < 200 ~ "средний коммит",
          TRUE ~ "большой коммит"
        ),
        count = n()
      )
    
    return(list(
      commits_with_clusters = df,
      cluster_summary = cluster_summary,
      n_clusters = n_clusters
    ))
    
  }, error = function(e) {
    return(git_error("clustering_error", paste("Ошибка кластеризации коммитов:", e$message)))
  })
}

classify_commit_type_by_metrics <- function(conn) {
  tryCatch({
    query <- "
      SELECT 
          c.commit,
          c.author_name,
          COUNT(*) as files_changed,
          SUM(d.count_add) as lines_added,
          SUM(d.count_del) as lines_deleted,
          SUM(d.count_add + d.count_del) as total_changes,
          SUM(CASE WHEN d.src_file LIKE '%.env%' OR d.src_file LIKE '%.key%' OR d.src_file LIKE '%secret%' OR d.src_file LIKE '%password%' THEN 1 ELSE 0 END) as sensitive_count,
          SUM(CASE WHEN d.src_file IS NULL THEN 1 ELSE 0 END) as new_files_count,
          SUM(CASE WHEN d.dst_file IS NULL THEN 1 ELSE 0 END) as deleted_files_count
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      GROUP BY c.commit, c.author_name
    "
    
    df <- DBI::dbGetQuery(conn, query)
    
    if (nrow(df) < 5) {
      return(git_error("insufficient_data_error", 
                       sprintf("Невозможно классифицировать коммиты: коммитов (%d) меньше 5.", nrow(df))))
    }
    
    df$predicted_type <- case_when(
      df$files_changed > 10 & df$total_changes > 500 ~ "крупное_изменение",
      df$new_files_count > 0 & df$deleted_files_count == 0 ~ "добавление_файлов",
      df$deleted_files_count > 0 & df$new_files_count == 0 ~ "удаление_файлов",
      df$sensitive_count > 0 ~ "чувствительное_изменение",
      df$files_changed == 1 & df$total_changes < 50 ~ "точечная_правка",
      df$lines_added / (df$lines_deleted + 1) > 5 ~ "добавление_кода",
      df$lines_deleted / (df$lines_added + 1) > 3 ~ "удаление_кода",
      TRUE ~ "обычный_коммит"
    )
    
    type_stats <- df %>%
      group_by(predicted_type) %>%
      summarise(count = n(), percentage = round(100 * n() / nrow(df), 1))
    
    return(list(
      commits_with_types = df,
      type_stats = type_stats
    ))
    
  }, error = function(e) {
    return(git_error("classification_error", paste("Ошибка классификации:", e$message)))
  })
}

forecast_developer_activity <- function(conn, author_name, forecast_days = 7) {
  tryCatch({
    query <- sprintf("
      SELECT 
          SUBSTR(CAST(c.date AS VARCHAR), 1, 10) as commit_date,
          COUNT(*) as commits
      FROM git_commit_history c
      WHERE c.author_name = '%s'
      GROUP BY commit_date
      ORDER BY commit_date
    ", author_name)
    
    df <- DBI::dbGetQuery(conn, query)
    
    if (nrow(df) < 3) {
      return(git_error("insufficient_data_error", 
                       sprintf("Невозможно построить прогноз для %s: дней с активностью (%d) меньше 3.", author_name, nrow(df))))
    }
    
    ts_data <- ts(df$commits, frequency = 1)
    
    fit <- tryCatch({
      auto.arima(ts_data, seasonal = FALSE)
    }, error = function(e) {
      tryCatch({
        arima(ts_data, order = c(1,0,1))
      }, error = function(e2) NULL)
    })
    
    if (is.null(fit)) {
      return(git_error("model_error", "Не удалось построить модель ARIMA. Данных слишком мало или они нестационарны."))
    }
    
    forecast_result <- forecast(fit, h = forecast_days)
    
    plot_data <- data.frame(
      day = 1:forecast_days,
      forecast = as.numeric(forecast_result$mean),
      lower = as.numeric(forecast_result$lower[, 2]),
      upper = as.numeric(forecast_result$upper[, 2])
    )
    
    expected <- round(sum(forecast_result$mean, na.rm = TRUE), 1)
    
    return(list(
      author = author_name,
      historical = df,
      forecast = forecast_result,
      expected_commits_next_week = expected,
      plot_data = plot_data
    ))
    
  }, error = function(e) {
    return(git_error("forecast_error", paste("Ошибка прогнозирования:", e$message)))
  })
}

plot_forecast <- function(forecast_result) {
  if (inherits(forecast_result, "error")) {
    cat("⚠️", forecast_result$message, "\n")
    return(invisible(NULL))
  }
  
  if (is.null(forecast_result$plot_data) || nrow(forecast_result$plot_data) == 0) {
    cat("Нет данных для построения графика\n")
    return(invisible(NULL))
  }
  
  plot_data <- forecast_result$plot_data
  
  if (all(is.na(plot_data$forecast)) || sum(plot_data$forecast, na.rm = TRUE) == 0) {
    cat("Нет значимых данных для прогноза\n")
    return(invisible(NULL))
  }
  
  y_max <- max(plot_data$upper, na.rm = TRUE) + 1
  if (is.infinite(y_max) || is.na(y_max)) y_max <- max(plot_data$forecast, na.rm = TRUE) + 1
  
  plot(plot_data$day, plot_data$forecast, 
       type = "b", 
       col = "blue",
       pch = 19,
       main = paste("Прогноз активности для", forecast_result$author),
       xlab = "Дни вперед", 
       ylab = "Количество коммитов",
       ylim = c(0, y_max))
  
  if (!all(is.na(plot_data$lower)) && !all(is.na(plot_data$upper))) {
    lines(plot_data$day, plot_data$lower, col = "red", lty = 2)
    lines(plot_data$day, plot_data$upper, col = "red", lty = 2)
    legend("topright", legend = c("Прогноз", "95% доверительный интервал"), 
           col = c("blue", "red"), lty = c(1, 2), cex = 0.8)
  }
  
  mtext(paste("Ожидается коммитов на неделе:", forecast_result$expected_commits_next_week), 
        side = 3, line = 0.5, cex = 0.9)
}

plot_forecast_ggplot <- function(forecast_result) {
  if (inherits(forecast_result, "error")) {
    cat("⚠️", forecast_result$message, "\n")
    return(invisible(NULL))
  }
  
  if (is.null(forecast_result$plot_data) || nrow(forecast_result$plot_data) == 0) {
    cat("Нет данных для построения графика\n")
    return(invisible(NULL))
  }
  
  plot_data <- forecast_result$plot_data
  
  if (all(is.na(plot_data$forecast))) {
    cat("Все значения прогноза - NA\n")
    return(invisible(NULL))
  }
  
  p <- ggplot(plot_data, aes(x = day)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70", alpha = 0.5) +
    geom_line(aes(y = forecast), color = "blue", size = 1) +
    geom_point(aes(y = forecast), color = "blue", size = 2) +
    labs(title = paste("Прогноз активности для", forecast_result$author),
         subtitle = paste("Ожидается коммитов на неделе:", forecast_result$expected_commits_next_week),
         x = "Дни вперед", 
         y = "Количество коммитов") +
    theme_minimal() +
    ylim(0, max(plot_data$upper, na.rm = TRUE) + 1)
  
  print(p)
}

get_activity_seasonality <- function(conn, author_name = NULL) {
  tryCatch({
    where_clause <- if (!is.null(author_name)) {
      sprintf("WHERE c.author_name = '%s'", author_name)
    } else ""
    
    query_hour <- sprintf("
      SELECT 
          CAST(SUBSTR(CAST(c.date AS VARCHAR), 12, 2) AS INTEGER) as hour,
          COUNT(*) as commits
      FROM git_commit_history c
      %s
      GROUP BY hour
      ORDER BY hour
    ", where_clause)
    
    hour_data <- DBI::dbGetQuery(conn, query_hour)
    
    if (nrow(hour_data) == 0) {
      return(git_error("no_data_error", "Нет данных для анализа сезонности"))
    }
    
    peak_hours <- hour_data[order(-hour_data$commits), ][1:3, ]
    
    return(list(
      by_hour = hour_data,
      peak_hours = peak_hours
    ))
    
  }, error = function(e) {
    return(git_error("seasonality_error", paste("Ошибка анализа сезонности:", e$message)))
  })
}
compare_with_team <- function(conn, author_name) {
  tryCatch({
    query_team <- "
      SELECT 
          COUNT(DISTINCT c.commit) as commits,
          AVG(d.count_add + d.count_del) as avg_commit_size,
          COUNT(DISTINCT SUBSTR(CAST(c.date AS VARCHAR), 1, 10)) as active_days
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
    "
    
    query_user <- sprintf("
      SELECT 
          COUNT(DISTINCT c.commit) as commits,
          AVG(d.count_add + d.count_del) as avg_commit_size,
          COUNT(DISTINCT SUBSTR(CAST(c.date AS VARCHAR), 1, 10)) as active_days
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      WHERE c.author_name = '%s'
    ", author_name)
    
    team <- DBI::dbGetQuery(conn, query_team)
    user <- DBI::dbGetQuery(conn, query_user)
    
    if (nrow(user) == 0) {
      return(git_error("no_developer_error", sprintf("Разработчик '%s' не найден", author_name)))
    }
    
    if (is.na(team$commits[1]) || team$commits[1] == 0) {
      return(git_error("insufficient_team_error", "Недостаточно данных для сравнения с командой"))
    }
    
    comparison <- data.frame(
      metric = c("Коммитов", "Средний размер", "Активных дней"),
      developer = as.numeric(user[1, ]),
      team_avg = as.numeric(team[1, ]),
      difference_percent = round(100 * (as.numeric(user[1, ]) - as.numeric(team[1, ])) / as.numeric(team[1, ]), 1)
    )
    
    comparison$status <- ifelse(comparison$difference_percent > 20, "выше среднего",
                                ifelse(comparison$difference_percent < -20, "ниже среднего", "в норме"))
    
    return(comparison)
    
  }, error = function(e) {
    return(git_error("comparison_error", paste("Ошибка сравнения:", e$message)))
  })
}

get_activity_trends <- function(conn, author_name = NULL) {
  tryCatch({
    where_clause <- if (!is.null(author_name)) {
      sprintf("WHERE c.author_name = '%s'", author_name)
    } else ""
    
    query <- sprintf("
      SELECT 
          SUBSTR(CAST(c.date AS VARCHAR), 1, 7) as month,
          COUNT(DISTINCT c.commit) as commits,
          AVG(d.count_add + d.count_del) as avg_size,
          COUNT(DISTINCT SUBSTR(CAST(c.date AS VARCHAR), 1, 10)) as active_days
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      %s
      GROUP BY month
      ORDER BY month
    ", where_clause)
    
    df <- DBI::dbGetQuery(conn, query)
    
    if (nrow(df) < 2) {
      return(git_error("insufficient_data_error", 
                       sprintf("Невозможно построить тренд: месяцев (%d) меньше 2.", nrow(df))))
    }
    
    df$trend_commits <- c(NA, diff(df$commits))
    df$trend_direction <- ifelse(df$trend_commits > 0, "рост",
                                 ifelse(df$trend_commits < 0, "падение", "стабильно"))
    
    overall_trend <- ifelse(coef(lm(commits ~ seq_len(nrow(df)), data = df))[2] > 0, 
                            "общий тренд: РОСТ", "общий тренд: ПАДЕНИЕ")
    
    return(list(
      monthly_data = df,
      overall_trend = overall_trend,
      best_month = df[which.max(df$commits), ],
      worst_month = df[which.min(df$commits), ]
    ))
    
  }, error = function(e) {
    return(git_error("trend_error", paste("Ошибка анализа трендов:", e$message)))
  })
}

run_anomalies_from_file <- function(conn, username = NULL) {
  if (!file.exists("R/anomalies_detection.R")) {
    return(git_error("file_not_found", "Файл anomalies_detection.R не найден"))
  }
  
  source("R/anomalies_detection.R")
  
  if (exists("get_all_anomalies")) {
    anomalies <- get_all_anomalies(conn, username = username)
    return(anomalies)
  } else {
    return(git_error("function_not_found", "Функция get_all_anomalies не найдена в anomalies_detection.R"))
  }
}