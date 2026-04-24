# ml_analysis.R
library(forecast)
library(cluster)

# ---- Подготовка данных для кластеризации (общая) ----
prepare_clustering_data <- function(conn, since = NULL, until = NULL) {
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
        unique_files
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
                    "unique_files")
  for (col in numeric_cols) {
    if (sd(df[[col]], na.rm = TRUE) > 0) {
      df_scaled[[col]] <- scale(df[[col]])
    } else {
      df_scaled[[col]] <- 0
    }
  }
  list(data = df, scaled = df_scaled, features = numeric_cols)
}

# ---- DBSCAN кластеризация ----
cluster_developers_dbscan <- function(conn, eps = 1.5, minPts = 3) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  ml_data <- prepare_clustering_data(conn)
  if (is_git_error(ml_data)) return(ml_data)
  
  dev_count <- nrow(ml_data$data)
  if (dev_count < 3) {
    return(git_error("insufficient_data_error", 
                     sprintf("Недостаточно разработчиков для кластеризации (требуется минимум 3, найдено %d)", dev_count)))
  }
  
  df_scaled <- ml_data$scaled[, ml_data$features, drop = FALSE]
  df_scaled <- df_scaled[complete.cases(df_scaled), ]
  if (nrow(df_scaled) < 3) {
    return(git_error("insufficient_data_error", 
                     "Недостаточно полных данных после очистки (требуется минимум 3)"))
  }
  
  if (!requireNamespace("dbscan", quietly = TRUE)) {
    return(git_error("missing_package", "Пакет 'dbscan' не установлен. Установите: install.packages('dbscan')"))
  }
  
  dbscan_result <- tryCatch(
    dbscan::dbscan(df_scaled, eps = eps, minPts = minPts),
    error = function(e) git_error("dbscan_error", paste("Ошибка DBSCAN:", e$message))
  )
  if (is_git_error(dbscan_result)) return(dbscan_result)
  
  ml_data$data$cluster <- dbscan_result$cluster
  ml_data$data$cluster_type <- ifelse(dbscan_result$cluster == 0, "Шум", paste0("Кластер ", dbscan_result$cluster))
  
  cluster_profiles <- aggregate(ml_data$data[, ml_data$features], 
                                by = list(cluster = ml_data$data$cluster), FUN = mean)
  cluster_stats <- as.data.frame(table(ml_data$data$cluster_type))
  names(cluster_stats) <- c("cluster_type", "developers_count")
  
  list(clustering = dbscan_result, data = ml_data$data,
       cluster_profiles = cluster_profiles, cluster_stats = cluster_stats,
       features = ml_data$features, eps = eps, minPts = minPts)
}

# ---- Визуализация DBSCAN ----
plot_dbscan_clusters <- function(dbscan_result) {
  if (missing(dbscan_result) || is.null(dbscan_result$data)) {
    cat("Нет данных для визуализации\n")
    return(invisible(NULL))
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cat("Установите ggplot2: install.packages('ggplot2')\n")
    return(invisible(NULL))
  }
  data <- dbscan_result$data
  numeric_cols <- dbscan_result$features
  X <- data[, numeric_cols, drop = FALSE]
  X_scaled <- scale(X)
  pca <- prcomp(X_scaled, center = TRUE, scale. = TRUE)
  pca_df <- as.data.frame(pca$x[, 1:2])
  pca_df$cluster_type <- data$cluster_type
  pca_df$author_name <- data$author_name
  pca_df$cluster <- data$cluster
  
  p <- ggplot2::ggplot(pca_df, ggplot2::aes(x = PC1, y = PC2, color = cluster_type)) +
    ggplot2::geom_point(size = 3, alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(cluster == 0, author_name, "")),
                       hjust = 0, vjust = -0.5, size = 3) +
    ggplot2::labs(title = "DBSCAN кластеризация разработчиков (PCA)",
                  color = "Тип кластера") +
    ggplot2::theme_minimal()
  print(p)
  invisible(p)
}

# ---- Прогноз ARIMA с поддержкой периода ----
forecast_developer_activity <- function(conn, author_name, forecast_days = 7, since = NULL, until = NULL) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  if (missing(author_name) || is.null(author_name) || author_name == "") {
    return(git_error("invalid_argument", "author_name обязателен"))
  }
  
  where <- sprintf("author_name = '%s'", author_name)
  if (!is.null(since)) where <- paste0(where, " AND date >= '", since, "'")
  if (!is.null(until)) where <- paste0(where, " AND date <= '", until, "'")
  
  # Получаем ежедневные коммиты
  query <- sprintf("
    SELECT date::DATE AS day, COUNT(*) AS commits
    FROM git_commit_history
    WHERE %s
    GROUP BY day
    ORDER BY day
  ", where)
  df <- tryCatch(
    DBI::dbGetQuery(conn, query),
    error = function(e) git_error("db_error", paste("Ошибка запроса истории:", e$message))
  )
  if (is_git_error(df)) return(df)
  if (nrow(df) < 2) {
    return(git_error("insufficient_data_error", paste("Недостаточно данных для", author_name)))
  }
  
  # Создаём непрерывный календарный ряд от первого до последнего дня
  all_days <- seq.Date(from = min(df$day), to = max(df$day), by = "day")
  full_df <- data.frame(day = all_days)
  full_df <- merge(full_df, df, by = "day", all.x = TRUE)
  full_df$commits[is.na(full_df$commits)] <- 0
  
  ts_data <- ts(full_df$commits, frequency = 7)
  
  # Попытка подобрать ARIMA или ETS
  fit <- tryCatch(auto.arima(ts_data, seasonal = TRUE), error = function(e) NULL)
  if (is.null(fit)) {
    fit <- tryCatch(ets(ts_data), error = function(e) NULL)
  }
  if (is.null(fit)) {
    # Если ничего не подошло – предупреждение и среднее
    warning("Не удалось подобрать модель ARIMA/ETS, используется среднее значение")
    forecast_mean <- rep(mean(full_df$commits), forecast_days)
    lower_val <- mean(full_df$commits) - sd(full_df$commits)
    upper_val <- mean(full_df$commits) + sd(full_df$commits)
    forecast_obj <- list(
      mean = forecast_mean,
      lower = matrix(rep(lower_val, each = 2), nrow = forecast_days, ncol = 2, byrow = TRUE),
      upper = matrix(rep(upper_val, each = 2), nrow = forecast_days, ncol = 2, byrow = TRUE)
    )
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
  
  # Добавляем предупреждения, если данные сильно разрежены
  warnings_list <- list()
  if (nrow(full_df) < 30) {
    warnings_list <- c(warnings_list, "Мало исторических данных (<30 дней). Прогноз может быть неточным.")
  }
  if (mean(full_df$commits == 0) > 0.5) {
    warnings_list <- c(warnings_list, "Более половины дней без коммитов – прогноз будет много нулей.")
  }
  
  list(author = author_name, historical = full_df, forecast = forecast_obj,
       expected_commits_next_week = expected, plot_data = plot_data,
       warnings = warnings_list)
}

forecast_activity_xgboost <- function(conn, author_name, forecast_days = 7, 
                                      features_lag = 7, nrounds = 100, since = NULL, until = NULL) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    return(git_error("missing_package", "Пакет 'xgboost' не установлен. Установите: install.packages('xgboost')"))
  }
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  if (missing(author_name) || is.null(author_name) || author_name == "") {
    return(git_error("invalid_argument", "author_name обязателен"))
  }
  
  where <- sprintf("author_name = '%s'", author_name)
  if (!is.null(since)) where <- paste0(where, " AND date >= '", since, "'")
  if (!is.null(until)) where <- paste0(where, " AND date <= '", until, "'")
  
  query <- sprintf("
    SELECT date::DATE AS ds, COUNT(*) AS y
    FROM git_commit_history
    WHERE %s
    GROUP BY date::DATE
    ORDER BY ds
  ", where)
  df <- tryCatch(
    DBI::dbGetQuery(conn, query),
    error = function(e) git_error("db_error", paste("Ошибка загрузки данных:", e$message))
  )
  if (is_git_error(df)) return(df)
  if (nrow(df) < 30) {
    return(git_error("insufficient_data_error", paste("Недостаточно данных для XGBoost (нужно >30 дней)", author_name)))
  }
  
  df <- df[order(df$ds), ]
  df$y <- as.numeric(df$y)
  
  # Функция создания признаков
  create_features <- function(data, lag) {
    for (i in 1:lag) {
      data[[paste0("lag_", i)]] <- dplyr::lag(data$y, i)
    }
    data$dow <- as.numeric(format(data$ds, "%u"))
    data$month <- as.numeric(format(data$ds, "%m"))
    data$dayofyear <- as.numeric(format(data$ds, "%j"))
    if (requireNamespace("zoo", quietly = TRUE)) {
      data$ma7 <- zoo::rollmean(data$y, k = 7, fill = NA, align = "right")
      data$ma7_lag1 <- dplyr::lag(data$ma7, 1)
    } else {
      data$ma7 <- NA
      data$ma7_lag1 <- NA
    }
    data
  }
  
  df_feat <- create_features(df, features_lag)
  df_feat <- df_feat[complete.cases(df_feat), ]
  if (nrow(df_feat) < 10) {
    return(git_error("insufficient_data_error", "Недостаточно полных наблюдений после создания признаков"))
  }
  
  x_cols <- setdiff(names(df_feat), c("ds", "y"))
  X <- as.matrix(df_feat[, x_cols])
  y <- df_feat$y
  
  # ----- ОБУЧЕНИЕ МОДЕЛИ (новый API: x и y) -----
  model <- xgboost::xgboost(
    x = X,
    y = y,
    nrounds = nrounds,
    objective = "reg:squarederror",
    learning_rate = 0.1,
    max_depth = 3,
    verbosity = 0
  )
  # -----------------------------------------------
  
  # ----- Рекурсивный прогноз -----
  last_known <- tail(df_feat, 1)
  predictions <- numeric(forecast_days)
  
  for (i in 1:forecast_days) {
    X_pred <- as.matrix(last_known[, x_cols, drop = FALSE])
    pred <- predict(model, X_pred)
    pred_val <- max(0, round(pred))
    predictions[i] <- pred_val
    if (i == forecast_days) break
    
    # --- Обновление признаков для следующего дня ---
    next_row <- last_known
    # Сдвиг лагов
    for (j in seq(features_lag, 1)) {
      if (j == 1) {
        next_row[[paste0("lag_", j)]] <- pred_val
      } else {
        next_row[[paste0("lag_", j)]] <- last_known[[paste0("lag_", j-1)]]
      }
    }
    # Скользящее среднее
    if (requireNamespace("zoo", quietly = TRUE)) {
      hist_y <- tail(df_feat$y, 6)
      window_vals <- c(hist_y, pred_val)
      next_row$ma7 <- mean(window_vals)
      next_row$ma7_lag1 <- last_known$ma7
    }
    # Календарь
    next_date <- last_known$ds + 1
    next_row$ds <- next_date
    next_row$dow <- as.numeric(format(next_date, "%u"))
    next_row$month <- as.numeric(format(next_date, "%m"))
    next_row$dayofyear <- as.numeric(format(next_date, "%j"))
    
    last_known <- next_row
  }
  
  expected <- sum(predictions)
  list(author = author_name, historical = df, predictions = predictions,
       expected_commits_next_week = expected, model = model)
}
# ---- Сезонность и тренды ----
get_activity_seasonality <- function(conn, author_name = NULL, since = NULL, until = NULL) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  where <- if (!is.null(author_name)) sprintf("author_name = '%s'", author_name) else "1=1"
  if (!is.null(since)) where <- paste0(where, " AND date >= '", since, "'")
  if (!is.null(until)) where <- paste0(where, " AND date <= '", until, "'")
  
  query <- sprintf("
    SELECT EXTRACT(HOUR FROM date) as hour, COUNT(*) as commits
    FROM git_commit_history
    WHERE %s
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

compare_with_team <- function(conn, author_name, team_usernames = NULL) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  if (missing(author_name) || is.null(author_name) || author_name == "") {
    return(git_error("invalid_argument", "author_name обязателен"))
  }
  
  user <- tryCatch(
    DBI::dbGetQuery(conn, sprintf("
      SELECT total_commits, avg_commit_size, active_days 
      FROM developer_metrics 
      WHERE author_name = '%s'
    ", gsub("'", "''", author_name))),
    error = function(e) git_error("db_error", paste("Ошибка получения пользователя:", e$message))
  )
  if (is_git_error(user)) return(user)
  if (nrow(user) == 0) {
    return(git_error("no_developer_error", paste("Разработчик", author_name, "не найден")))
  }
  
  if (!is.null(team_usernames) && length(team_usernames) > 0) {
    names_quoted <- paste0("'", gsub("'", "''", team_usernames), "'", collapse = ", ")
    team_query <- sprintf("
      SELECT AVG(total_commits) as avg_commits, 
             AVG(avg_commit_size) as avg_size, 
             AVG(active_days) as avg_days
      FROM developer_metrics
      WHERE author_name IN (%s)
    ", names_quoted)
  } else {
    team_query <- "
      SELECT AVG(total_commits) as avg_commits, 
             AVG(avg_commit_size) as avg_size, 
             AVG(active_days) as avg_days
      FROM developer_metrics
    "
  }
  team <- tryCatch(
    DBI::dbGetQuery(conn, team_query),
    error = function(e) git_error("db_error", paste("Ошибка получения команды:", e$message))
  )
  if (is_git_error(team)) return(team)
  
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

get_activity_trends <- function(conn, author_name = NULL, since = NULL, until = NULL) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  where <- if (!is.null(author_name)) sprintf("author_name = '%s'", author_name) else "1=1"
  if (!is.null(since)) where <- paste0(where, " AND date >= '", since, "'")
  if (!is.null(until)) where <- paste0(where, " AND date <= '", until, "'")
  
  query <- sprintf("
    SELECT DATE_TRUNC('month', date) as month, COUNT(*) as commits
    FROM git_commit_history
    WHERE %s
    GROUP BY month ORDER BY month
  ", where)
  df <- tryCatch(
    DBI::dbGetQuery(conn, query),
    error = function(e) git_error("db_error", paste("Ошибка трендов:", e$message))
  )
  if (is_git_error(df)) return(df)
  if (nrow(df) < 2) {
    return(git_error("insufficient_data_error", 
                     "Недостаточно месяцев для анализа тренда (нужно минимум 2 месяца)"))
  }
  
  x <- seq_len(nrow(df))
  fit <- lm(commits ~ x, data = df)
  slope <- coef(fit)[2]
  avg_commits <- mean(df$commits)
  relative_change <- slope / avg_commits * 100
  
  if (slope > 0) {
    trend_text <- sprintf("РОСТ: в среднем +%.1f коммита в месяц (%.1f%% от среднего)", 
                          slope, relative_change)
    trend_direction <- "рост"
  } else {
    trend_text <- sprintf("ПАДЕНИЕ: в среднем %.1f коммита в месяц (%.1f%% от среднего)", 
                          slope, relative_change)
    trend_direction <- "падение"
  }
  
  df$trend <- c(NA, diff(df$commits))
  df$direction <- ifelse(df$trend > 0, "рост", ifelse(df$trend < 0, "падение", "стабильно"))
  
  list(monthly_data = df, 
       overall_trend = trend_text,
       trend_direction = trend_direction,
       slope_per_month = slope,
       relative_change_percent = relative_change,
       best_month = df[which.max(df$commits), ],
       worst_month = df[which.min(df$commits), ])
}

# ---- ML-аномалии и подготовка признаков ----
prepare_anomaly_features <- function(conn, author_name = NULL, since = NULL, until = NULL) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  where_clause <- if (!is.null(author_name)) {
    sprintf("c.author_name = '%s'", author_name)
  } else "1=1"
  
  if (!is.null(since)) where_clause <- paste0(where_clause, " AND c.date >= '", since, "'")
  if (!is.null(until)) where_clause <- paste0(where_clause, " AND c.date <= '", until, "'")
  
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
    WHERE %s
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

get_ml_anomalies <- function(conn, author_name = NULL, threshold = 0.95, 
                             return_features = TRUE, since = NULL, until = NULL) {
  if (!requireNamespace("solitude", quietly = TRUE)) {
    return(git_error("missing_package", "Пакет 'solitude' не установлен. Установите: install.packages('solitude')"))
  }
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  
  features_df <- prepare_anomaly_features(conn, author_name, since, until)
  if (is_git_error(features_df)) return(features_df)
  if (nrow(features_df) == 0) return(data.frame())
  
  X <- features_df[, c("hour", "dow", "n_files", "commit_size", "add_del_ratio")]
  iso <- solitude::isolationForest$new(sample_size = min(nrow(X), 10000), num_trees = 100)
  iso$fit(X)
  scores <- iso$predict(X)
  features_df$anomaly_score <- scores$anomaly_score
  
  quantile_thresh <- quantile(features_df$anomaly_score, probs = threshold, na.rm = TRUE)
  anomalies <- features_df[features_df$anomaly_score >= quantile_thresh, ]
  
  if (nrow(anomalies) == 0) {
    cat(sprintf("ML-аномалий не найдено (порог: %.2f%%)\n", threshold * 100))
    return(data.frame())
  }
  
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
  
  if (return_features && nrow(anomalies) > 0) {
    result$hour <- anomalies$hour
    result$dow <- anomalies$dow
    result$n_files <- anomalies$n_files
    result$commit_size <- anomalies$commit_size
    result$add_del_ratio <- anomalies$add_del_ratio
  }
  
  # Сортировка по убыванию anomaly_score
  result <- result[order(-result$anomaly_score), ]
  
  cat(sprintf("Найдено %d ML-аномалий (порог: %.2f%%)\n", nrow(result), threshold * 100))
  return(result)
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

plot_forecast <- function(forecast_result) {
  if (missing(forecast_result)) {
    cat("Ошибка: forecast_result не передан\n")
    return(invisible(NULL))
  }
  if (is_git_error(forecast_result)) {
    cat(forecast_result$message, "\n")
    return(invisible(NULL))
  }
  if (!is.null(forecast_result$plot_data)) {
    plot_data <- forecast_result$plot_data
    if (all(is.na(plot_data$forecast))) {
      cat("Нет данных для прогноза\n")
      return(invisible(NULL))
    }
    par(mar = c(5, 4, 4, 2) + 0.1)
    y_max <- max(plot_data$upper, na.rm = TRUE) + 1
    plot(plot_data$day, plot_data$forecast, type = "b", col = "blue", pch = 19,
         main = paste("Прогноз для", forecast_result$author),
         xlab = "Дни", ylab = "Коммиты", ylim = c(0, y_max))
    lines(plot_data$day, plot_data$lower, col = "red", lty = 2)
    lines(plot_data$day, plot_data$upper, col = "red", lty = 2)
    legend("topright", legend = c("Прогноз", "95% ДИ"), col = c("blue", "red"), lty = c(1,2))
    mtext(paste("Ожидается:", forecast_result$expected_commits_next_week, "коммитов"), side = 3)
  } else if (!is.null(forecast_result$predictions)) {
    # Для XGBoost прогноза
    preds <- forecast_result$predictions
    plot(1:length(preds), preds, type = "b", col = "blue", pch = 19,
         main = paste("Прогноз XGBoost для", forecast_result$author),
         xlab = "Дни", ylab = "Коммиты")
    mtext(paste("Ожидается:", forecast_result$expected_commits_next_week, "коммитов"), side = 3)
  } else {
    cat("Неизвестный формат прогноза\n")
  }
}