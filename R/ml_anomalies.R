get_activity_seasonality <- function(conn, author_name = NULL, since = NULL, until = NULL) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  tryCatch({
    where <- if (!is.null(author_name)) sprintf("author_name = '%s'", author_name) else "1=1"
    if (!is.null(since)) where <- paste0(where, " AND date >= '", since, "'")
    if (!is.null(until)) where <- paste0(where, " AND date <= '", until, "'")
    
    query <- sprintf("
      SELECT EXTRACT(HOUR FROM CAST(date AS TIMESTAMP)) as hour, COUNT(*) as commits
      FROM git_commit_history
      WHERE %s
      GROUP BY hour ORDER BY hour
    ", where)
    
    hour_data <- DBI::dbGetQuery(conn, query)
    if (nrow(hour_data) == 0) {
      return(git_error("no_data_error", "Нет данных для анализа сезонности"))
    }
    peak_hours <- hour_data[order(-hour_data$commits), ][1:3, ]
    list(by_hour = hour_data, peak_hours = peak_hours)
  }, error = function(e) {
    git_error("db_error", paste("Ошибка сезонности:", e$message))
  })
}

get_activity_trends <- function(conn, author_name = NULL, since = NULL, until = NULL) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  tryCatch({
    where <- if (!is.null(author_name)) sprintf("author_name = '%s'", author_name) else "1=1"
    if (!is.null(since)) where <- paste0(where, " AND date >= '", since, "'")
    if (!is.null(until)) where <- paste0(where, " AND date <= '", until, "'")
    
    query <- sprintf("
      SELECT DATE_TRUNC('month', CAST(date AS TIMESTAMP)) as month, COUNT(*) as commits
      FROM git_commit_history
      WHERE %s
      GROUP BY month ORDER BY month
    ", where)
    
    df <- DBI::dbGetQuery(conn, query)
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
  }, error = function(e) {
    git_error("db_error", paste("Ошибка трендов:", e$message))
  })
}

prepare_anomaly_features <- function(conn, author_name = NULL, since = NULL, until = NULL) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  tryCatch({
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
          EXTRACT(HOUR FROM CAST(c.date AS TIMESTAMP)) AS hour,
          EXTRACT(DOW FROM CAST(c.date AS TIMESTAMP)) AS dow,
          COUNT(DISTINCT COALESCE(d.src_file, d.dst_file)) AS n_files,
          SUM(d.count_add + d.count_del) AS commit_size,
          CASE 
            WHEN SUM(d.count_del) = 0 THEN NULL 
            ELSE SUM(d.count_add) / SUM(d.count_del) 
          END AS add_del_ratio
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      WHERE %s
      GROUP BY c.commit, c.author_name, c.date
    ", where_clause)
    
    df <- DBI::dbGetQuery(conn, query)
    if (nrow(df) == 0) {
      return(git_error("no_data_error", "Нет данных для подготовки признаков"))
    }
    med <- median(df$add_del_ratio, na.rm = TRUE)
    df$add_del_ratio[is.na(df$add_del_ratio)] <- med
    df
  }, error = function(e) {
    git_error("db_error", paste("Ошибка подготовки признаков:", e$message))
  })
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
  if (nrow(features_df) == 0) {
    cat("Нет данных для ML-аномалий\n")
    return(data.frame())
  }
  if (nrow(features_df) < 10) {
    cat(sprintf("Слишком мало коммитов (%d) для ML-анализа. Нужно минимум 10.\n", nrow(features_df)))
    return(data.frame())
  }
  
  X <- features_df[, c("hour", "dow", "n_files", "commit_size", "add_del_ratio")]
  X <- na.omit(X)
  if (nrow(X) < 10) return(data.frame())
  
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
    if (commit_size > 500) {
      reasons <- c(reasons, paste0("очень большой коммит (", commit_size, " строк)"))
    } else if (commit_size < 10 && commit_size > 0) {
      reasons <- c(reasons, paste0("очень маленький коммит (", commit_size, " строк)"))
    }
    if (n_files > 5) reasons <- c(reasons, paste0("много файлов (", n_files, ")"))
    
    if (add_del_ratio > 3) {
      if (add_del_ratio > 100) {
        reasons <- c(reasons, "почти все строки добавлены, удалений почти нет")
      } else {
        reasons <- c(reasons, paste0("добавлено в ", round(add_del_ratio, 1), " раз больше, чем удалено"))
      }
    } else if (add_del_ratio < 0.33 && add_del_ratio != 0) {
      del_ratio <- round(1 / add_del_ratio, 1)
      if (del_ratio > 100) {
        reasons <- c(reasons, "почти все строки удалены, добавлений почти нет")
      } else {
        reasons <- c(reasons, paste0("удалено в ", del_ratio, " раз больше, чем добавлено"))
      }
    }
    
    if (length(reasons) == 0) {
      "необычное сочетание признаков"
    } else {
      paste(reasons, collapse = ", ")
    }
  })
  
  result <- data.frame(
    author_name = anomalies$author_name,
    date = anomalies$date,
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
  
  result <- result[order(-result$anomaly_score), ]
  total_found <- nrow(result)
  cat(sprintf("Найдено %d ML-аномалий (порог: %.2f%%)\n", total_found, threshold * 100))
  if (total_found > 100) {
    cat(sprintf("Показаны первые 100 из %d:\n", total_found))
    print(head(result, 100))
  } else {
    print(result)
  }
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