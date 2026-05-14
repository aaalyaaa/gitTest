#' @export
get_activity_seasonality <- function(conn, author_name = NULL, since = NULL, until = NULL, repo_id = NULL) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  tryCatch({
    where <- if (!is.null(author_name)) sprintf("author_name = '%s'", author_name) else "1=1"
    if (!is.null(since)) where <- paste0(where, " AND date >= '", since, "'")
    if (!is.null(until)) where <- paste0(where, " AND date <= '", until, "'")
    if (!is.null(repo_id)) where <- paste0(where, " AND repo_id = ", repo_id)
    
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
#' @export
get_activity_trends <- function(conn, author_name = NULL, since = NULL, until = NULL, repo_id = NULL) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  tryCatch({
    where <- if (!is.null(author_name)) sprintf("author_name = '%s'", author_name) else "1=1"
    if (!is.null(since)) where <- paste0(where, " AND date >= '", since, "'")
    if (!is.null(until)) where <- paste0(where, " AND date <= '", until, "'")
    if (!is.null(repo_id)) where <- paste0(where, " AND repo_id = ", repo_id)
    
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

prepare_anomaly_features <- function(conn, author_name = NULL, since = NULL, until = NULL, repo_id = NULL) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  tryCatch({
    where_clause <- if (!is.null(author_name)) {
      sprintf("c.author_name = '%s'", author_name)
    } else "1=1"
    if (!is.null(since)) where_clause <- paste0(where_clause, " AND c.date >= '", since, "'")
    if (!is.null(until)) where_clause <- paste0(where_clause, " AND c.date <= '", until, "'")
    if (!is.null(repo_id)) where_clause <- paste0(where_clause, " AND c.repo_id = ", repo_id)
    
    query <- sprintf("
      SELECT 
          c.commit,
          c.author_name,
          c.date,
          LENGTH(c.message) AS message_length,
          EXTRACT(HOUR FROM CAST(c.date AS TIMESTAMP)) AS hour,
          EXTRACT(DOW FROM CAST(c.date AS TIMESTAMP)) AS dow,
          COUNT(DISTINCT CASE WHEN d.count_add > 0 THEN COALESCE(d.src_file, d.dst_file) END) AS n_added_files,
          COUNT(DISTINCT CASE WHEN d.count_del > 0 THEN COALESCE(d.src_file, d.dst_file) END) AS n_deleted_files,
          COUNT(DISTINCT COALESCE(d.src_file, d.dst_file)) AS n_files,
          SUM(d.count_add + d.count_del) AS commit_size,
          COUNT(DISTINCT d.file_extension) AS file_type_diversity
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      WHERE %s
      GROUP BY c.commit, c.author_name, c.date, c.message
    ", where_clause)
    
    df <- DBI::dbGetQuery(conn, query)
    if (nrow(df) == 0) {
      return(git_error("no_data_error", "Нет данных для подготовки признаков"))
    }
    df
  }, error = function(e) {
    git_error("db_error", paste("Ошибка подготовки признаков:", e$message))
  })
}
#' @export
get_ml_anomalies <- function(conn, author_name = NULL, score_threshold = 0.7,
                             return_features = TRUE, since = NULL, until = NULL, repo_id = NULL) {
  if (!requireNamespace("solitude", quietly = TRUE)) {
    return(git_error("missing_package", "Пакет 'solitude' не установлен. Установите: install.packages('solitude')"))
  }
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  
  features_df <- prepare_anomaly_features(conn, author_name, since, until, repo_id)
  if (is_git_error(features_df)) return(features_df)
  if (nrow(features_df) == 0) {
    cat("Нет данных для ML-аномалий\n")
    return(data.frame())
  }
  if (nrow(features_df) < 10) {
    cat(sprintf("Слишком мало коммитов (%d) для ML-анализа. Нужно минимум 10.\n", nrow(features_df)))
    return(data.frame())
  }
  
  X <- features_df[, c("hour", "dow", "n_files", "commit_size", "message_length",
                       "n_added_files", "n_deleted_files", "file_type_diversity")]
  X <- na.omit(X)
  if (nrow(X) < 10) return(data.frame())
  
  iso <- solitude::isolationForest$new(sample_size = min(nrow(X), 10000), num_trees = 100)
  iso$fit(X)
  scores <- iso$predict(X)
  features_df$anomaly_score <- scores$anomaly_score
  
  anomalies <- features_df[features_df$anomaly_score >= score_threshold, ]
  
  if (nrow(anomalies) == 0) {
    cat(sprintf("ML-аномалий не найдено (порог: %.2f)\n", score_threshold))
    return(data.frame())
  }
  
  anomalies$explanation <- apply(anomalies, 1, function(row) {
    reasons <- c()
    hour <- as.numeric(row["hour"])
    dow <- as.numeric(row["dow"])
    n_files <- as.numeric(row["n_files"])
    commit_size <- as.numeric(row["commit_size"])
    msg_len <- as.numeric(row["message_length"])
    added <- as.numeric(row["n_added_files"])
    deleted <- as.numeric(row["n_deleted_files"])
    diversity <- as.numeric(row["file_type_diversity"])
    
    if (hour < 6 || hour > 22) reasons <- c(reasons, "ночное время")
    if (dow %in% c(0, 6)) reasons <- c(reasons, "выходной день")
    if (commit_size > 500) {
      reasons <- c(reasons, paste0("очень большой коммит (", commit_size, " строк)"))
    } else if (commit_size < 10 && commit_size > 0) {
      reasons <- c(reasons, paste0("очень маленький коммит (", commit_size, " строк)"))
    }
    if (n_files > 5) reasons <- c(reasons, paste0("много файлов (", n_files, ")"))
    if (msg_len < 10) reasons <- c(reasons, "очень короткое сообщение")
    if (msg_len > 500) reasons <- c(reasons, "очень длинное сообщение")
    if (added > 10 && deleted == 0) reasons <- c(reasons, "только добавления, без удалений")
    if (deleted > 10 && added == 0) reasons <- c(reasons, "только удаления, без добавлений")
    if (diversity > 3) reasons <- c(reasons, paste0("много разных типов файлов (", diversity, ")"))
    
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
    result$message_length <- anomalies$message_length
    result$n_added_files <- anomalies$n_added_files
    result$n_deleted_files <- anomalies$n_deleted_files
    result$file_type_diversity <- anomalies$file_type_diversity
  }
  
  result <- result[order(-result$anomaly_score), ]
  cat(sprintf("Найдено %d ML-аномалий (порог: %.2f)\n", nrow(result), score_threshold))
  return(result)
}

#' @export
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