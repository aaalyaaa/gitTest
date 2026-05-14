#' @export
ensure_metrics_exist <- function(conn) {
  tables <- tryCatch(
    DBI::dbGetQuery(conn, "SELECT name FROM sqlite_master WHERE type='table' AND name='developer_metrics'"),
    error = function(e) data.frame()
  )
  if (nrow(tables) == 0) {
    message("Таблица developer_metrics не найдена. Вызов refresh_developer_metrics()...")
    res <- refresh_developer_metrics(conn)
    if (is_git_error(res)) return(res)
  }
  invisible(TRUE)
}
#' @export
get_all_anomalies <- function(conn, username = NULL, limit = Inf, since = NULL, until = NULL, repo_id = NULL) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  
  metrics_ok <- ensure_metrics_exist(conn)
  if (is_git_error(metrics_ok)) return(metrics_ok)
  
  result <- data.frame()
  errors <- list()
  
  safe_query <- function(sql_template, name, needs_where = TRUE) {
    where_part <- if (!is.null(username) && needs_where) 
      sprintf("AND author_name LIKE '%%%s%%'", username) else ""
    if (!is.null(since)) where_part <- paste0(where_part, " AND date >= '", since, "'")
    if (!is.null(until)) where_part <- paste0(where_part, " AND date <= '", until, "'")
    if (!is.null(repo_id)) where_part <- paste0(where_part, " AND repo_id = ", repo_id)
    sql <- sprintf(sql_template, where_part)
    
    res <- tryCatch(
      DBI::dbGetQuery(conn, sql),
      error = function(e) git_error("db_error", paste(name, ":", e$message))
    )
    if (is_git_error(res)) {
      errors <<- c(errors, list(res))
      return(data.frame())
    }
    return(res)
  }
  
  # 1. Ночные коммиты
  night_sql <- "
    SELECT author_name, date, 'night_commit' as anomaly_type,
           'Коммит в нерабочее время (ночь)' as description
    FROM git_commit_history
    WHERE (EXTRACT(HOUR FROM date) >= 22 OR EXTRACT(HOUR FROM date) < 6) %s
  "
  night <- safe_query(night_sql, "night_commits")
  if (nrow(night) > 0) result <- rbind(result, night)
  
  # 2. Выходные
  weekend_sql <- "
    SELECT author_name, date, 'weekend_commit' as anomaly_type,
           'Коммит в выходной день' as description
    FROM git_commit_history
    WHERE EXTRACT(DOW FROM date) IN (0, 6) %s
  "
  weekend <- safe_query(weekend_sql, "weekend_commits")
  if (nrow(weekend) > 0) result <- rbind(result, weekend)
  
  # 3. Большие коммиты (>500 строк)
  large_sql <- "
    SELECT c.author_name, c.date, 'large_commit' as anomaly_type,
           CONCAT('Изменено ', SUM(d.count_add + d.count_del), ' строк') as description
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    WHERE 1=1 %s
    GROUP BY c.author_name, c.commit, c.date
    HAVING SUM(d.count_add + d.count_del) > 500
  "
  large <- safe_query(large_sql, "large_commits")
  if (nrow(large) > 0) result <- rbind(result, large)
  
  # 4. Длинные перерывы (>7 дней)
  break_sql <- "
    WITH commit_dates AS (
      SELECT author_name, CAST(date AS DATE) as commit_date,
             ROW_NUMBER() OVER (PARTITION BY author_name ORDER BY CAST(date AS DATE)) as rn
      FROM git_commit_history
      WHERE 1=1 %s
      GROUP BY author_name, CAST(date AS DATE)
    )
    SELECT c1.author_name, CAST(c1.commit_date AS TIMESTAMPTZ) as date,
           'long_break' as anomaly_type,
           CONCAT('Перерыв ', (c2.commit_date - c1.commit_date), ' дней') as description
    FROM commit_dates c1
    JOIN commit_dates c2 ON c1.author_name = c2.author_name AND c2.rn = c1.rn + 1
    WHERE (c2.commit_date - c1.commit_date) > 7
  "
  long_break <- safe_query(break_sql, "long_breaks")
  if (nrow(long_break) > 0) result <- rbind(result, long_break)
  
  # 5. Пустые сообщения (<3 символов)
  empty_sql <- "
    SELECT author_name, date, 'empty_message' as anomaly_type,
           'Коммит без содержательного сообщения' as description
    FROM git_commit_history
    WHERE (message IS NULL OR LENGTH(TRIM(message)) < 3) %s
  "
  empty <- safe_query(empty_sql, "empty_messages")
  if (nrow(empty) > 0) result <- rbind(result, empty)

  
  if (nrow(result) == 0 && length(errors) > 0) {
    return(errors[[1]])
  }
  
  if (nrow(result) > 0) {
    result$anomaly_id <- 1:nrow(result)
    result <- result[order(result$author_name, result$date), ]
    if (is.finite(limit) && limit > 0 && nrow(result) > limit) {
      cat(sprintf("Предупреждение: общее число аномалий превышает лимит (%d). Возвращены первые %d.\n", nrow(result), limit))
      result <- result[1:limit, ]
    }
  }
  
  attr(result, "errors") <- errors
  cat(sprintf("\n Найдено rule‑аномалий: %d\n", nrow(result)))
  return(result)
}
get_user_anomalies <- function(conn, author_name) {
  author_esc <- gsub("'", "''", author_name)
  DBI::dbGetQuery(conn, sprintf("
    SELECT anomaly_type, date, description,
    FROM anomalies
    WHERE author_name = '%s'
    ORDER BY date
  ", author_esc))
}
#' @export
cache_anomalies <- function(conn, score_threshold = 0.7, since = NULL, until = NULL, min_commits = 10, repo_id = NULL) {
  if (missing(conn) || is.null(conn)) return(git_error("invalid_argument", "conn не может быть NULL"))
  
  rule_anom <- get_all_anomalies(conn, since = since, until = until, limit = Inf, repo_id = repo_id)
  if (is_git_error(rule_anom)) return(rule_anom)
  
  where_date <- ""
  if (!is.null(since)) where_date <- paste0(where_date, " AND date >= '", since, "'")
  if (!is.null(until)) where_date <- paste0(where_date, " AND date <= '", until, "'")
  if (!is.null(repo_id)) where_date <- paste0(where_date, " AND repo_id = ", repo_id)
  
  authors_query <- sprintf("
    SELECT author_name, COUNT(*) as n
    FROM git_commit_history
    WHERE 1=1 %s
    GROUP BY author_name
  ", where_date)
  authors <- DBI::dbGetQuery(conn, authors_query)
  
  all_ml_anom <- data.frame()
  
  for (i in seq_len(nrow(authors))) {
    auth <- authors$author_name[i]
    ncom <- authors$n[i]
    if (ncom < min_commits) {
      cat(sprintf("Автор %s: только %d коммитов, пропускаем ML-аномалии\n", auth, ncom))
      next
    }
    ml <- tryCatch(
      get_ml_anomalies(conn, author_name = auth, score_threshold = score_threshold,
                       since = since, until = until, return_features = FALSE, repo_id = repo_id),
      error = function(e) data.frame()
    )
    if (is.data.frame(ml) && nrow(ml) > 0) {
      ml$anomaly_type <- "ml_anomaly"
      ml$description <- ml$explanation
      ml$anomaly_id <- NA
      ml <- ml[, c("author_name", "date", "anomaly_type", "description", "anomaly_id")]
      all_ml_anom <- rbind(all_ml_anom, ml)
    }
  }
  
  all_anom <- rbind(rule_anom, all_ml_anom)
  if (nrow(all_anom) > 0) {
    all_anom$anomaly_id <- 1:nrow(all_anom)
    DBI::dbExecute(conn, "DROP TABLE IF EXISTS anomalies")
    DBI::dbWriteTable(conn, "anomalies", all_anom)
    cat(sprintf("Таблица anomalies создана: %d записей\n", nrow(all_anom)))
  } else {
    cat("Аномалий не найдено, таблица anomalies не создана.\n")
  }
  invisible(TRUE)
}
#' @export
get_frequent_file_edits <- function(conn, username = NULL, since = NULL, until = NULL, threshold = 10, repo_id = NULL) {
  if (missing(conn) || is.null(conn)) return(git_error("invalid_argument", "conn не может быть NULL"))
  where <- ""
  if (!is.null(username)) where <- paste0(where, " AND c.author_name LIKE '%%", username, "%%'")
  if (!is.null(since)) where <- paste0(where, " AND c.date >= '", since, "'")
  if (!is.null(until)) where <- paste0(where, " AND c.date <= '", until, "'")
  if (!is.null(repo_id)) where <- paste0(where, " AND c.repo_id = ", repo_id)
  if (where != "") where <- paste0("WHERE 1=1", where) else where <- "WHERE 1=1"
  
  query <- sprintf("
    SELECT 
      c.author_name,
      CAST(c.date AS DATE) as day,
      COALESCE(d.src_file, d.dst_file) as file_path,
      COUNT(*) as edits
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    %s
    GROUP BY c.author_name, CAST(c.date AS DATE), COALESCE(d.src_file, d.dst_file)
    HAVING COUNT(*) > %d
    ORDER BY c.author_name, day
  ", where, threshold)
  
  result <- tryCatch(
    DBI::dbGetQuery(conn, query),
    error = function(e) git_error("db_error", paste("Ошибка получения частых правок:", e$message))
  )
  if (is_git_error(result)) return(result)
  if (nrow(result) == 0) return(data.frame())
  
  agg <- aggregate(edits ~ author_name, data = result, FUN = length)
  names(agg) <- c("author_name", "days_with_frequent_edits")
  return(agg)
}
#' @export
get_anomaly_stats <- function(conn = NULL, author_name = NULL, since = NULL, until = NULL, repo_id = NULL, anomalies = NULL) {
  # Если передан data.frame anomalies – используем его (старый режим)
  if (!is.null(anomalies)) {
    if (is_git_error(anomalies)) return(anomalies)
    if (nrow(anomalies) == 0) return(data.frame(anomaly_type = character(), count = numeric(), percentage = numeric()))
    stats <- aggregate(anomaly_id ~ anomaly_type, data = anomalies, FUN = length)
    names(stats) <- c("anomaly_type", "count")
    stats <- stats[order(-stats$count), ]
    stats$percentage <- round(100 * stats$count / sum(stats$count), 2)
    return(stats)
  }
  
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL, если не передан anomalies"))
  }
  
  tables <- DBI::dbGetQuery(conn, "SELECT name FROM sqlite_master WHERE type='table' AND name='anomalies'")
  if (nrow(tables) == 0) {
    return(git_error("no_table", "Таблица anomalies не найдена. Вызовите cache_anomalies() сначала."))
  }
  
  where_clauses <- c()
  if (!is.null(author_name)) {
    where_clauses <- c(where_clauses, sprintf("author_name = '%s'", gsub("'", "''", author_name)))
  }
  if (!is.null(since)) {
    where_clauses <- c(where_clauses, sprintf("date >= '%s'", since))
  }
  if (!is.null(until)) {
    where_clauses <- c(where_clauses, sprintf("date <= '%s'", until))
  }
  
  where_sql <- if (length(where_clauses) > 0) paste("WHERE", paste(where_clauses, collapse = " AND ")) else ""
  query <- sprintf("SELECT anomaly_type, anomaly_id FROM anomalies %s", where_sql)
  df <- DBI::dbGetQuery(conn, query)
  
  if (nrow(df) == 0) {
    return(data.frame(anomaly_type = character(), count = numeric(), percentage = numeric()))
  }
  
  stats <- aggregate(anomaly_id ~ anomaly_type, data = df, FUN = length)
  names(stats) <- c("anomaly_type", "count")
  stats <- stats[order(-stats$count), ]
  stats$percentage <- round(100 * stats$count / sum(stats$count), 2)
  return(stats)
}
