# anomalies_detection.R
# Общие утилиты, обнаружение аномалий и их HR‑форматирование

git_error <- function(class, message, ...) {
  structure(list(message = message, ...), class = c(class, "error", "condition"))
}

`%||%` <- function(x, y) if (is.null(x)) y else x

is_git_error <- function(x) inherits(x, "error")

stop_if_error <- function(x, msg = NULL) {
  if (is_git_error(x)) {
    stop(if (is.null(msg)) x$message else paste(msg, x$message, sep = ": "))
  }
  return(x)
}

get_all_anomalies <- function(conn, username = NULL, limit = 1000, since = NULL, until = NULL) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  
  result <- data.frame()
  errors <- list()
  
  safe_query <- function(sql_template, name, needs_where = TRUE) {
    where_part <- if (!is.null(username) && needs_where) 
      sprintf("AND author_name LIKE '%%%s%%'", username) else ""
    if (!is.null(since)) where_part <- paste0(where_part, " AND date >= '", since, "'")
    if (!is.null(until)) where_part <- paste0(where_part, " AND date <= '", until, "'")
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
  
  night_sql <- "
    SELECT author_name, date, 'night_commit' as anomaly_type,
           'Коммит в нерабочее время (ночь)' as description
    FROM git_commit_history
    WHERE (EXTRACT(HOUR FROM date) >= 22 OR EXTRACT(HOUR FROM date) < 6) %s
  "
  night <- safe_query(night_sql, "night_commits")
  if (nrow(night) > 0) result <- rbind(result, night)
  
  weekend_sql <- "
    SELECT author_name, date, 'weekend_commit' as anomaly_type,
           'Коммит в выходной день' as description
    FROM git_commit_history
    WHERE EXTRACT(DOW FROM date) IN (0, 6) %s
  "
  weekend <- safe_query(weekend_sql, "weekend_commits")
  if (nrow(weekend) > 0) result <- rbind(result, weekend)
  
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
  
  empty_sql <- "
    SELECT author_name, date, 'empty_message' as anomaly_type,
           'Коммит без содержательного сообщения' as description
    FROM git_commit_history
    WHERE (message IS NULL OR LENGTH(TRIM(message)) < 3) %s
  "
  empty <- safe_query(empty_sql, "empty_messages")
  if (nrow(empty) > 0) result <- rbind(result, empty)
  
  pattern_sql <- "
    WITH monthly_stats AS (
      SELECT author_name, DATE_TRUNC('month', date) as month, COUNT(*) as commits_per_month
      FROM git_commit_history
      WHERE 1=1 %s
      GROUP BY author_name, DATE_TRUNC('month', date)
    ),
    changes AS (
      SELECT author_name, month, commits_per_month,
             LAG(commits_per_month) OVER (PARTITION BY author_name ORDER BY month) as prev_commits
      FROM monthly_stats
    )
    SELECT author_name, month as date, 'pattern_change' as anomaly_type,
           CONCAT('Активность изменилась с ', prev_commits, ' на ', commits_per_month, ' коммитов') as description
    FROM changes
    WHERE prev_commits IS NOT NULL 
      AND (commits_per_month > prev_commits * 2 OR commits_per_month < prev_commits / 2)
      AND ABS(commits_per_month - prev_commits) >= 10
  "
  pattern <- safe_query(pattern_sql, "pattern_changes")
  if (nrow(pattern) > 0) result <- rbind(result, pattern)
  
  if (nrow(result) == 0 && length(errors) > 0) {
    return(errors[[1]])
  }
  
  if (nrow(result) > 0) {
    result$anomaly_id <- 1:nrow(result)
    result <- result[order(result$author_name, result$date), ]
    if (!is.null(limit) && limit > 0 && nrow(result) > limit) {
      result <- result[1:limit, ]
      cat(sprintf("Предупреждение: общее число аномалий превышает лимит (%d). Возвращены первые %d.\n", nrow(result), limit))
    }
  }
  
  attr(result, "errors") <- errors
  cat(sprintf("\n Найдено rule‑аномалий: %d\n", nrow(result)))
  return(result)
}

#' Кеширование всех аномалий (rule + ML) в таблицу anomalies
cache_anomalies <- function(conn, ml_threshold = 0.95, since = NULL, until = NULL) {
  if (missing(conn) || is.null(conn)) return(git_error("invalid_argument", "conn не может быть NULL"))
  
  rule_anom <- get_all_anomalies(conn, since = since, until = until)
  if (is_git_error(rule_anom)) return(rule_anom)
  
  ml_anom <- tryCatch(
    get_ml_anomalies(conn, threshold = ml_threshold, since = since, until = until),
    error = function(e) data.frame()
  )
  
  if (!is_git_error(ml_anom) && nrow(ml_anom) > 0) {
    ml_anom$anomaly_type <- "ml_anomaly"
    ml_anom$description <- ml_anom$explanation
    ml_anom$anomaly_id <- NA
    ml_anom <- ml_anom[, c("author_name", "date", "anomaly_type", "description", "anomaly_id")]
  } else {
    ml_anom <- data.frame()
  }
  
  all_anom <- rbind(rule_anom, ml_anom)
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

#' Частые правки одного файла (>10 раз в день)
get_frequent_file_edits <- function(conn, username = NULL, since = NULL, until = NULL, threshold = 10) {
  if (missing(conn) || is.null(conn)) return(git_error("invalid_argument", "conn не может быть NULL"))
  where <- ""
  if (!is.null(username)) where <- paste0(where, " AND c.author_name LIKE '%%", username, "%%'")
  if (!is.null(since)) where <- paste0(where, " AND c.date >= '", since, "'")
  if (!is.null(until)) where <- paste0(where, " AND c.date <= '", until, "'")
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

#' Статистика по типам аномалий
get_anomaly_stats <- function(anomalies) {
  if (missing(anomalies)) return(git_error("invalid_argument", "anomalies не может быть пропущен"))
  if (is_git_error(anomalies)) return(anomalies)
  if (nrow(anomalies) == 0) return(data.frame(anomaly_type = character(), count = numeric()))
  stats <- aggregate(anomaly_id ~ anomaly_type, data = anomalies, FUN = length)
  names(stats) <- c("anomaly_type", "count")
  stats <- stats[order(-stats$count), ]
  stats$percentage <- round(100 * stats$count / sum(stats$count), 2)
  return(stats)
}

#' Топ разработчиков по количеству аномалий
get_top_anomaly_developers <- function(anomalies, n = 5) {
  if (missing(anomalies)) return(git_error("invalid_argument", "anomalies не может быть пропущен"))
  if (is_git_error(anomalies)) return(anomalies)
  if (nrow(anomalies) == 0) return(data.frame(author_name = character(), anomaly_count = numeric()))
  top <- aggregate(anomaly_id ~ author_name, data = anomalies, FUN = length)
  names(top) <- c("author_name", "anomaly_count")
  top <- top[order(-top$anomaly_count), ]
  return(head(top, n))
}

#' HR‑форматирование аномалий (без параметра period)
format_anomalies_for_hr <- function(rule_anomalies, ml_anomalies = NULL, frequent_edits = NULL) {
  work_pattern <- list()
  work_quality <- list()
  
  night_count <- if (!is.null(rule_anomalies) && nrow(rule_anomalies) > 0) {
    sum(rule_anomalies$anomaly_type == "night_commit", na.rm = TRUE)
  } else 0
  work_pattern$nights <- if (night_count > 0) paste0("работает по ночам (", night_count, " раз)") else "не коммитит по ночам"
  
  weekend_count <- if (!is.null(rule_anomalies) && nrow(rule_anomalies) > 0) {
    sum(rule_anomalies$anomaly_type == "weekend_commit", na.rm = TRUE)
  } else 0
  work_pattern$weekends <- if (weekend_count > 0) paste0("коммитит в выходные (", weekend_count, " раз)") else "не работает в выходные"
  
  breaks <- if (!is.null(rule_anomalies) && nrow(rule_anomalies) > 0) {
    rule_anomalies[rule_anomalies$anomaly_type == "long_break", ]
  } else data.frame()
  if (nrow(breaks) > 0) {
    max_break <- max(as.numeric(gsub("\\D", "", breaks$description)), na.rm = TRUE)
    break_months <- format(as.Date(breaks$date), "%B")
    max_break_month <- break_months[which.max(as.numeric(gsub("\\D", "", breaks$description)))]
    work_pattern$breaks <- paste0("был перерыв ", max_break, " дней в ", max_break_month)
  } else {
    work_pattern$breaks <- "нет длинных перерывов"
  }
  
  large_count <- if (!is.null(rule_anomalies) && nrow(rule_anomalies) > 0) {
    sum(rule_anomalies$anomaly_type == "large_commit", na.rm = TRUE)
  } else 0
  work_quality$large_commits <- if (large_count > 0) paste0(large_count, " очень больших коммита (>500 строк)") else "нет очень больших коммитов"
  
  empty_count <- if (!is.null(rule_anomalies) && nrow(rule_anomalies) > 0) {
    sum(rule_anomalies$anomaly_type == "empty_message", na.rm = TRUE)
  } else 0
  work_quality$empty_messages <- if (empty_count > 0) paste0(empty_count, " коммитов без содержательного сообщения") else "все коммиты имеют сообщения"
  
  freq_days <- if (!is.null(frequent_edits) && nrow(frequent_edits) > 0) {
    sum(frequent_edits$days_with_frequent_edits, na.rm = TRUE)
  } else 0
  work_quality$frequent_edits <- if (freq_days > 0) paste0(freq_days, " дней с частыми правками одного файла (>10 раз/день)") else "нет дней с частыми правками одного файла"
  
  ml_count <- if (!is.null(ml_anomalies) && nrow(ml_anomalies) > 0) nrow(ml_anomalies) else 0
  work_quality$ml_anomalies <- if (ml_count > 0) paste0(ml_count, " коммитов с необычными паттернами") else "нет необычных паттернов коммитов"
  
  list(work_pattern = work_pattern, work_quality = work_quality)
}