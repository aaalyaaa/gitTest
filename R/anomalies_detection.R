# Общие утилиты и обнаружение аномалий

#' Создание объекта ошибки (исходная версия, без изменений)
git_error <- function(class, message, ...) {
  structure(list(message = message, ...), class = c(class, "error", "condition"))
}

#' Оператор "или" с защитой от NULL
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Проверка, является ли объект ошибкой git_error
is_git_error <- function(x) {
  inherits(x, "error")
}

stop_if_error <- function(x, msg = NULL) {
  if (is_git_error(x)) {
    stop(if (is.null(msg)) x$message else paste(msg, x$message, sep = ": "))
  }
  return(x)
}


#' Полное обнаружение всех видов аномалий
get_all_anomalies <- function(conn, username = NULL, limit = 1000) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  
  result <- data.frame()
  errors <- list()
  
  safe_query <- function(sql_template, name, needs_where = TRUE) {
    where_part <- if (!is.null(username) && needs_where) 
      sprintf("AND author_name LIKE '%%%s%%'", username) else ""
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
  
  # 2. Коммиты в выходные
  weekend_sql <- "
    SELECT author_name, date, 'weekend_commit' as anomaly_type,
           'Коммит в выходной день' as description
    FROM git_commit_history
    WHERE EXTRACT(DOW FROM date) IN (0, 6) %s
  "
  weekend <- safe_query(weekend_sql, "weekend_commits")
  if (nrow(weekend) > 0) result <- rbind(result, weekend)
  
  # 3. Аномально большие коммиты (>500 строк)
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
  
  # 4. Аномально маленькие коммиты (<5 строк)
  tiny_sql <- "
    SELECT c.author_name, c.date, 'tiny_commit' as anomaly_type,
           CONCAT('Изменено всего ', SUM(d.count_add + d.count_del), ' строк') as description
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    WHERE 1=1 %s
    GROUP BY c.author_name, c.commit, c.date
    HAVING SUM(d.count_add + d.count_del) < 5
  "
  tiny <- safe_query(tiny_sql, "tiny_commits")
  if (nrow(tiny) > 0) result <- rbind(result, tiny)
  
  # 5. Длинные перерывы (>7 дней)
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
  
  # 6. Частые изменения одного файла (>10 раз в день)
  frequent_sql <- "
    SELECT c.author_name, MIN(c.date) as date, 'frequent_file_changes' as anomaly_type,
           CONCAT('Файл ', COALESCE(d.src_file, d.dst_file), ' изменен много раз (', COUNT(*), ' раз)') as description
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    WHERE 1=1 %s
    GROUP BY c.author_name, CAST(c.date AS DATE), COALESCE(d.src_file, d.dst_file)
    HAVING COUNT(*) > 10
  "
  frequent <- safe_query(frequent_sql, "frequent_files")
  if (nrow(frequent) > 0) result <- rbind(result, frequent)
  
  # 7. Коммиты без сообщения
  empty_sql <- "
    SELECT author_name, date, 'empty_message' as anomaly_type,
           'Коммит без содержательного сообщения' as description
    FROM git_commit_history
    WHERE (message IS NULL OR LENGTH(TRIM(message)) < 5) %s
  "
  empty <- safe_query(empty_sql, "empty_messages")
  if (nrow(empty) > 0) result <- rbind(result, empty)
  
  # 8. Изменение паттерна активности
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
  "
  pattern <- safe_query(pattern_sql, "pattern_changes")
  if (nrow(pattern) > 0) result <- rbind(result, pattern)
  
  # Если нет ни одной записи, но были ошибки – возвращаем первую ошибку
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
  cat(sprintf("\n Найдено аномалий: %d\n", nrow(result)))
  return(result)
}

#' Статистика по типам аномалий
get_anomaly_stats <- function(anomalies) {
  if (missing(anomalies)) {
    return(git_error("invalid_argument", "anomalies не может быть пропущен"))
  }
  if (is_git_error(anomalies)) return(anomalies)
  if (nrow(anomalies) == 0) {
    return(data.frame(anomaly_type = character(), count = numeric()))
  }
  stats <- aggregate(anomaly_id ~ anomaly_type, data = anomalies, FUN = length)
  names(stats) <- c("anomaly_type", "count")
  stats <- stats[order(-stats$count), ]
  stats$percentage <- round(100 * stats$count / sum(stats$count), 2)
  return(stats)
}

#' Топ разработчиков по количеству аномалий
get_top_anomaly_developers <- function(anomalies, n = 5) {
  if (missing(anomalies)) {
    return(git_error("invalid_argument", "anomalies не может быть пропущен"))
  }
  if (is_git_error(anomalies)) return(anomalies)
  if (nrow(anomalies) == 0) {
    return(data.frame(author_name = character(), anomaly_count = numeric()))
  }
  top <- aggregate(anomaly_id ~ author_name, data = anomalies, FUN = length)
  names(top) <- c("author_name", "anomaly_count")
  top <- top[order(-top$anomaly_count), ]
  return(head(top, n))
}