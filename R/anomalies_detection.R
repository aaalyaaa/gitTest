#' Обнаружение аномалий в Git-активности
#' 
#' Набор функций для выявления различных видов аномалий
#' в активности разработчиков

#' Полное обнаружение всех видов аномалий
#' @param conn Подключение к базе данных DuckDB
#' @param username Никнейм разработчика (опционально)
#' @return data.frame со всеми аномалиями
get_all_anomalies <- function(conn, username = NULL) {
  
  result <- data.frame()
  
  # 1. Ночные коммиты (22:00 - 6:00)
  cat("1. Поиск ночных коммитов...\n")
  
  if (!is.null(username)) {
    query_night <- sprintf("
      SELECT 
          author_name,
          date,
          'night_commit' as anomaly_type,
          'Коммит в нерабочее время (ночь)' as description
      FROM git_commit_history
      WHERE author_name LIKE '%%%s%%'
        AND (CAST(SUBSTR(CAST(date AS VARCHAR), 12, 2) AS INTEGER) >= 22 
             OR CAST(SUBSTR(CAST(date AS VARCHAR), 12, 2) AS INTEGER) < 6)
    ", username)
  } else {
    query_night <- "
      SELECT 
          author_name,
          date,
          'night_commit' as anomaly_type,
          'Коммит в нерабочее время (ночь)' as description
      FROM git_commit_history
      WHERE (CAST(SUBSTR(CAST(date AS VARCHAR), 12, 2) AS INTEGER) >= 22 
             OR CAST(SUBSTR(CAST(date AS VARCHAR), 12, 2) AS INTEGER) < 6)
    "
  }
  
  night <- DBI::dbGetQuery(conn, query_night)
  if (nrow(night) > 0) result <- rbind(result, night)
  
  # 2. Коммиты в выходные
  cat("2. Поиск коммитов в выходные...\n")
  
  if (!is.null(username)) {
    query_weekend <- sprintf("
      SELECT 
          author_name,
          date,
          SUBSTR(CAST(date AS VARCHAR), 1, 10) as commit_date
      FROM git_commit_history
      WHERE author_name LIKE '%%%s%%'
    ", username)
  } else {
    query_weekend <- "
      SELECT 
          author_name,
          date,
          SUBSTR(CAST(date AS VARCHAR), 1, 10) as commit_date
      FROM git_commit_history
    "
  }
  
  all_dates <- DBI::dbGetQuery(conn, query_weekend)
  if (nrow(all_dates) > 0) {
    all_dates$weekday <- strftime(as.Date(all_dates$commit_date), "%w")
    weekend_dates <- all_dates[all_dates$weekday %in% c("0", "6"), ]
    
    if (nrow(weekend_dates) > 0) {
      weekend <- data.frame(
        author_name = weekend_dates$author_name,
        date = weekend_dates$date,
        anomaly_type = "weekend_commit",
        description = "Коммит в выходной день",
        stringsAsFactors = FALSE
      )
      result <- rbind(result, weekend)
    }
  }
  
  # 3. Аномально большие коммиты (>500 строк)
  cat("3. Поиск больших коммитов...\n")
  
  if (!is.null(username)) {
    query_large <- sprintf("
      SELECT 
          c.author_name,
          ANY_VALUE(c.date) as date,
          'large_commit' as anomaly_type,
          CONCAT('Изменено ', SUM(d.count_add + d.count_del), ' строк') as description
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      WHERE c.author_name LIKE '%%%s%%'
      GROUP BY c.author_name, c.commit
      HAVING SUM(d.count_add + d.count_del) > 500
    ", username)
  } else {
    query_large <- "
      SELECT 
          c.author_name,
          ANY_VALUE(c.date) as date,
          'large_commit' as anomaly_type,
          CONCAT('Изменено ', SUM(d.count_add + d.count_del), ' строк') as description
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      GROUP BY c.author_name, c.commit
      HAVING SUM(d.count_add + d.count_del) > 500
    "
  }
  
  large <- DBI::dbGetQuery(conn, query_large)
  if (nrow(large) > 0) result <- rbind(result, large)
  
  # 4. Аномально маленькие коммиты (<5 строк)
  cat("4. Поиск маленьких коммитов...\n")
  
  if (!is.null(username)) {
    query_small <- sprintf("
      SELECT 
          c.author_name,
          ANY_VALUE(c.date) as date,
          'tiny_commit' as anomaly_type,
          CONCAT('Изменено всего ', SUM(d.count_add + d.count_del), ' строк') as description
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      WHERE c.author_name LIKE '%%%s%%'
      GROUP BY c.author_name, c.commit
      HAVING SUM(d.count_add + d.count_del) < 5
    ", username)
  } else {
    query_small <- "
      SELECT 
          c.author_name,
          ANY_VALUE(c.date) as date,
          'tiny_commit' as anomaly_type,
          CONCAT('Изменено всего ', SUM(d.count_add + d.count_del), ' строк') as description
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      GROUP BY c.author_name, c.commit
      HAVING SUM(d.count_add + d.count_del) < 5
    "
  }
  
  small <- DBI::dbGetQuery(conn, query_small)
  if (nrow(small) > 0) result <- rbind(result, small)
  
  # 5. Коммиты с чувствительными файлами
  cat("5. Поиск чувствительных файлов...\n")
  
  if (!is.null(username)) {
    query_sensitive <- sprintf("
      SELECT 
          c.author_name,
          ANY_VALUE(c.date) as date,
          'sensitive_file' as anomaly_type,
          CONCAT('Изменен чувствительный файл: ', ANY_VALUE(d.src_file)) as description
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      WHERE c.author_name LIKE '%%%s%%'
        AND (d.src_file LIKE '%%%.env%%%' OR d.src_file LIKE '%%%.key%%%' OR d.src_file LIKE '%%%secret%%%' OR d.src_file LIKE '%%%password%%%')
      GROUP BY c.author_name, c.commit
    ", username)
  } else {
    query_sensitive <- "
      SELECT 
          c.author_name,
          ANY_VALUE(c.date) as date,
          'sensitive_file' as anomaly_type,
          CONCAT('Изменен чувствительный файл: ', ANY_VALUE(d.src_file)) as description
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      WHERE (d.src_file LIKE '%.env%' OR d.src_file LIKE '%.key%' OR d.src_file LIKE '%secret%' OR d.src_file LIKE '%password%')
      GROUP BY c.author_name, c.commit
    "
  }
  
  sensitive <- DBI::dbGetQuery(conn, query_sensitive)
  if (nrow(sensitive) > 0) result <- rbind(result, sensitive)
  
  # 6. Длинные перерывы в работе (>7 дней)
  cat("6. Поиск длинных перерывов...\n")
  
  if (!is.null(username)) {
    query_break <- sprintf("
      WITH commit_dates AS (
        SELECT 
            author_name,
            CAST(SUBSTR(CAST(MIN(date) AS VARCHAR), 1, 10) AS DATE) as commit_date,
            ROW_NUMBER() OVER (PARTITION BY author_name ORDER BY MIN(date)) as rn
        FROM git_commit_history
        WHERE author_name LIKE '%%%s%%'
        GROUP BY author_name, CAST(SUBSTR(CAST(date AS VARCHAR), 1, 10) AS DATE)
      )
      SELECT 
          c1.author_name,
          'long_break' as anomaly_type,
          CONCAT('Перерыв ', (c2.commit_date - c1.commit_date), ' дней') as description,
          CAST(c1.commit_date AS VARCHAR) as date
      FROM commit_dates c1
      JOIN commit_dates c2 ON c1.author_name = c2.author_name AND c2.rn = c1.rn + 1
      WHERE (c2.commit_date - c1.commit_date) > 7
    ", username)
  } else {
    query_break <- "
      WITH commit_dates AS (
        SELECT 
            author_name,
            CAST(SUBSTR(CAST(MIN(date) AS VARCHAR), 1, 10) AS DATE) as commit_date,
            ROW_NUMBER() OVER (PARTITION BY author_name ORDER BY MIN(date)) as rn
        FROM git_commit_history
        GROUP BY author_name, CAST(SUBSTR(CAST(date AS VARCHAR), 1, 10) AS DATE)
      )
      SELECT 
          c1.author_name,
          'long_break' as anomaly_type,
          CONCAT('Перерыв ', (c2.commit_date - c1.commit_date), ' дней') as description,
          CAST(c1.commit_date AS VARCHAR) as date
      FROM commit_dates c1
      JOIN commit_dates c2 ON c1.author_name = c2.author_name AND c2.rn = c1.rn + 1
      WHERE (c2.commit_date - c1.commit_date) > 7
    "
  }
  
  long_break <- DBI::dbGetQuery(conn, query_break)
  if (nrow(long_break) > 0) result <- rbind(result, long_break)
  
  # 7. Частые коммиты в один файл (>10 раз в день)
  cat("7. Поиск частых изменений одного файла...\n")
  
  if (!is.null(username)) {
    query_frequent <- sprintf("
      SELECT 
          c.author_name,
          ANY_VALUE(c.date) as date,
          'frequent_file_changes' as anomaly_type,
          CONCAT('Файл ', ANY_VALUE(d.src_file), ' изменен много раз (', COUNT(*), ' раз)') as description
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      WHERE c.author_name LIKE '%%%s%%'
      GROUP BY c.author_name, CAST(SUBSTR(CAST(c.date AS VARCHAR), 1, 10) AS DATE), d.src_file
      HAVING COUNT(*) > 10
    ", username)
  } else {
    query_frequent <- "
      SELECT 
          c.author_name,
          ANY_VALUE(c.date) as date,
          'frequent_file_changes' as anomaly_type,
          CONCAT('Файл ', ANY_VALUE(d.src_file), ' изменен много раз (', COUNT(*), ' раз)') as description
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      GROUP BY c.author_name, CAST(SUBSTR(CAST(c.date AS VARCHAR), 1, 10) AS DATE), d.src_file
      HAVING COUNT(*) > 10
    "
  }
  
  frequent <- DBI::dbGetQuery(conn, query_frequent)
  if (nrow(frequent) > 0) result <- rbind(result, frequent)
  
  # 8. Коммиты без сообщения
  cat("8. Поиск коммитов без сообщения...\n")
  
  if (!is.null(username)) {
    query_no_message <- sprintf("
      SELECT 
          author_name,
          date,
          'empty_message' as anomaly_type,
          'Коммит без содержательного сообщения' as description
      FROM git_commit_history
      WHERE author_name LIKE '%%%s%%'
        AND (message IS NULL OR LENGTH(TRIM(message)) < 5)
    ", username)
  } else {
    query_no_message <- "
      SELECT 
          author_name,
          date,
          'empty_message' as anomaly_type,
          'Коммит без содержательного сообщения' as description
      FROM git_commit_history
      WHERE (message IS NULL OR LENGTH(TRIM(message)) < 5)
    "
  }
  
  no_message <- DBI::dbGetQuery(conn, query_no_message)
  if (nrow(no_message) > 0) result <- rbind(result, no_message)
  
  # 9. Резкое изменение паттерна активности
  cat("9. Поиск изменения паттерна активности...\n")
  
  if (!is.null(username)) {
    query_pattern_change <- sprintf("
      WITH monthly_stats AS (
        SELECT 
            author_name,
            SUBSTR(CAST(date AS VARCHAR), 1, 7) as month,
            COUNT(*) as commits_per_month
        FROM git_commit_history
        WHERE author_name LIKE '%%%s%%'
        GROUP BY author_name, month
      ),
      changes AS (
        SELECT 
            author_name,
            month,
            commits_per_month,
            LAG(commits_per_month) OVER (PARTITION BY author_name ORDER BY month) as prev_commits
        FROM monthly_stats
      )
      SELECT 
          author_name,
          month as date,
          'pattern_change' as anomaly_type,
          CONCAT('Активность изменилась с ', prev_commits, ' на ', commits_per_month, ' коммитов') as description
      FROM changes
      WHERE prev_commits IS NOT NULL 
        AND (commits_per_month > prev_commits * 2 OR commits_per_month < prev_commits / 2)
    ", username)
  } else {
    query_pattern_change <- "
      WITH monthly_stats AS (
        SELECT 
            author_name,
            SUBSTR(CAST(date AS VARCHAR), 1, 7) as month,
            COUNT(*) as commits_per_month
        FROM git_commit_history
        GROUP BY author_name, month
      ),
      changes AS (
        SELECT 
            author_name,
            month,
            commits_per_month,
            LAG(commits_per_month) OVER (PARTITION BY author_name ORDER BY month) as prev_commits
        FROM monthly_stats
      )
      SELECT 
          author_name,
          month as date,
          'pattern_change' as anomaly_type,
          CONCAT('Активность изменилась с ', prev_commits, ' на ', commits_per_month, ' коммитов') as description
      FROM changes
      WHERE prev_commits IS NOT NULL 
        AND (commits_per_month > prev_commits * 2 OR commits_per_month < prev_commits / 2)
    "
  }
  
  pattern_change <- DBI::dbGetQuery(conn, query_pattern_change)
  if (nrow(pattern_change) > 0) result <- rbind(result, pattern_change)
  
  if (nrow(result) > 0) {
    result$anomaly_id <- 1:nrow(result)
    result <- result[order(result$author_name, result$date), ]
  }
  
  cat(sprintf("\n=== ИТОГО НАЙДЕНО АНОМАЛИЙ: %d ===\n", nrow(result)))
  
  return(result)
}

#' Статистика по типам аномалий
#' @param anomalies Результат функции get_all_anomalies()
#' @return data.frame со статистикой
get_anomaly_stats <- function(anomalies) {
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
#' @param anomalies Результат функции get_all_anomalies()
#' @param n Количество разработчиков
#' @return data.frame с топ разработчиками
get_top_anomaly_developers <- function(anomalies, n = 5) {
  if (nrow(anomalies) == 0) {
    return(data.frame(author_name = character(), anomaly_count = numeric()))
  }
  
  top <- aggregate(anomaly_id ~ author_name, data = anomalies, FUN = length)
  names(top) <- c("author_name", "anomaly_count")
  top <- top[order(-top$anomaly_count), ]
  
  return(head(top, n))
}

