#' Обнаружение аномалий в Git-активности

#' Полное обнаружение всех видов аномалий
#' @param conn Подключение к базе данных DuckDB
#' @return data.frame со всеми аномалиями
get_all_anomalies <- function(conn) {
  
  result <- data.frame()
  
  # 1. Ночные коммиты (22:00 - 6:00)
  cat("1. Поиск ночных коммитов...\n")
  query_night <- "
    SELECT 
        author_name,
        author_email,
        date,
        'night_commit' as anomaly_type,
        CAST(SUBSTR(date, 12, 2) AS INTEGER) as hour,
        NULL as day,
        NULL as mismatch,
        NULL as total_changes,
        NULL as file,
        NULL as start_break,
        NULL as end_break,
        NULL as changes_count,
        NULL as message,
        NULL as month,
        'Коммит в нерабочее время (ночь)' as description
    FROM git_commit_history
    WHERE CAST(SUBSTR(date, 12, 2) AS INTEGER) >= 22 
       OR CAST(SUBSTR(date, 12, 2) AS INTEGER) < 6
  "
  night <- DBI::dbGetQuery(conn, query_night)
  result <- rbind(result, night)
  
  # 2. Коммиты в выходные
  cat("2. Поиск коммитов в выходные...\n")
  query_weekend <- "
    SELECT 
        author_name,
        author_email,
        date,
        'weekend_commit' as anomaly_type,
        NULL as hour,
        CASE CAST(strftime('%w', CAST(SUBSTR(date, 1, 10) AS DATE)) AS INTEGER)
            WHEN 0 THEN 'Воскресенье'
            WHEN 6 THEN 'Суббота'
        END as day,
        NULL as mismatch,
        NULL as total_changes,
        NULL as file,
        NULL as start_break,
        NULL as end_break,
        NULL as changes_count,
        NULL as message,
        NULL as month,
        'Коммит в выходной день' as description
    FROM git_commit_history
    WHERE CAST(strftime('%w', CAST(SUBSTR(date, 1, 10) AS DATE)) AS INTEGER) IN (0, 6)
  "
  weekend <- DBI::dbGetQuery(conn, query_weekend)
  result <- rbind(result, weekend)
  
  # 3. Несоответствие автора и коммитера
  cat("3. Поиск несоответствий автор/коммитер...\n")
  query_mismatch <- "
    SELECT 
        author_name,
        author_email,
        date,
        'author_committer_mismatch' as anomaly_type,
        NULL as hour,
        NULL as day,
        (author_name || ' != ' || committer_name) as mismatch,
        NULL as total_changes,
        NULL as file,
        NULL as start_break,
        NULL as end_break,
        NULL as changes_count,
        NULL as message,
        NULL as month,
        'Автор и коммитер различаются' as description
    FROM git_commit_history
    WHERE author_name != committer_name
  "
  mismatch <- DBI::dbGetQuery(conn, query_mismatch)
  result <- rbind(result, mismatch)
  
  # 4. Аномально большие коммиты (>500 строк)
  cat("4. Поиск больших коммитов...\n")
  query_large <- "
    SELECT 
        c.author_name,
        c.author_email,
        c.date,
        'large_commit' as anomaly_type,
        NULL as hour,
        NULL as day,
        NULL as mismatch,
        SUM(d.count_add + d.count_del) as total_changes,
        NULL as file,
        NULL as start_break,
        NULL as end_break,
        NULL as changes_count,
        NULL as message,
        NULL as month,
        ('Изменено ' || CAST(SUM(d.count_add + d.count_del) AS VARCHAR) || ' строк') as description
    FROM git_commit_history c
    JOIN git_diff d ON c.commit = d.commit
    GROUP BY c.author_name, c.author_email, c.date, c.commit
    HAVING SUM(d.count_add + d.count_del) > 500
  "
  large <- DBI::dbGetQuery(conn, query_large)
  result <- rbind(result, large)
  
  # 5. Аномально маленькие коммиты (менее 5 строк)
  cat("5. Поиск маленьких коммитов...\n")
  query_small <- "
    SELECT 
        c.author_name,
        c.author_email,
        c.date,
        'tiny_commit' as anomaly_type,
        NULL as hour,
        NULL as day,
        NULL as mismatch,
        SUM(d.count_add + d.count_del) as total_changes,
        NULL as file,
        NULL as start_break,
        NULL as end_break,
        NULL as changes_count,
        NULL as message,
        NULL as month,
        ('Изменено всего ' || CAST(SUM(d.count_add + d.count_del) AS VARCHAR) || ' строк') as description
    FROM git_commit_history c
    JOIN git_diff d ON c.commit = d.commit
    GROUP BY c.author_name, c.author_email, c.date, c.commit
    HAVING SUM(d.count_add + d.count_del) < 5
  "
  small <- DBI::dbGetQuery(conn, query_small)
  result <- rbind(result, small)
  
  # 6. Коммиты с чувствительными файлами
  cat("6. Поиск чувствительных файлов...\n")
  query_sensitive <- "
    SELECT 
        c.author_name,
        c.author_email,
        c.date,
        'sensitive_file' as anomaly_type,
        NULL as hour,
        NULL as day,
        NULL as mismatch,
        NULL as total_changes,
        d.src_file as file,
        NULL as start_break,
        NULL as end_break,
        NULL as changes_count,
        NULL as message,
        NULL as month,
        ('Изменен чувствительный файл: ' || d.src_file) as description
    FROM git_commit_history c
    JOIN git_diff d ON c.commit = d.commit
    WHERE d.is_sensitive = TRUE
  "
  sensitive <- DBI::dbGetQuery(conn, query_sensitive)
  result <- rbind(result, sensitive)
  
  # 7. Длинные перерывы (>7 дней)
  cat("7. Поиск длинных перерывов...\n")
  query_break <- "
    WITH commit_dates AS (
      SELECT 
          author_email,
          author_name,
          SUBSTR(date, 1, 10) as commit_date,
          ROW_NUMBER() OVER (PARTITION BY author_email ORDER BY SUBSTR(date, 1, 10)) as rn
      FROM git_commit_history
      GROUP BY author_email, author_name, SUBSTR(date, 1, 10)
    )
    SELECT 
        c1.author_name,
        c1.author_email,
        c1.commit_date as date,
        'long_break' as anomaly_type,
        NULL as hour,
        NULL as day,
        NULL as mismatch,
        NULL as total_changes,
        NULL as file,
        c1.commit_date as start_break,
        c2.commit_date as end_break,
        NULL as changes_count,
        NULL as message,
        NULL as month,
        ('Перерыв ' || CAST((JULIAN(CAST(c2.commit_date AS DATE)) - JULIAN(CAST(c1.commit_date AS DATE))) AS INTEGER) || ' дней') as description
    FROM commit_dates c1
    JOIN commit_dates c2 ON c1.author_email = c2.author_email AND c2.rn = c1.rn + 1
    WHERE (JULIAN(CAST(c2.commit_date AS DATE)) - JULIAN(CAST(c1.commit_date AS DATE))) > 7
  "
  long_break <- DBI::dbGetQuery(conn, query_break)
  result <- rbind(result, long_break)
  
  # 8. Частые изменения одного файла (>10 раз в день)
  cat("8. Поиск частых изменений одного файла...\n")
  query_frequent <- "
    SELECT 
        c.author_name,
        c.author_email,
        c.date,
        'frequent_file_changes' as anomaly_type,
        NULL as hour,
        NULL as day,
        NULL as mismatch,
        NULL as total_changes,
        d.src_file as file,
        NULL as start_break,
        NULL as end_break,
        COUNT(*) as changes_count,
        NULL as message,
        NULL as month,
        ('Файл ' || d.src_file || ' изменен много раз') as description
    FROM git_commit_history c
    JOIN git_diff d ON c.commit = d.commit
    GROUP BY c.author_name, c.author_email, SUBSTR(c.date, 1, 10), d.src_file, c.date
    HAVING COUNT(*) > 10
  "
  frequent <- DBI::dbGetQuery(conn, query_frequent)
  result <- rbind(result, frequent)
  
  # 9. Коммиты без сообщения
  cat("9. Поиск коммитов без сообщения...\n")
  query_no_message <- "
    SELECT 
        author_name,
        author_email,
        date,
        'empty_message' as anomaly_type,
        NULL as hour,
        NULL as day,
        NULL as mismatch,
        NULL as total_changes,
        NULL as file,
        NULL as start_break,
        NULL as end_break,
        NULL as changes_count,
        message,
        NULL as month,
        'Коммит без содержательного сообщения' as description
    FROM git_commit_history
    WHERE message IS NULL OR LENGTH(TRIM(message)) < 5
  "
  no_message <- DBI::dbGetQuery(conn, query_no_message)
  result <- rbind(result, no_message)
  
  # 10. Резкое изменение паттерна активности
  cat("10. Поиск изменения паттерна активности...\n")
  query_pattern_change <- "
    WITH monthly_stats AS (
      SELECT 
          author_email,
          SUBSTR(date, 1, 7) as month,
          COUNT(*) as commits_per_month,
          AVG(CAST(SUBSTR(date, 12, 2) AS INTEGER)) as avg_hour
      FROM git_commit_history
      GROUP BY author_email, month
    ),
    changes AS (
      SELECT 
          author_email,
          month,
          commits_per_month,
          avg_hour,
          LAG(commits_per_month) OVER (PARTITION BY author_email ORDER BY month) as prev_commits,
          LAG(avg_hour) OVER (PARTITION BY author_email ORDER BY month) as prev_hour
      FROM monthly_stats
    )
    SELECT 
        NULL as author_name,
        author_email,
        NULL as date,
        'pattern_change' as anomaly_type,
        NULL as hour,
        NULL as day,
        NULL as mismatch,
        NULL as total_changes,
        NULL as file,
        NULL as start_break,
        NULL as end_break,
        NULL as changes_count,
        NULL as message,
        month,
        ('Активность изменилась с ' || CAST(prev_commits AS VARCHAR) || ' на ' || CAST(commits_per_month AS VARCHAR) || ' коммитов') as description
    FROM changes
    WHERE prev_commits IS NOT NULL 
      AND (commits_per_month > prev_commits * 2 OR commits_per_month < prev_commits / 2)
  "
  pattern_change <- DBI::dbGetQuery(conn, query_pattern_change)
  result <- rbind(result, pattern_change)
  
  if (nrow(result) > 0) {
    result$anomaly_id <- 1:nrow(result)
    result <- result[order(result$author_email, result$date), ]
  }
  
  cat(sprintf("\n=== ИТОГО НАЙДЕНО АНОМАЛИЙ: %d ===\n", nrow(result)))
  
  return(result)
}

#' Статистика по типам аномалий
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

#' Топ разработчиков по аномалиям
get_top_anomaly_developers <- function(anomalies, n = 5) {
  if (nrow(anomalies) == 0) {
    return(data.frame(author_email = character(), anomaly_count = numeric()))
  }
  top <- aggregate(anomaly_id ~ author_email + author_name, data = anomalies, FUN = length)
  names(top) <- c("author_email", "author_name", "anomaly_count")
  top <- top[order(-top$anomaly_count), ]
  return(head(top, n))
}

#' Экспорт аномалий
export_anomalies_to_csv <- function(anomalies, output_dir = "anomalies") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- file.path(output_dir, sprintf("all_anomalies_%s.csv", timestamp))
  write.csv(anomalies, filename, row.names = FALSE)
  
  stats <- get_anomaly_stats(anomalies)
  stats_file <- file.path(output_dir, sprintf("anomaly_stats_%s.csv", timestamp))
  write.csv(stats, stats_file, row.names = FALSE)
  
  top <- get_top_anomaly_developers(anomalies)
  top_file <- file.path(output_dir, sprintf("top_anomaly_devs_%s.csv", timestamp))
  write.csv(top, top_file, row.names = FALSE)
  
  cat(sprintf("Сохранены файлы:\n- %s\n- %s\n- %s\n", filename, stats_file, top_file))
}


#' Аномалии для конкретного разработчика
#' @param conn Подключение к БД
#' @param username Никнейм разработчика
get_anomalies_for_developer <- function(conn, username) {
  # Получаем все аномалии
  all_anomalies <- get_all_anomalies(conn)
  
  # Фильтруем по имени
  dev_anomalies <- all_anomalies[grepl(username, all_anomalies$author_name, ignore.case = TRUE), ]
  
  if (nrow(dev_anomalies) == 0) {
    cat("Аномалий не найдено\n")
  } else {
    cat(sprintf("Найдено аномалий: %d\n", nrow(dev_anomalies)))
    print(dev_anomalies[, c("date", "anomaly_type", "description")])
  }
  
  return(dev_anomalies)
}