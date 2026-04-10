#' Профилирование разработчиков
#' 
#' Набор функций для анализа активности разработчиков из Git-репозитория
#' 
#' @author Ваше имя
#' @description Пакет функций для анализа активности разработчиков, 
#'              включая временные паттерны, продуктивность, технологический стек и безопасность

# ============================================================================
# 1. БАЗОВЫЕ ФУНКЦИИ ДЛЯ СТАТИСТИКИ
# ============================================================================

#' Получить общую статистику по разработчикам
#' @param conn Подключение к базе данных DuckDB
#' @param username Никнейм разработчика (опционально)
#' @param email Email разработчика (опционально)
#' @return data.frame с результатами анализа
get_developer_stats <- function(conn, username = NULL, email = NULL) {
  query <- "
    SELECT 
        author_name,
        author_email,
        COUNT(DISTINCT commit) as total_commits,
        COUNT(DISTINCT repo) as repos_count,
        MIN(date) as first_commit,
        MAX(date) as last_commit,
        COUNT(DISTINCT SUBSTR(date, 1, 10)) as active_days
    FROM git_commit_history
  "
  
  conditions <- c()
  if (!is.null(username)) {
    conditions <- c(conditions, sprintf("author_name LIKE '%%%s%%'", username))
  }
  if (!is.null(email)) {
    conditions <- c(conditions, sprintf("author_email = '%s'", email))
  }
  
  if (length(conditions) > 0) {
    query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
  }
  
  query <- paste0(query, " GROUP BY author_name, author_email ORDER BY total_commits DESC")
  
  DBI::dbGetQuery(conn, query)
}

#' Анализ временных паттернов по часам
#' @param conn Подключение к базе данных DuckDB
#' @param username Никнейм разработчика (опционально)
#' @param email Email разработчика (опционально)
#' @return data.frame с результатами анализа
get_time_patterns <- function(conn, username = NULL, email = NULL) {
  query <- "
    SELECT 
        author_name,
        author_email,
        CAST(SUBSTR(date, 12, 2) AS INTEGER) as hour,
        COUNT(*) as commits_count
    FROM git_commit_history
  "
  
  conditions <- c()
  if (!is.null(username)) {
    conditions <- c(conditions, sprintf("author_name LIKE '%%%s%%'", username))
  }
  if (!is.null(email)) {
    conditions <- c(conditions, sprintf("author_email = '%s'", email))
  }
  
  if (length(conditions) > 0) {
    query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
  }
  
  query <- paste0(query, " GROUP BY author_name, author_email, hour ORDER BY author_name, hour")
  
  DBI::dbGetQuery(conn, query)
}

#' Часовое распределение с процентами
#' @param conn Подключение к базе данных DuckDB
#' @param username Никнейм разработчика (опционально)
#' @param email Email разработчика (опционально)
#' @return data.frame с результатами анализа
get_hourly_distribution <- function(conn, username = NULL, email = NULL) {
  query <- "
    SELECT 
        author_name,
        author_email,
        CAST(SUBSTR(date, 12, 2) AS INTEGER) as hour,
        COUNT(*) as commits,
        ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (PARTITION BY author_name, author_email), 2) as percentage
    FROM git_commit_history
  "
  
  conditions <- c()
  if (!is.null(username)) {
    conditions <- c(conditions, sprintf("author_name LIKE '%%%s%%'", username))
  }
  if (!is.null(email)) {
    conditions <- c(conditions, sprintf("author_email = '%s'", email))
  }
  
  if (length(conditions) > 0) {
    query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
  }
  
  query <- paste0(query, " GROUP BY author_name, author_email, hour ORDER BY author_name, hour")
  
  DBI::dbGetQuery(conn, query)
}

#' Анализ активности по дням недели
#' @param conn Подключение к базе данных DuckDB
#' @param username Никнейм разработчика (опционально)
#' @param email Email разработчика (опционально)
#' @return data.frame с результатами анализа
get_weekday_patterns <- function(conn, username = NULL, email = NULL) {
  query <- "
    SELECT 
        author_name,
        author_email,
        CASE CAST(strftime('%w', CAST(SUBSTR(date, 1, 10) AS DATE)) AS INTEGER)
            WHEN 0 THEN 'Воскресенье'
            WHEN 1 THEN 'Понедельник'
            WHEN 2 THEN 'Вторник'
            WHEN 3 THEN 'Среда'
            WHEN 4 THEN 'Четверг'
            WHEN 5 THEN 'Пятница'
            WHEN 6 THEN 'Суббота'
        END as weekday,
        COUNT(*) as commits,
        ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (PARTITION BY author_name, author_email), 2) as percentage
    FROM git_commit_history
  "
  
  conditions <- c()
  if (!is.null(username)) {
    conditions <- c(conditions, sprintf("author_name LIKE '%%%s%%'", username))
  }
  if (!is.null(email)) {
    conditions <- c(conditions, sprintf("author_email = '%s'", email))
  }
  
  if (length(conditions) > 0) {
    query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
  }
  
  query <- paste0(query, " GROUP BY author_name, author_email, weekday ORDER BY author_name, commits DESC")
  
  DBI::dbGetQuery(conn, query)
}

#' Анализ работы в выходные
#' @param conn Подключение к базе данных DuckDB
#' @param username Никнейм разработчика (опционально)
#' @param email Email разработчика (опционально)
#' @return data.frame с результатами анализа
get_weekend_analysis <- function(conn, username = NULL, email = NULL) {
  query <- "
    SELECT 
        author_name,
        author_email,
        COUNT(*) as total_commits,
        SUM(CASE 
            WHEN CAST(strftime('%w', CAST(SUBSTR(date, 1, 10) AS DATE)) AS INTEGER) IN (0, 6)
            THEN 1 ELSE 0 END) as weekend_commits,
        ROUND(100.0 * SUM(CASE 
            WHEN CAST(strftime('%w', CAST(SUBSTR(date, 1, 10) AS DATE)) AS INTEGER) IN (0, 6)
            THEN 1 ELSE 0 END) / COUNT(*), 2) as weekend_percentage
    FROM git_commit_history
  "
  
  conditions <- c()
  if (!is.null(username)) {
    conditions <- c(conditions, sprintf("author_name LIKE '%%%s%%'", username))
  }
  if (!is.null(email)) {
    conditions <- c(conditions, sprintf("author_email = '%s'", email))
  }
  
  if (length(conditions) > 0) {
    query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
  }
  
  query <- paste0(query, " GROUP BY author_name, author_email HAVING total_commits > 5 ORDER BY weekend_percentage DESC")
  
  DBI::dbGetQuery(conn, query)
}

# ============================================================================
# 2. АНАЛИЗ ПРОДУКТИВНОСТИ И КАЧЕСТВА КОДА
# ============================================================================

#' Анализ объема изменений
#' @param conn Подключение к базе данных DuckDB
#' @param username Никнейм разработчика (опционально)
#' @param email Email разработчика (опционально)
#' @return data.frame с результатами анализа
get_change_volume <- function(conn, username = NULL, email = NULL) {
  query <- "
    SELECT 
        c.author_name,
        c.author_email,
        COUNT(DISTINCT c.commit) as commits,
        SUM(d.count_add) as total_lines_added,
        SUM(d.count_del) as total_lines_deleted,
        ROUND(AVG(d.count_add), 2) as avg_lines_added,
        ROUND(AVG(d.count_del), 2) as avg_lines_deleted
    FROM git_commit_history c
    JOIN git_diff d ON c.commit = d.commit
  "
  
  conditions <- c()
  if (!is.null(username)) {
    conditions <- c(conditions, sprintf("c.author_name LIKE '%%%s%%'", username))
  }
  if (!is.null(email)) {
    conditions <- c(conditions, sprintf("c.author_email = '%s'", email))
  }
  
  if (length(conditions) > 0) {
    query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
  }
  
  query <- paste0(query, " GROUP BY c.author_name, c.author_email ORDER BY commits DESC")
  
  DBI::dbGetQuery(conn, query)
}

#' Анализ продуктивности (коммиты и изменения по дням)
#' @param conn Подключение к базе данных DuckDB
#' @param email Email разработчика (опционально)
#' @param username Никнейм разработчика (опционально)
#' @return data.frame с ежедневной статистикой
get_productivity_daily <- function(conn, email = NULL, username = NULL) {
  query <- "
    SELECT 
        c.author_name,
        c.author_email,
        SUBSTR(c.date, 1, 10) as commit_date,
        COUNT(DISTINCT c.commit) as commits_per_day,
        SUM(d.count_add) as lines_added_per_day,
        SUM(d.count_del) as lines_deleted_per_day,
        ROUND(AVG(d.count_add), 2) as avg_add_per_commit,
        ROUND(AVG(d.count_del), 2) as avg_del_per_commit
    FROM git_commit_history c
    JOIN git_diff d ON c.commit = d.commit
  "
  
  conditions <- c()
  if (!is.null(username)) {
    conditions <- c(conditions, sprintf("c.author_name LIKE '%%%s%%'", username))
  }
  if (!is.null(email)) {
    conditions <- c(conditions, sprintf("c.author_email = '%s'", email))
  }
  
  if (length(conditions) > 0) {
    query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
  }
  
  query <- paste0(query, " GROUP BY c.author_name, c.author_email, commit_date ORDER BY commit_date")
  
  DBI::dbGetQuery(conn, query)
}

#' Анализ размера коммитов (классификация)
#' @param conn Подключение к базе данных DuckDB
#' @param email Email разработчика (опционально)
#' @param username Никнейм разработчика (опционально)
#' @return data.frame с классификацией коммитов
get_commit_size_analysis <- function(conn, email = NULL, username = NULL) {
  query <- "
    WITH commit_sizes AS (
      SELECT 
          c.author_name,
          c.author_email,
          c.commit,
          (SUM(d.count_add) + SUM(d.count_del)) as total_changes
      FROM git_commit_history c
      JOIN git_diff d ON c.commit = d.commit
  "
  
  conditions <- c()
  if (!is.null(username)) {
    conditions <- c(conditions, sprintf("c.author_name LIKE '%%%s%%'", username))
  }
  if (!is.null(email)) {
    conditions <- c(conditions, sprintf("c.author_email = '%s'", email))
  }
  
  if (length(conditions) > 0) {
    query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
  }
  
  query <- paste0(query, "
      GROUP BY c.author_name, c.author_email, c.commit
    )
    SELECT 
        author_name,
        author_email,
        CASE 
            WHEN total_changes <= 10 THEN 'очень маленький (1-10)'
            WHEN total_changes <= 50 THEN 'маленький (11-50)'
            WHEN total_changes <= 200 THEN 'средний (51-200)'
            WHEN total_changes <= 500 THEN 'большой (201-500)'
            ELSE 'очень большой (500+)'
        END as commit_size_category,
        COUNT(*) as commits_count,
        ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (PARTITION BY author_name, author_email), 2) as percentage
    FROM commit_sizes
    GROUP BY author_name, author_email, commit_size_category
    ORDER BY author_name, commits_count DESC
  ")
  
  DBI::dbGetQuery(conn, query)
}

#' Анализ стабильности (соотношение добавлений/удалений)
#' @param conn Подключение к базе данных DuckDB
#' @param email Email разработчика (опционально)
#' @param username Никнейм разработчика (опционально)
#' @return data.frame с метриками стабильности
get_stability_metrics <- function(conn, email = NULL, username = NULL) {
  query <- "
    SELECT 
        c.author_name,
        c.author_email,
        COUNT(DISTINCT c.commit) as total_commits,
        SUM(d.count_add) as total_added,
        SUM(d.count_del) as total_deleted,
        ROUND(SUM(d.count_add) / NULLIF(SUM(d.count_del), 0), 2) as add_del_ratio,
        ROUND(SUM(d.count_del) * 100.0 / NULLIF(SUM(d.count_add), 0), 2) as rewrite_percentage
    FROM git_commit_history c
    JOIN git_diff d ON c.commit = d.commit
  "
  
  conditions <- c()
  if (!is.null(username)) {
    conditions <- c(conditions, sprintf("c.author_name LIKE '%%%s%%'", username))
  }
  if (!is.null(email)) {
    conditions <- c(conditions, sprintf("c.author_email = '%s'", email))
  }
  
  if (length(conditions) > 0) {
    query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
  }
  
  query <- paste0(query, " GROUP BY c.author_name, c.author_email ORDER BY total_commits DESC")
  
  DBI::dbGetQuery(conn, query)
}

# ============================================================================
# 3. АНАЛИЗ ТЕХНОЛОГИЧЕСКОГО СТЕКА
# ============================================================================

#' Предпочитаемые языки программирования
#' @param conn Подключение к базе данных DuckDB
#' @param email Email разработчика (опционально)
#' @param username Никнейм разработчика (опционально)
#' @return data.frame с статистикой по расширениям файлов
get_preferred_languages <- function(conn, email = NULL, username = NULL) {
  query <- "
    SELECT 
        c.author_name,
        c.author_email,
        d.file_extension,
        COUNT(*) as changes,
        SUM(d.count_add) as lines_added,
        SUM(d.count_del) as lines_deleted,
        COUNT(DISTINCT d.src_file) as files_affected,
        ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (PARTITION BY c.author_name, c.author_email), 2) as percentage
    FROM git_commit_history c
    JOIN git_diff d ON c.commit = d.commit
    WHERE d.file_extension != '' AND d.file_extension IS NOT NULL
  "
  
  conditions <- c()
  if (!is.null(username)) {
    conditions <- c(conditions, sprintf("c.author_name LIKE '%%%s%%'", username))
  }
  if (!is.null(email)) {
    conditions <- c(conditions, sprintf("c.author_email = '%s'", email))
  }
  
  if (length(conditions) > 0) {
    query <- paste0(query, " AND ", paste(conditions, collapse = " AND "))
  }
  
  query <- paste0(query, " GROUP BY c.author_name, c.author_email, d.file_extension ORDER BY c.author_name, changes DESC")
  
  DBI::dbGetQuery(conn, query)
}

#' Анализ работы с конкретными файлами/директориями
#' @param conn Подключение к базе данных DuckDB
#' @param email Email разработчика (опционально)
#' @param username Никнейм разработчика (опционально)
#' @return data.frame с частотой изменений файлов
get_file_change_frequency <- function(conn, email = NULL, username = NULL) {
  query <- "
    SELECT 
        c.author_name,
        c.author_email,
        d.src_file,
        d.file_extension,
        COUNT(*) as change_count,
        MIN(c.date) as first_change,
        MAX(c.date) as last_change
    FROM git_commit_history c
    JOIN git_diff d ON c.commit = d.commit
  "
  
  conditions <- c()
  if (!is.null(username)) {
    conditions <- c(conditions, sprintf("c.author_name LIKE '%%%s%%'", username))
  }
  if (!is.null(email)) {
    conditions <- c(conditions, sprintf("c.author_email = '%s'", email))
  }
  
  if (length(conditions) > 0) {
    query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
  }
  
  query <- paste0(query, " GROUP BY c.author_name, c.author_email, d.src_file, d.file_extension ORDER BY change_count DESC LIMIT 20")
  
  DBI::dbGetQuery(conn, query)
}

# ============================================================================
# 4. АНАЛИЗ БЕЗОПАСНОСТИ
# ============================================================================

#' Работа с чувствительными файлами (безопасность)
#' @param conn Подключение к базе данных DuckDB
#' @param email Email разработчика (опционально)
#' @param username Никнейм разработчика (опционально)
#' @return data.frame с анализом чувствительных изменений
get_sensitive_files_analysis <- function(conn, email = NULL, username = NULL) {
  query <- "
    SELECT 
        c.author_name,
        c.author_email,
        c.date,
        d.src_file,
        d.dst_file,
        d.is_sensitive,
        d.count_add + d.count_del as total_changes,
        d.code
    FROM git_commit_history c
    JOIN git_diff d ON c.commit = d.commit
    WHERE d.is_sensitive = TRUE
  "
  
  conditions <- c()
  if (!is.null(username)) {
    conditions <- c(conditions, sprintf("c.author_name LIKE '%%%s%%'", username))
  }
  if (!is.null(email)) {
    conditions <- c(conditions, sprintf("c.author_email = '%s'", email))
  }
  
  if (length(conditions) > 0) {
    query <- paste0(query, " AND ", paste(conditions, collapse = " AND "))
  }
  
  query <- paste0(query, " ORDER BY c.date DESC")
  
  DBI::dbGetQuery(conn, query)
}

#' Выявление аномальной активности
#' @param conn Подключение к базе данных DuckDB
#' @param threshold_hour Порог для ночных коммитов (по умолчанию 22-5)
#' @return data.frame с аномалиями
get_anomaly_detection <- function(conn, threshold_hour = 22) {
  query <- "
    WITH hourly_stats AS (
      SELECT 
          author_email,
          AVG(commits_per_hour) as avg_commits,
          STDDEV(commits_per_hour) as std_commits
      FROM (
          SELECT 
              author_email,
              CAST(SUBSTR(date, 12, 2) AS INTEGER) as hour,
              COUNT(*) as commits_per_hour
          FROM git_commit_history
          GROUP BY author_email, hour
      )
      GROUP BY author_email
    )
    SELECT 
        c.author_name,
        c.author_email,
        c.date,
        CAST(SUBSTR(c.date, 12, 2) AS INTEGER) as hour,
        CASE 
            WHEN CAST(SUBSTR(c.date, 12, 2) AS INTEGER) >= 22 OR CAST(SUBSTR(c.date, 12, 2) AS INTEGER) < 6 
            THEN 'night_commit'
            WHEN c.author_name != c.committer_name 
            THEN 'author_committer_mismatch'
            ELSE 'normal'
        END as anomaly_type
    FROM git_commit_history c
    WHERE 
        CAST(SUBSTR(c.date, 12, 2) AS INTEGER) >= 22 OR CAST(SUBSTR(c.date, 12, 2) AS INTEGER) < 6
        OR c.author_name != c.committer_name
    ORDER BY c.date DESC
  "
  
  DBI::dbGetQuery(conn, query)
}

# ============================================================================
# 5. АНАЛИЗ ПАТТЕРНОВ РАБОТЫ
# ============================================================================

#' Самые продуктивные дни
#' @param conn Подключение к базе данных DuckDB
#' @param email Email разработчика (опционально)
#' @param username Никнейм разработчика (опционально)
#' @return data.frame с топ днями
get_most_productive_days <- function(conn, email = NULL, username = NULL) {
  query <- "
    SELECT 
        author_name,
        author_email,
        SUBSTR(date, 1, 10) as day,
        COUNT(*) as commits,
        SUM(d.count_add) as lines_added
    FROM git_commit_history c
    JOIN git_diff d ON c.commit = d.commit
  "
  
  conditions <- c()
  if (!is.null(username)) {
    conditions <- c(conditions, sprintf("c.author_name LIKE '%%%s%%'", username))
  }
  if (!is.null(email)) {
    conditions <- c(conditions, sprintf("c.author_email = '%s'", email))
  }
  
  if (length(conditions) > 0) {
    query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
  }
  
  query <- paste0(query, " GROUP BY author_name, author_email, day ORDER BY commits DESC LIMIT 10")
  
  DBI::dbGetQuery(conn, query)
}

#' Анализ перерывов в работе
#' @param conn Подключение к базе данных DuckDB
#' @param email Email разработчика (опционально)
#' @param username Никнейм разработчика (опционально)
#' @return data.frame с анализом перерывов
get_work_breaks <- function(conn, email = NULL, username = NULL) {
  query <- "
    WITH commit_dates AS (
      SELECT 
          author_name,
          author_email,
          SUBSTR(date, 1, 10) as commit_date,
          ROW_NUMBER() OVER (PARTITION BY author_email ORDER BY SUBSTR(date, 1, 10)) as rn
      FROM git_commit_history
      GROUP BY author_name, author_email, SUBSTR(date, 1, 10)
  "
  
  conditions <- c()
  if (!is.null(username)) {
    conditions <- c(conditions, sprintf("author_name LIKE '%%%s%%'", username))
  }
  if (!is.null(email)) {
    conditions <- c(conditions, sprintf("author_email = '%s'", email))
  }
  
  if (length(conditions) > 0) {
    query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
  }
  
  query <- paste0(query, "
    )
    SELECT 
        c1.author_name,
        c1.author_email,
        c1.commit_date as start_break,
        c2.commit_date as end_break,
        JULIANDAY(c2.commit_date) - JULIANDAY(c1.commit_date) as break_days
    FROM commit_dates c1
    JOIN commit_dates c2 ON c1.author_email = c2.author_email AND c2.rn = c1.rn + 1
    WHERE JULIANDAY(c2.commit_date) - JULIANDAY(c1.commit_date) > 7
    ORDER BY break_days DESC
  ")
  
  DBI::dbGetQuery(conn, query)
}

#' Тренды активности по месяцам
#' @param conn Подключение к базе данных DuckDB
#' @param email Email разработчика (опционально)
#' @param username Никнейм разработчика (опционально)
#' @return data.frame с месячными трендами
get_monthly_trends <- function(conn, email = NULL, username = NULL) {
  query <- "
    SELECT 
        author_name,
        author_email,
        SUBSTR(date, 1, 7) as month,
        COUNT(*) as commits,
        SUM(d.count_add) as lines_added,
        SUM(d.count_del) as lines_deleted
    FROM git_commit_history c
    JOIN git_diff d ON c.commit = d.commit
  "
  
  conditions <- c()
  if (!is.null(username)) {
    conditions <- c(conditions, sprintf("c.author_name LIKE '%%%s%%'", username))
  }
  if (!is.null(email)) {
    conditions <- c(conditions, sprintf("c.author_email = '%s'", email))
  }
  
  if (length(conditions) > 0) {
    query <- paste0(query, " WHERE ", paste(conditions, collapse = " AND "))
  }
  
  query <- paste0(query, " GROUP BY author_name, author_email, month ORDER BY month")
  
  DBI::dbGetQuery(conn, query)
}

# ============================================================================
# 6. ФУНКЦИИ ДЛЯ ПОЛНОГО ПРОФИЛЯ
# ============================================================================

#' Полный профиль разработчика (по email)
#' @param conn Подключение к базе данных DuckDB
#' @param email Email разработчика (обязательный)
#' @return list с результатами анализа
get_developer_profile_by_email <- function(conn, email) {
  if (missing(email) || is.null(email)) {
    stop("Необходимо указать email разработчика")
  }
  
  cat("\n=== ЗАГРУЗКА ПРОФИЛЯ РАЗРАБОТЧИКА ===\n")
  cat(sprintf("Email: %s\n", email))
  cat("=====================================\n\n")
  
  list(
    basic_stats = get_developer_stats(conn, email = email),
    time_patterns = get_time_patterns(conn, email = email),
    hourly_distribution = get_hourly_distribution(conn, email = email),
    weekday_patterns = get_weekday_patterns(conn, email = email),
    weekend_analysis = get_weekend_analysis(conn, email = email),
    change_volume = get_change_volume(conn, email = email),
    commit_size = get_commit_size_analysis(conn, email = email),
    stability = get_stability_metrics(conn, email = email),
    languages = get_preferred_languages(conn, email = email),
    sensitive_files = get_sensitive_files_analysis(conn, email = email),
    productivity_daily = get_productivity_daily(conn, email = email),
    monthly_trends = get_monthly_trends(conn, email = email)
  )
}

#' Полный профиль разработчика (по нику)
#' @param conn Подключение к базе данных DuckDB
#' @param username Никнейм разработчика (обязательный)
#' @return list с результатами анализа
get_developer_profile_by_username <- function(conn, username) {
  if (missing(username) || is.null(username)) {
    stop("Необходимо указать никнейм разработчика")
  }
  
  cat("\n=== ЗАГРУЗКА ПРОФИЛЯ РАЗРАБОТЧИКА ===\n")
  cat(sprintf("Username: %s\n", username))
  cat("=====================================\n\n")
  
  list(
    basic_stats = get_developer_stats(conn, username = username),
    time_patterns = get_time_patterns(conn, username = username),
    hourly_distribution = get_hourly_distribution(conn, username = username),
    weekday_patterns = get_weekday_patterns(conn, username = username),
    weekend_analysis = get_weekend_analysis(conn, username = username),
    change_volume = get_change_volume(conn, username = username),
    commit_size = get_commit_size_analysis(conn, username = username),
    stability = get_stability_metrics(conn, username = username),
    languages = get_preferred_languages(conn, username = username),
    sensitive_files = get_sensitive_files_analysis(conn, username = username),
    productivity_daily = get_productivity_daily(conn, username = username),
    monthly_trends = get_monthly_trends(conn, username = username)
  )
}

# ============================================================================
# 7. ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
# ============================================================================

#' Поиск разработчиков по имени или email
#' @param conn Подключение к базе данных DuckDB
#' @param search_term Поисковый запрос
#' @return data.frame с найденными разработчиками
search_developers <- function(conn, search_term) {
  query <- sprintf("
    SELECT DISTINCT
        author_name,
        author_email,
        COUNT(*) as commits
    FROM git_commit_history
    WHERE author_name LIKE '%%%s%%' 
       OR author_email LIKE '%%%s%%'
    GROUP BY author_name, author_email
    ORDER BY commits DESC
  ", search_term, search_term)
  
  DBI::dbGetQuery(conn, query)
}

#' Сводная статистика по всем разработчикам
#' @param conn Подключение к базе данных DuckDB
#' @return data.frame со сводной статистикой
get_summary_stats <- function(conn) {
  query <- "
    SELECT 
        COUNT(DISTINCT author_email) as total_developers,
        COUNT(DISTINCT commit) as total_commits,
        MIN(date) as first_commit_date,
        MAX(date) as last_commit_date,
        COUNT(DISTINCT SUBSTR(date, 1, 10)) as total_active_days,
        COUNT(DISTINCT repo) as total_repos
    FROM git_commit_history
  "
  
  stats <- DBI::dbGetQuery(conn, query)
  
  # Топ-5 разработчиков по коммитам
  top5 <- DBI::dbGetQuery(conn, "
    SELECT 
        author_name,
        author_email,
        COUNT(*) as commits
    FROM git_commit_history
    GROUP BY author_name, author_email
    ORDER BY commits DESC
    LIMIT 5
  ")
  
  list(
    overview = stats,
    top_5_developers = top5
  )
}

#' Экспорт профиля в CSV
#' @param profile Профиль разработчика (список от get_developer_profile_*)
#' @param output_dir Директория для сохранения
export_profile_to_csv <- function(profile, output_dir = "profiles") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  author_email <- profile$basic_stats$author_email[1]
  safe_name <- gsub("[^a-zA-Z0-9]", "_", author_email)
  
  for (name in names(profile)) {
    if (is.data.frame(profile[[name]]) && nrow(profile[[name]]) > 0) {
      filename <- file.path(output_dir, sprintf("%s_%s_%s.csv", safe_name, name, timestamp))
      write.csv(profile[[name]], filename, row.names = FALSE)
      cat(sprintf("Сохранен: %s\n", filename))
    }
  }
}
