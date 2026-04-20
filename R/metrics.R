# metrics.R
# Единый источник метрик разработчиков

#' Обновить таблицу developer_metrics (витрина метрик)
#' @param conn Подключение к DuckDB
refresh_developer_metrics <- function(conn) {
  
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS developer_metrics (
      author_name VARCHAR PRIMARY KEY,
      total_commits INTEGER,
      active_days INTEGER,
      first_commit DATE,
      last_commit DATE,
      repos_count INTEGER,
      night_commits INTEGER,
      weekend_commits INTEGER,
      total_added INTEGER,
      total_deleted INTEGER,
      avg_commit_size REAL,
      unique_files INTEGER,
      avg_time_between_commits REAL,
      bus_factor_contribution REAL,
      prev_month_commits INTEGER,
      current_month_commits INTEGER,
      trend_direction VARCHAR
    )
  ")
  
  DBI::dbExecute(conn, "DELETE FROM developer_metrics")
  
  query <- "
    WITH 
    base AS (
      SELECT 
        author_name,
        COUNT(DISTINCT commit) AS total_commits,
        COUNT(DISTINCT repo) AS repos_count,
        MIN(date) AS first_commit,
        MAX(date) AS last_commit,
        COUNT(DISTINCT CAST(date AS DATE)) AS active_days,
        SUM(CASE WHEN EXTRACT(HOUR FROM date) BETWEEN 0 AND 5 THEN 1 ELSE 0 END) AS night_commits,
        SUM(CASE WHEN EXTRACT(DOW FROM date) IN (0,6) THEN 1 ELSE 0 END) AS weekend_commits
      FROM git_commit_history
      GROUP BY author_name
    ),
    code_changes AS (
      SELECT 
        c.author_name,
        SUM(d.count_add) AS total_added,
        SUM(d.count_del) AS total_deleted,
        AVG(d.count_add + d.count_del) AS avg_commit_size,
        COUNT(DISTINCT d.src_file) AS unique_files
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      GROUP BY c.author_name
    ),
    commit_gaps AS (
      SELECT 
        author_name,
        AVG(gap_days) AS avg_time_between_commits
      FROM (
        SELECT 
          author_name,
          commit_date,
          LAG(commit_date) OVER (PARTITION BY author_name ORDER BY commit_date) AS prev_date,
          DATEDIFF('day', LAG(commit_date) OVER (PARTITION BY author_name ORDER BY commit_date), commit_date) AS gap_days
        FROM (
          SELECT DISTINCT author_name, CAST(date AS DATE) AS commit_date
          FROM git_commit_history
        ) t
      ) gaps
      WHERE gap_days IS NOT NULL
      GROUP BY author_name
    ),
    total_project_commits AS (
      SELECT SUM(total_commits) AS total FROM base
    ),
    monthly_trend AS (
      SELECT 
        author_name,
        SUM(CASE WHEN month = current_month THEN commits ELSE 0 END) AS current_month_commits,
        SUM(CASE WHEN month = current_month - INTERVAL '1' MONTH THEN commits ELSE 0 END) AS prev_month_commits
      FROM (
        SELECT 
          author_name,
          DATE_TRUNC('month', date) AS month,
          COUNT(*) AS commits,
          MAX(DATE_TRUNC('month', date)) OVER () AS current_month
        FROM git_commit_history
        GROUP BY author_name, DATE_TRUNC('month', date)
      ) t
      GROUP BY author_name
    )
    INSERT INTO developer_metrics
    SELECT 
      b.author_name,
      b.total_commits,
      b.active_days,
      b.first_commit,
      b.last_commit,
      b.repos_count,
      b.night_commits,
      b.weekend_commits,
      COALESCE(cc.total_added, 0) AS total_added,
      COALESCE(cc.total_deleted, 0) AS total_deleted,
      COALESCE(cc.avg_commit_size, 0) AS avg_commit_size,
      COALESCE(cc.unique_files, 0) AS unique_files,
      cg.avg_time_between_commits AS avg_time_between_commits,
      ROUND(1.0 * b.total_commits / tpc.total, 4) AS bus_factor_contribution,
      COALESCE(mt.prev_month_commits, 0) AS prev_month_commits,
      COALESCE(mt.current_month_commits, 0) AS current_month_commits,
      CASE 
        WHEN COALESCE(mt.current_month_commits, 0) > COALESCE(mt.prev_month_commits, 0) THEN 'рост'
        WHEN COALESCE(mt.current_month_commits, 0) < COALESCE(mt.prev_month_commits, 0) THEN 'падение'
        ELSE 'стабильно'
      END AS trend_direction
    FROM base b
    LEFT JOIN code_changes cc ON b.author_name = cc.author_name
    LEFT JOIN commit_gaps cg ON b.author_name = cg.author_name
    CROSS JOIN total_project_commits tpc
    LEFT JOIN monthly_trend mt ON b.author_name = mt.author_name
  "
  
  DBI::dbExecute(conn, query)
  message("Таблица developer_metrics обновлена")
}

#' Получить базовую статистику по разработчику из витрины
#' @param conn Подключение к БД
#' @param username Имя разработчика (опционально)
get_developer_stats <- function(conn, username = NULL) {
  query <- "SELECT * FROM developer_metrics"
  if (!is.null(username)) {
    query <- sprintf("%s WHERE author_name LIKE '%%%s%%'", query, username)
  }
  DBI::dbGetQuery(conn, query)
}

#' Сводная статистика по команде
get_summary_stats <- function(conn) {
  query <- "
    SELECT 
      COUNT(*) AS total_developers,
      SUM(total_commits) AS total_commits,
      MIN(first_commit) AS first_commit,
      MAX(last_commit) AS last_commit,
      AVG(total_commits) AS avg_commits_per_dev,
      SUM(CASE WHEN bus_factor_contribution > 0.5 THEN 1 ELSE 0 END) AS critical_developers
    FROM developer_metrics
  "
  overview <- DBI::dbGetQuery(conn, query)
  
  top5 <- DBI::dbGetQuery(conn, "
    SELECT author_name, total_commits
    FROM developer_metrics
    ORDER BY total_commits DESC
    LIMIT 5
  ")
  
  list(overview = overview, top_5_developers = top5)
}

#' Получить командные метрики (продуктивность, риски) на основе витрины
get_team_metrics <- function(conn) {
  df <- DBI::dbGetQuery(conn, "
    SELECT 
      author_name,
      total_commits,
      active_days,
      total_commits / NULLIF(active_days, 0) AS commits_per_day,
      avg_commit_size,
      unique_files,
      night_commits,
      weekend_commits,
      avg_time_between_commits,
      bus_factor_contribution,
      current_month_commits,
      prev_month_commits,
      trend_direction
    FROM developer_metrics
    ORDER BY total_commits DESC
  ")
  
  df$productivity_level <- cut(df$commits_per_day,
                               breaks = c(-Inf, 0.5, 1.5, 3, Inf),
                               labels = c("низкая", "средняя", "высокая", "очень высокая"))
  df
}

#' Оценка рисков команды (на основе витрины)
get_team_risks <- function(conn) {
  df <- DBI::dbGetQuery(conn, "
    SELECT 
      author_name,
      total_commits,
      night_commits,
      weekend_commits,
      total_commits / NULLIF(active_days, 0) AS commits_per_day,
      avg_time_between_commits,
      CASE 
        WHEN (night_commits * 1.0 / total_commits) > 0.3 OR (weekend_commits * 1.0 / total_commits) > 0.2 THEN 'high'
        WHEN (night_commits * 1.0 / total_commits) > 0.15 OR (weekend_commits * 1.0 / total_commits) > 0.1 THEN 'medium'
        ELSE 'low'
      END AS burnout_risk,
      -- Bug risk: если много уникальных файлов и большой размер коммита
      CASE 
        WHEN unique_files > 50 AND avg_commit_size > 300 THEN 'high'
        WHEN unique_files > 20 OR avg_commit_size > 150 THEN 'medium'
        ELSE 'low'
      END AS bug_risk
    FROM developer_metrics
  ")
  df[, c("author_name", "total_commits", "burnout_risk", "bug_risk", "avg_time_between_commits")]
}