# metrics.R
# Единый источник метрик разработчиков

#' Обновить таблицу developer_metrics (витрина метрик)
#' @param conn Подключение к DuckDB
refresh_developer_metrics <- function(conn) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  
  DBI::dbExecute(conn, "DROP TABLE IF EXISTS developer_metrics")
  
  DBI::dbExecute(conn, "
    CREATE TABLE developer_metrics (
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
      contribution_share REAL,
      prev_month_commits INTEGER,
      current_month_commits INTEGER,
      trend_direction VARCHAR,
      avg_commit_hour REAL,
      avg_add_per_commit REAL,
      avg_del_per_commit REAL
    )
  ")
  
  query <- "
    WITH 
    total_commits_all AS (
      SELECT COUNT(*) AS total FROM git_commit_history
    ),
    base AS (
      SELECT 
        author_name,
        COUNT(DISTINCT commit) AS total_commits,
        COUNT(DISTINCT repo) AS repos_count,
        MIN(date) AS first_commit,
        MAX(date) AS last_commit,
        COUNT(DISTINCT CAST(date AS DATE)) AS active_days,
        SUM(CASE WHEN EXTRACT(HOUR FROM date) >= 22 OR EXTRACT(HOUR FROM date) < 6 THEN 1 ELSE 0 END) AS night_commits,
        SUM(CASE WHEN EXTRACT(DOW FROM date) IN (0,6) THEN 1 ELSE 0 END) AS weekend_commits,
        AVG(EXTRACT(HOUR FROM date)) AS avg_commit_hour
      FROM git_commit_history
      GROUP BY author_name
    ),
    code_changes AS (
      SELECT 
        c.author_name,
        SUM(d.count_add) AS total_added,
        SUM(d.count_del) AS total_deleted,
        AVG(d.count_add + d.count_del) AS avg_commit_size,
        COUNT(DISTINCT COALESCE(d.src_file, d.dst_file)) AS unique_files,
        AVG(d.count_add) AS avg_add_per_commit,
        AVG(d.count_del) AS avg_del_per_commit
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      GROUP BY c.author_name
    ),
    commit_gaps AS (
      SELECT 
        author_name,
        AVG(gap_hours) AS avg_time_between_commits
      FROM (
        SELECT 
          author_name,
          date AS commit_date,
          LAG(date) OVER (PARTITION BY author_name ORDER BY date) AS prev_date,
          EXTRACT(EPOCH FROM (date - LAG(date) OVER (PARTITION BY author_name ORDER BY date))) / 3600.0 AS gap_hours
        FROM git_commit_history
      ) gaps
      WHERE gap_hours IS NOT NULL
      GROUP BY author_name
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
      cg.avg_time_between_commits,
      ROUND(1.0 * b.total_commits / NULLIF((SELECT total FROM total_commits_all), 0), 4) AS contribution_share,
      COALESCE(mt.prev_month_commits, 0) AS prev_month_commits,
      COALESCE(mt.current_month_commits, 0) AS current_month_commits,
      CASE 
        WHEN COALESCE(mt.current_month_commits, 0) > COALESCE(mt.prev_month_commits, 0) THEN 'рост'
        WHEN COALESCE(mt.current_month_commits, 0) < COALESCE(mt.prev_month_commits, 0) THEN 'падение'
        ELSE 'стабильно'
      END AS trend_direction,
      COALESCE(b.avg_commit_hour, 0) AS avg_commit_hour,
      COALESCE(cc.avg_add_per_commit, 0) AS avg_add_per_commit,
      COALESCE(cc.avg_del_per_commit, 0) AS avg_del_per_commit
    FROM base b
    LEFT JOIN code_changes cc ON b.author_name = cc.author_name
    LEFT JOIN commit_gaps cg ON b.author_name = cg.author_name
    LEFT JOIN monthly_trend mt ON b.author_name = mt.author_name
  "
  
  result <- tryCatch(
    DBI::dbExecute(conn, query),
    error = function(e) git_error("db_error", paste("Ошибка обновления метрик:", e$message))
  )
  if (is_git_error(result)) return(result)
  message("Таблица developer_metrics обновлена")
  return(invisible(TRUE))
}
#' Получить базовую статистику по разработчику из витрины
get_developer_stats <- function(conn, username = NULL) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  query <- "SELECT * FROM developer_metrics"
  if (!is.null(username)) {
    query <- paste(query, "WHERE author_name LIKE ?")
    params <- list(paste0("%", username, "%"))
  } else {
    params <- NULL
  }
  result <- tryCatch(
    DBI::dbGetQuery(conn, query, params = params),
    error = function(e) git_error("db_error", paste("Ошибка запроса статистики:", e$message))
  )
  # Если ошибка БД – вернём её, иначе даже пустой data.frame – успех
  return(result)
}

#' Сводная статистика по команде
get_summary_stats <- function(conn) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  query <- "
    SELECT 
      COUNT(*) AS total_developers,
      SUM(total_commits) AS total_commits,
      MIN(first_commit) AS first_commit,
      MAX(last_commit) AS last_commit,
      AVG(total_commits) AS avg_commits_per_dev,
      SUM(CASE WHEN contribution_share > 0.5 THEN 1 ELSE 0 END) AS critical_developers
    FROM developer_metrics
  "
  overview <- tryCatch(
    DBI::dbGetQuery(conn, query),
    error = function(e) git_error("db_error", paste("Ошибка сводной статистики:", e$message))
  )
  if (is_git_error(overview)) return(overview)
  
  top5 <- tryCatch(
    DBI::dbGetQuery(conn, "
      SELECT author_name, total_commits
      FROM developer_metrics
      ORDER BY total_commits DESC
      LIMIT 5
    "),
    error = function(e) git_error("db_error", paste("Ошибка получения топ-5:", e$message))
  )
  if (is_git_error(top5)) return(top5)
  
  list(overview = overview, top_5_developers = top5)
}

#' Получить командные метрики
get_team_metrics <- function(conn) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  df <- tryCatch(
    DBI::dbGetQuery(conn, "
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
        contribution_share,
        current_month_commits,
        prev_month_commits,
        trend_direction
      FROM developer_metrics
      ORDER BY total_commits DESC
    "),
    error = function(e) git_error("db_error", paste("Ошибка получения командных метрик:", e$message))
  )
  if (is_git_error(df)) return(df)
  
  if (nrow(df) > 0) {
    df$productivity_level <- cut(df$commits_per_day,
                                 breaks = c(-Inf, 0.5, 1.5, 3, Inf),
                                 labels = c("низкая", "средняя", "высокая", "очень высокая"))
  }
  df
}

#' Оценка рисков команды
get_team_risks <- function(conn) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  df <- tryCatch(
    DBI::dbGetQuery(conn, "
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
        CASE 
          WHEN unique_files > 50 AND avg_commit_size > 300 THEN 'high'
          WHEN unique_files > 20 OR avg_commit_size > 150 THEN 'medium'
          ELSE 'low'
        END AS bug_risk
      FROM developer_metrics
    "),
    error = function(e) git_error("db_error", paste("Ошибка оценки рисков:", e$message))
  )
  if (is_git_error(df)) return(df)
  
  if (nrow(df) > 0) {
    df <- df[, c("author_name", "total_commits", "burnout_risk", "bug_risk", "avg_time_between_commits")]
  }
  df
}