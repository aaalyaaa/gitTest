# metrics.R
# Единый источник метрик разработчиков

# metrics.R
refresh_developer_metrics <- function(conn) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  DBI::dbExecute(conn, "DROP TABLE IF EXISTS developer_metrics")
  DBI::dbExecute(conn, "DROP TABLE IF EXISTS developer_languages")
  
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
      avg_commit_hour REAL,
      avg_add_per_commit REAL,
      avg_del_per_commit REAL,
      primary_language VARCHAR,
      secondary_language VARCHAR,
      language_count INTEGER
    )
  ")
  
  lang_whitelist <- c(
    "c", "cpp", "cs", "java", "py", "go", "rb", "rs", "r", "js", "ts", 
    "jsx", "tsx", "php", "scala", "kt", "swift", "dart", "lua", "sql", 
    "html", "css", "scss", "R", "Rmd", "jl", "ex", "exs", "erl", "hrl", 
    "m", "mm", "groovy", "nim", "zig", "v", "odin"
  )
  whitelist_str <- paste0("('", paste(lang_whitelist, collapse = "','"), "')")
  
  query <- sprintf("
    WITH 
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
    ),
    lang_stats AS (
      SELECT 
        author_name,
        MAX(CASE WHEN lang_rank = 1 THEN file_extension END) AS primary_language,
        MAX(CASE WHEN lang_rank = 2 THEN file_extension END) AS secondary_language,
        COUNT(DISTINCT file_extension) AS language_count
      FROM (
        SELECT 
          c.author_name,
          d.file_extension,
          COUNT(*) AS cnt,
          ROW_NUMBER() OVER (PARTITION BY c.author_name ORDER BY COUNT(*) DESC) AS lang_rank
        FROM git_commit_history c
        JOIN git_file_changes d ON c.commit = d.commit
        WHERE d.file_extension IS NOT NULL AND d.file_extension != ''
          AND d.file_extension IN %s
        GROUP BY c.author_name, d.file_extension
      ) t
      GROUP BY author_name
    ),
    author_repo_total AS (
      SELECT 
        c.author_name,
        SUM(repo_stats.total_commits_in_repo) AS total_commits_in_my_repos
      FROM (
        SELECT DISTINCT author_name, repo_id
        FROM git_commit_history
      ) c
      JOIN (
        SELECT repo_id, COUNT(*) AS total_commits_in_repo
        FROM git_commit_history
        GROUP BY repo_id
      ) repo_stats ON c.repo_id = repo_stats.repo_id
      GROUP BY c.author_name
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
      ROUND(1.0 * b.total_commits / NULLIF(art.total_commits_in_my_repos, 0), 4) AS contribution_share,
      COALESCE(mt.prev_month_commits, 0) AS prev_month_commits,
      COALESCE(mt.current_month_commits, 0) AS current_month_commits,
      ROUND(COALESCE(b.avg_commit_hour, 0), 2) AS avg_commit_hour,
      ROUND(COALESCE(cc.avg_add_per_commit, 0), 2) AS avg_add_per_commit,
      ROUND(COALESCE(cc.avg_del_per_commit, 0), 2) AS avg_del_per_commit,
      COALESCE(ls.primary_language, 'unknown') AS primary_language,
      COALESCE(ls.secondary_language, '') AS secondary_language,
      COALESCE(ls.language_count, 0) AS language_count
    FROM base b
    LEFT JOIN code_changes cc ON b.author_name = cc.author_name
    LEFT JOIN commit_gaps cg ON b.author_name = cg.author_name
    LEFT JOIN monthly_trend mt ON b.author_name = mt.author_name
    LEFT JOIN lang_stats ls ON b.author_name = ls.author_name
    LEFT JOIN author_repo_total art ON b.author_name = art.author_name
  ", whitelist_str)
  
  DBI::dbExecute(conn, query)
  
  DBI::dbExecute(conn, sprintf("
    CREATE TABLE developer_languages AS
    SELECT 
      author_name,
      file_extension,
      COUNT(*) AS file_changes,
      ROW_NUMBER() OVER (PARTITION BY author_name ORDER BY COUNT(*) DESC) AS lang_rank
    FROM (
      SELECT 
        c.author_name,
        d.file_extension
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      WHERE d.file_extension IS NOT NULL AND d.file_extension != ''
        AND d.file_extension IN %s
    ) t
    GROUP BY author_name, file_extension
  ", whitelist_str))
  
  message("Таблица developer_metrics обновлена, developer_languages создана")
  invisible(TRUE)
}

get_developer_stats <- function(conn, username = NULL) {
  if (missing(conn) || is.null(conn)) return(git_error("invalid_argument", "conn не может быть NULL"))
  query <- "SELECT * FROM developer_metrics"
  if (!is.null(username)) {
    query <- paste(query, "WHERE author_name LIKE ?")
    params <- list(paste0("%", username, "%"))
  } else params <- NULL
  DBI::dbGetQuery(conn, query, params = params)
}

get_summary_stats <- function(conn) {
  if (missing(conn) || is.null(conn)) return(git_error("invalid_argument", "conn не может быть NULL"))
  overview <- DBI::dbGetQuery(conn, "
    SELECT 
      COUNT(*) AS total_developers,
      SUM(total_commits) AS total_commits,
      MIN(first_commit) AS first_commit,
      MAX(last_commit) AS last_commit,
      AVG(total_commits) AS avg_commits_per_dev,
      SUM(CASE WHEN contribution_share > 0.5 THEN 1 ELSE 0 END) AS critical_developers
    FROM developer_metrics
  ")
  top5 <- DBI::dbGetQuery(conn, "
    SELECT author_name, total_commits
    FROM developer_metrics
    ORDER BY total_commits DESC
    LIMIT 5
  ")
  list(overview = overview, top_5_developers = top5)
}