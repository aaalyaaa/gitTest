library(dplyr)
library(tidyr)
library(forecast)
library(cluster)
library(randomForest)
library(factoextra)

prepare_clustering_data <- function(conn) {
  query <- "
    WITH developer_metrics AS (
      SELECT 
          c.author_name,
          COUNT(DISTINCT c.commit) as total_commits,
          COUNT(DISTINCT CAST(c.date AS VARCHAR)) as active_days,
          AVG(CAST(SUBSTR(CAST(c.date AS VARCHAR), 12, 2) AS INTEGER)) as avg_commit_hour,
          SUM(CASE WHEN CAST(SUBSTR(CAST(c.date AS VARCHAR), 12, 2) AS INTEGER) BETWEEN 0 AND 5 
               THEN 1 ELSE 0 END) as night_commits,
          SUM(CASE WHEN strftime('%w', CAST(c.date AS DATE)) IN ('0','6') 
               THEN 1 ELSE 0 END) as weekend_commits,
          SUM(d.count_add) as total_added,
          SUM(d.count_del) as total_deleted,
          AVG(d.count_add) as avg_add_per_commit,
          AVG(d.count_del) as avg_del_per_commit,
          COUNT(DISTINCT d.file_extension) as unique_extensions,
          SUM(CASE WHEN d.is_sensitive = TRUE THEN 1 ELSE 0 END) as sensitive_changes,
          COUNT(DISTINCT d.src_file) as unique_files
      FROM git_commit_history c
      JOIN git_file_changes d ON c.commit = d.commit
      GROUP BY c.author_name
    )
    SELECT * FROM developer_metrics
  "
  
  df <- DBI::dbGetQuery(conn, query)
  
  if (nrow(df) == 0) {
    return(list(error = "Нет данных для ML анализа"))
  }
  
  df_scaled <- df
  numeric_cols <- c("total_commits", "active_days", "avg_commit_hour", 
                    "night_commits", "weekend_commits", "total_added", 
                    "total_deleted", "avg_add_per_commit", "avg_del_per_commit",
                    "unique_extensions", "sensitive_changes", "unique_files")
  
  for (col in numeric_cols) {
    if (sd(df[[col]], na.rm = TRUE) > 0) {
      df_scaled[[col]] <- scale(df[[col]])
    } else {
      df_scaled[[col]] <- 0
    }
  }
  
  return(list(
    data = df,
    scaled = df_scaled,
    features = numeric_cols
  ))
}

cluster_developers <- function(conn, n_clusters = 3) {
  ml_data <- prepare_clustering_data(conn)
  if (!is.null(ml_data$error)) {
    return(ml_data)
  }
  
  if (nrow(ml_data$data) < n_clusters) {
    return(list(error = sprintf("Разработчиков (%d) меньше чем кластеров (%d)", 
                                nrow(ml_data$data), n_clusters)))
  }
  
  df_scaled <- ml_data$scaled[, ml_data$features, drop = FALSE]
  
  set.seed(123)
  kmeans_result <- kmeans(df_scaled, centers = n_clusters, nstart = 25)
  
  ml_data$data$cluster <- kmeans_result$cluster
  
  cluster_profiles <- aggregate(ml_data$data[, ml_data$features], 
                                by = list(cluster = ml_data$data$cluster), 
                                FUN = mean)
  
  cluster_names <- c()
  for (i in 1:n_clusters) {
    profile <- cluster_profiles[i, ]
    
    if (profile$total_commits > median(ml_data$data$total_commits)) {
      if (profile$avg_commit_hour < 8 || profile$avg_commit_hour > 22) {
        cluster_names <- c(cluster_names, "Ночной трудоголик")
      } else if (profile$weekend_commits > 5) {
        cluster_names <- c(cluster_names, "Работает в выходные")
      } else {
        cluster_names <- c(cluster_names, "Активный разработчик")
      }
    } else {
      if (profile$unique_extensions > median(ml_data$data$unique_extensions)) {
        cluster_names <- c(cluster_names, "Многотехнологичный")
      } else if (profile$night_commits > 3) {
        cluster_names <- c(cluster_names, "Совенок")
      } else {
        cluster_names <- c(cluster_names, "Специалист")
      }
    }
  }
  
  ml_data$data$cluster_type <- cluster_names[kmeans_result$cluster]
  cluster_profiles$cluster_type <- cluster_names
  
  cluster_stats <- aggregate(author_name ~ cluster_type, 
                             data = ml_data$data, 
                             FUN = length)
  names(cluster_stats) <- c("cluster_type", "developers_count")
  
  return(list(
    clustering = kmeans_result,
    data = ml_data$data,
    cluster_profiles = cluster_profiles,
    cluster_stats = cluster_stats,
    features = ml_data$features
  ))
}

cluster_commits <- function(conn, n_clusters = 3) {
  query <- "
    SELECT 
        c.commit,
        c.author_name,
        c.date,
        COUNT(*) as files_changed,
        SUM(d.count_add) as lines_added,
        SUM(d.count_del) as lines_deleted,
        SUM(d.count_add + d.count_del) as total_changes,
        COUNT(DISTINCT d.file_extension) as unique_extensions
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    GROUP BY c.commit, c.author_name, c.date
  "
  
  df <- DBI::dbGetQuery(conn, query)
  
  if (nrow(df) < 5) {
    return(list(error = "Недостаточно коммитов для кластеризации (нужно минимум 5)"))
  }
  
  features <- df[, c("files_changed", "lines_added", "lines_deleted", "total_changes", "unique_extensions")]
  features_scaled <- scale(features)
  
  set.seed(123)
  clusters <- kmeans(features_scaled, centers = min(n_clusters, nrow(df)), nstart = 10)
  
  df$commit_cluster <- clusters$cluster
  
  cluster_summary <- df %>%
    group_by(commit_cluster) %>%
    summarise(
      avg_files = mean(files_changed),
      avg_lines = mean(total_changes),
      тип = case_when(
        mean(total_changes) < 50 ~ "маленький коммит",
        mean(total_changes) < 200 ~ "средний коммит",
        TRUE ~ "большой коммит"
      ),
      count = n()
    )
  
  return(list(
    commits_with_clusters = df,
    cluster_summary = cluster_summary
  ))
}

find_anomaly_commits <- function(conn) {
  query <- "
    SELECT 
        c.commit,
        c.author_name,
        c.date,
        COUNT(*) as files_changed,
        SUM(d.count_add) as lines_added,
        SUM(d.count_del) as lines_deleted,
        SUM(d.count_add + d.count_del) as total_changes,
        COUNT(DISTINCT d.file_extension) as unique_extensions
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    GROUP BY c.commit, c.author_name, c.date
  "
  
  df <- DBI::dbGetQuery(conn, query)
  
  if (nrow(df) < 3) {
    return(data.frame(anomaly_warning = "Недостаточно коммитов для анализа"))
  }
  
  threshold_high <- quantile(df$total_changes, 0.95, na.rm = TRUE)
  threshold_low <- quantile(df$total_changes, 0.05, na.rm = TRUE)
  
  df$is_anomaly <- df$total_changes > threshold_high | df$total_changes < threshold_low
  df$anomaly_type <- ifelse(df$total_changes > threshold_high, "слишком большой",
                            ifelse(df$total_changes < threshold_low, "слишком маленький", "норма"))
  
  anomalies <- df[df$is_anomaly == TRUE, ]
  
  return(list(
    all_commits = df,
    anomalies = anomalies,
    threshold_high = threshold_high,
    threshold_low = threshold_low
  ))
}

classify_commit_type_by_metrics <- function(conn) {
  query <- "
    SELECT 
        c.commit,
        c.author_name,
        COUNT(*) as files_changed,
        SUM(d.count_add) as lines_added,
        SUM(d.count_del) as lines_deleted,
        SUM(d.count_add + d.count_del) as total_changes,
        COUNT(DISTINCT d.file_extension) as unique_extensions,
        SUM(CASE WHEN d.is_sensitive = TRUE THEN 1 ELSE 0 END) as sensitive_count
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    GROUP BY c.commit, c.author_name
  "
  
  df <- DBI::dbGetQuery(conn, query)
  
  if (nrow(df) < 10) {
    return(list(error = "Недостаточно коммитов для классификации (нужно минимум 10)"))
  }
  
  df$predicted_type <- case_when(
    df$files_changed > 10 & df$total_changes > 500 ~ "крупное_изменение",
    df$files_changed == 1 & df$total_changes < 50 ~ "точечная_правка",
    df$unique_extensions > 3 ~ "кросс_платформенное",
    df$sensitive_count > 0 ~ "чувствительное_изменение",
    df$lines_added / (df$lines_deleted + 1) > 5 ~ "добавление_кода",
    df$lines_deleted / (df$lines_added + 1) > 3 ~ "удаление_кода",
    TRUE ~ "обычный_коммит"
  )
  
  type_stats <- df %>%
    group_by(predicted_type) %>%
    summarise(count = n(), percentage = round(100 * n() / nrow(df), 1))
  
  return(list(
    commits_with_types = df,
    type_stats = type_stats
  ))
}

forecast_developer_activity <- function(conn, author_name, forecast_days = 7) {
  query <- sprintf("
    SELECT 
        CAST(SUBSTR(CAST(c.date AS VARCHAR), 1, 10) AS DATE) as commit_date,
        COUNT(*) as commits
    FROM git_commit_history c
    WHERE c.author_name = '%s'
    GROUP BY commit_date
    ORDER BY commit_date
  ", author_name)
  
  df <- DBI::dbGetQuery(conn, query)
  
  if (nrow(df) < 5) {
    return(list(error = sprintf("Недостаточно данных для %s (нужно минимум 5 дней)", author_name)))
  }
  
  ts_data <- ts(df$commits, frequency = min(7, nrow(df)))
  
  fit <- tryCatch({
    auto.arima(ts_data)
  }, error = function(e) NULL)
  
  if (is.null(fit)) {
    return(list(error = "Не удалось построить модель ARIMA"))
  }
  
  forecast_result <- forecast(fit, h = forecast_days)
  
  return(list(
    author = author_name,
    historical = df,
    forecast = forecast_result,
    expected_commits_next_week = round(sum(forecast_result$mean), 1),
    plot_data = data.frame(
      day = 1:forecast_days,
      forecast = as.numeric(forecast_result$mean),
      lower = as.numeric(forecast_result$lower[, 2]),
      upper = as.numeric(forecast_result$upper[, 2])
    )
  ))
}

get_activity_seasonality <- function(conn, author_name = NULL) {
  where_clause <- if (!is.null(author_name)) {
    sprintf("WHERE author_name = '%s'", author_name)
  } else ""
  
  query_hour <- sprintf("
    SELECT 
        CAST(SUBSTR(CAST(c.date AS VARCHAR), 12, 2) AS INTEGER) as hour,
        COUNT(*) as commits
    FROM git_commit_history c
    %s
    GROUP BY hour
    ORDER BY hour
  ", where_clause)
  
  query_weekday <- sprintf("
    SELECT 
        CASE strftime('%%w', CAST(c.date AS DATE))
            WHEN '0' THEN 'Воскресенье'
            WHEN '1' THEN 'Понедельник'
            WHEN '2' THEN 'Вторник'
            WHEN '3' THEN 'Среда'
            WHEN '4' THEN 'Четверг'
            WHEN '5' THEN 'Пятница'
            WHEN '6' THEN 'Суббота'
        END as weekday,
        COUNT(*) as commits
    FROM git_commit_history c
    %s
    GROUP BY weekday
    ORDER BY commits DESC
  ", where_clause)
  
  hour_data <- DBI::dbGetQuery(conn, query_hour)
  weekday_data <- DBI::dbGetQuery(conn, query_weekday)
  
  if (nrow(hour_data) > 0) {
    peak_hours <- hour_data[order(-hour_data$commits), ][1:3, ]
  } else {
    peak_hours <- data.frame()
  }
  
  most_active_day <- if (nrow(weekday_data) > 0) weekday_data[1, ] else data.frame()
  
  return(list(
    by_hour = hour_data,
    by_weekday = weekday_data,
    peak_hours = peak_hours,
    most_active_day = most_active_day
  ))
}

find_related_files <- function(conn, min_co_occurrence = 2) {
  query <- "
    SELECT 
        c.commit,
        d.src_file
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    WHERE d.src_file != ''
  "
  
  df <- DBI::dbGetQuery(conn, query)
  
  if (nrow(df) < 10) {
    return(list(error = "Недостаточно данных"))
  }
  
  file_pairs <- df %>%
    group_by(commit) %>%
    summarise(files = list(src_file)) %>%
    filter(lengths(files) > 1) %>%
    tidyr::unnest(files) %>%
    group_by(commit) %>%
    summarise(pairs = list(combn(files, 2, simplify = FALSE))) %>%
    tidyr::unnest(pairs) %>%
    mutate(
      file1 = sapply(pairs, `[`, 1),
      file2 = sapply(pairs, `[`, 2)
    ) %>%
    select(-pairs) %>%
    group_by(file1, file2) %>%
    summarise(together = n(), .groups = 'drop') %>%
    filter(together >= min_co_occurrence) %>%
    arrange(desc(together))
  
  return(list(
    related_files = file_pairs,
    recommendation = "Если меняете file1, вероятно, нужно поменять и file2"
  ))
}

compare_with_team <- function(conn, author_name) {
  query_user <- sprintf("
    SELECT 
        COUNT(DISTINCT c.commit) as commits,
        AVG(d.count_add + d.count_del) as avg_commit_size,
        COUNT(DISTINCT CAST(c.date AS VARCHAR))) as active_days
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    WHERE c.author_name = '%s'
  ", author_name)
  
  query_team <- "
    SELECT 
        AVG(commits) as avg_team_commits,
        AVG(avg_size) as avg_team_size,
        AVG(days) as avg_team_days
    FROM (
        SELECT 
            c.author_name,
            COUNT(DISTINCT c.commit) as commits,
            AVG(d.count_add + d.count_del) as avg_size,
            COUNT(DISTINCT CAST(c.date AS VARCHAR))) as days
        FROM git_commit_history c
        JOIN git_file_changes d ON c.commit = d.commit
        GROUP BY c.author_name
    )
  "
  
  user <- DBI::dbGetQuery(conn, query_user)
  team <- DBI::dbGetQuery(conn, query_team)
  
  comparison <- data.frame(
    metric = c("Коммитов", "Средний размер", "Активных дней"),
    developer = as.numeric(user[1, ]),
    team_avg = as.numeric(team[1, ]),
    difference_percent = round(100 * (as.numeric(user[1, ]) - as.numeric(team[1, ])) / as.numeric(team[1, ]), 1)
  )
  
  comparison$status <- ifelse(comparison$difference_percent > 20, "выше среднего",
                              ifelse(comparison$difference_percent < -20, "ниже среднего", "в норме"))
  
  return(comparison)
}

get_activity_trends <- function(conn, author_name = NULL) {
  where_clause <- if (!is.null(author_name)) {
    sprintf("WHERE c.author_name = '%s'", author_name)
  } else ""
  
  query <- sprintf("
    SELECT 
        SUBSTR(CAST(c.date AS VARCHAR), 1, 7) as month,
        COUNT(DISTINCT c.commit) as commits,
        AVG(d.count_add + d.count_del) as avg_size,
        COUNT(DISTINCT CAST(c.date AS VARCHAR))) as active_days
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    %s
    GROUP BY month
    ORDER BY month
  ", where_clause)
  
  df <- DBI::dbGetQuery(conn, query)
  
  if (nrow(df) < 2) {
    return(list(error = "Недостаточно месяцев для анализа тренда"))
  }
  
  df$trend_commits <- ifelse(nrow(df) > 1, c(NA, diff(df$commits)), NA)
  df$trend_direction <- ifelse(df$trend_commits > 0, "рост",
                               ifelse(df$trend_commits < 0, "падение", "стабильно"))
  
  overall_trend <- ifelse(coef(lm(commits ~ seq_len(nrow(df)), data = df))[2] > 0, 
                          "общий тренд: РОСТ", "общий тренд: ПАДЕНИЕ")
  
  return(list(
    monthly_data = df,
    overall_trend = overall_trend,
    best_month = df[which.max(df$commits), ],
    worst_month = df[which.min(df$commits), ]
  ))
}