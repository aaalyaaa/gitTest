# team_analytics.R

get_developer_profile <- function(conn, author_name, precomputed_clusters = NULL) {
  if (missing(conn) || is.null(conn)) {
    return(list(error = "conn не может быть NULL"))
  }
  if (missing(author_name) || is.null(author_name) || author_name == "") {
    return(list(error = "author_name обязателен"))
  }
  
  stats <- get_developer_stats(conn, username = author_name)
  if (is_git_error(stats)) {
    return(list(error = paste("Ошибка получения статистики:", stats$message)))
  }
  if (nrow(stats) == 0) {
    return(list(error = paste("Разработчик", author_name, "не найден")))
  }
  
  primary_lang <- stats$primary_language[1]
  secondary_lang <- stats$secondary_language[1]
  if (is.na(secondary_lang) || secondary_lang == "") secondary_lang <- "нет"
  
  season <- tryCatch(get_activity_seasonality(conn, author_name = author_name), error = function(e) NULL)
  work_style <- "unknown"
  if (!is.null(season) && !is_git_error(season) && nrow(season$peak_hours) > 0) {
    peak_hour <- season$peak_hours$hour[1]
    work_style <- ifelse(peak_hour < 8 | peak_hour > 22, "night_owl", "day_person")
  }
  
  team_avg <- tryCatch({
    team_stats <- get_developer_stats(conn)
    if (!is_git_error(team_stats)) mean(team_stats$total_commits, na.rm = TRUE) else NA
  }, error = function(e) NA)
  
  contribution <- "unknown"
  if (!is.na(team_avg) && !is.na(stats$total_commits[1])) {
    if (stats$total_commits[1] > team_avg * 1.2) contribution <- "high"
    else if (stats$total_commits[1] < team_avg * 0.8) contribution <- "low"
    else contribution <- "medium"
  }
  
  list(
    name = author_name,
    main_language = primary_lang,
    second_language = secondary_lang,
    work_style = work_style,
    contribution = contribution,
    total_commits = stats$total_commits[1],
    active_days = stats$active_days[1]
  )
}

compare_with_team <- function(conn, author_name, team_usernames = NULL) {
  if (missing(conn) || is.null(conn)) {
    return(git_error("invalid_argument", "conn не может быть NULL"))
  }
  if (missing(author_name) || is.null(author_name) || author_name == "") {
    return(git_error("invalid_argument", "author_name обязателен"))
  }
  
  user <- tryCatch(
    DBI::dbGetQuery(conn, sprintf("
      SELECT total_commits, avg_commit_size, active_days 
      FROM developer_metrics 
      WHERE author_name = '%s'
    ", gsub("'", "''", author_name))),
    error = function(e) git_error("db_error", paste("Ошибка получения пользователя:", e$message))
  )
  if (is_git_error(user)) return(user)
  if (nrow(user) == 0) {
    return(git_error("no_developer_error", paste("Разработчик", author_name, "не найден")))
  }
  
  if (!is.null(team_usernames) && length(team_usernames) > 0) {
    names_quoted <- paste0("'", gsub("'", "''", team_usernames), "'", collapse = ", ")
    team_query <- sprintf("
      SELECT AVG(total_commits) as avg_commits, 
             AVG(avg_commit_size) as avg_size, 
             AVG(active_days) as avg_days
      FROM developer_metrics
      WHERE author_name IN (%s)
    ", names_quoted)
  } else {
    team_query <- "
      SELECT AVG(total_commits) as avg_commits, 
             AVG(avg_commit_size) as avg_size, 
             AVG(active_days) as avg_days
      FROM developer_metrics
    "
  }
  team <- tryCatch(
    DBI::dbGetQuery(conn, team_query),
    error = function(e) git_error("db_error", paste("Ошибка получения команды:", e$message))
  )
  if (is_git_error(team)) return(team)
  
  data.frame(
    metric = c("Коммитов", "Средний размер", "Активных дней"),
    developer = as.numeric(user[1, ]),
    team_avg = as.numeric(team[1, ]),
    diff_percent = round(100 * (as.numeric(user[1, ]) - as.numeric(team[1, ])) / as.numeric(team[1, ]), 1),
    status = ifelse(abs(round(100 * (as.numeric(user[1, ]) - as.numeric(team[1, ])) / as.numeric(team[1, ]), 1)) > 20,
                    ifelse(round(100 * (as.numeric(user[1, ]) - as.numeric(team[1, ])) / as.numeric(team[1, ]), 1) > 0,
                           "выше среднего", "ниже среднего"), "в норме")
  )
}

#' Рекомендация команды (с проверкой всех языков через developer_languages)
recommend_team <- function(conn, project_type = "generic", team_size = 5, 
                           required_languages = NULL, since_date = NULL,
                           mode = c("balanced", "language", "productivity", "recent")) {
  
  if (missing(conn) || is.null(conn)) {
    cat("Ошибка: conn не может быть NULL\n")
    return(NULL)
  }
  
  mode <- match.arg(mode)
  
  weights_base <- switch(mode,
                         balanced = c(lang = 0.3125, recent = 0.3125, scope = 0.125, productivity = 0.25),
                         language = c(lang = 0.70, recent = 0.10, scope = 0.10, productivity = 0.10),
                         productivity = c(lang = 0.10, recent = 0.15, scope = 0.10, productivity = 0.65),
                         recent = c(lang = 0.10, recent = 0.60, scope = 0.10, productivity = 0.20)
  )
  
  compatibility_weight <- switch(mode,
                                 balanced = 0.20,
                                 language = 0.05,
                                 productivity = 0.10,
                                 recent = 0.10
  )
  
  if (is.null(required_languages)) {
    project_languages <- list(
      web = c("js", "ts", "html", "css", "vue", "react", "jsx", "tsx"),
      backend = c("go", "python", "java", "kotlin", "csharp", "php", "ruby", "scala"),
      data_science = c("python", "r", "sql", "julia", "scala"),
      mobile = c("swift", "kotlin", "java", "dart", "objective-c"),
      devops = c("go", "python", "ruby", "hcl", "yaml", "sh", "groovy"),
      game_dev = c("cpp", "csharp", "lua", "python", "c"),
      frontend = c("js", "ts", "html", "css", "scss", "less"),
      fullstack = c("js", "ts", "python", "java", "go", "php", "ruby")
    )
    required_languages <- project_languages[[project_type]]
    if (is.null(required_languages)) required_languages <- c("python", "js", "go")
  }
  
  if (is.null(since_date)) {
    since_date <- Sys.Date() - 90
  }
  
  # 1. Получаем всех разработчиков из витрины
  devs <- tryCatch(
    DBI::dbGetQuery(conn, "
      SELECT author_name, total_commits, active_days, repos_count, 
             total_added, total_deleted, avg_commit_size,
             primary_language, secondary_language
      FROM developer_metrics
    "),
    error = function(e) {
      cat("Ошибка получения данных разработчиков:", e$message, "\n")
      return(NULL)
    }
  )
  if (is.null(devs) || nrow(devs) == 0) {
    cat("Нет данных о разработчиках\n")
    return(NULL)
  }
  
  # 2. Фильтр по языку через таблицу developer_languages (все языки)
  all_langs <- DBI::dbGetQuery(conn, "
    SELECT author_name, file_extension
    FROM developer_languages
  ")
  author_all_langs <- split(all_langs$file_extension, all_langs$author_name)
  
  lang_match <- sapply(devs$author_name, function(author) {
    langs <- author_all_langs[[author]]
    if (is.null(langs)) return(FALSE)
    any(langs %in% required_languages)
  })
  devs <- devs[lang_match, ]
  if (nrow(devs) == 0) {
    cat("Нет разработчиков с требуемыми языками:", paste(required_languages, collapse = ", "), "\n")
    return(NULL)
  }
  
  # 3. Активность за последние 3 месяца
  active_query <- sprintf("
    SELECT author_name, COUNT(*) AS recent_commits
    FROM git_commit_history
    WHERE date >= '%s'
    GROUP BY author_name
  ", since_date)
  recent <- tryCatch(
    DBI::dbGetQuery(conn, active_query),
    error = function(e) {
      cat("Ошибка получения активности за период:", e$message, "\n")
      return(data.frame(author_name = character(), recent_commits = numeric()))
    }
  )
  devs$recent_commits <- ifelse(devs$author_name %in% recent$author_name, 
                                recent$recent_commits[match(devs$author_name, recent$author_name)], 0)
  devs$active_recent <- as.numeric(devs$recent_commits > 0)
  
  # 4. Широта опыта
  max_repos <- max(devs$repos_count, na.rm = TRUE)
  devs$scope_score <- ifelse(max_repos > 0, devs$repos_count / max_repos, 0)
  
  # 5. Продуктивность
  devs$productivity_raw <- devs$total_added * (devs$total_commits / pmax(devs$active_days, 1))
  max_prod <- max(devs$productivity_raw, na.rm = TRUE)
  devs$productivity_score <- ifelse(max_prod > 0, devs$productivity_raw / max_prod, 0)
  
  # 6. Базовый балл (вес языка = 1, т.к. язык уже отфильтрован)
  devs$base_score <- weights_base["lang"] * 1 + 
    weights_base["recent"] * devs$active_recent +
    weights_base["scope"] * devs$scope_score +
    weights_base["productivity"] * devs$productivity_score
  
  # 7. Данные для совместимости (общие репозитории)
  repos_data <- DBI::dbGetQuery(conn, "
    SELECT DISTINCT author_name, repo_id
    FROM git_commit_history
  ")
  author_repos <- split(repos_data$repo_id, repos_data$author_name)
  
  compute_compatibility <- function(candidate_name, selected_names) {
    if (length(selected_names) == 0) return(0)
    candidate_repos <- author_repos[[candidate_name]]
    if (is.null(candidate_repos) || length(candidate_repos) == 0) return(0)
    compat_sum <- 0
    for (sel in selected_names) {
      sel_repos <- author_repos[[sel]]
      if (!is.null(sel_repos)) {
        common <- length(intersect(candidate_repos, sel_repos))
        compat_sum <- compat_sum + common
      }
    }
    compat_sum / length(selected_names)
  }
  
  # 8. Итеративный отбор
  selected <- c()
  remaining <- devs
  total_weight <- 1 - compatibility_weight
  for (i in 1:team_size) {
    if (nrow(remaining) == 0) break
    candidate_scores <- sapply(1:nrow(remaining), function(j) {
      base <- remaining$base_score[j]
      compat <- compute_compatibility(remaining$author_name[j], selected)
      total <- total_weight * base + compatibility_weight * compat
      return(total)
    })
    best_idx <- which.max(candidate_scores)
    best <- remaining[best_idx, ]
    selected <- c(selected, best$author_name)
    remaining <- remaining[-best_idx, ]
  }
  
  # 9. Результат
  result <- devs[devs$author_name %in% selected, 
                 c("author_name", "primary_language", "secondary_language", 
                   "total_commits", "total_added", "repos_count", "recent_commits")]
  result$score <- NA
  for (nm in selected) {
    result$score[result$author_name == nm] <- devs$base_score[devs$author_name == nm]
  }
  names(result)[7] <- "recent_commits"
  
  result_list <- list(team = result, usernames = selected)
  cat("\n=== Рекомендуемый состав команды ===\n")
  cat("Режим приоритета:", mode, "\n")
  cat("Вес совместимости:", compatibility_weight, "\n")
  cat("Требуемые языки:", paste(required_languages, collapse = ", "), "\n")
  cat("Период активности: с", since_date, "\n")
  cat("Размер команды:", length(selected), "\n")
  print(result)
  invisible(result_list)
}

print_team_report <- function(conn, include_anomalies = FALSE) {
  if (missing(conn) || is.null(conn)) {
    cat("Ошибка: conn не может быть NULL\n")
    return(invisible(NULL))
  }
  
  cat("Отчет по команде разработчиков\n")
  
  summary <- get_summary_stats(conn)
  if (!is_git_error(summary)) {
    cat("Всего разработчиков:", summary$overview$total_developers, "\n")
    cat("Всего коммитов:", summary$overview$total_commits, "\n")
    cat("Критически важных разработчиков (вклад >0.5):", summary$overview$critical_developers, "\n")
  } else {
    cat("Ошибка получения сводной статистики:", summary$message, "\n")
  }
  
  metrics <- get_team_metrics(conn)
  if (!is_git_error(metrics) && nrow(metrics) > 0) {
    cat("\nПродуктивность\n")
    print(metrics[, c("author_name", "total_commits", "commits_per_day", "trend_direction")])
  } else if (is_git_error(metrics)) {
    cat("Ошибка получения метрик:", metrics$message, "\n")
  }
  
  if (include_anomalies) {
    anomalies <- tryCatch(get_all_anomalies(conn), error = function(e) NULL)
    if (!is.null(anomalies) && !is_git_error(anomalies) && nrow(anomalies) > 0) {
      cat("\n Топ аномалий\n")
      top_anom <- get_top_anomaly_developers(anomalies, n = 3)
      if (nrow(top_anom) > 0) print(top_anom)
    }
  }
}