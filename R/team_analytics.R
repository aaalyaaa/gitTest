#' Получить профиль разработчика
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
  
  if (!is.null(precomputed_clusters) && !is_git_error(precomputed_clusters)) {
    clusters <- precomputed_clusters
  } else {
    clusters <- tryCatch(cluster_developers(conn), error = function(e) NULL)
  }
  cluster_type <- NA
  if (!is.null(clusters) && !is_git_error(clusters) && nrow(clusters$data) > 0) {
    match <- clusters$data[clusters$data$author_name == author_name, "cluster_type"]
    if (length(match) > 0) cluster_type <- as.character(match[1])
  }
  
  langs <- tryCatch({
    query <- sprintf("
      SELECT d.file_extension, COUNT(*) as cnt
      FROM git_commit_history c 
      JOIN git_file_changes d ON c.commit = d.commit
      WHERE c.author_name = '%s' AND d.file_extension != ''
      GROUP BY d.file_extension ORDER BY cnt DESC LIMIT 1
    ", author_name)
    DBI::dbGetQuery(conn, query)
  }, error = function(e) data.frame())
  main_lang <- if (nrow(langs) > 0) langs$file_extension[1] else "unknown"
  
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
  
  role <- switch(cluster_type,
                 "Ночной трудоголик" = "Ведущий разработчик (нестандартный график)",
                 "Активный разработчик" = "Лидер / core contributor",
                 "Многофайловый" = "Архитектор / интегратор систем",
                 "Низкоактивный ночной" = "Эксперт в узкой области (ночная работа)",
                 "Стабильный специалист" = "Стабильный исполнитель / поддержка",
                 "Разработчик")
  
  
  list(
    name = author_name, role = role, main_language = main_lang,
    work_style = work_style, contribution = contribution,
    total_commits = stats$total_commits[1],
    active_days = stats$active_days[1],
    cluster = cluster_type
  )
}
project_languages <- list(
  web        = c("js", "ts", "html", "css", "vue", "react", "jsx", "tsx"),
  backend    = c("go", "python", "java", "kotlin", "csharp", "php", "ruby", "scala"),
  data_science = c("python", "r", "sql", "julia", "scala"),
  mobile     = c("swift", "kotlin", "java", "dart", "objective-c"),
  devops     = c("go", "python", "ruby", "hcl", "yaml", "sh", "groovy"),
  game_dev   = c("cpp", "csharp", "lua", "python", "c"),
  frontend   = c("js", "ts", "html", "css", "scss", "less"),
  fullstack  = c("js", "ts", "python", "java", "go", "php", "ruby")
)
recommend_team <- function(conn, project_type = "generic", team_size = 5, required_languages = NULL) {
  if (missing(conn) || is.null(conn)) {
    cat("Ошибка: conn не может быть NULL\n")
    return(NULL)
  }
  
  clusters <- cluster_developers(conn)
  if (is_git_error(clusters)) {
    cat("Ошибка кластеризации:", clusters$message, "\n")
    return(NULL)
  }
  devs <- clusters$data
  if (nrow(devs) < team_size) {
    cat("Доступно только", nrow(devs), "разработчиков. Уменьшаем размер команды.\n")
    team_size <- nrow(devs)
  }
  
  # Фильтрация по языкам
  if (!is.null(required_languages) && length(required_languages) > 0) {
    lang_query <- "
      SELECT author_name, primary_language
      FROM developer_metrics
      WHERE primary_language IS NOT NULL AND primary_language != 'unknown'
    "
    dev_langs <- DBI::dbGetQuery(conn, lang_query)
    valid_devs <- dev_langs[dev_langs$primary_language %in% required_languages, "author_name"]
    devs <- devs[devs$author_name %in% valid_devs, ]
    
    if (nrow(devs) == 0) {
      cat("Нет разработчиков с требуемыми языками:", paste(required_languages, collapse = ", "), "\n")
      return(NULL)
    }
    if (nrow(devs) < team_size) {
      cat(sprintf("Только %d разработчиков владеют нужными языками (%s). Уменьшаем размер команды до %d.\n", 
                  nrow(devs), paste(required_languages, collapse = ", "), nrow(devs)))
      team_size <- nrow(devs)
    }
  }
  
  role_targets <- list(
    web = c("Активный разработчик" = 0.4, "Многофайловый" = 0.2, "Стабильный специалист" = 0.2, "Низкоактивный ночной" = 0.1, "Ночной трудоголик" = 0.1),
    backend = c("Активный разработчик" = 0.3, "Многофайловый" = 0.3, "Ночной трудоголик" = 0.2, "Стабильный специалист" = 0.1, "Низкоактивный ночной" = 0.1),
    data_science = c("Ночной трудоголик" = 0.4, "Активный разработчик" = 0.3, "Многофайловый" = 0.2, "Стабильный специалист" = 0.1, "Низкоактивный ночной" = 0.0),
    mobile = c("Активный разработчик" = 0.5, "Стабильный специалист" = 0.3, "Низкоактивный ночной" = 0.1, "Многофайловый" = 0.1, "Ночной трудоголик" = 0.0),
    devops = c("Многофайловый" = 0.4, "Активный разработчик" = 0.3, "Ночной трудоголик" = 0.2, "Низкоактивный ночной" = 0.1, "Стабильный специалист" = 0.0),
    game_dev = c("Активный разработчик" = 0.35, "Ночной трудоголик" = 0.35, "Многофайловый" = 0.2, "Низкоактивный ночной" = 0.1, "Стабильный специалист" = 0.0),
    frontend = c("Активный разработчик" = 0.4, "Стабильный специалист" = 0.3, "Ночной трудоголик" = 0.1, "Многофайловый" = 0.1, "Низкоактивный ночной" = 0.1),
    fullstack = c("Активный разработчик" = 0.35, "Стабильный специалист" = 0.25, "Многофайловый" = 0.2, "Ночной трудоголик" = 0.1, "Низкоактивный ночной" = 0.1)
  )
  targets <- role_targets[[project_type]]
  if (is.null(targets)) targets <- role_targets[["web"]]
  
  selected <- c()
  for (role in names(targets)) {
    candidates <- devs[devs$cluster_type == role, ]
    n_needed <- round(team_size * targets[role])
    if (n_needed > 0 && nrow(candidates) > 0) {
      chosen <- head(candidates[order(-candidates$total_commits), "author_name"], n_needed)
      selected <- c(selected, chosen)
    }
  }
  if (length(selected) < team_size) {
    remaining <- devs[!devs$author_name %in% selected, ]
    remaining <- remaining[order(-remaining$total_commits), "author_name"]
    selected <- c(selected, head(remaining, team_size - length(selected)))
  }
  
  result <- data.frame(
    author_name = selected,
    role = devs$cluster_type[match(selected, devs$author_name)],
    commits = devs$total_commits[match(selected, devs$author_name)]
  )
  
  # Добавляем основной язык
  langs_info <- DBI::dbGetQuery(conn, sprintf("
    SELECT author_name, primary_language 
    FROM developer_metrics 
    WHERE author_name IN (%s)
  ", paste(sprintf("'%s'", selected), collapse = ",")))
  result$primary_language <- langs_info$primary_language[match(result$author_name, langs_info$author_name)]
  
  cat("\n=== Рекомендуемый состав для проекта: ", project_type, " ===\n", sep = "")
  if (!is.null(required_languages)) {
    cat("Фильтр по языкам:", paste(required_languages, collapse = ", "), "\n")
  }
  cat("Размер команды:", team_size, "\n")
  print(result[, c("author_name", "role", "primary_language", "commits")])
  cat("\n--- Сводка по ролям ---\n")
  print(table(result$role))
  cat("\n--- Языки команды ---\n")
  print(table(result$primary_language))
  cat("\nСовместимость команды:\n")
  cat("- Разнообразие ролей:", length(unique(result$role)), "из", length(names(targets)), "\n")
  cat("- Средняя продуктивность:", round(mean(result$commits)), "коммитов\n")
  invisible(result)
}

#' Сводный отчёт по команде
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
    print(metrics[, c("author_name", "total_commits", "commits_per_day", "productivity_level", "trend_direction")])
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