# team_analytics.R (исправленный)
# Зависит от metrics.R и ml_analysis.R

#' Получить профиль разработчика для HR/тимлида (интегрирует витрину и ML)
get_developer_profile <- function(conn, author_name) {
  stats <- get_developer_stats(conn, username = author_name)
  if (inherits(stats, "error") || nrow(stats) == 0) {
    return(list(error = paste("Разработчик", author_name, "не найден")))
  }
  
  # Кластеризация (ML)
  clusters <- tryCatch(cluster_developers(conn), error = function(e) NULL)
  cluster_type <- NA
  if (!is.null(clusters) && !inherits(clusters, "error") && nrow(clusters$data) > 0) {
    match <- clusters$data[clusters$data$author_name == author_name, "cluster_type"]
    if (length(match) > 0) cluster_type <- as.character(match[1])
  }
  
  # Предпочитаемые языки (берём из сырых данных, т.к. в витрине нет)
  langs <- tryCatch({
    query <- sprintf("
      SELECT d.file_extension, COUNT(*) as cnt
      FROM git_commit_history c JOIN git_file_changes d ON c.commit = d.commit
      WHERE c.author_name = '%s' AND d.file_extension != ''
      GROUP BY d.file_extension ORDER BY cnt DESC LIMIT 1
    ", author_name)
    DBI::dbGetQuery(conn, query)
  }, error = function(e) data.frame())
  main_lang <- if (nrow(langs) > 0) langs$file_extension[1] else "unknown"
  
  # Сезонность (ML)
  season <- tryCatch(get_activity_seasonality(conn, author_name = author_name), error = function(e) NULL)
  work_style <- "unknown"
  if (!is.null(season) && !inherits(season, "error") && nrow(season$peak_hours) > 0) {
    peak_hour <- season$peak_hours$hour[1]
    work_style <- ifelse(peak_hour < 8 | peak_hour > 22, "night_owl", "day_person")
  }
  
  # Сравнение с командой (из витрины)
  team_avg <- tryCatch({
    team_stats <- get_developer_stats(conn)
    mean(team_stats$total_commits, na.rm = TRUE)
  }, error = function(e) NA)
  
  contribution <- "unknown"
  if (!is.na(team_avg) && !is.na(stats$total_commits[1])) {
    if (stats$total_commits[1] > team_avg * 1.2) contribution <- "high"
    else if (stats$total_commits[1] < team_avg * 0.8) contribution <- "low"
    else contribution <- "medium"
  }
  
  role <- switch(cluster_type,
                 "Ночной трудоголик" = "Senior / нестандартный график",
                 "Активный разработчик" = "Лидер / core contributor",
                 "Многофайловый" = "Архитектор / интегратор",
                 "Совенок" = "Эксперт в узкой области",
                 "Специалист" = "Junior / стабильный исполнитель",
                 "Разработчик")
  
  rec_tasks <- switch(cluster_type,
                      "Ночной трудоголик" = "сложные задачи без жёстких дедлайнов, исследование",
                      "Активный разработчик" = "критичные фичи, code review, менторство",
                      "Многофайловый" = "рефакторинг, интеграция модулей",
                      "Совенок" = "глубокие технические задачи, оптимизация",
                      "Специалист" = "поддержка, документация, багфиксы",
                      "участие в кодовой базе")
  
  list(
    name = author_name, role = role, main_language = main_lang,
    work_style = work_style, contribution = contribution,
    total_commits = stats$total_commits[1],
    active_days = stats$active_days[1],
    recommended_tasks = rec_tasks, cluster = cluster_type
  )
}

#' Рекомендовать состав команды (использует ML-кластеризацию)
recommend_team <- function(conn, project_type = "generic", team_size = 5) {
  clusters <- cluster_developers(conn)
  if (inherits(clusters, "error")) {
    cat("Ошибка кластеризации:", clusters$message, "\n"); return(NULL)
  }
  devs <- clusters$data
  if (nrow(devs) < team_size) {
    cat("Доступно только", nrow(devs), "разработчиков. Уменьшаем размер команды.\n")
    team_size <- nrow(devs)
  }
  
  role_targets <- list(
    web = c("Активный разработчик" = 0.4, "Многофайловый" = 0.2, "Специалист" = 0.2, "Совенок" = 0.1, "Ночной трудоголик" = 0.1),
    backend = c("Активный разработчик" = 0.3, "Многофайловый" = 0.3, "Совенок" = 0.2, "Специалист" = 0.1, "Ночной трудоголик" = 0.1),
    data_science = c("Совенок" = 0.4, "Активный разработчик" = 0.3, "Многофайловый" = 0.2, "Специалист" = 0.1, "Ночной трудоголик" = 0.0),
    mobile = c("Активный разработчик" = 0.5, "Специалист" = 0.3, "Совенок" = 0.1, "Многофайловый" = 0.1, "Ночной трудоголик" = 0.0),
    generic = c("Активный разработчик" = 0.35, "Специалист" = 0.25, "Многофайловый" = 0.2, "Совенок" = 0.1, "Ночной трудоголик" = 0.1)
  )
  targets <- role_targets[[project_type]] %||% role_targets[["generic"]]
  
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
  cat("\n=== Рекомендуемый состав для проекта: ", project_type, " ===\n", sep = "")
  cat("Размер команды:", team_size, "\n"); print(result)
  cat("\n--- Сводка по ролям ---\n"); print(table(result$role))
  cat("\nСовместимость команды:\n")
  cat("- Разнообразие ролей:", length(unique(result$role)), "из", length(names(targets)), "\n")
  cat("- Средняя продуктивность:", round(mean(result$commits)), "коммитов\n")
  invisible(result)
}

#' Сводный отчёт по команде (использует витрину и ML)
print_team_report <- function(conn) {
  cat("\n========================================\n")
  cat("        ОТЧЁТ ПО КОМАНДЕ РАЗРАБОТЧИКОВ\n")
  cat("========================================\n")
  
  summary <- get_summary_stats(conn)
  if (!inherits(summary, "error")) {
    cat("\n--- ОБЩАЯ СТАТИСТИКА ---\n")
    cat("Всего разработчиков:", summary$overview$total_developers, "\n")
    cat("Всего коммитов:", summary$overview$total_commits, "\n")
    cat("Критически важных разработчиков (bus factor >0.5):", summary$overview$critical_developers, "\n")
  }
  
  metrics <- get_team_metrics(conn)
  if (!inherits(metrics, "error") && nrow(metrics) > 0) {
    cat("\n--- ПРОДУКТИВНОСТЬ ---\n")
    print(metrics[, c("author_name", "total_commits", "commits_per_day", "productivity_level", "trend_direction")])
  }
  
  risks <- get_team_risks(conn)
  if (!inherits(risks, "error") && nrow(risks) > 0) {
    cat("\n--- ОЦЕНКА РИСКОВ ---\n")
    print(risks[, c("author_name", "burnout_risk", "bug_risk", "avg_time_between_commits")])
  }
  
  # Аномалии – оставляем вызов из anomalies_detection.R (не трогаем)
  if (exists("get_all_anomalies")) {
    anomalies <- tryCatch(get_all_anomalies(conn), error = function(e) NULL)
    if (!is.null(anomalies) && nrow(anomalies) > 0) {
      cat("\n--- ТОП АНОМАЛИЙ ---\n")
      top_anom <- get_top_anomaly_developers(anomalies, n = 3)
      if (nrow(top_anom) > 0) print(top_anom)
    }
  }
  cat("\n========================================\n")
}

# Вспомогательный оператор %||% (если нет в base)
`%||%` <- function(x, y) if (is.null(x)) y else x