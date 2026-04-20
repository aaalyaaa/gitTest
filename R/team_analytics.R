# team_analytics.R
# Функции для HR-аналитики, оценки продуктивности и подбора команд

#' Получить профиль разработчика для HR/тимлида
#' @param conn подключение к БД
#' @param author_name имя разработчика
#' @return список с характеристиками
get_developer_profile <- function(conn, author_name) {
  
  stats <- get_developer_stats(conn, username = author_name)
  if (inherits(stats, "error") || nrow(stats) == 0) {
    return(list(error = paste("Разработчик", author_name, "не найден")))
  }
  
  clusters <- tryCatch(cluster_developers(conn), error = function(e) NULL)
  cluster_type <- NA
  if (!is.null(clusters) && !inherits(clusters, "error") && nrow(clusters$data) > 0) {
    match <- clusters$data[clusters$data$author_name == author_name, "cluster_type"]
    if (length(match) > 0) cluster_type <- as.character(match[1])
  }
  
  langs <- get_preferred_languages(conn, username = author_name)
  if (!inherits(langs, "error") && nrow(langs) > 0) {
    main_lang <- langs$file_extension[1]
  } else {
    main_lang <- "unknown"
  }
  
  season <- get_activity_seasonality(conn, author_name = author_name)
  if (!inherits(season, "error") && nrow(season$peak_hours) > 0) {
    peak_hour <- season$peak_hours$hour[1]
    work_style <- ifelse(peak_hour < 8 | peak_hour > 22, "night_owl", "day_person")
  } else {
    work_style <- "unknown"
  }
  
  team_avg <- tryCatch({
    team_stats <- get_developer_stats(conn)
    mean(team_stats$total_commits, na.rm = TRUE)
  }, error = function(e) NA)
  
  if (!is.na(team_avg) && !is.na(stats$total_commits[1])) {
    if (stats$total_commits[1] > team_avg * 1.2) contribution <- "high"
    else if (stats$total_commits[1] < team_avg * 0.8) contribution <- "low"
    else contribution <- "medium"
  } else {
    contribution <- "unknown"
  }
  
  if (!is.na(cluster_type)) {
    role <- switch(cluster_type,
                   "Ночной трудоголик" = "Senior / нестандартный график",
                   "Активный разработчик" = "Лидер / core contributor",
                   "Многофайловый" = "Архитектор / интегратор",
                   "Совенок" = "Эксперт в узкой области",
                   "Специалист" = "Junior / стабильный исполнитель",
                   "Аналитик / ML")
  } else {
    role <- "Разработчик"
  }
  
  rec_tasks <- switch(cluster_type,
                      "Ночной трудоголик" = "сложные задачи без жёстких дедлайнов, исследование",
                      "Активный разработчик" = "критичные фичи, code review, менторство",
                      "Многофайловый" = "рефакторинг, интеграция модулей",
                      "Совенок" = "глубокие технические задачи, оптимизация",
                      "Специалист" = "поддержка, документация, багфиксы",
                      "участие в кодовой базе")
  
  return(list(
    name = author_name,
    role = role,
    main_language = main_lang,
    work_style = work_style,
    contribution = contribution,
    total_commits = stats$total_commits[1],
    active_days = stats$active_days[1],
    recommended_tasks = rec_tasks,
    cluster = cluster_type
  ))
}

#' Получить таблицу метрик всех разработчиков
#' @param conn подключение к БД
#' @return data.frame с ключевыми показателями
get_team_metrics <- function(conn) {
  
  stats <- get_developer_stats(conn)
  if (inherits(stats, "error")) {
    return(data.frame(error = stats$message))
  }
  
  if ("author_name" %in% names(stats)) {
    names(stats)[names(stats) == "author_name"] <- "author_name"
  }
  
  clusters <- tryCatch(cluster_developers(conn), error = function(e) NULL)
  cluster_df <- data.frame(author_name = character(), cluster_type = character())
  if (!is.null(clusters) && !inherits(clusters, "error") && nrow(clusters$data) > 0) {
    cluster_df <- clusters$data[, c("author_name", "cluster_type")]
  }
  
  anomalies <- tryCatch(get_all_anomalies(conn), error = function(e) NULL)
  anomaly_counts <- data.frame(author_name = character(), anomaly_count = numeric())
  if (!is.null(anomalies) && !inherits(anomalies, "error") && nrow(anomalies) > 0) {
    temp <- as.data.frame(table(anomalies$author_name))
    names(temp) <- c("author_name", "anomaly_count")
    anomaly_counts <- temp
  }
  
  lang_div <- tryCatch({
    langs <- get_preferred_languages(conn)
    if (!inherits(langs, "error") && nrow(langs) > 0) {
      aggregate(file_extension ~ author_name, data = langs, 
                FUN = function(x) paste(unique(x), collapse = ","))
    } else data.frame(author_name = character(), languages = character())
  }, error = function(e) data.frame(author_name = character(), languages = character()))
  
  result <- stats
  
  if (nrow(cluster_df) > 0) {
    result <- merge(result, cluster_df, by = "author_name", all.x = TRUE)
  } else {
    result$cluster_type <- NA
  }
  
  if (nrow(anomaly_counts) > 0) {
    result <- merge(result, anomaly_counts, by = "author_name", all.x = TRUE)
    result$anomaly_count[is.na(result$anomaly_count)] <- 0
  } else {
    result$anomaly_count <- 0
  }
  
  if (nrow(lang_div) > 0) {
    result <- merge(result, lang_div, by = "author_name", all.x = TRUE)
  } else {
    result$languages <- NA
  }
  
  result$commits_per_day <- round(result$total_commits / result$active_days, 2)
  result$productivity_level <- cut(result$commits_per_day, 
                                   breaks = c(-Inf, 0.5, 1.5, 3, Inf),
                                   labels = c("низкая", "средняя", "высокая", "очень высокая"))
  
  cols <- c("author_name", "cluster_type", "total_commits", "active_days", 
            "commits_per_day", "productivity_level", "languages", "anomaly_count")
  cols <- cols[cols %in% names(result)]
  result <- result[, cols]
  
  return(result)
}

#' Рекомендовать состав команды под тип проекта
#' @param conn подключение к БД
#' @param project_type тип проекта: "web", "backend", "data_science", "mobile", "generic"
#' @param team_size желаемый размер команды
#' @return data.frame с рекомендуемыми разработчиками
recommend_team <- function(conn, project_type = "generic", team_size = 5) {
  
  clusters <- cluster_developers(conn)
  if (inherits(clusters, "error")) {
    cat("Ошибка кластеризации:", clusters$message, "\n")
    return(NULL)
  }
  
  devs <- clusters$data
  if (nrow(devs) < team_size) {
    cat("Внимание: доступно только", nrow(devs), "разработчиков. Уменьшаем размер команды.\n")
    team_size <- nrow(devs)
  }
  
  role_targets <- list(
    web = c("Активный разработчик" = 0.4,
            "Многофайловый" = 0.2,
            "Специалист" = 0.2,
            "Совенок" = 0.1,
            "Ночной трудоголик" = 0.1),
    backend = c("Активный разработчик" = 0.3,
                "Многофайловый" = 0.3,
                "Совенок" = 0.2,
                "Специалист" = 0.1,
                "Ночной трудоголик" = 0.1),
    data_science = c("Совенок" = 0.4,
                     "Активный разработчик" = 0.3,
                     "Многофайловый" = 0.2,
                     "Специалист" = 0.1,
                     "Ночной трудоголик" = 0.0),
    mobile = c("Активный разработчик" = 0.5,
               "Специалист" = 0.3,
               "Совенок" = 0.1,
               "Многофайловый" = 0.1,
               "Ночной трудоголик" = 0.0),
    generic = c("Активный разработчик" = 0.35,
                "Специалист" = 0.25,
                "Многофайловый" = 0.2,
                "Совенок" = 0.1,
                "Ночной трудоголик" = 0.1)
  )
  
  targets <- role_targets[[project_type]]
  if (is.null(targets)) targets <- role_targets[["generic"]]
  
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
  
  cat("\n=== Рекомендуемый состав команды для проекта: ", project_type, " ===\n", sep = "")
  cat("Размер команды:", team_size, "\n")
  print(result)
  
  cat("\n--- Сводка по ролям ---\n")
  print(table(result$role))
  
  diversity <- length(unique(result$role)) / length(names(targets))
  cat("\nСовместимость команды:\n")
  cat("- Разнообразие ролей:", length(unique(result$role)), "из", length(names(targets)), "\n")
  cat("- Средняя продуктивность:", round(mean(result$commits)), "коммитов\n")
  
  return(invisible(result))
}

#' Оценка рисков для команды
#' @param conn подключение к БД
#' @return data.frame с рисками
get_team_risks <- function(conn) {
  
  # 1. Получаем данные по ночным коммитам
  query_night <- "
    SELECT 
        author_name,
        COUNT(*) as total_commits,
        SUM(CASE WHEN CAST(SUBSTR(CAST(date AS VARCHAR), 12, 2) AS INTEGER) BETWEEN 0 AND 5 
                 THEN 1 ELSE 0 END) as night_commits
    FROM git_commit_history
    GROUP BY author_name
  "
  base_risks <- DBI::dbGetQuery(conn, query_night)
  base_risks$night_ratio <- base_risks$night_commits / base_risks$total_commits
  
  # 2. Получаем данные по выходным дням
  query_all_dates <- "
    SELECT 
        author_name,
        SUBSTR(CAST(date AS VARCHAR), 1, 10) as commit_date
    FROM git_commit_history
  "
  all_dates <- DBI::dbGetQuery(conn, query_all_dates)
  
  # Считаем выходные коммиты через R
  if (nrow(all_dates) > 0) {
    all_dates$weekday <- strftime(as.Date(all_dates$commit_date), "%w")
    all_dates$is_weekend <- all_dates$weekday %in% c("0", "6")
    
    # Агрегируем по разработчикам
    weekend_stats <- aggregate(is_weekend ~ author_name, data = all_dates, FUN = sum)
    names(weekend_stats) <- c("author_name", "weekend_commits")
    
    # Добавляем в base_risks
    base_risks <- merge(base_risks, weekend_stats, by = "author_name", all.x = TRUE)
    base_risks$weekend_commits[is.na(base_risks$weekend_commits)] <- 0
  } else {
    base_risks$weekend_commits <- 0
  }
  
  base_risks$weekend_ratio <- base_risks$weekend_commits / base_risks$total_commits
  
  # 3. Риск выгорания
  base_risks$burnout_risk <- ifelse(base_risks$night_ratio > 0.3 | base_risks$weekend_ratio > 0.2, "high",
                                    ifelse(base_risks$night_ratio > 0.15 | base_risks$weekend_ratio > 0.1, "medium", "low"))
  
  # 4. Аномалии (риск багов)
  anomalies <- tryCatch(get_all_anomalies(conn), error = function(e) NULL)
  
  # Создаём пустой датафрейм для risk_count
  risk_count <- data.frame(author_name = base_risks$author_name, high_risk_events = 0)
  
  if (!is.null(anomalies) && !inherits(anomalies, "error") && nrow(anomalies) > 0) {
    risky_types <- c("large_commit", "sensitive_file", "frequent_file_changes")
    risky_anom <- anomalies[anomalies$anomaly_type %in% risky_types, ]
    
    if (nrow(risky_anom) > 0) {
      # Считаем количество опасных аномалий на разработчика
      temp_count <- as.data.frame(table(risky_anom$author_name))
      names(temp_count) <- c("author_name", "high_risk_events")
      risk_count <- merge(risk_count, temp_count, by = "author_name", all.x = TRUE)
      risk_count$high_risk_events <- ifelse(is.na(risk_count$high_risk_events.y), 
                                            risk_count$high_risk_events.x, 
                                            risk_count$high_risk_events.y)
      risk_count <- risk_count[, c("author_name", "high_risk_events")]
    }
  }
  
  # 5. Объединяем результаты
  result <- merge(base_risks, risk_count, by = "author_name", all.x = TRUE)
  result$high_risk_events[is.na(result$high_risk_events)] <- 0
  
  # 6. Риск багов
  result$bug_risk <- ifelse(result$high_risk_events > 3, "high",
                            ifelse(result$high_risk_events > 0, "medium", "low"))
  
  # 7. Итоговые колонки
  result <- result[, c("author_name", "total_commits", "night_ratio", "weekend_ratio", 
                       "burnout_risk", "high_risk_events", "bug_risk")]
  
  return(result)
}

#' Сводный отчёт по команде
#' @param conn подключение к БД
print_team_report <- function(conn) {
  cat("\n========================================\n")
  cat("        ОТЧЁТ ПО КОМАНДЕ РАЗРАБОТЧИКОВ\n")
  cat("========================================\n")
  
  summary <- get_summary_stats(conn)
  if (!inherits(summary, "error")) {
    cat("\n--- ОБЩАЯ СТАТИСТИКА ---\n")
    cat("Всего разработчиков:", summary$overview$total_developers, "\n")
    cat("Всего коммитов:", summary$overview$total_commits, "\n")
  }
  
  metrics <- get_team_metrics(conn)
  if (!inherits(metrics, "error") && nrow(metrics) > 0) {
    cat("\n--- ПРОДУКТИВНОСТЬ ---\n")
    print(metrics[, c("author_name", "total_commits", "commits_per_day", "productivity_level")])
  }
  
  risks <- get_team_risks(conn)
  if (!inherits(risks, "error") && nrow(risks) > 0) {
    cat("\n--- ОЦЕНКА РИСКОВ ---\n")
    print(risks[, c("author_name", "burnout_risk", "bug_risk")])
  }
  
  anomalies <- tryCatch(get_all_anomalies(conn), error = function(e) NULL)
  if (!is.null(anomalies) && !inherits(anomalies, "error") && nrow(anomalies) > 0) {
    cat("\n--- ТОП АНОМАЛИЙ ---\n")
    top_anom <- get_top_anomaly_developers(anomalies, n = 3)
    if (nrow(top_anom) > 0) print(top_anom)
  }
  
  cat("\n========================================\n")
}