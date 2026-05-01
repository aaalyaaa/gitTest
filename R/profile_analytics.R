# profile_analytics.R
# Функции для профилирования отдельных разработчиков
# Исправлено: зависимости из репозитория добавляются только если разработчик изменял файлы зависимостей

git_error <- function(class, message, ...) {
  structure(list(message = message, ...), class = c(class, "error", "condition"))
}
is_git_error <- function(x) inherits(x, "error")
`%||%` <- function(x, y) if (is.null(x)) y else x

# -------------------- Технологический стек: словарь библиотек --------------------
tech_dictionary <- list(
  "django" = c("django"), "flask" = c("flask"), "fastapi" = c("fastapi"),
  "tornado" = c("tornado"), "pandas" = c("pandas"), "numpy" = c("numpy"),
  "scikit-learn" = c("scikit-learn", "sklearn"), "pytorch" = c("pytorch", "torch"),
  "tensorflow" = c("tensorflow", "tf"), "keras" = c("keras"), "xgboost" = c("xgboost"),
  "lightgbm" = c("lightgbm"), "catboost" = c("catboost"), "huggingface" = c("huggingface", "transformers"),
  "opencv" = c("opencv", "cv2"), "matplotlib" = c("matplotlib"), "seaborn" = c("seaborn"),
  "plotly" = c("plotly"), "statsmodels" = c("statsmodels"), "polars" = c("polars"),
  "sqlalchemy" = c("sqlalchemy"), "alembic" = c("alembic"), "psycopg2" = c("psycopg2"),
  "pymongo" = c("pymongo"), "redis" = c("redis"), "celery" = c("celery"), "pydantic" = c("pydantic"),
  "pytest" = c("pytest"), "unittest" = c("unittest"), "hypothesis" = c("hypothesis"),
  "airflow" = c("airflow"), "prefect" = c("prefect"), "mlflow" = c("mlflow"),
  "uvicorn" = c("uvicorn"), "gunicorn" = c("gunicorn"),
  "react" = c("react", "React", "ReactDOM"), "angular" = c("angular", "@angular"),
  "vue" = c("vue", "Vue"), "svelte" = c("svelte"), "nextjs" = c("next", "nextjs"),
  "nodejs" = c("node", "node.js"), "express" = c("express"), "nestjs" = c("@nestjs", "nestjs"),
  "jest" = c("jest"), "cypress" = c("cypress"), "prisma" = c("prisma"), "graphql" = c("graphql"),
  "spring" = c("spring", "springframework"), "hibernate" = c("hibernate"), "maven" = c("maven"),
  "gradle" = c("gradle"), "junit" = c("junit"), "gin" = c("gin"), "gorm" = c("gorm"),
  "tokio" = c("tokio"), "serde" = c("serde"), "dotnet" = c(".NET", "dotnet"),
  "rails" = c("rails"), "laravel" = c("laravel"), "ggplot2" = c("ggplot2"), "dplyr" = c("dplyr"),
  "tidyverse" = c("tidyverse"), "shiny" = c("shiny"), "postgresql" = c("postgres", "postgresql"),
  "mysql" = c("mysql"), "docker" = c("docker"), "kubernetes" = c("kubernetes", "k8s"),
  "terraform" = c("terraform"), "aws" = c("aws"), "azure" = c("azure"), "gcp" = c("gcp", "google cloud"),
  "react_native" = c("react native"), "flutter" = c("flutter")
)

# -------------------- Базовое определение по файлам и расширениям --------------------
get_tech_stack_base <- function(conn, author_name) {
  if (missing(conn) || is.null(conn)) return(git_error("invalid_argument", "conn не может быть NULL"))
  if (missing(author_name) || author_name == "") return(git_error("invalid_argument", "author_name обязателен"))
  
  query <- sprintf("
    SELECT DISTINCT COALESCE(d.src_file, d.dst_file) as file_path
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    WHERE c.author_name = '%s'
  ", gsub("'", "''", author_name))
  
  files <- tryCatch(DBI::dbGetQuery(conn, query), error = function(e) data.frame())
  if (nrow(files) == 0) return(character())
  file_paths <- files$file_path
  detected <- character()
  
  special_files <- list(
    "python" = c("requirements.txt", "setup.py", "pyproject.toml"),
    "r" = c("DESCRIPTION", "NAMESPACE", ".Rproj"),
    "javascript" = c("package.json", "yarn.lock"),
    "typescript" = c("tsconfig.json"),
    "java" = c("pom.xml", "build.gradle"),
    "go" = c("go.mod"),
    "rust" = c("Cargo.toml"),
    "csharp" = c(".csproj", ".sln"),
    "php" = c("composer.json"),
    "ruby" = c("Gemfile"),
    "docker" = c("Dockerfile"),
    "terraform" = c(".tf")
  )
  for (tech in names(special_files)) {
    if (any(sapply(special_files[[tech]], function(p) any(grepl(p, file_paths, ignore.case = TRUE))))) {
      detected <- c(detected, tech)
    }
  }
  
  ext_map <- list(
    "python" = "\\.py$", "r" = "\\.R$", "javascript" = "\\.js$", "typescript" = "\\.ts$",
    "java" = "\\.java$", "go" = "\\.go$", "rust" = "\\.rs$", "csharp" = "\\.cs$",
    "cpp" = "\\.(cpp|hpp|cc|cxx|c|h)$", "php" = "\\.php$", "ruby" = "\\.rb$",
    "swift" = "\\.swift$", "scala" = "\\.scala$", "dart" = "\\.dart$", "lua" = "\\.lua$",
    "sql" = "\\.sql$", "html_css" = "\\.(html|css|scss)$"
  )
  for (tech in names(ext_map)) {
    if (tech %in% detected) next
    if (any(grepl(ext_map[[tech]], file_paths, ignore.case = TRUE))) detected <- c(detected, tech)
  }
  unique(detected)
}

# -------------------- Чтение файлов зависимостей из репозитория --------------------
read_dependency_files <- function(repo_path) {
  if (!dir.exists(repo_path)) return(character())
  detected <- character()
  
  req_file <- file.path(repo_path, "requirements.txt")
  if (file.exists(req_file)) {
    lines <- tryCatch(readLines(req_file, warn = FALSE), error = function(e) character())
    for (line in lines) {
      line <- trimws(line)
      if (line == "" || startsWith(line, "#")) next
      pkg <- strsplit(line, "[=<>!]")[[1]][1]
      for (tech_name in names(tech_dictionary)) {
        if (any(sapply(tech_dictionary[[tech_name]], function(x) grepl(x, pkg, ignore.case = TRUE)))) {
          detected <- c(detected, tech_name)
        }
      }
    }
  }
  
  pkg_json <- file.path(repo_path, "package.json")
  if (file.exists(pkg_json) && requireNamespace("jsonlite", quietly = TRUE)) {
    json_content <- tryCatch(jsonlite::read_json(pkg_json), error = function(e) list())
    if (!is.null(json_content$dependencies)) {
      deps <- names(json_content$dependencies)
      for (dep in deps) {
        for (tech_name in names(tech_dictionary)) {
          if (any(sapply(tech_dictionary[[tech_name]], function(x) grepl(x, dep, ignore.case = TRUE)))) {
            detected <- c(detected, tech_name)
          }
        }
      }
    }
  }
  
  go_mod <- file.path(repo_path, "go.mod")
  if (file.exists(go_mod)) {
    lines <- readLines(go_mod, warn = FALSE)
    for (line in lines) {
      if (grepl("^require", line)) {
        parts <- strsplit(line, " ")[[1]]
        if (length(parts) >= 2) {
          module <- parts[2]
          for (tech_name in names(tech_dictionary)) {
            if (any(sapply(tech_dictionary[[tech_name]], function(x) grepl(x, module, ignore.case = TRUE)))) {
              detected <- c(detected, tech_name)
            }
          }
        }
      }
    }
  }
  
  cargo <- file.path(repo_path, "Cargo.toml")
  if (file.exists(cargo)) {
    content <- paste(readLines(cargo, warn = FALSE), collapse = "\n")
    for (tech_name in names(tech_dictionary)) {
      if (any(sapply(tech_dictionary[[tech_name]], function(x) grepl(x, content, ignore.case = TRUE)))) {
        detected <- c(detected, tech_name)
      }
    }
  }
  
  pom <- file.path(repo_path, "pom.xml")
  if (file.exists(pom)) {
    content <- paste(readLines(pom, warn = FALSE), collapse = "\n")
    for (tech_name in names(tech_dictionary)) {
      if (any(sapply(tech_dictionary[[tech_name]], function(x) grepl(x, content, ignore.case = TRUE)))) {
        detected <- c(detected, tech_name)
      }
    }
  }
  unique(detected)
}

# -------------------- Получение репозиториев, где разработчик изменял файлы зависимостей --------------------
get_repos_with_dependency_changes <- function(conn, author_name) {
  dep_files <- c(
    "requirements.txt", "setup.py", "pyproject.toml",
    "package.json", "yarn.lock",
    "go.mod",
    "Cargo.toml",
    "pom.xml", "build.gradle", "settings.gradle",
    "composer.json",
    "Gemfile",
    "DESCRIPTION", "NAMESPACE"
  )
  query <- sprintf("
    SELECT DISTINCT rp.path
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    JOIN repo_path rp ON c.repo_id = rp.id
    WHERE c.author_name = '%s'
  ", gsub("'", "''", author_name))
  
  files_df <- tryCatch(DBI::dbGetQuery(conn, query), error = function(e) data.frame())
  if (nrow(files_df) == 0) return(character())
  
  dep_files_lower <- tolower(dep_files)
  changed_repos <- character()
  for (i in seq_len(nrow(files_df))) {
    repo_path <- files_df$path[i]
    # В запросе выше мы не получили конкретные файлы, нужно переписать запрос, чтобы получить имена файлов.
    # Лучше сделать отдельный запрос на получение изменённых файлов.
    # Переделаем:
  }
  # Более простой и надёжный способ: отдельный запрос на файлы
  files_query <- sprintf("
    SELECT DISTINCT COALESCE(d.dst_file, d.src_file) as file_name, rp.path as repo_path
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    JOIN repo_path rp ON c.repo_id = rp.id
    WHERE c.author_name = '%s'
  ", gsub("'", "''", author_name))
  
  changes <- tryCatch(DBI::dbGetQuery(conn, files_query), error = function(e) data.frame())
  if (nrow(changes) == 0) return(character())
  
  dep_files_lower <- tolower(dep_files)
  is_dep_file <- sapply(tolower(basename(changes$file_name)), function(f) f %in% dep_files_lower)
  unique(changes$repo_path[is_dep_file])
}

# -------------------- Поиск библиотек в коде (added_code) --------------------
extract_libraries_from_code <- function(conn, author_name) {
  query <- sprintf("
    SELECT DISTINCT d.added_code
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    WHERE c.author_name = '%s' AND d.added_code IS NOT NULL
  ", gsub("'", "''", author_name))
  
  added_codes <- tryCatch(DBI::dbGetQuery(conn, query)$added_code, error = function(e) character())
  if (length(added_codes) == 0) return(character())
  
  all_code <- paste(added_codes, collapse = "\n")
  detected <- character()
  for (tech_name in names(tech_dictionary)) {
    if (any(sapply(tech_dictionary[[tech_name]], function(pat) grepl(pat, all_code, ignore.case = TRUE)))) {
      detected <- c(detected, tech_name)
    }
  }
  unique(detected)
}

# -------------------- Основная функция технологического стека (исправленная) --------------------
get_tech_stack <- function(conn, author_name) {
  if (missing(conn) || is.null(conn)) return(git_error("invalid_argument", "conn не может быть NULL"))
  if (missing(author_name) || author_name == "") return(git_error("invalid_argument", "author_name обязателен"))
  
  # Источник 1: анализ путей и расширений файлов
  base_stack <- suppressWarnings(get_tech_stack_base(conn, author_name))
  if (is_git_error(base_stack)) base_stack <- character()
  
  # Источник 2: библиотеки, найденные в добавленном коде (импорты)
  lib_stack <- suppressWarnings(extract_libraries_from_code(conn, author_name))
  if (is_git_error(lib_stack)) lib_stack <- character()
  
  # Источник 3: зависимости из файлов, только если разработчик изменял эти файлы
  repos_with_deps <- suppressWarnings(get_repos_with_dependency_changes(conn, author_name))
  dep_stack <- character()
  for (rp in repos_with_deps) {
    if (dir.exists(rp)) {
      dep_stack <- c(dep_stack, read_dependency_files(rp))
    }
  }
  dep_stack <- unique(dep_stack)
  
  full_stack <- unique(c(base_stack, lib_stack, dep_stack))
  return(full_stack)
}

# -------------------- Остальные функции (без изменений) --------------------
get_commit_type_profile <- function(conn, author_name) {
  query <- sprintf("
    SELECT SUM(d.count_add + d.count_del) as commit_size
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    WHERE c.author_name = '%s'
    GROUP BY c.commit
  ", gsub("'", "''", author_name))
  sizes <- tryCatch(DBI::dbGetQuery(conn, query)$commit_size, error = function(e) numeric())
  if (length(sizes) == 0) return(list())
  list(
    tiny = sum(sizes < 10),
    small = sum(sizes >= 10 & sizes < 100),
    medium = sum(sizes >= 100 & sizes < 500),
    large = sum(sizes >= 500),
    avg_size = mean(sizes),
    median_size = median(sizes)
  )
}

get_user_repositories <- function(conn, author_name) {
  query <- sprintf("
    SELECT DISTINCT 
      rp.repo,
      rm.stars,
      rm.forks,
      rm.open_issues,
      rm.primary_language,
      rm.all_languages,
      rm.description,
      rm.license,
      rm.owner_login
    FROM git_commit_history ch
    JOIN repo_path rp ON ch.repo_id = rp.id
    LEFT JOIN repo_metadata rm ON rp.id = rm.repo_id
    WHERE ch.author_name = '%s'
    ORDER BY rp.repo
  ", gsub("'", "''", author_name))
  tryCatch(DBI::dbGetQuery(conn, query), error = function(e) data.frame())
}

get_developer_profile <- function(conn, author_name) {
  if (missing(conn) || is.null(conn)) return(list(error = git_error("invalid_argument", "conn не может быть NULL")))
  if (missing(author_name) || author_name == "") return(list(error = git_error("invalid_argument", "author_name обязателен")))
  
  stats <- tryCatch(get_developer_stats(conn, username = author_name), error = function(e) data.frame())
  if (is_git_error(stats) || nrow(stats) == 0) return(list(error = paste("Разработчик", author_name, "не найден")))
  
  tech_stack <- tryCatch(get_tech_stack(conn, author_name), error = function(e) character())
  commit_profile <- tryCatch(get_commit_type_profile(conn, author_name), error = function(e) list())
  
  season <- tryCatch(get_activity_seasonality(conn, author_name = author_name), error = function(e) NULL)
  work_style <- "unknown"
  if (!is.null(season) && !is_git_error(season) && nrow(season$peak_hours) > 0) {
    peak_hour <- season$peak_hours$hour[1]
    work_style <- ifelse(peak_hour < 8 | peak_hour > 22, "night_owl", "day_person")
  }
  
  all_stats <- tryCatch(get_developer_stats(conn), error = function(e) data.frame())
  team_avg <- if (nrow(all_stats) > 0) mean(all_stats$total_commits, na.rm = TRUE) else NA
  contribution <- if (!is.na(team_avg) && stats$total_commits[1] > team_avg * 1.2) "high"
  else if (!is.na(team_avg) && stats$total_commits[1] < team_avg * 0.8) "low"
  else "medium"
  
  anomalies_table_exists <- tryCatch(
    DBI::dbGetQuery(conn, "SELECT COUNT(*) FROM information_schema.tables WHERE table_name = 'anomalies'")$count[1] > 0,
    error = function(e) FALSE
  )
  anomaly_count <- 0
  anomaly_types <- character()
  if (anomalies_table_exists) {
    anom_df <- tryCatch(
      DBI::dbGetQuery(conn, sprintf("SELECT anomaly_type FROM anomalies WHERE author_name = '%s'", gsub("'", "''", author_name))),
      error = function(e) data.frame()
    )
    if (nrow(anom_df) > 0) {
      anomaly_count <- nrow(anom_df)
      anomaly_types <- unique(anom_df$anomaly_type)
    }
  }
  
  repos <- tryCatch(get_user_repositories(conn, author_name), error = function(e) data.frame())
  
  rule_anom <- tryCatch(get_all_anomalies(conn, username = author_name), error = function(e) data.frame())
  ml_anom <- tryCatch(get_ml_anomalies(conn, author_name = author_name), error = function(e) data.frame())
  freq_edits <- tryCatch(get_frequent_file_edits(conn, username = author_name), error = function(e) data.frame())
  hr_anomalies <- format_anomalies_for_hr(rule_anom, ml_anom, freq_edits)
  
  list(
    name = author_name,
    main_language = stats$primary_language[1] %||% "unknown",
    secondary_language = stats$secondary_language[1] %||% "нет",
    tech_stack = tech_stack,
    work_style = work_style,
    contribution = contribution,
    total_commits = stats$total_commits[1],
    active_days = stats$active_days[1],
    commit_type_profile = commit_profile,
    anomaly_count = anomaly_count,
    anomaly_types = anomaly_types,
    repositories = repos,
    hr_anomalies = hr_anomalies
  )
}
