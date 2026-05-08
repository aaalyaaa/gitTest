tech_dictionary <- list(
  # Python веб-фреймворки
  "django" = "django", "flask" = "flask", "fastapi" = "fastapi",
  "tornado" = "tornado", "starlette" = "starlette", "litestar" = "litestar", "pyramid" = "pyramid",
  # Python данные и ML
  "pandas" = "pandas", "numpy" = "numpy", "scipy" = "scipy",
  "scikit-learn" = "scikit-learn|sklearn", "pytorch" = "pytorch|torch",
  "tensorflow" = "tensorflow|tf", "keras" = "keras", "xgboost" = "xgboost",
  "lightgbm" = "lightgbm", "catboost" = "catboost",
  "transformers" = "transformers|huggingface", "opencv" = "opencv|cv2",
  "pillow" = "pillow|PIL", "matplotlib" = "matplotlib", "seaborn" = "seaborn",
  "plotly" = "plotly", "bokeh" = "bokeh", "statsmodels" = "statsmodels",
  "pyspark" = "pyspark", "dask" = "dask", "polars" = "polars", "ray" = "ray",
  "nltk" = "nltk", "spacy" = "spacy", "gensim" = "gensim",
  # Python базы данных и ORM
  "sqlalchemy" = "sqlalchemy", "alembic" = "alembic", "psycopg2" = "psycopg2",
  "pymongo" = "pymongo", "redis" = "redis", "tortoise-orm" = "tortoise",
  "pydantic" = "pydantic", "motor" = "motor", "elasticsearch-py" = "elasticsearch",
  # Python тестирование
  "pytest" = "pytest", "unittest" = "unittest", "hypothesis" = "hypothesis",
  "factory-boy" = "factory", "mock" = "mock", "tox" = "tox",
  # Python инфраструктура
  "celery" = "celery", "airflow" = "airflow", "prefect" = "prefect",
  "mlflow" = "mlflow", "uvicorn" = "uvicorn", "gunicorn" = "gunicorn",
  "httpx" = "httpx", "aiohttp" = "aiohttp", "requests" = "requests",
  # JS/TS фронтенд
  "react" = "react|React|ReactDOM", "angular" = "angular|@angular",
  "vue" = "vue|Vue", "svelte" = "svelte", "solid" = "solid-js|solid",
  "nextjs" = "next|nextjs", "nuxt" = "nuxt", "remix" = "remix",
  "astro" = "astro", "qwik" = "qwik", "gatsby" = "gatsby",
  # JS/TS бэкенд
  "nodejs" = "node|node.js", "express" = "express", "nestjs" = "@nestjs|nestjs",
  "fastify" = "fastify", "hono" = "hono", "koa" = "koa", "adonis" = "adonis",
  # JS/TS тестирование
  "jest" = "jest", "vitest" = "vitest", "cypress" = "cypress",
  "playwright" = "playwright", "mocha" = "mocha", "chai" = "chai",
  "testing-library" = "@testing-library",
  # JS/TS утилиты
  "prisma" = "prisma", "typeorm" = "typeorm", "sequelize" = "sequelize",
  "graphql" = "graphql", "apollo" = "apollo", "trpc" = "trpc|tRPC",
  "zod" = "zod", "webpack" = "webpack", "vite" = "vite", "eslint" = "eslint",
  "prettier" = "prettier", "turbopack" = "turbopack", "nx" = "nx",
  # Java/Kotlin
  "spring" = "spring|springframework", "spring-boot" = "springboot",
  "spring-security" = "springsecurity", "spring-cloud" = "springcloud",
  "hibernate" = "hibernate", "jpa" = "jpa", "maven" = "maven",
  "gradle" = "gradle", "junit" = "junit", "mockito" = "mockito",
  "quarkus" = "quarkus", "micronaut" = "micronaut", "ktor" = "ktor",
  "exposed" = "exposed", "kotlin-coroutines" = "coroutines",
  # Go
  "gin" = "gin", "echo" = "echo", "fiber" = "fiber", "chi" = "chi",
  "gorilla" = "gorilla", "gorm" = "gorm", "sqlx" = "sqlx",
  "testify" = "testify", "cobra" = "cobra", "viper" = "viper",
  "grpc-go" = "grpc", "fx" = "fx", "ent" = "ent",
  # Rust
  "tokio" = "tokio", "axum" = "axum", "actix-web" = "actix",
  "rocket" = "rocket", "serde" = "serde", "diesel" = "diesel",
  "sqlx" = "sqlx", "reqwest" = "reqwest", "clap" = "clap",
  "tonic" = "tonic", "bevy" = "bevy",
  # C# / .NET
  "aspnet" = "asp.net|aspnet", "entity-framework" = "entityframework",
  "blazor" = "blazor", "maui" = "maui", "signalr" = "signalr",
  "nunit" = "nunit", "xunit" = "xunit", "moq" = "moq",
  "automapper" = "automapper", "mediatr" = "mediatr",
  # Ruby
  "rails" = "rails", "sinatra" = "sinatra", "rspec" = "rspec",
  "sidekiq" = "sidekiq", "devise" = "devise", "capistrano" = "capistrano",
  "faraday" = "faraday",
  # PHP
  "laravel" = "laravel", "symfony" = "symfony", "wordpress" = "wordpress",
  "phpunit" = "phpunit", "composer" = "composer", "livewire" = "livewire",
  "filament" = "filament",
  # R
  "ggplot2" = "ggplot2", "dplyr" = "dplyr", "tidyverse" = "tidyverse",
  "tidyr" = "tidyr", "readr" = "readr", "purrr" = "purrr",
  "lubridate" = "lubridate", "shiny" = "shiny", "rmarkdown" = "rmarkdown",
  "caret" = "caret", "tidymodels" = "tidymodels", "mlr3" = "mlr3",
  "data.table" = "data.table", "DBI" = "DBI", "RSQLite" = "RSQLite",
  "httr" = "httr", "jsonlite" = "jsonlite", "testthat" = "testthat",
  "plumber" = "plumber", "targets" = "targets",
  # Swift/Objective-C
  "swiftui" = "swiftui", "uikit" = "uikit", "combine" = "combine",
  "xctest" = "xctest", "cocoapods" = "cocoapods", "spm" = "swiftpackage",
  "alamofire" = "alamofire", "realm" = "realm",
  # Dart/Flutter
  "flutter" = "flutter", "riverpod" = "riverpod", "bloc" = "bloc",
  "provider" = "provider", "dio" = "dio", "getx" = "getx",
  # Scala
  "akka" = "akka", "play" = "playframework", "cats" = "cats",
  "zio" = "zio", "spark" = "spark", "sbt" = "sbt"
)

tech_group_map <- list(
  cloud = c("aws", "azure", "gcp", "heroku", "vercel", "netlify", "cloudflare", "railway", "flyio", "render"),
  database = c("postgresql", "mysql", "sqlite", "mongodb", "redis", "mariadb", "elasticsearch", "oracle",
               "dynamodb", "firebase", "bigquery", "clickhouse", "cockroachdb", "cassandra", "neo4j", "influxdb"),
  frontend = c("react", "angular", "vue", "svelte", "solid", "nextjs", "nuxt", "remix", "astro", "gatsby",
               "react_native", "flutter", "expo", "jetpack-compose", "swiftui", "capacitor", "ionic"),
  backend = c("nodejs", "express", "nestjs", "fastify", "django", "flask", "fastapi", "tornado", "starlette",
              "spring", "spring-boot", "gin", "echo", "fiber", "rails", "laravel", "aspnet", "actix-web", "axum",
              "rocket", "tokio", "ktor"),
  devops = c("docker", "kubernetes", "terraform", "ansible", "helm", "github-actions", "gitlab-ci", "jenkins",
             "circleci", "argocd", "pulumi"),
  data_ml = c("pandas", "numpy", "scikit-learn", "pytorch", "tensorflow", "keras", "xgboost", "lightgbm", "catboost",
              "transformers", "spark", "dask", "polars", "ray", "nltk", "spacy", "gensim", "mlflow", "airflow"),
  testing = c("pytest", "unittest", "jest", "vitest", "cypress", "playwright", "junit", "mockito", "rspec",
              "testthat", "phpunit")
)

get_tech_stack <- function(conn, author_name, include_groups = TRUE) {
  if (missing(conn) || is.null(conn)) return(git_error("invalid_argument", "conn не может быть NULL"))
  if (missing(author_name) || author_name == "") return(git_error("invalid_argument", "author_name обязателен"))
  
  base_stack <- suppressWarnings(get_tech_stack_base(conn, author_name))
  if (is_git_error(base_stack)) base_stack <- character()
  lib_stack <- suppressWarnings(extract_libraries_from_code(conn, author_name))
  if (is_git_error(lib_stack)) lib_stack <- character()
  
  all_techs <- unique(c(base_stack, lib_stack))
  if (length(all_techs) == 0) {
    if (include_groups) return(data.frame(technology = character(), group = character()))
    else return(character())
  }
  
  if (include_groups) {
    groups <- sapply(all_techs, get_tech_group)
    return(data.frame(technology = all_techs, group = groups, stringsAsFactors = FALSE))
  } else {
    return(all_techs)
  }
}
get_tech_group <- function(tech) {
  for (grp in names(tech_group_map)) {
    if (tech %in% tech_group_map[[grp]]) return(grp)
  }
  return("other")
}

get_tech_stack_base <- function(conn, author_name) {
  if (missing(conn) || is.null(conn)) return(git_error("invalid_argument", "conn не может быть NULL"))
  if (missing(author_name) || author_name == "") return(git_error("invalid_argument", "author_name обязателен"))
  
  query <- "
    SELECT DISTINCT COALESCE(d.src_file, d.dst_file) as file_path
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    WHERE c.author_name = ?
  "
  files <- tryCatch(DBI::dbGetQuery(conn, query, params = list(author_name)), error = function(e) data.frame())
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

extract_libraries_from_code <- function(conn, author_name) {
  query <- "
    SELECT d.src_file, d.dst_file, d.added_code, d.file_extension
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    WHERE c.author_name = ? AND d.added_code IS NOT NULL
  "
  data <- tryCatch(DBI::dbGetQuery(conn, query, params = list(author_name)), error = function(e) data.frame())
  if (nrow(data) == 0) return(character())
  
  detected <- character()
  
  import_patterns <- list(
    python = "^(import|from)\\s+([a-zA-Z0-9_]+)",
    javascript = "(require\\(['\"]([^'\"]+)['\"]|from\\s+['\"]([^'\"]+)['\"]|import\\s+['\"]([^'\"]+)['\"])",
    typescript = "(require\\(['\"]([^'\"]+)['\"]|from\\s+['\"]([^'\"]+)['\"]|import\\s+['\"]([^'\"]+)['\"])",
    go = "\\\"([a-zA-Z0-9_/]+)\\\"",
    rust = "^use\\s+([a-zA-Z0-9_:]+)",
    java = "^import\\s+([a-zA-Z0-9_.]+)",
    csharp = "^using\\s+([a-zA-Z0-9_.]+)",
    php = "^(use|require|include)\\s+([a-zA-Z0-9_\\\\]+)",
    ruby = "^(require|include)\\s+['\"]([^'\"]+)['\"]"
  )
  
  for (i in 1:nrow(data)) {
    ext <- tolower(data$file_extension[i])
    code <- data$added_code[i]
    if (is.na(code) || nchar(trimws(code)) == 0) next
    
    lang <- switch(ext,
                   py = "python",
                   r = "r", R = "r", Rmd = "r",
                   js = "javascript", ts = "typescript", jsx = "javascript", tsx = "typescript",
                   go = "go",
                   rs = "rust",
                   java = "java",
                   cs = "csharp",
                   php = "php",
                   rb = "ruby",
                   "unknown"
    )
    if (lang == "unknown") next
    
    pattern <- import_patterns[[lang]]
    if (is.null(pattern)) next
    
    matches <- regmatches(code, gregexpr(pattern, code, perl = TRUE))[[1]]
    if (length(matches) == 0) next
    
    libs <- c()
    for (m in matches) {
      if (lang == "python") {
        lib <- sub("^(import|from)\\s+", "", m)
        lib <- strsplit(lib, "\\s+")[[1]][1]
        libs <- c(libs, lib)
      } else if (lang %in% c("javascript", "typescript")) {
        lib <- sub(".*['\"]([^'\"]+)['\"].*", "\\1", m)
        libs <- c(libs, lib)
      } else if (lang == "go") {
        lib <- sub('^"', '', m)
        lib <- sub('"$', '', lib)
        lib <- basename(lib)
        libs <- c(libs, lib)
      } else if (lang == "rust") {
        lib <- sub("^use\\s+", "", m)
        lib <- strsplit(lib, "::")[[1]][1]
        libs <- c(libs, lib)
      } else {
        lib <- sub("^[a-zA-Z]+\\s+", "", m)
        lib <- strsplit(lib, "[[:space:];:]+")[[1]][1]
        libs <- c(libs, lib)
      }
    }
    
    for (lib in unique(libs)) {
      for (tech in names(tech_dictionary)) {
        patterns <- tech_dictionary[[tech]]
        tokens <- unlist(strsplit(patterns, "\\|"))
        if (any(grepl(paste0("^", lib, "$"), tokens, ignore.case = TRUE)) ||
            any(grepl(paste0("^", lib), tokens, ignore.case = TRUE))) {
          detected <- c(detected, tech)
          break
        }
      }
    }
  }
  unique(detected)
}

get_tech_stack_with_groups <- function(conn, author_name) {
  if (missing(conn) || is.null(conn)) return(git_error("invalid_argument", "conn не может быть NULL"))
  if (missing(author_name) || author_name == "") return(git_error("invalid_argument", "author_name обязателен"))
  
  base_stack <- suppressWarnings(get_tech_stack_base(conn, author_name))
  if (is_git_error(base_stack)) base_stack <- character()
  lib_stack <- suppressWarnings(extract_libraries_from_code(conn, author_name))
  if (is_git_error(lib_stack)) lib_stack <- character()
  
  all_techs <- unique(c(base_stack, lib_stack))
  if (length(all_techs) == 0) return(data.frame(technology = character(), group = character()))
  groups <- sapply(all_techs, get_tech_group)
  data.frame(technology = all_techs, group = groups, stringsAsFactors = FALSE)
}

get_commit_size_profile <- function(conn, author_name, since = NULL, until = NULL) {
  if (missing(conn) || is.null(conn)) return(git_error("invalid_argument", "conn не может быть NULL"))
  if (missing(author_name) || author_name == "") return(git_error("invalid_argument", "author_name обязателен"))
  
  where <- "c.author_name = ?"
  params <- list(author_name)
  if (!is.null(since)) {
    where <- paste(where, "AND c.date >= ?")
    params <- c(params, since)
  }
  if (!is.null(until)) {
    where <- paste(where, "AND c.date <= ?")
    params <- c(params, until)
  }
  
  query <- sprintf("
    SELECT SUM(d.count_add + d.count_del) as commit_size
    FROM git_commit_history c
    JOIN git_file_changes d ON c.commit = d.commit
    WHERE %s
    GROUP BY c.commit
  ", where)
  
  sizes <- tryCatch(DBI::dbGetQuery(conn, query, params = params)$commit_size, error = function(e) numeric())
  if (length(sizes) == 0) {
    return(list(tiny=0, small=0, medium=0, large=0, avg_size=0, median_size=0))
  }
  
  list(
    tiny = sum(sizes < 10),
    small = sum(sizes >= 10 & sizes < 100),
    medium = sum(sizes >= 100 & sizes < 1000),
    large = sum(sizes >= 1000),
    avg_size = mean(sizes),
    median_size = median(sizes)
  )
}

get_user_repositories <- function(conn, author_name, since = NULL, until = NULL) {
  if (missing(conn) || is.null(conn)) return(git_error("invalid_argument", "conn не может быть NULL"))
  if (missing(author_name) || author_name == "") return(git_error("invalid_argument", "author_name обязателен"))
  
  where <- "ch.author_name = ?"
  params <- list(author_name)
  if (!is.null(since)) {
    where <- paste(where, "AND ch.date >= ?")
    params <- c(params, since)
  }
  if (!is.null(until)) {
    where <- paste(where, "AND ch.date <= ?")
    params <- c(params, until)
  }
  
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
    WHERE %s
    ORDER BY rp.repo
  ", where)
  
  tryCatch(DBI::dbGetQuery(conn, query, params = params), error = function(e) data.frame())
}

get_developer_profile <- function(conn, author_name, since = NULL, until = NULL) {
  if (missing(conn) || is.null(conn)) return(list(error = git_error("invalid_argument", "conn не может быть NULL")))
  if (missing(author_name) || author_name == "") return(list(error = git_error("invalid_argument", "author_name обязателен")))
  
  stats <- tryCatch(get_developer_stats(conn, username = author_name), error = function(e) data.frame())
  if (is_git_error(stats) || nrow(stats) == 0) {
    return(list(error = paste("Разработчик", author_name, "не найден или ошибка метрик")))
  }
  
  tech_df <- tryCatch(get_tech_stack_with_groups(conn, author_name), error = function(e) data.frame())
  if (is_git_error(tech_df)) tech_df <- data.frame()
  tech_stack <- if (nrow(tech_df) > 0) tech_df$technology else character()
  tech_groups <- if (nrow(tech_df) > 0) tech_df$group else character()
  
  size_profile <- tryCatch(get_commit_size_profile(conn, author_name, since, until), error = function(e) list(tiny=0, small=0, medium=0, large=0, avg_size=0, median_size=0))
  total_in_period <- size_profile$tiny + size_profile$small + size_profile$medium + size_profile$large
  active_days_in_period <- NA
  
  season <- tryCatch(get_activity_seasonality(conn, author_name = author_name, since = since, until = until), error = function(e) NULL)
  work_style <- "unknown"
  if (!is.null(season) && !is_git_error(season) && !is.null(season$by_hour) && nrow(season$by_hour) > 0) {
    night_hours <- c(22,23,0,1,2,3,4,5)
    total_commits <- sum(season$by_hour$commits)
    night_commits <- sum(season$by_hour$commits[season$by_hour$hour %in% night_hours])
    night_ratio <- if (total_commits > 0) night_commits / total_commits else 0
    work_style <- ifelse(night_ratio > 0.4, "night_owl", "day_person")
  }
  
  all_stats <- tryCatch(get_developer_stats(conn), error = function(e) data.frame())
  team_avg <- if (nrow(all_stats) > 0) mean(all_stats$total_commits, na.rm = TRUE) else NA
  contribution <- if (!is.na(team_avg) && stats$total_commits[1] > team_avg * 1.2) "high"
  else if (!is.na(team_avg) && stats$total_commits[1] < team_avg * 0.8) "low"
  else "medium"
  
  anomalies_table_exists <- tryCatch(
    DBI::dbGetQuery(conn, "SELECT COUNT(*) FROM sqlite_master WHERE type='table' AND name='anomalies'")$count[1] > 0,
    error = function(e) FALSE
  )
  anomaly_count <- 0
  anomaly_types <- character()
  if (anomalies_table_exists) {
    where_date <- ""
    if (!is.null(since)) where_date <- paste0(where_date, " AND date >= '", since, "'")
    if (!is.null(until)) where_date <- paste0(where_date, " AND date <= '", until, "'")
    anom_df <- tryCatch(
      DBI::dbGetQuery(conn, sprintf("SELECT anomaly_type FROM anomalies WHERE author_name = '%s' %s", gsub("'", "''", author_name), where_date)),
      error = function(e) data.frame()
    )
    if (nrow(anom_df) > 0) {
      anomaly_count <- nrow(anom_df)
      anomaly_types <- unique(anom_df$anomaly_type)
    }
  }
  
  repos <- tryCatch(get_user_repositories(conn, author_name, since, until), error = function(e) data.frame())
  
  rule_anom <- tryCatch(get_all_anomalies(conn, username = author_name, since = since, until = until), error = function(e) data.frame())
  ml_anom <- tryCatch(get_ml_anomalies(conn, author_name = author_name, since = since, until = until), error = function(e) data.frame())
  freq_edits <- tryCatch(get_frequent_file_edits(conn, username = author_name, since = since, until = until), error = function(e) data.frame())
  hr_anomalies <- format_anomalies_for_hr(rule_anom, ml_anom, freq_edits)
  
  list(
    name = author_name,
    main_language = stats$primary_language[1] %||% "unknown",
    secondary_language = stats$secondary_language[1] %||% "нет",
    tech_stack = tech_stack,
    tech_groups = tech_groups,
    work_style = work_style,
    contribution = contribution,
    total_commits = stats$total_commits[1],
    active_days = stats$active_days[1],
    commits_in_period = total_in_period,
    active_days_in_period = active_days_in_period,
    commit_size_profile = size_profile,
    anomaly_count = anomaly_count,
    anomaly_types = anomaly_types,
    repositories = repos,
    hr_anomalies = hr_anomalies
  )
}