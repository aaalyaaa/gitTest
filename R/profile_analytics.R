tech_dictionary <- list(
  "django" = "django", "flask" = "flask", "fastapi" = "fastapi",
  "tornado" = "tornado", "starlette" = "starlette", "litestar" = "litestar", "pyramid" = "pyramid",
  "pandas" = "pandas", "numpy" = "numpy", "scipy" = "scipy",
  "scikit-learn" = "scikit-learn|sklearn", "pytorch" = "pytorch|torch",
  "tensorflow" = "tensorflow|tf", "keras" = "keras", "xgboost" = "xgboost",
  "lightgbm" = "lightgbm", "catboost" = "catboost",
  "transformers" = "transformers|huggingface", "opencv" = "opencv|cv2",
  "pillow" = "pillow|PIL", "matplotlib" = "matplotlib", "seaborn" = "seaborn",
  "plotly" = "plotly", "bokeh" = "bokeh", "statsmodels" = "statsmodels",
  "pyspark" = "pyspark", "dask" = "dask", "polars" = "polars", "ray" = "ray",
  "nltk" = "nltk", "spacy" = "spacy", "gensim" = "gensim",
  "sqlalchemy" = "sqlalchemy", "alembic" = "alembic", "psycopg2" = "psycopg2",
  "pymongo" = "pymongo", "redis" = "redis", "tortoise-orm" = "tortoise",
  "pydantic" = "pydantic", "motor" = "motor", "elasticsearch-py" = "elasticsearch",
  "pytest" = "pytest", "unittest" = "unittest", "hypothesis" = "hypothesis",
  "factory-boy" = "factory", "mock" = "mock", "tox" = "tox",
  "celery" = "celery", "airflow" = "airflow", "prefect" = "prefect",
  "mlflow" = "mlflow", "uvicorn" = "uvicorn", "gunicorn" = "gunicorn",
  "httpx" = "httpx", "aiohttp" = "aiohttp", "requests" = "requests",
  "react" = "react|React|ReactDOM", "angular" = "angular|@angular",
  "vue" = "vue|Vue", "svelte" = "svelte", "solid" = "solid-js|solid",
  "nextjs" = "next|nextjs", "nuxt" = "nuxt", "remix" = "remix",
  "astro" = "astro", "qwik" = "qwik", "gatsby" = "gatsby",
  "nodejs" = "node|node.js", "express" = "express", "nestjs" = "@nestjs|nestjs",
  "fastify" = "fastify", "hono" = "hono", "koa" = "koa", "adonis" = "adonis",
  "jest" = "jest", "vitest" = "vitest", "cypress" = "cypress",
  "playwright" = "playwright", "mocha" = "mocha", "chai" = "chai",
  "testing-library" = "@testing-library",
  "prisma" = "prisma", "typeorm" = "typeorm", "sequelize" = "sequelize",
  "graphql" = "graphql", "apollo" = "apollo", "trpc" = "trpc|tRPC",
  "zod" = "zod", "webpack" = "webpack", "vite" = "vite", "eslint" = "eslint",
  "prettier" = "prettier", "turbopack" = "turbopack", "nx" = "nx",
  "spring" = "spring|springframework", "spring-boot" = "springboot",
  "spring-security" = "springsecurity", "spring-cloud" = "springcloud",
  "hibernate" = "hibernate", "jpa" = "jpa", "maven" = "maven",
  "gradle" = "gradle", "junit" = "junit", "mockito" = "mockito",
  "quarkus" = "quarkus", "micronaut" = "micronaut", "ktor" = "ktor",
  "exposed" = "exposed", "kotlin-coroutines" = "coroutines",
  "gin" = "gin", "echo" = "echo", "fiber" = "fiber", "chi" = "chi",
  "gorilla" = "gorilla", "gorm" = "gorm", "sqlx" = "sqlx",
  "testify" = "testify", "cobra" = "cobra", "viper" = "viper",
  "grpc-go" = "grpc", "fx" = "fx", "ent" = "ent",
  "tokio" = "tokio", "axum" = "axum", "actix-web" = "actix",
  "rocket" = "rocket", "serde" = "serde", "diesel" = "diesel",
  "sqlx-rust" = "sqlx", "reqwest" = "reqwest", "clap" = "clap",
  "tonic" = "tonic", "bevy" = "bevy",
  "aspnet" = "asp.net|aspnet", "entity-framework" = "entityframework",
  "blazor" = "blazor", "maui" = "maui", "signalr" = "signalr",
  "nunit" = "nunit", "xunit" = "xunit", "moq" = "moq",
  "automapper" = "automapper", "mediatr" = "mediatr",
  "rails" = "rails", "sinatra" = "sinatra", "rspec" = "rspec",
  "sidekiq" = "sidekiq", "devise" = "devise", "capistrano" = "capistrano",
  "faraday" = "faraday",
  "laravel" = "laravel", "symfony" = "symfony", "wordpress" = "wordpress",
  "phpunit" = "phpunit", "composer" = "composer", "livewire" = "livewire",
  "filament" = "filament",
  "ggplot2" = "ggplot2", "dplyr" = "dplyr", "tidyverse" = "tidyverse",
  "tidyr" = "tidyr", "readr" = "readr", "purrr" = "purrr",
  "lubridate" = "lubridate", "shiny" = "shiny", "rmarkdown" = "rmarkdown",
  "caret" = "caret", "tidymodels" = "tidymodels", "mlr3" = "mlr3",
  "data.table" = "data.table", "DBI" = "DBI", "RSQLite" = "RSQLite",
  "httr" = "httr", "jsonlite" = "jsonlite", "testthat" = "testthat",
  "plumber" = "plumber", "targets" = "targets",
  "swiftui" = "swiftui", "uikit" = "uikit", "combine" = "combine",
  "xctest" = "xctest", "cocoapods" = "cocoapods", "spm" = "swiftpackage",
  "alamofire" = "alamofire", "realm" = "realm",
  "flutter" = "flutter", "riverpod" = "riverpod", "bloc" = "bloc",
  "provider" = "provider", "dio" = "dio", "getx" = "getx",
  "akka" = "akka", "play" = "playframework", "cats" = "cats",
  "zio" = "zio", "spark" = "spark", "sbt" = "sbt"
)

tech_group_map <- list(
  cloud    = c("aws", "azure", "gcp", "heroku", "vercel", "netlify", "cloudflare", "railway", "flyio", "render"),
  database = c("postgresql", "mysql", "sqlite", "mongodb", "redis", "mariadb", "elasticsearch", "oracle",
               "dynamodb", "firebase", "bigquery", "clickhouse", "cockroachdb", "cassandra", "neo4j", "influxdb"),
  frontend = c("react", "angular", "vue", "svelte", "solid", "nextjs", "nuxt", "remix", "astro", "gatsby",
               "html_css"),
  backend  = c("nodejs", "express", "nestjs", "fastify", "django", "flask", "fastapi", "tornado", "starlette",
               "spring", "spring-boot", "gin", "echo", "fiber", "rails", "laravel", "aspnet", "actix-web",
               "axum", "rocket", "tokio", "ktor", "go", "php", "ruby", "scala", "clojure", "elixir"),
  devops   = c("docker", "kubernetes", "terraform", "ansible", "helm", "github-actions", "gitlab-ci", "jenkins",
               "circleci", "argocd", "pulumi"),
  data_ml  = c("pandas", "numpy", "scikit-learn", "pytorch", "tensorflow", "keras", "xgboost", "lightgbm",
               "catboost", "transformers", "spark", "dask", "polars", "ray", "nltk", "spacy", "gensim",
               "mlflow", "airflow"),
  testing  = c("pytest", "unittest", "jest", "vitest", "cypress", "playwright", "junit", "mockito", "rspec",
               "testthat", "phpunit"),
  mobile   = c("flutter", "react_native", "expo", "jetpack-compose", "swiftui", "capacitor", "ionic", "kotlin", "swift", "dart"),
  embedded = c("cpp", "rust", "c", "zig", "nim", "v", "odin", "arduino", "freertos")
)

import_patterns <- list(
  py   = "^\\s*(import|from)\\s+([a-zA-Z0-9_\\-\\.]+)",
  r    = "^\\s*(library|require)\\s*\\(\\s*[\"']?([a-zA-Z0-9\\.]+)",
  R    = "^\\s*(library|require)\\s*\\(\\s*[\"']?([a-zA-Z0-9\\.]+)",
  js   = "(require\\s*\\([\"']([a-zA-Z0-9@/\\-\\.]+)[\"']\\)|from\\s+[\"']([a-zA-Z0-9@/\\-\\.]+)[\"'])",
  ts   = "(require\\s*\\([\"']([a-zA-Z0-9@/\\-\\.]+)[\"']\\)|from\\s+[\"']([a-zA-Z0-9@/\\-\\.]+)[\"'])",
  jsx  = "(require\\s*\\([\"']([a-zA-Z0-9@/\\-\\.]+)[\"']\\)|from\\s+[\"']([a-zA-Z0-9@/\\-\\.]+)[\"'])",
  tsx  = "(require\\s*\\([\"']([a-zA-Z0-9@/\\-\\.]+)[\"']\\)|from\\s+[\"']([a-zA-Z0-9@/\\-\\.]+)[\"'])",
  go   = "\"([a-zA-Z0-9\\.\\-/]+)\"",
  rs   = "^\\s*use\\s+([a-zA-Z0-9_:]+)",
  java = "^\\s*import\\s+([a-zA-Z0-9\\.\\*]+)",
  kt   = "^\\s*import\\s+([a-zA-Z0-9\\.\\*]+)",
  rb   = "^\\s*require\\s+[\"']([a-zA-Z0-9_\\-/]+)[\"']",
  php  = "^\\s*(use|require|include)\\s+[\"']?([a-zA-Z0-9_\\\\]+)"
)

get_tech_group <- function(tech) {
  for (grp in names(tech_group_map)) {
    if (tech %in% tech_group_map[[grp]]) return(grp)
  }
  return("other")
}
#' @export
get_tech_stack <- function(conn, author_name, repo_id = NULL) {
  if (missing(conn) || is.null(conn)) return(git_error("invalid_argument", "conn не может быть NULL"))
  if (missing(author_name) || author_name == "") return(git_error("invalid_argument", "author_name обязателен"))
  
  repo_filter <- if (!is.null(repo_id)) sprintf("AND c.repo_id = %d", repo_id) else ""
  
  files_df <- tryCatch(
    DBI::dbGetQuery(conn,
                    sprintf("
        SELECT DISTINCT COALESCE(d.src_file, d.dst_file) AS file_path
        FROM git_commit_history c
        JOIN git_file_changes d ON c.commit = d.commit
        WHERE c.author_name = ? %s
      ", repo_filter),
                    params = list(author_name)
    ),
    error = function(e) data.frame()
  )
  file_paths <- if (nrow(files_df) > 0) files_df$file_path else character()
  detected <- character()
  
  special_files <- list(
    "python"     = c("requirements.txt", "setup.py", "pyproject.toml", "Pipfile"),
    "r"          = c("DESCRIPTION", "NAMESPACE", ".Rproj"),
    "javascript" = c("package.json", "yarn.lock"),
    "typescript" = c("tsconfig.json"),
    "java"       = c("pom.xml", "build.gradle"),
    "go"         = c("go.mod"),
    "rust"       = c("Cargo.toml"),
    "csharp"     = c(".csproj", ".sln"),
    "php"        = c("composer.json"),
    "ruby"       = c("Gemfile"),
    "docker"     = c("Dockerfile", "docker-compose.yml", "docker-compose.yaml"),
    "terraform"  = c(".tf"),
    "kotlin"     = c("build.gradle.kts"),
    "swift"      = c("Package.swift", ".xcodeproj"),
    "dart"       = c("pubspec.yaml")
  )
  for (tech in names(special_files)) {
    if (any(sapply(special_files[[tech]], function(p) any(grepl(p, file_paths, ignore.case = TRUE))))) {
      detected <- c(detected, tech)
    }
  }
  
  ext_map <- list(
    "python"     = "\\.py$",
    "r"          = "\\.[Rr]$|\\.Rmd$",
    "javascript" = "\\.jsx?$",
    "typescript" = "\\.tsx?$",
    "java"       = "\\.java$",
    "go"         = "\\.go$",
    "rust"       = "\\.rs$",
    "csharp"     = "\\.cs$",
    "cpp"        = "\\.(cpp|hpp|cc|cxx|c|h)$",
    "php"        = "\\.php$",
    "ruby"       = "\\.rb$",
    "swift"      = "\\.swift$",
    "scala"      = "\\.scala$",
    "dart"       = "\\.dart$",
    "lua"        = "\\.lua$",
    "sql"        = "\\.sql$",
    "html_css"   = "\\.(html|css|scss|sass)$",
    "shell"      = "\\.(sh|bash|zsh)$",
    "kotlin"     = "\\.kt$",
    "vue"        = "\\.vue$",
    "svelte"     = "\\.svelte$"
  )
  for (tech in names(ext_map)) {
    if (tech %in% detected) next
    if (length(file_paths) > 0 && any(grepl(ext_map[[tech]], file_paths, ignore.case = TRUE))) {
      detected <- c(detected, tech)
    }
  }
  
  code_df <- tryCatch(
    DBI::dbGetQuery(conn,
                    sprintf("
        SELECT d.added_code, d.file_extension
        FROM git_commit_history c
        JOIN git_file_changes d ON c.commit = d.commit
        WHERE c.author_name = ? %s
          AND d.file_extension IN ('py','r','R','js','ts','jsx','tsx','go','rs','java','kt','rb','php')
          AND d.added_code IS NOT NULL
          AND d.added_code != ''
      ", repo_filter),
                    params = list(author_name)
    ),
    error = function(e) data.frame()
  )
  
  if (nrow(code_df) > 0) {
    code_by_ext <- split(code_df$added_code, tolower(code_df$file_extension))
    for (ext in names(code_by_ext)) {
      pattern <- import_patterns[[ext]]
      if (is.null(pattern)) next
      all_lines <- unlist(strsplit(paste(code_by_ext[[ext]], collapse = "\n"), "\n"))
      matches   <- regmatches(all_lines, regexpr(pattern, all_lines, perl = TRUE))
      if (length(matches) == 0) next
      imported <- tolower(matches)
      imported <- gsub("^(import|from|require|use|library|include)\\s*\\(?[\"'\\s]*", "", imported)
      imported <- gsub("[\"')\\s;]+$", "", imported)
      imported <- gsub("^([a-z0-9_@-]+).*", "\\1", imported)
      imported <- unique(imported[nchar(imported) >= 2])
      for (tech in names(tech_dictionary)) {
        tech_variants <- unlist(strsplit(tech_dictionary[[tech]], "\\|"))
        tech_variants <- tolower(gsub("[^a-z0-9_\\-]", "", tech_variants))
        tech_variants <- tech_variants[nchar(tech_variants) >= 2]
        if (any(tech_variants %in% imported)) {
          detected <- c(detected, tech)
        }
      }
    }
  }
  
  all_techs <- unique(detected)
  if (length(all_techs) == 0) {
    return(data.frame(technology = character(), group = character(), stringsAsFactors = FALSE))
  }
  data.frame(
    technology = all_techs,
    group      = sapply(all_techs, get_tech_group),
    stringsAsFactors = FALSE,
    row.names  = NULL
  )
}
#' @export
get_tech_list <- function(conn, author_name, repo_id = NULL) {
  df <- get_tech_stack(conn, author_name, repo_id)
  if (is_git_error(df) || nrow(df) == 0) {
    return(character())
  }
  df$technology
}
#' @export
get_commit_size_profile <- function(conn, author_name, since = NULL, until = NULL, repo_id = NULL) {
  if (missing(conn) || is.null(conn)) return(git_error("invalid_argument", "conn не может быть NULL"))
  if (missing(author_name) || author_name == "") return(git_error("invalid_argument", "author_name обязателен"))
  
  where_extra <- ""
  if (!is.null(since)) where_extra <- paste0(where_extra, " AND c.date >= '", since, "'")
  if (!is.null(until)) where_extra <- paste0(where_extra, " AND c.date <= '", until, "'")
  if (!is.null(repo_id)) where_extra <- paste0(where_extra, " AND c.repo_id = ", repo_id)
  
  sizes <- tryCatch(
    DBI::dbGetQuery(conn,
                    sprintf(
                      "SELECT SUM(d.count_add + d.count_del) AS commit_size
         FROM git_commit_history c
         JOIN git_file_changes d ON c.commit = d.commit
         WHERE c.author_name = ?%s
         GROUP BY c.commit",
                      where_extra
                    ),
                    params = list(author_name)
    )$commit_size,
    error = function(e) numeric()
  )
  if (length(sizes) == 0) {
    return(list(tiny = 0, small = 0, medium = 0, large = 0, avg_size = 0, median_size = 0))
  }
  list(
    tiny        = sum(sizes < 10),
    small       = sum(sizes >= 10  & sizes < 100),
    medium      = sum(sizes >= 100 & sizes < 500),
    large       = sum(sizes >= 500),
    avg_size    = round(mean(sizes),   1),
    median_size = round(median(sizes), 1)
  )
}
#' @export
get_user_repositories <- function(conn, author_name, since = NULL, until = NULL, repo_id = NULL) {
  if (missing(conn) || is.null(conn)) return(git_error("invalid_argument", "conn не может быть NULL"))
  if (missing(author_name) || author_name == "") return(git_error("invalid_argument", "author_name обязателен"))
  
  where_extra <- ""
  if (!is.null(since)) where_extra <- paste0(where_extra, " AND ch.date >= '", since, "'")
  if (!is.null(until)) where_extra <- paste0(where_extra, " AND ch.date <= '", until, "'")
  if (!is.null(repo_id)) where_extra <- paste0(where_extra, " AND ch.repo_id = ", repo_id)
  
  tryCatch(
    DBI::dbGetQuery(conn,
                    sprintf(
                      "SELECT DISTINCT
           rp.repo,
           rm.stars, rm.forks, rm.open_issues,
           rm.primary_language, rm.all_languages,
           rm.description, rm.license, rm.owner_login
         FROM git_commit_history ch
         JOIN repo_path rp ON ch.repo_id = rp.id
         LEFT JOIN repo_metadata rm ON rp.id = rm.repo_id
         WHERE ch.author_name = ?%s
         ORDER BY rp.repo",
                      where_extra
                    ),
                    params = list(author_name)
    ),
    error = function(e) data.frame()
  )
}
#' @export
get_developer_role <- function(conn, author_name, repo_id = NULL) {
  tech_df <- get_tech_stack(conn, author_name, repo_id = repo_id)
  if (is_git_error(tech_df) || nrow(tech_df) == 0) return("No technology detected")
  groups <- tech_df$group
  if (length(groups) == 0) return("No technology detected")
  
  freq <- table(groups)
  primary_group <- names(freq)[which.max(freq)]
  
  role_map <- c(
    "backend"  = "Backend Developer",
    "frontend" = "Frontend Developer",
    "devops"   = "DevOps Engineer",
    "data_ml"  = "Data/ML Engineer",
    "database" = "Database Engineer",
    "cloud"    = "Cloud Engineer",
    "testing"  = "QA Engineer",
    "mobile"   = "Mobile Developer",
    "embedded" = "Embedded/Systems Developer",
    "other"    = "Generalist"
  )
  role <- role_map[primary_group]
  if (is.na(role)) role <- "Generalist"
  
  if (all(c("frontend", "backend") %in% names(freq)) && length(freq) >= 2) {
    if (abs(freq["frontend"] - freq["backend"]) <= max(freq) / 2) {
      role <- "Full‑stack Developer"
    }
  }
  return(role)
}
