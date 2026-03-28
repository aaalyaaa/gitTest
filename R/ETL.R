#' Prepare a Git repository for analysis
#'
#' @param repo_url URL of the repository (or NULL for local)
#' @param repo_name Name of the repository
#' @param clone_dir Directory where to clone
#' @param local_path Path to local repository (if mode = 0)
#' @param mode 0 = local, 1 = remote
#' @return Path to the repository
#' @export
prepare_repo <- function(mode, repo_url = NULL, repo_name = NULL,
                         clone_dir = NULL, local_path = NULL) {

  if (mode == 0) {
    if (!dir.exists(local_path)) {
      stop("Local repository path does not exist: ", local_path)
    }
    cmd <- sprintf('git -C "%s" pull', local_path)
    system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    return(local_path)

  } else if (mode == 1) {
    if (is.null(clone_dir)) {
      clone_dir <- tempdir()
    }

    repo_path <- file.path(clone_dir, repo_name)

    if (dir.exists(repo_path)) {
      cmd <- sprintf('git -C "%s" pull', repo_path)
      system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    } else {
      cmd <- sprintf('git clone "%s" "%s"', repo_url, repo_path)
      system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    }

    return(repo_path)
  } else {
    stop("Invalid mode. Use 0 for local, 1 for remote")
  }
}


#' Get commit history from a Git repository
#'
#' @param repo_path Path to local Git repository
#' @param repo_id Numeric ID of the repository
#' @param repo_name Name of the repository
#' @param since Optional: only commits after this commit hash
#' @return Data frame with commit history (includes author email, committer info, branches)
#' @export
get_commit_history <- function(repo_path, repo_id, repo_name, since = NULL) {

  # %H - полный хэш коммита
  # %P - хэш родительского коммита
  # %an - имя автора
  # %ae - email автора
  # %cn - имя коммитера
  # %ce - email коммитера
  # %ai - дата автора (ISO формат)
  # %s - сообщение коммита (первая строка)
  # %d - ссылки на ветки и теги
  format_string <- "%H\t%P\t%an\t%ae\t%cn\t%ce\t%ai\t%s\t%d"

  if (is.null(since)) {
    cmd <- sprintf(
      'git -C "%s" log --format="%s" --all',
      repo_path, format_string
    )
  } else {
    cmd <- sprintf(
      'git -C "%s" log %s..HEAD --format="%s"',
      repo_path, since, format_string
    )
  }

  output <- system(cmd, intern = TRUE)

  if (length(output) == 0) {
    return(data.frame())
  }

  commits <- data.frame(lines = output, stringsAsFactors = FALSE) %>%
    tidyr::separate(
      col = lines,
      into = c("commit", "parent_commit", "author_name", "author_email",
               "committer_name", "committer_email", "date", "message", "branches"),
      sep = "\t",
      fill = "right"
    ) %>%
    dplyr::mutate(
      branches = stringr::str_replace_all(branches, "[()]", ""),
      branches = dplyr::na_if(branches, ""),

      repo_id = repo_id,
      repo = repo_name
    )

  return(commits)
}

#' Parse git diff output into structured data frame
#'
#' @param diff_lines Character vector of git diff output
#' @param repo_id Numeric ID of the repository
#' @param repo_name Name of the repository
#' @return Data frame with parsed diff information including file types and change statistics
#' @export
parse_git_diff <- function(diff_lines, repo_id, repo_name) {

  if (length(diff_lines) == 0 || all(diff_lines == "")) {
    return(data.frame())
  }

  df <- data.frame(lines = diff_lines, stringsAsFactors = FALSE)

  df <- df %>%
    dplyr::filter(
      !grepl('^Author', lines),
      !grepl('^Date', lines),
      !grepl('^diff', lines),
      !grepl('^index', lines),
      !grepl('^deleted', lines),
      !grepl('^new', lines),
      !grepl('^Merge', lines),
      lines != ""
    )

  df <- df %>%
    dplyr::mutate(
      is_commit_start = grepl("^commit ", lines),
      commit_id = cumsum(is_commit_start)
    ) %>%
    dplyr::mutate(
      commit = ifelse(is_commit_start,
                      stringr::str_replace(lines, "^commit ", ""),
                      NA_character_)
    )

  df <- df %>%
    dplyr::mutate(
      src_file = stringr::str_sub(
        stringr::str_extract(lines, "^--- .*"),
        4
      ),
      dst_file = stringr::str_sub(
        stringr::str_extract(lines, "^\\+\\+\\+ .*"),
        4
      )
    )

  df <- df %>%
    dplyr::mutate(
      range = stringr::str_match(
        lines,
        "-([0-9]+)(?:,([0-9]+))?\\s*\\+([0-9]+)(?:,([0-9]+))?"
      ),
      start_del = as.integer(range[,2]),
      count_del = ifelse(grepl("^@@ ", lines),
                         ifelse(is.na(as.integer(range[,3])), 1L, as.integer(range[,3])),
                         NA_integer_),
      start_add = as.integer(range[,4]),
      count_add = ifelse(grepl("^@@ ", lines),
                         ifelse(is.na(as.integer(range[,5])), 1L, as.integer(range[,5])),
                         NA_integer_)
    )

  df <- df %>%
    dplyr::mutate(
      is_segment_start = grepl("^@@ ", lines),
      segment_id = cumsum(is_segment_start)
    )

  df <- df %>%
    tidyr::fill(commit, src_file, dst_file, start_del, count_del,
                start_add, count_add, segment_id)

  df <- df %>%
    dplyr::filter(!is_commit_start, !grepl("^@@ ", lines), !is.na(src_file))

  if (nrow(df) == 0) {
    return(data.frame())
  }

  df <- df %>%
    dplyr::mutate(
      is_add = grepl("^\\+", lines),
      code = stringr::str_sub(lines, 2)
    ) %>%
    dplyr::filter(code != "")

  df <- df %>%
    dplyr::mutate(
      file_path = dplyr::coalesce(dst_file, src_file),
      file_extension = tools::file_ext(file_path),
      is_sensitive = grepl("\\.env$|password|secret|key\\.|pem$|private|credentials",
                           file_path, ignore.case = TRUE)
    )

  code_aggregated <- df %>%
    dplyr::group_by(segment_id, is_add) %>%
    dplyr::summarise(
      code = paste(code, collapse = ""),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      code = stringr::str_replace_all(code, "[^[:alnum:][:punct:]]", "")
    ) %>%
    dplyr::filter(code != "")

  unique_segments <- df %>%
    dplyr::select(commit, src_file, dst_file, file_extension, is_sensitive,
                  start_del, count_del, start_add, count_add, segment_id) %>%
    dplyr::distinct()

  result <- dplyr::left_join(
    unique_segments,
    code_aggregated,
    by = "segment_id"
  ) %>%
    dplyr::mutate(
      repo_id = repo_id,
      repo = repo_name
    ) %>%
    dplyr::select(
      commit, src_file, dst_file, file_extension, is_sensitive,
      start_del, count_del, start_add, count_add, code, is_add,
      repo, repo_id
    )

  return(result)
}

#' Initialize DuckDB database for storing Git data
#'
#' @param db_path Path to DuckDB file
#' @return Database connection
#' @export
init_db <- function(db_path = "git.duckdb") {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)

  # Таблица репозиториев
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS repo_path (
      id INTEGER,
      repo VARCHAR,
      path VARCHAR
    )
  ")

  # Таблица истории коммитов (расширенная)
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS git_commit_history (
      commit VARCHAR,
      parent_commit VARCHAR,
      author_name VARCHAR,
      author_email VARCHAR,
      committer_name VARCHAR,
      committer_email VARCHAR,
      date VARCHAR,
      message VARCHAR,
      branches VARCHAR,
      repo VARCHAR,
      repo_id INTEGER
    )
  ")

  # Таблица изменений (расширенная)
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS git_diff (
      commit VARCHAR,
      src_file VARCHAR,
      dst_file VARCHAR,
      file_extension VARCHAR,
      is_sensitive BOOLEAN,
      start_del INTEGER,
      count_del INTEGER,
      start_add INTEGER,
      count_add INTEGER,
      code VARCHAR,
      is_add BOOLEAN,
      repo VARCHAR,
      repo_id INTEGER
    )
  ")

  return(con)
}


#' Get or create repository ID
#'
#' @param con Database connection
#' @param repo_name Repository name
#' @param repo_path Path to repository
#' @return Repository ID
#' @export
get_or_create_repo_id <- function(con, repo_name, repo_path) {
  # Проверяем, существует ли уже
  query <- sprintf(
    "SELECT id FROM repo_path WHERE repo = '%s' AND path = '%s'",
    repo_name, repo_path
  )
  existing <- DBI::dbGetQuery(con, query)

  if (nrow(existing) > 0) {
    return(existing$id[1])
  }

  # Создаем новый ID
  max_id <- DBI::dbGetQuery(con, "SELECT COALESCE(MAX(id), 0) AS max_id FROM repo_path")$max_id
  new_id <- max_id + 1

  DBI::dbExecute(con, sprintf(
    "INSERT INTO repo_path (id, repo, path) VALUES (%d, '%s', '%s')",
    new_id, repo_name, repo_path
  ))

  return(new_id)
}


#' Write repository data to database
#'
#' @param con Database connection
#' @param repo_name Repository name
#' @param repo_path Path to repository
#' @export
write_repo_to_db <- function(con, repo_name, repo_path) {

  repo_id <- get_or_create_repo_id(con, repo_name, repo_path)

  # Проверяем, есть ли уже данные
  has_data <- DBI::dbGetQuery(con, sprintf(
    "SELECT EXISTS(SELECT 1 FROM git_commit_history WHERE repo_id = %d) AS has_data",
    repo_id
  ))$has_data

  if (has_data) {
    # Получаем последний коммит в базе
    last_commit <- DBI::dbGetQuery(con, sprintf(
      "SELECT commit FROM git_commit_history WHERE repo_id = %d ORDER BY date DESC LIMIT 1",
      repo_id
    ))$commit

    if (length(last_commit) > 0 && !is.na(last_commit)) {
      # Получаем только новые коммиты
      commits <- get_commit_history(repo_path, repo_id, repo_name, since = last_commit)

      if (nrow(commits) > 0) {
        DBI::dbWriteTable(con, "git_commit_history", commits, append = TRUE)

        # Для diff нужно получить новые коммиты
        cmd <- sprintf(
          'git -C "%s" log -p --unified=0 -w %s..HEAD',
          repo_path, last_commit
        )
        diff_output <- system(cmd, intern = TRUE)
        diff_df <- parse_git_diff(diff_output, repo_id, repo_name)

        if (nrow(diff_df) > 0) {
          DBI::dbWriteTable(con, "git_diff", diff_df, append = TRUE)
        }
      }
    }
  } else {
    # Первая загрузка: все коммиты
    commits <- get_commit_history(repo_path, repo_id, repo_name, since = NULL)
    if (nrow(commits) > 0) {
      DBI::dbWriteTable(con, "git_commit_history", commits, append = TRUE)
    }

    # Получаем все diff
    cmd <- sprintf('git -C "%s" log -p --unified=0 -w', repo_path)
    diff_output <- system(cmd, intern = TRUE)
    diff_df <- parse_git_diff(diff_output, repo_id, repo_name)

    if (nrow(diff_df) > 0) {
      DBI::dbWriteTable(con, "git_diff", diff_df, append = TRUE)
    }
  }
}


#' Run the complete ETL pipeline
#'
#' @param mode 0 = local, 1 = remote, 2 = GitHub user
#' @param repo_url URL for remote repository (mode 1)
#' @param repo_local_dir Local path (mode 0)
#' @param clone_dir Directory for cloning (mode 1 or 2)
#' @param username GitHub username (mode 2)
#' @param db_path Path to DuckDB database
#' @return List with status and message
#' @export
run_etl_pipeline <- function(mode, repo_url = NA, repo_local_dir = NA,
                             clone_dir = NA, username = NA,
                             db_path = "git.duckdb") {

  tryCatch({
    # Инициализируем базу
    con <- init_db(db_path)

    if (mode == 0) {
      # Локальный репозиторий
      if (is.na(repo_local_dir)) {
        stop("For mode 0, please provide repo_local_dir")
      }
      repo_path <- prepare_repo(
        mode = 0,
        local_path = repo_local_dir
      )
      repo_name <- basename(repo_local_dir)
      write_repo_to_db(con, repo_name, repo_path)

    } else if (mode == 1) {
      # Удаленный репозиторий по URL
      if (is.na(repo_url)) {
        stop("For mode 1, please provide repo_url")
      }
      repo_name <- gsub(".*/(.+)\\.git$", "\\1", repo_url)
      if (is.na(clone_dir)) {
        clone_dir <- tempdir()
      }
      repo_path <- prepare_repo(
        mode = 1,
        repo_url = repo_url,
        repo_name = repo_name,
        clone_dir = clone_dir
      )
      write_repo_to_db(con, repo_name, repo_path)

    } else if (mode == 2) {
      # Все репозитории пользователя GitHub
      if (is.na(username)) {
        stop("For mode 2, please provide username")
      }
      if (is.na(clone_dir)) {
        clone_dir <- tempdir()
      }
      repo_list <- get_github_repos(username)

      for (repo_url_item in repo_list) {
        repo_name <- gsub(".*/(.+)\\.git$", "\\1", repo_url_item)
        repo_path <- prepare_repo(
          mode = 1,
          repo_url = repo_url_item,
          repo_name = repo_name,
          clone_dir = clone_dir
        )
        write_repo_to_db(con, repo_name, repo_path)
      }
    } else {
      stop("Invalid mode. Use 0 (local), 1 (remote), or 2 (GitHub user)")
    }

    DBI::dbDisconnect(con, shutdown = TRUE)

    return(list(
      status = "success",
      message = "Data successfully loaded"
    ))

  }, error = function(e) {
    return(list(
      status = "error",
      message = e$message
    ))
  })
}

#' Extract file extension from file path
#'
#' @param file_path Character file path
#' @return Character extension (without dot)
#' @export
get_file_extension <- function(file_path) {
  tools::file_ext(file_path)
}


#' Check if file is sensitive (contains secrets, keys, etc.)
#'
#' @param file_path Character file path
#' @return Logical
#' @export
is_sensitive_file <- function(file_path) {
  sensitive_patterns <- c("\\.env$", "password", "secret", "key\\.", "pem$",
                          "private", "credentials", "token", "cert")
  any(grepl(paste(sensitive_patterns, collapse = "|"),
            file_path, ignore.case = TRUE))
}


#' Normalize author by email
#'
#' @param author_name Author name string
#' @param author_email Author email string
#' @return Normalized author identifier
#' @export
normalize_author <- function(author_name, author_email) {
  if (!is.na(author_email) && author_email != "") {
    return(author_email)
  } else {
    return(tolower(trimws(author_name)))
  }
}

