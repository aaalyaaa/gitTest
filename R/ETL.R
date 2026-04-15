#' @importFrom magrittr %>%
NULL
#' Clone or update a Git repository
#'
#' @param mode 0 = local, 1 = remote
#' @param repo_url GitHub URL (for mode = 1)
#' @param clone_dir Where to clone (for mode = 1)
#' @param local_path Path to local repo (for mode = 0)
#' @return Path to repository
clone_or_pull <- function(mode, repo_url = NULL, clone_dir = NULL, local_path = NULL) {

  git_error <- function(class, message, ...) {
    structure(
      list(message = message, ...),
      class = c(class, "error", "condition")
    )
  }

  if (mode == 0) {
    if (is.null(local_path) || local_path == "") {
      stop(git_error("path_required_error", "local_path is required for mode = 0"))
    }
    if (!dir.exists(local_path)) {
      stop(git_error("path_not_found_error", paste("Path does not exist:", local_path), path = local_path))
    }
    if (!dir.exists(file.path(local_path, ".git"))) {
      stop(git_error("not_git_repo_error", paste("Not a Git repository:", local_path), path = local_path))
    }
    system(sprintf('git -C "%s" pull', local_path))
    return(local_path)
  }

  if (mode == 1) {
    if (is.null(repo_url) || repo_url == "") {
      stop(git_error("url_required_error", "repo_url is required for mode = 1"))
    }

    if (!grepl("github\\.com", repo_url, ignore.case = TRUE)) {
      stop(git_error("invalid_url_error", "Only GitHub URLs are supported", url = repo_url))
    }

    repo_url <- gsub("/$", "", repo_url)
    if (!grepl("\\.git$", repo_url)) {
      repo_url <- paste0(repo_url, ".git")
    }

    if (!grepl("^https?://github\\.com/[^/]+/[^/]+\\.git$", repo_url)) {
      stop(git_error("invalid_url_format_error",
                     "Invalid GitHub URL format. Expected: https://github.com/username/repo.git",
                     url = repo_url))
    }

    check_cmd <- sprintf('git ls-remote "%s" HEAD', repo_url)
    check_result <- system(check_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    if (check_result != 0) {
      stop(git_error("repo_not_found_error",
                     "Repository not found or inaccessible. Check URL and your connection.",
                     url = repo_url))
    }

    if (is.null(clone_dir)) clone_dir <- tempdir()

    if (!dir.exists(clone_dir)) {
      stop(git_error("clone_dir_error", paste("Clone directory does not exist:", clone_dir), path = clone_dir))
    }

    repo_name <- gsub(".*/(.+)\\.git$", "\\1", repo_url)
    repo_path <- file.path(clone_dir, repo_name)

    if (dir.exists(repo_path)) {
      system(sprintf('git -C "%s" pull', repo_path))
    } else {
      system(sprintf('git clone "%s" "%s"', repo_url, repo_path))
    }
    return(repo_path)
  }

  stop("mode must be 0 (local) or 1 (remote)")
}

#' Get commit history from a Git repository
#'
#' @param repo_path Path to local Git repository
#' @param repo_id Numeric ID of the repository
#' @param since Optional: only commits after this commit hash
#' @return Data frame with commit history
#' @export
get_commit_history <- function(repo_path, repo_id, since = NULL) {
  # %H - хэш коммита
  # %P - хэш родительского коммита
  # %an - имя автора
  # %ae - email автора
  # %ai - дата
  # %s - сообщение коммита

  format_string <- "%H\t%P\t%an\t%ai\t%s"

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

  repo_name <- basename(repo_path)

  commits <- data.frame(lines = output, stringsAsFactors = FALSE) %>%
    tidyr::separate(
      col = lines,
      into = c("commit", "parent_commit", "author_name", "date", "message"),
      sep = "\t",
      fill = "right"
    ) %>%
    dplyr::mutate(
      date = stringr::str_replace(date, " \\+[0-9]{4}$", ""),
      date = as.character(lubridate::ymd_hms(date, tz = "UTC")),
      repo_id = repo_id,
      repo = repo_name
    )

  return(commits)
}

#' Parse git log -p output into commit blocks
#'
#' @param repo_path Path to local Git repository
#' @return List of character vectors, each vector is one commit block
get_commits <- function(repo_path) {

  con <- pipe(sprintf('git -C "%s" log -p --unified=0 -w --ignore-blank-lines', repo_path))
  lines <- readLines(con)
  close(con)

  commit_starts <- grep("^commit ", lines)

  commits_blocks <- list()
  for (i in seq_along(commit_starts)) {
    start <- commit_starts[i]
    end <- ifelse(i < length(commit_starts), commit_starts[i+1] - 1, length(lines))
    commits_blocks[[i]] <- lines[start:end]
  }

  return(commits_blocks)
}

#' Parse a hunk line (@@ ... @@) to extract line numbers
#' Supports multiple formats:
#' - @@ -10,5 +10,8 @@
#' - @@ -10 +10 @@
#' - @@ -10,5 +10 @@
#' - @@ -10 +10,5 @@
#'
#' @param line A string starting with "@@ "
#' @return Numeric vector of length 4: c(start_del, count_del, start_add, count_add) or NULL if parsing fails
parse_hunk_line <- function(line) {
  # Шаблон: -start_del(,count_del)? +start_add(,count_add)?
  pattern <- "^@@ -([0-9]+)(?:,([0-9]+))? \\+([0-9]+)(?:,([0-9]+))? @@"
  matches <- regexec(pattern, line)
  parts <- regmatches(line, matches)[[1]]

  if (length(parts) < 4) return(NULL)

  start_del <- as.integer(parts[2])
  count_del <- ifelse(is.na(parts[3]) || parts[3] == "", 1, as.integer(parts[3]))
  start_add <- as.integer(parts[4])
  count_add <- ifelse(is.na(parts[5]) || parts[5] == "", 1, as.integer(parts[5]))

  return(c(start_del, count_del, start_add, count_add))
}

#' Parse a single commit block into a data frame of changes
#'
#' @param block Character vector representing one commit (from get_commits())
#' @param repo_id Numeric ID of the repository
#' @return Data frame with columns:
#'         commit, src_file, dst_file, start_del, count_del, start_add, count_add, added_code, deleted_code
parse_commit <- function(block, repo_id) {
  first_line <- block[1]
  hash <- sub("^commit ", "", first_line)
  hash <- strsplit(hash, " ")[[1]][1]

  diff_lines <- grep("^diff --git", block)
  if (length(diff_lines) == 0) return(NULL)

  results <- list()

  for (i in seq_along(diff_lines)) {
    start <- diff_lines[i]
    end <- ifelse(i < length(diff_lines), diff_lines[i+1] - 1, length(block))
    file_block <- block[start:end]

    src_line <- grep("^--- ", file_block, value = TRUE)[1]
    dst_line <- grep("^\\+\\+\\+ ", file_block, value = TRUE)[1]
    if (is.na(src_line) || is.na(dst_line)) next

    src_file <- sub("^--- a/", "", src_line)
    src_file <- sub("^--- /dev/null", NA_character_, src_file)
    dst_file <- sub("^\\+\\+\\+ b/", "", dst_line)

    file_for_ext <- ifelse(!is.na(dst_file), dst_file, src_file)
    if (is.na(file_for_ext)) {
      file_extension <- ""
    } else {
      file_name <- basename(file_for_ext)

      special_files <- c(
        "description" = "description",
        "namespace" = "namespace",
        ".rbuildignore" = "rbuildignore",
        ".rprofile" = "rprofile",
        ".renviron" = "renviron",
        "license" = "license",
        "readme" = "readme",
        "news" = "news",
        "changelog" = "changelog",
        "todo" = "todo",
        "code_of_conduct" = "code_of_conduct",
        "contributing" = "contributing",
        "maintainers" = "maintainers",
        "security" = "security",
        ".gitignore" = "gitignore",
        ".gitattributes" = "gitattributes",
        ".gitmodules" = "gitmodules",
        ".dockerignore" = "dockerignore",
        "dockerfile" = "dockerfile",
        "makefile" = "makefile",
        "makevars" = "makevars"
      )

      file_name_lower <- tolower(file_name)

      if (file_name_lower %in% names(special_files)) {
        file_extension <- special_files[file_name_lower]
      } else if (grepl("\\.", file_name)) {
        file_extension <- tolower(sub(".*\\.", "", file_name))
      } else {
        file_extension <- ""
      }
    }

    hunk_indices <- grep("^@@ ", file_block)

    for (idx in hunk_indices) {
      hunk_line <- file_block[idx]

      parsed <- parse_hunk_line(hunk_line)
      if (is.null(parsed)) next

      start_del <- parsed[1]
      count_del <- parsed[2]
      start_add <- parsed[3]
      count_add <- parsed[4]

      code_lines <- c()
      next_idx <- idx + 1
      while (next_idx <= length(file_block)) {
        next_line <- file_block[next_idx]
        if (grepl("^@@ ", next_line)) break
        if (grepl("^[+-]", next_line)) {
          code_lines <- c(code_lines, next_line)
        }
        next_idx <- next_idx + 1
      }

      added <- code_lines[startsWith(code_lines, "+")]
      added <- added[!startsWith(added, "+++")]
      added <- trimws(substr(added, 2, nchar(added)))
      added <- added[added != ""]
      added_code <- paste(added, collapse = "\n")
      added_code <- gsub(" +\n", "\n", added_code)

      deleted <- code_lines[startsWith(code_lines, "-")]
      deleted <- deleted[!startsWith(deleted, "---")]
      deleted <- trimws(substr(deleted, 2, nchar(deleted)))
      deleted <- deleted[deleted != ""]
      deleted_code <- paste(deleted, collapse = "\n")
      deleted_code <- gsub(" +\n", "\n", deleted_code)

      results[[length(results) + 1]] <- data.frame(
        repo_id = repo_id,
        commit = hash,
        src_file = src_file,
        dst_file = dst_file,
        file_extension = file_extension,
        start_del = start_del,
        count_del = count_del,
        start_add = start_add,
        count_add = count_add,
        added_code = added_code,
        deleted_code = deleted_code,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(results) == 0) return(NULL)
  do.call(rbind, results)
}

#' Initialize DuckDB database
#'
#' Creates three tables:
#' - repo_path: stores repositories
#' - git_commit_history: stores commit information
#' - git_file_changes: stores code changes
#'
#' @param db_path Path to DuckDB file (default: "git.duckdb")
#' @return Database connection object
init_db <- function(db_path = "git.duckdb") {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)

  DBI::dbExecute(con, "INSTALL icu;")
  DBI::dbExecute(con, "LOAD icu;")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS repo_path (
      id INTEGER PRIMARY KEY,
      repo VARCHAR NOT NULL,
      path VARCHAR NOT NULL
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS git_commit_history (
      repo_id INTEGER,
      commit VARCHAR(40) NOT NULL,
      parent_commit VARCHAR(40),
      author_name VARCHAR,
      date TIMESTAMPTZ,
      message TEXT,
      repo VARCHAR,
      PRIMARY KEY (repo_id, commit),
      FOREIGN KEY (repo_id) REFERENCES repo_path(id)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS git_file_changes (
      repo_id INTEGER,
      commit VARCHAR(40) NOT NULL,
      src_file VARCHAR,
      dst_file VARCHAR,
      file_extension VARCHAR,
      start_del INTEGER,
      count_del INTEGER,
      start_add INTEGER,
      count_add INTEGER,
      added_code TEXT,
      deleted_code TEXT,
      FOREIGN KEY (repo_id, commit) REFERENCES git_commit_history(repo_id, commit)
    )
  ")

  return(con)
}

#' Get or create repository ID
#'
#' @param con Database connection (from init_db)
#' @param repo_name Name of the repository
#' @param repo_path Path to the repository
#' @return Repository ID (integer)
repo_id <- function(con, repo_name, repo_path) {

  if (!DBI::dbIsValid(con)) {
    stop("Database connection is not valid. Please re-establish connection.")
  }

  query <- sprintf(
    "SELECT id FROM repo_path WHERE repo = '%s' AND path = '%s'",
    repo_name, repo_path
  )
  existing <- DBI::dbGetQuery(con, query)

  if (nrow(existing) > 0) {
    return(existing$id[1])
  }

  max_id <- DBI::dbGetQuery(con, "SELECT COALESCE(MAX(id), 0) AS max_id FROM repo_path")$max_id
  new_id <- max_id + 1

  DBI::dbExecute(con, sprintf(
    "INSERT INTO repo_path (id, repo, path) VALUES (%d, '%s', '%s')",
    new_id, repo_name, repo_path
  ))

  return(new_id)
}

#' Write data to database
#' @param con Database connection (from init_db)
#' @param repo_name Name of the repository
#' @param repo_path Path to the repository
#' @return Invisibly returns number of new commits written
write_repo_to_db <- function(con, repo_name, repo_path) {

  if (!DBI::dbIsValid(con)) {
    stop("Database connection is not valid. Please re-establish connection.")
  }

  repo_id_value <- repo_id(con, repo_name, repo_path)

  has_data <- DBI::dbGetQuery(con, sprintf(
    "SELECT EXISTS(SELECT 1 FROM git_commit_history WHERE repo_id = %d) AS has_data",
    repo_id_value
  ))$has_data

  if (has_data) {
    last_commit <- DBI::dbGetQuery(con, sprintf(
      "SELECT commit FROM git_commit_history
       WHERE repo_id = %d
       ORDER BY date DESC LIMIT 1",
      repo_id_value
    ))$commit

    commits <- get_commit_history(repo_path, repo_id_value, since = last_commit)
  } else {
    commits <- get_commit_history(repo_path, repo_id_value)
  }

  if (nrow(commits) == 0) {
    message("No new commits to add")
    return(invisible(0))
  }

  DBI::dbWriteTable(con, "git_commit_history", commits, append = TRUE)

  all_blocks <- get_commits(repo_path)

  block_hashes <- sapply(all_blocks, function(block) {
    first_line <- block[1]
    hash <- sub("^commit ", "", first_line)
    strsplit(hash, " ")[[1]][1]
  })

  keep <- block_hashes %in% commits$commit
  blocks <- all_blocks[keep]

  all_changes <- list()
  for (i in seq_along(blocks)) {
    res <- parse_commit(blocks[[i]], repo_id_value)
    if (!is.null(res)) all_changes[[i]] <- res
  }

  changes_df <- do.call(rbind, all_changes)

  if (nrow(changes_df) > 0) {
    DBI::dbWriteTable(con, "git_file_changes", changes_df, append = TRUE)
  }

  message(sprintf("Added %d new commits and %d changes",
                  nrow(commits), nrow(changes_df)))

  invisible(nrow(commits))
}

#' Run ETL pipeline
#'
#' @param mode 0 = local, 1 = remote
#' @param repo_url GitHub URL (for mode = 1)
#' @param local_path Path to local repo (for mode = 0)
#' @param clone_dir Directory for cloning (for mode = 1, optional)
#' @param db_path Path to DuckDB database (default: "git.duckdb")
#' @return List with status, message, repo_path, and db_path
#' @export
run_etl_pipeline <- function(mode, repo_url = NULL, local_path = NULL,
                             clone_dir = NULL, db_path = "git.duckdb") {

  tryCatch({
    if (mode == 0) {
      if (is.null(local_path)) {
        stop("local_path is required for mode = 0")
      }
      repo_path <- clone_or_pull(mode = 0, local_path = local_path)
      repo_name <- basename(local_path)

    } else if (mode == 1) {
      if (is.null(repo_url)) {
        stop("repo_url is required for mode = 1")
      }
      repo_path <- clone_or_pull(mode = 1, repo_url = repo_url, clone_dir = clone_dir)
      repo_name <- gsub(".*/(.+)\\.git$", "\\1", repo_url)

    } else {
      stop("mode must be 0 (local) or 1 (remote)")
    }

    con <- init_db(db_path)

    write_repo_to_db(con, repo_name, repo_path)

    DBI::dbDisconnect(con, shutdown = TRUE)

    return(list(
      status = "success",
      message = sprintf("Repository '%s' successfully loaded", repo_name),
      repo_path = repo_path,
      db_path = db_path
    ))

  }, error = function(e) {
    return(list(
      status = "error",
      message = e$message
    ))
  })
}
