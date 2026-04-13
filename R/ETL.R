#' Clone or update a Git repository
#'
#' @param mode 0 = local, 1 = remote
#' @param repo_url GitHub URL (for mode = 1)
#' @param clone_dir Where to clone (for mode = 1)
#' @param local_path Path to local repo (for mode = 0)
#' @return Path to repository
clone_or_pull <- function(mode, repo_url = NULL, clone_dir = NULL, local_path = NULL) {

  if (mode == 0) {
    system(sprintf('git -C "%s" pull', local_path))
    return(local_path)
  }

  if (mode == 1) {
    if (is.null(clone_dir)) clone_dir <- tempdir()

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
  format_string <- "%H\t%P\t%an\t%ae\t%ai\t%s"

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
      into = c("commit", "parent_commit", "author_name", "author_email", "date", "message"),
      sep = "\t",
      fill = "right"
    ) %>%
    dplyr::mutate(
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
      added_code <- paste(substr(added, 2, nchar(added)), collapse = "\n")

      deleted <- code_lines[startsWith(code_lines, "-")]
      deleted <- deleted[!startsWith(deleted, "---")]
      deleted_code <- paste(substr(deleted, 2, nchar(deleted)), collapse = "\n")

      results[[length(results) + 1]] <- data.frame(
        repo_id = repo_id,
        commit = hash,
        src_file = src_file,
        dst_file = dst_file,
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
