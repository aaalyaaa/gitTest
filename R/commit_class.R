# Классификация коммитов с использованием предобученной модели
#' @export
load_commit_model <- function() {
  model_path <- system.file("extdata", "commit_classifier_model.rds", package = utils::packageName())
  if (model_path == "") {
    stop("Модель не найдена. Убедитесь, что пакет установлен корректно.")
  }
  readRDS(model_path)
}

# Словари для группировки типов коммитов
commit_type_groups_en <- c(
  "feat"     = "New Feature",
  "fix"      = "Bug Fix",
  "refactor" = "Code Improvement",
  "style"    = "Code Improvement",
  "perf"     = "Code Improvement",
  "test"     = "Tests",
  "docs"     = "Documentation",
  "ci"       = "Infrastructure",
  "build"    = "Infrastructure",
  "chore"    = "Infrastructure"
)

commit_type_groups_ru <- c(
  "feat"     = "Новая функциональность",
  "fix"      = "Исправления",
  "refactor" = "Улучшение кода",
  "style"    = "Улучшение кода",
  "perf"     = "Улучшение кода",
  "test"     = "Тесты",
  "docs"     = "Документация",
  "ci"       = "Инфраструктура",
  "build"    = "Инфраструктура",
  "chore"    = "Инфраструктура"
)
#' Классификация коммитов в базе данных
#' @export
classify_commits_in_db <- function(conn, model_obj, table_name = "git_commit_history",
                                   author_name = NULL, batch_size = 1000, repo_id = NULL,
                                   group_mode = c("en", "ru")) {
  group_mode <- match.arg(group_mode)
  
  where_clause <- ""
  if (!is.null(author_name)) {
    where_clause <- sprintf("author_name = '%s'", gsub("'", "''", author_name))
  }
  if (!is.null(repo_id)) {
    if (where_clause != "") where_clause <- paste0(where_clause, " AND ")
    where_clause <- paste0(where_clause, "repo_id = ", repo_id)
  }
  if (where_clause != "") where_clause <- paste0("WHERE ", where_clause)
  
  query <- sprintf("SELECT commit, message FROM %s %s", table_name, where_clause)
  commits <- DBI::dbGetQuery(conn, query)
  commits <- commits[!is.na(commits$message) & nchar(trimws(commits$message)) > 0, ]
  if (nrow(commits) == 0) return(invisible(FALSE))
  message("Классификация ", nrow(commits), " коммитов...")
  predictions <- character(nrow(commits))
  
  for (start in seq(1, nrow(commits), by = batch_size)) {
    end <- min(start + batch_size - 1, nrow(commits))
    batch_messages <- commits$message[start:end]
    batch_features <- model_obj$transform(batch_messages)
    pred_obj <- predict(model_obj$model, data = batch_features)
    predictions[start:end] <- as.character(pred_obj$predictions)
  }
  
  # Применяем группировку, если нужно
  if (group_mode == "en") {
    predictions <- commit_type_groups_en[predictions]
    # Если какой-то тип не найден в словаре, оставляем как есть
    predictions[is.na(predictions)] <- predictions[is.na(predictions)]
  } else if (group_mode == "ru") {
    predictions <- commit_type_groups_ru[predictions]
    predictions[is.na(predictions)] <- predictions[is.na(predictions)]
  }
  
  commits$predicted_type <- predictions
  col_exists <- DBI::dbGetQuery(conn, sprintf("PRAGMA table_info(%s)", table_name))$name
  if (!"predicted_commit_type" %in% col_exists) {
    DBI::dbExecute(conn, sprintf("ALTER TABLE %s ADD COLUMN predicted_commit_type VARCHAR", table_name))
  }
  
  for (i in seq_len(nrow(commits))) {
    DBI::dbExecute(conn, sprintf("
      UPDATE %s SET predicted_commit_type = '%s' WHERE commit = '%s'
    ", table_name, commits$predicted_type[i], commits$commit[i]))
  }
  message("Готово.")
  invisible(TRUE)
}

#' Загрузка датасета
#' @noRd
download_annotated_csv <- function(save_path = "allcommits.csv") {
  if (file.exists(save_path)) {
    message("Файл уже существует: ", save_path)
    return(save_path)
  }
  url <- "https://raw.githubusercontent.com/0x404/conventional-commit-classification/main/Dataset/allcommits.csv"
  message("Скачивание датасета...")
  tryCatch({
    download.file(url, destfile = save_path, mode = "wb", quiet = FALSE)
    message("Датасет сохранён как ", save_path)
  }, error = function(e) {
    stop("Не удалось скачать файл: ", e$message)
  })
  return(save_path)
}

#' @noRd
read_safe_csv <- function(path) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    df <- data.table::fread(path, encoding = "UTF-8", data.table = FALSE)
  } else {
    df <- read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                   quote = "\"", escape = FALSE, comment.char = "")
  }
  names(df) <- tolower(names(df))
  names(df) <- gsub("^\ufeff", "", names(df))
  names(df) <- trimws(names(df))
  return(df)
}

#' @noRd
load_annotated_dataset <- function(csv_path = NULL) {
  if (is.null(csv_path)) csv_path <- download_annotated_csv()
  df <- read_safe_csv(csv_path)
  msg_candidates <- c("commit_message", "message", "msg", "text")
  msg_col <- msg_candidates[msg_candidates %in% names(df)][1]
  type_candidates <- c("annotated_type", "type", "label", "commit_type")
  type_col <- type_candidates[type_candidates %in% names(df)][1]
  if (is.na(msg_col) || is.na(type_col)) {
    stop("Не найдены колонки сообщения или типа")
  }
  df$message <- df[[msg_col]]
  df$type <- as.factor(df[[type_col]])
  df <- df[!is.na(df$message) & nchar(trimws(df$message)) > 0, ]
  cat("Загружено", nrow(df), "коммитов, классы:", paste(levels(df$type), collapse=", "), "\n")
  return(df)
}

#' Обучение модели 
#' @noRd
train_commit_classifier <- function(csv_path = NULL, max_features = 5000, num.trees = 100) {
  if (!requireNamespace("tidytext", quietly = TRUE)) stop("Установите tidytext")
  if (!requireNamespace("ranger", quietly = TRUE)) stop("Установите ranger")
  if (!requireNamespace("Matrix", quietly = TRUE)) stop("Установите Matrix")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Установите dplyr")
  
  df <- load_annotated_dataset(csv_path)
  df$id <- 1:nrow(df)
  
  word_counts <- df |>
    dplyr::select(id, message) |>
    tidytext::unnest_tokens(word, message) |>
    dplyr::anti_join(tidytext::stop_words, by = "word") |>
    dplyr::count(id, word)
  
  doc_freq <- word_counts |>
    dplyr::group_by(word) |>
    dplyr::summarise(df = dplyr::n(), .groups = "drop")
  
  N <- nrow(df)
  doc_freq$idf <- log(N / doc_freq$df)
  
  tfidf_data <- word_counts |>
    dplyr::left_join(doc_freq, by = "word") |>
    dplyr::mutate(tf_idf = n * idf)
  
  word_importance <- tfidf_data |>
    dplyr::group_by(word) |>
    dplyr::summarise(total_tfidf = sum(tf_idf), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total_tfidf)) |>
    dplyr::slice_head(n = max_features)
  
  keep_words <- word_importance$word
  cat("Оставлено", length(keep_words), "слов\n")
  
  tfidf_filtered <- tfidf_data |> dplyr::filter(word %in% keep_words)
  doc_freq_filtered <- doc_freq |> dplyr::filter(word %in% keep_words)
  
  tfidf_filtered <- tfidf_filtered[!is.na(tfidf_filtered$tf_idf), ]
  rows <- tfidf_filtered$id
  cols <- match(tfidf_filtered$word, keep_words)
  if (any(is.na(cols))) {
    keep_ok <- !is.na(cols)
    rows <- rows[keep_ok]
    cols <- cols[keep_ok]
    tfidf_filtered <- tfidf_filtered[keep_ok, ]
  }
  vals <- tfidf_filtered$tf_idf
  dtm <- Matrix::sparseMatrix(i = rows, j = cols, x = vals,
                              dims = c(N, length(keep_words)),
                              dimnames = list(NULL, keep_words))
  target <- df$type
  
  cat("Обучение ranger (", num.trees, "деревьев)...\n")
  model <- ranger::ranger(x = dtm, y = target, num.trees = num.trees,
                          mtry = max(1, floor(sqrt(ncol(dtm)))),
                          num.threads = parallel::detectCores() - 1,
                          verbose = TRUE)
  
  train_acc <- mean(model$predictions == target)
  cat(sprintf("Точность на обучении (OOB): %.1f%%\n", train_acc * 100))
  
  idf_weights <- doc_freq_filtered$idf
  names(idf_weights) <- keep_words
  
  transform_new_messages <- function(new_messages) {
    temp <- data.frame(id = seq_along(new_messages), message = new_messages)
    words <- temp |>
      tidytext::unnest_tokens(word, message) |>
      dplyr::anti_join(tidytext::stop_words, by = "word") |>
      dplyr::count(id, word) |>
      dplyr::filter(word %in% keep_words)
    
    if (nrow(words) == 0) {
      return(Matrix::sparseMatrix(i = 1, j = 1, x = 0,
                                  dims = c(length(new_messages), length(keep_words)),
                                  dimnames = list(NULL, keep_words)))
    }
    words$idf <- idf_weights[words$word]
    words$tf_idf <- words$n * words$idf
    rows <- words$id
    cols <- match(words$word, keep_words)
    vals <- words$tf_idf
    if (any(is.na(cols))) {
      keep <- !is.na(cols)
      rows <- rows[keep]
      cols <- cols[keep]
      vals <- vals[keep]
    }
    dtm_new <- Matrix::sparseMatrix(i = rows, j = cols, x = vals,
                                    dims = c(length(new_messages), length(keep_words)),
                                    dimnames = list(NULL, keep_words))
    return(dtm_new)
  }
  
  result <- list(
    model = model,
    transform = transform_new_messages,
    keep_words = keep_words,
    classes = levels(df$type),
    train_accuracy = train_acc,
    oob_error = 1 - train_acc
  )
  class(result) <- "commit_classifier"
  return(result)
}