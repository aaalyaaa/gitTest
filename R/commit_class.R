# commit_classifier.R
# Классификация Conventional Commits с удалением стоп-слов и отбором признаков

commit_type_labels <- c(
  "feat"     = "Новая функциональность",
  "fix"      = "Исправление ошибки",
  "docs"     = "Документация",
  "style"    = "Стиль кода",
  "refactor" = "Рефакторинг",
  "perf"     = "Улучшение производительности",
  "test"     = "Тесты",
  "build"    = "Сборка",
  "ci"       = "CI/CD",
  "chore"    = "Обслуживание"
)

download_annotated_csv <- function(save_path = "annotated_commits.csv") {
  if (file.exists(save_path)) return(save_path)
  url <- "https://raw.githubusercontent.com/0x404/conventional-commit-classification/main/Dataset/annotated_commits.csv"
  message("Скачивание датасета...")
  download.file(url, destfile = save_path, mode = "wb", quiet = FALSE)
  return(save_path)
}

#' Обучение модели с удалением стоп-слов и отбором top max_features признаков
train_commit_classifier <- function(csv_path = NULL, max_features = 1000) {
  if (is.null(csv_path)) csv_path <- download_annotated_csv()
  if (!requireNamespace("tidytext", quietly = TRUE)) stop("Установите tidytext")
  if (!requireNamespace("randomForest", quietly = TRUE)) stop("Установите randomForest")
  
  library(tidytext); library(dplyr); library(randomForest); library(Matrix)
  
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  names(df) <- tolower(names(df))
  msg_col <- if ("commit_message" %in% names(df)) "commit_message" else "message"
  type_col <- if ("annotated_type" %in% names(df)) "annotated_type" else "type"
  df$message <- df[[msg_col]]
  df$type <- as.factor(df[[type_col]])
  df <- df[!is.na(df$message) & nchar(trimws(df$message)) > 0, ]
  df$id <- 1:nrow(df)
  cat("Обучающая выборка:", nrow(df), "коммитов, классов:", paste(levels(df$type), collapse=", "), "\n")
  
  # Частоты слов с удалением стоп-слов
  word_counts <- df %>%
    select(id, message) %>%
    unnest_tokens(word, message) %>%
    anti_join(stop_words, by = "word") %>%
    count(id, word)
  
  cat("Уникальных слов после удаления стоп-слов:", length(unique(word_counts$word)), "\n")
  
  # Документная частота и IDF
  doc_freq <- word_counts %>%
    group_by(word) %>%
    summarise(df = n(), .groups = "drop")
  N <- nrow(df)
  doc_freq$idf <- log(N / doc_freq$df)
  
  # TF-IDF
  tfidf_data <- word_counts %>%
    left_join(doc_freq, by = "word") %>%
    mutate(tf_idf = n * idf)
  
  # Оставляем только top max_features слов по сумме tf-idf
  word_importance <- tfidf_data %>%
    group_by(word) %>%
    summarise(total_tfidf = sum(tf_idf), .groups = "drop") %>%
    arrange(desc(total_tfidf)) %>%
    slice_head(n = max_features)
  keep_words <- word_importance$word
  cat("Оставлено", length(keep_words), "наиболее информативных слов\n")
  
  tfidf_filtered <- tfidf_data %>% filter(word %in% keep_words)
  doc_freq_filtered <- doc_freq %>% filter(word %in% keep_words)
  
  # Проверяем и чистим данные перед созданием матрицы
  tfidf_filtered <- tfidf_filtered[!is.na(tfidf_filtered$tf_idf), ]
  rows <- tfidf_filtered$id
  cols <- match(tfidf_filtered$word, keep_words)
  if (any(is.na(cols))) {
    warning("NA в match, удаляем")
    keep_ok <- !is.na(cols)
    rows <- rows[keep_ok]
    cols <- cols[keep_ok]
    tfidf_filtered <- tfidf_filtered[keep_ok, ]
  }
  vals <- tfidf_filtered$tf_idf
  if (max(rows) > N) stop("rows > N")
  if (max(cols) > length(keep_words)) stop("cols > length(keep_words)")
  
  dtm <- sparseMatrix(i = rows, j = cols, x = vals,
                      dims = c(N, length(keep_words)),
                      dimnames = list(NULL, make.names(keep_words)))
  X <- as.matrix(dtm)
  train_df <- as.data.frame(X)
  train_df$label <- df$type
  names(train_df) <- make.names(names(train_df))
  
  # Удаляем признаки с нулевой дисперсией (если остались)
  numeric_cols <- names(train_df)[sapply(train_df, is.numeric)]
  variances <- sapply(train_df[, numeric_cols], var)
  keep <- variances > 0
  if (sum(!keep) > 0) {
    cat("Удалено", sum(!keep), "признаков с нулевой дисперсией\n")
    train_df <- train_df[, c(numeric_cols[keep], "label")]
  }
  
  predictors <- train_df[, names(train_df) != "label", drop = FALSE]
  target <- train_df$label
  
  cat("Обучение Random Forest (ntree=100)...\n")
  model <- randomForest(x = predictors, y = target, ntree = 100, importance = TRUE)
  
  train_pred <- predict(model, predictors)
  train_acc <- sum(train_pred == target) / length(target)
  cat(sprintf("Точность на обучении: %.1f%%\n", train_acc * 100))
  
  oob_err <- model$err.rate[nrow(model$err.rate), "OOB"]
  cat(sprintf("OOB ошибка: %.1f%%\n", oob_err * 100))
  
  # Функция трансформации новых сообщений
  idf_weights <- doc_freq_filtered$idf
  names(idf_weights) <- keep_words
  train_cols <- names(predictors)
  
  transform_new_messages <- function(new_messages) {
    temp <- data.frame(id = seq_along(new_messages), message = new_messages)
    words <- temp %>%
      unnest_tokens(word, message) %>%
      anti_join(stop_words, by = "word") %>%
      count(id, word) %>%
      filter(word %in% keep_words)
    if (nrow(words) == 0) {
      return(as.data.frame(matrix(0, nrow = length(new_messages), ncol = length(train_cols),
                                  dimnames = list(NULL, train_cols))))
    }
    words$idf <- idf_weights[words$word]
    words$tf_idf <- words$n * words$idf
    rows <- words$id
    cols <- match(words$word, keep_words)
    vals <- words$tf_idf
    # Проверка
    if (any(is.na(cols))) {
      keep_ok <- !is.na(cols)
      rows <- rows[keep_ok]; cols <- cols[keep_ok]; vals <- vals[keep_ok]
    }
    dtm <- sparseMatrix(i = rows, j = cols, x = vals,
                        dims = c(length(new_messages), length(keep_words)),
                        dimnames = list(NULL, make.names(keep_words)))
    new_df <- as.data.frame(as.matrix(dtm))
    names(new_df) <- make.names(names(new_df))
    missing_cols <- setdiff(train_cols, names(new_df))
    for (col in missing_cols) new_df[[col]] <- 0
    new_df <- new_df[, train_cols, drop = FALSE]
    return(new_df)
  }
  
  list(
    model = model,
    transform = transform_new_messages,
    train_cols = train_cols,
    classes = levels(df$type),
    train_accuracy = train_acc,
    oob_error = oob_err
  )
}

#' Оценка на тестовой выборке (с удалением стоп-слов)
evaluate_classifier <- function(csv_path = NULL, train_ratio = 0.8, max_features = 1000) {
  if (is.null(csv_path)) csv_path <- download_annotated_csv()
  
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  names(df) <- tolower(names(df))
  msg_col <- if ("commit_message" %in% names(df)) "commit_message" else "message"
  type_col <- if ("annotated_type" %in% names(df)) "annotated_type" else "type"
  df$message <- df[[msg_col]]
  df$type <- as.factor(df[[type_col]])
  df <- df[!is.na(df$message) & nchar(trimws(df$message)) > 0, ]
  df$id <- 1:nrow(df)
  
  set.seed(123)
  train_idx <- sample(1:nrow(df), size = floor(train_ratio * nrow(df)))
  train_df <- df[train_idx, ]
  test_df <- df[-train_idx, ]
  # Создаём локальные индексы, начинающиеся с 1
  train_df$local_id <- 1:nrow(train_df)
  test_df$local_id <- 1:nrow(test_df)
  
  cat("Разделение: train =", nrow(train_df), ", test =", nrow(test_df), "\n")
  
  library(tidytext); library(dplyr); library(Matrix); library(randomForest)
  
  # ---- Обучение на train_df ----
  word_counts_train <- train_df %>%
    select(local_id, message) %>%
    unnest_tokens(word, message) %>%
    anti_join(stop_words, by = "word") %>%
    count(local_id, word) %>%
    rename(id = local_id)
  
  doc_freq <- word_counts_train %>%
    group_by(word) %>%
    summarise(df = n(), .groups = "drop")
  N_train <- nrow(train_df)
  doc_freq$idf <- log(N_train / doc_freq$df)
  
  tfidf_train <- word_counts_train %>%
    left_join(doc_freq, by = "word") %>%
    mutate(tf_idf = n * idf)
  
  # Отбираем top max_features слов
  word_importance <- tfidf_train %>%
    group_by(word) %>%
    summarise(total_tfidf = sum(tf_idf), .groups = "drop") %>%
    arrange(desc(total_tfidf)) %>%
    slice_head(n = max_features)
  keep_words <- word_importance$word
  cat("Оставлено", length(keep_words), "слов\n")
  
  tfidf_train <- tfidf_train %>% filter(word %in% keep_words)
  doc_freq <- doc_freq %>% filter(word %in% keep_words)
  
  # Чистим перед матрицей
  tfidf_train <- tfidf_train[!is.na(tfidf_train$tf_idf), ]
  rows <- tfidf_train$id
  cols <- match(tfidf_train$word, keep_words)
  if (any(is.na(cols))) {
    keep_ok <- !is.na(cols)
    rows <- rows[keep_ok]; cols <- cols[keep_ok]; tfidf_train <- tfidf_train[keep_ok, ]
  }
  vals <- tfidf_train$tf_idf
  if (max(rows) > N_train) stop("rows > N_train")
  if (max(cols) > length(keep_words)) stop("cols > length(keep_words)")
  
  dtm_train <- sparseMatrix(i = rows, j = cols, x = vals,
                            dims = c(N_train, length(keep_words)),
                            dimnames = list(NULL, make.names(keep_words)))
  X_train <- as.matrix(dtm_train)
  train_data <- as.data.frame(X_train)
  train_data$label <- train_df$type
  names(train_data) <- make.names(names(train_data))
  
  numeric_cols <- names(train_data)[sapply(train_data, is.numeric)]
  variances <- sapply(train_data[, numeric_cols], var)
  keep_cols <- variances > 0
  if (sum(!keep_cols) > 0) {
    train_data <- train_data[, c(numeric_cols[keep_cols], "label")]
  }
  predictors_train <- train_data[, names(train_data) != "label", drop = FALSE]
  target_train <- train_data$label
  model <- randomForest(x = predictors_train, y = target_train, ntree = 100)
  
  # ---- Тестовая выборка ----
  word_counts_test <- test_df %>%
    select(local_id, message) %>%
    unnest_tokens(word, message) %>%
    anti_join(stop_words, by = "word") %>%
    count(local_id, word) %>%
    rename(id = local_id) %>%
    filter(word %in% keep_words)
  
  if (nrow(word_counts_test) == 0) {
    cat("В тестовой выборке нет слов из словаря!\n")
    return(list(accuracy = 0, confusion = NULL))
  }
  word_counts_test <- word_counts_test %>%
    left_join(doc_freq[, c("word", "idf")], by = "word") %>%
    mutate(tf_idf = n * idf) %>%
    filter(!is.na(tf_idf))
  
  rows_test <- word_counts_test$id
  cols_test <- match(word_counts_test$word, keep_words)
  vals_test <- word_counts_test$tf_idf
  if (any(is.na(cols_test))) {
    keep_ok <- !is.na(cols_test)
    rows_test <- rows_test[keep_ok]; cols_test <- cols_test[keep_ok]; vals_test <- vals_test[keep_ok]
  }
  if (max(rows_test) > nrow(test_df)) stop("rows_test > nrow(test_df)")
  if (max(cols_test) > length(keep_words)) stop("cols_test > length(keep_words)")
  
  dtm_test <- sparseMatrix(i = rows_test, j = cols_test, x = vals_test,
                           dims = c(nrow(test_df), length(keep_words)),
                           dimnames = list(NULL, make.names(keep_words)))
  X_test <- as.matrix(dtm_test)
  test_data <- as.data.frame(X_test)
  names(test_data) <- make.names(names(test_data))
  
  missing_cols <- setdiff(colnames(predictors_train), colnames(test_data))
  for (col in missing_cols) test_data[[col]] <- 0
  test_data <- test_data[, colnames(predictors_train), drop = FALSE]
  
  predictions <- predict(model, test_data)
  actual <- test_df$type
  confusion <- table(pred = predictions, actual = actual)
  accuracy <- sum(diag(confusion)) / sum(confusion)
  
  cat("\nОценка на тестовой выборке\n")
  cat(sprintf("Точность: %.1f%%\n", accuracy * 100))
  print(confusion)
  
  class_report <- data.frame()
  for (cls in sort(unique(actual))) {
    tp <- confusion[cls, cls]
    fp <- sum(confusion[, cls]) - tp
    fn <- sum(confusion[cls, ]) - tp
    precision <- ifelse(tp+fp==0, NA, tp/(tp+fp))
    recall <- ifelse(tp+fn==0, NA, tp/(tp+fn))
    f1 <- ifelse(is.na(precision)|is.na(recall), NA, 2*precision*recall/(precision+recall))
    class_report <- rbind(class_report, data.frame(
      class = cls, precision = round(precision,3), recall = round(recall,3),
      f1 = round(f1,3), support = sum(actual==cls)
    ))
  }
  print(class_report)
  return(list(accuracy = accuracy, confusion = confusion, class_report = class_report))
}

# ---- Классификация в БД ----
classify_commits_in_db <- function(conn, model_obj, table_name = "git_commit_history", 
                                   author_name = NULL, batch_size = 1000) {
  where_clause <- if (!is.null(author_name)) {
    sprintf("WHERE author_name = '%s'", gsub("'", "''", author_name))
  } else ""
  query <- sprintf("SELECT commit, message FROM %s %s", table_name, where_clause)
  commits <- DBI::dbGetQuery(conn, query)
  commits <- commits[!is.na(commits$message) & nchar(trimws(commits$message)) > 0, ]
  if (nrow(commits) == 0) return(invisible(FALSE))
  message("Классификация ", nrow(commits), " коммитов...")
  predictions <- character(nrow(commits))
  for (start in seq(1, nrow(commits), by = batch_size)) {
    end <- min(start+batch_size-1, nrow(commits))
    batch_messages <- commits$message[start:end]
    batch_features <- model_obj$transform(batch_messages)
    batch_pred <- predict(model_obj$model, batch_features)
    predictions[start:end] <- as.character(batch_pred)
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

# ---- Профиль с читаемыми названиями ----
get_commit_type_profile_ml <- function(conn, author_name, labels = commit_type_labels) {
  query <- sprintf("
    SELECT predicted_commit_type, COUNT(*) as n
    FROM git_commit_history
    WHERE author_name = '%s' AND predicted_commit_type IS NOT NULL
    GROUP BY predicted_commit_type
  ", gsub("'", "''", author_name))
  df <- DBI::dbGetQuery(conn, query)
  if (nrow(df) == 0) return(data.frame())
  df$percentage <- round(100 * df$n / sum(df$n), 1)
  df <- df[order(-df$n), ]
  df$type_name <- ifelse(df$predicted_commit_type %in% names(labels), labels[df$predicted_commit_type], df$predicted_commit_type)
  return(df[, c("type_name", "n", "percentage")])
}