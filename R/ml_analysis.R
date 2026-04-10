#' Кластеризация разработчиков (K-means)
cluster_developers <- function(conn, n_clusters = NULL) {
  library(cluster)
  
  # Подготовка данных
  ml_data <- prepare_ml_data(conn)
  if (!is.null(ml_data$error)) {
    cat(ml_data$error, "\n")
    return(ml_data)
  }
  
  n_devs <- nrow(ml_data$data)
  
  # Автоматически определяем количество кластеров
  if (is.null(n_clusters)) {
    if (n_devs >= 5) {
      n_clusters <- 3
    } else if (n_devs >= 3) {
      n_clusters <- 2
    } else {
      n_clusters <- 1
    }
  }
  
  # Проверка: кластеров не может быть больше чем разработчиков
  if (n_clusters > n_devs) {
    n_clusters <- max(1, n_devs - 1)
    cat(sprintf("Уменьшено количество кластеров до %d (разработчиков: %d)\n", n_clusters, n_devs))
  }
  
  # Если разработчиков меньше 2, кластеризация невозможна
  if (n_devs < 2) {
    cat("Недостаточно разработчиков для кластеризации (нужно минимум 2)\n")
    return(list(error = "Недостаточно данных", data = ml_data$data))
  }
  
  df_scaled <- ml_data$scaled[, ml_data$features, drop = FALSE]
  
  # K-means кластеризация
  set.seed(123)
  kmeans_result <- kmeans(df_scaled, centers = n_clusters, nstart = 25)
  
  # Добавление кластеров к исходным данным
  ml_data$data$cluster <- kmeans_result$cluster
  
  # Характеристики кластеров
  cluster_profiles <- aggregate(ml_data$data[, ml_data$features], 
                                by = list(cluster = ml_data$data$cluster), 
                                FUN = mean)
  
  # Назначение типов кластеров (только если есть больше 1 кластера)
  if (n_clusters > 1) {
    median_commits <- median(ml_data$data$total_commits, na.rm = TRUE)
    median_extensions <- median(ml_data$data$unique_extensions, na.rm = TRUE)
    
    cluster_names <- c()
    for (i in 1:n_clusters) {
      profile <- cluster_profiles[i, ]
      if (profile$total_commits > median_commits) {
        if (profile$avg_commit_hour < 8 || profile$avg_commit_hour > 22) {
          cluster_names <- c(cluster_names, "Ночной трудоголик")
        } else if (profile$weekend_commits > 5) {
          cluster_names <- c(cluster_names, "Работает в выходные")
        } else {
          cluster_names <- c(cluster_names, "Активный разработчик")
        }
      } else {
        if (profile$unique_extensions > median_extensions) {
          cluster_names <- c(cluster_names, "Многотехнологичный")
        } else if (profile$night_commits > 3) {
          cluster_names <- c(cluster_names, "Совенок")
        } else {
          cluster_names <- c(cluster_names, "Специалист")
        }
      }
    }
    ml_data$data$cluster_type <- cluster_names[kmeans_result$cluster]
    cluster_profiles$cluster_type <- cluster_names
  } else {
    # Все в одном кластере
    ml_data$data$cluster_type <- "Единый профиль"
    cluster_profiles$cluster_type <- "Единый профиль"
  }
  
  # Статистика по кластерам
  cluster_stats <- aggregate(author_email ~ cluster_type, 
                             data = ml_data$data, 
                             FUN = length)
  names(cluster_stats) <- c("cluster_type", "developers_count")
  
  # Вывод результатов
  cat("\n=== РЕЗУЛЬТАТЫ КЛАСТЕРИЗАЦИИ ===\n")
  cat(sprintf("Всего разработчиков: %d\n", n_devs))
  cat(sprintf("Количество кластеров: %d\n", n_clusters))
  print(cluster_stats)
  
  if (n_clusters > 1) {
    cat("\n=== РАСПРЕДЕЛЕНИЕ ПО КЛАСТЕРАМ ===\n")
    print(table(ml_data$data$cluster_type))
    
    cat("\n=== ДЕТАЛИ ПО РАЗРАБОТЧИКАМ ===\n")
    print(ml_data$data[, c("author_name", "cluster_type", "total_commits")])
  }
  
  return(list(
    clustering = if(n_clusters > 1) kmeans_result else NULL,
    data = ml_data$data,
    cluster_profiles = cluster_profiles,
    cluster_stats = cluster_stats,
    features = ml_data$features
  ))
}