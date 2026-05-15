#' Запустить Shiny-дашборд GitProfiler
#' @export
run_app <- function() {
  app_dir <- system.file("app", package = "gitTest")
  if (app_dir == "") {
    stop("Не удалось найти папку приложения. Переустановите пакет.")
  }
  shiny::runApp(app_dir, launch.browser = TRUE)
}