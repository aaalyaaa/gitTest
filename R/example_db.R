#' Connect to the built-in example database
#'
#' This function provides access to a example database
#' that comes with the package. It contains real data from the
#' dbipAnalyzer repository for demonstration purposes.
#'
#' @return A DBI connection to the DuckDB database (read-only)
#' @export
#'
#' @examples
#' \dontrun{
#' con <- example_db()
#' DBI::dbListTables(con)
#' DBI::dbGetQuery(con, "SELECT author_name, COUNT(*) FROM git_commit_history GROUP BY author_name")
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' }
example_db <- function() {
  db_path <- system.file("extdata", "git_example.duckdb", package = "gitTest")

  if (db_path == "") {
    stop("Example database not found. Please reinstall the package.", call. = FALSE)
  }

  DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
}
