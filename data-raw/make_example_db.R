## code to prepare example database from real repository

library(gitTest)

example_repo_url <- "https://github.com/aaalyaaa/dbipAnalyzer.git"

temp_clone_dir <- tempdir()

result <- run_etl_pipeline(
  mode = 1,
  repo_url = example_repo_url,
  clone_dir = temp_clone_dir
)

if (result$status == "success") {
  dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
  file.copy("git.duckdb", "inst/extdata/git_example.duckdb", overwrite = TRUE)
  file.remove("git.duckdb")

  message("Example database created at inst/extdata/git_example.duckdb")
  message("Repository: ", example_repo_url)
  message("Commits loaded: ", result$message)
} else {
  message("❌ Failed to create example database: ", result$message)
}
