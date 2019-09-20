#' Run the Shiny Application
#'
#' @export
run_app <- function(...) {
  app_path <- system.file("app/app.Rmd", package = "iccbot")
  rmarkdown::run(
    app_path,
    shiny_args = list(launch.browser = TRUE)
  )
}
