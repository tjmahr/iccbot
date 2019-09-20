#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
run_app <- function(...) {
  app_path <- system.file("app/app.Rmd", package = "iccbot")
  app_dir <- system.file("app", package = "iccbot")
  rmarkdown::run(
    app_path,
    default_file = "app.Rmd",
    shiny_args = list(launch.browser = TRUE)
  )
}
