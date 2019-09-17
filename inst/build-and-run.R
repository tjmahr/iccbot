library(tidyverse)

# # Code for the example in icc-notes.Rmd
# t1 <- c(60, 68, 78, 80, 85)
# t2 <- c(80, 88, 98, 100, 100)
# t1a <- c(60, 68, 78, 80, 85) - mean(t1)
# t2a <- c(80, 88, 98, 100, 100) - mean(t2)
# demo1 <- matrix(c(t1, t2), ncol = 2)
# demo1a <- matrix(c(t1a, t2a), ncol = 2)
# i1 <- irr::icc(demo1, model = "twoway",  type = "consistency")
# i2 <- irr::icc(demo1, model = "twoway", type = "agreement")
# i1a <- irr::icc(demo1a, model = "twoway",  type = "consistency")
# i2a <- irr::icc(demo1a, model = "twoway", type = "agreement")
# i1 <- add_formatted_results_to_icc(i1, icc_digits = 2)
# i2 <- add_formatted_results_to_icc(i2, icc_digits = 2)

# Build the ICC Notes page separately so that the references there are
# plugged in and don't interfere with the references on the main app page
rmarkdown::pandoc_convert(
  here::here("inst/app/icc-notes.Rmd"),
  to = "gfm-tex_math_single_backslash-tex_math_double_backslash+tex_math_dollars",
  from = "markdown+autolink_bare_uris+citations+header_attributes-tex_math_single_backslash+tex_math_dollars",
  output = here::here("inst/app/icc-notes.md"),
  citeproc = TRUE
)

readr::read_lines(here::here("inst/app/icc-notes.md")) %>%
  # Change the references id so that the main app page's references don't show
  # up in ICC notes
  stringr::str_replace("id=\"refs\"", "id=\"refs2\"") %>%
  # Unescape the parenthetical brackets to that they don't turn into math
  stringr::str_replace_all("\\\\\\[", "[") %>%
  stringr::str_replace_all("\\\\\\]", "]") %>%
  # Fix some escaping so the actual math that is used does show up
  stringr::str_replace_all("[\\\\]{2}", "\\\\") %>%
  readr::write_lines(here::here("inst/app/icc-notes.md"))

rmarkdown::run(here::here("inst/app/app.Rmd"))
