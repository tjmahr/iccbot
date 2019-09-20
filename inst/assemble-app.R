library(tidyverse)

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

# Insert the notes into the main app so that only file is needed to run app
document <- readr::read_lines(here::here("inst/app/app-skeleton.Rmd"))

document[which(document == "__notes_here__")] <-
  readr::read_file(here::here("inst/app/icc-notes.md"))

document %>%
  readr::write_lines(
    here::here("inst/app/app.Rmd")
  )
