library(tidyverse)

rmarkdown::pandoc_convert(
  "icc-notes.Rmd",
  to = "gfm-tex_math_single_backslash-tex_math_double_backslash+tex_math_dollars",
  from = "markdown+autolink_bare_uris+citations+header_attributes-tex_math_single_backslash+tex_math_dollars",
  output = "icc-notes.md",
  citeproc = TRUE
)

readr::read_lines("icc-notes.md") %>%
  stringr::str_replace("id=\"refs\"", "id=\"refs2\"") %>%
  stringr::str_replace_all("\\\\\\[", "[") %>%
  stringr::str_replace_all("\\\\\\]", "]") %>%
  stringr::str_replace_all("[\\\\]{2}", "\\\\") %>%
  readr::write_lines("icc-notes.md")
