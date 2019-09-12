rmarkdown::pandoc_convert(
  "test.Rmd",
  to = "gfm-tex_math_single_backslash",
  from = "markdown+autolink_bare_uris+citations+header_attributes-tex_math_single_backslash",
  output = "test.md",
  citeproc = TRUE
)

readr::read_lines("test.md") %>%
  stringr::str_replace("id=\"refs\"", "id=\"refs2\"") %>%
  readr::write_lines("test.md")
