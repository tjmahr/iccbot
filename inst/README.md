
Overview 

  - `app/app-skeleton.Rmd` - main part of the app. work on this file to change
    app.

  - `app/icc-notes.Rmd` - the notes tab
  - `app/icc-notes.md` - notes tab with citations and references filled in
  - `app/refs.bib` and `app/apa.csl` - files to generate citations

  - `app/app.Rmd` a file created by combining `app-skeleton.Rmd` and
    `icc-notes.md`

`assemble-app.R` creates the `icc-notes.md` file and inserts it into
`app-skeleton.Rmd`, saves the output to `app.Rmd`.
