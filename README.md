# retroharmonize-publication

This repo is created to make two publications. Please use the following structure.

## Folders

**root** - The two articles, `.bib` bibliography files, and `yml` files for markdown conversions, plus reproducbile `docx`, `pdf`, `epub` versions. Work in the `Rmd` markdown files. If you do not write R code, just ignore the R code chunks, and use it as a clean markdown text.

**not_included** - user's scrap directory, excluded by `.gitignore`.  Please put your non-synchronized scaps and code doodles here.

**data-raw** - data as downloaded, received, as a starting point of our reproducible work. You will find here 5 CAP surveys.

**R** - R code written for the publications.  It is better to write stand-alone R codes, and put final 'chunks' into the `.Rmd` files.

**data** - Final data outputs that will be placed in the articles.

## Article 1. Ex Post Harmonization And Data Integration of Cultural Access and Participation Surveys
`harmonized_cap.Rmd`

## Article 2. Ex Post Survey Harmonization with retroharmonize
`Retroharmonize_article.Rmd`