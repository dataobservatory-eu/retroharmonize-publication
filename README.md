# retroharmonize-publication

This repo is created to make two publications. Please use the following structure.

## Folders

**root** - The two articles, `.bib` bibliography files, and `yml` files for markdown conversions, plus reproducbile `docx`, `pdf`, `epub` versions. Work in the `Rmd` markdown files. If you do not write R code, just ignore the R code chunks, and use it as a clean markdown text.

**bib** - please save here individual BibTex entries.  The consolidated entries will should be placed in one of the main `.bib` files in the root folder. The pandoc / knitr / RStudio workflow can have hickuups with bib files, so try to save individual files first in `bib/xyz.bib`. The literature review items should be added to the `surveyharmonization.bib` fájl, making sure that at least the title={}, author={} and the year={} fields are not empty, i.e. if there is only a date ={2022-05-06} field, you create a year = {2022} field, too.

**not_included** - user's scrap directory, excluded by `.gitignore`.  Please put your non-synchronized scaps and code doodles here.

**data-raw** - data as downloaded, received, as a starting point of our reproducible work. You will find here 5 CAP surveys.

**R** - R code written for the publications.  It is better to write stand-alone R codes, and put final 'chunks' into the `.Rmd` files.

**data** - Final data outputs that will be placed in the articles.

## Article 1. Ex Post Survey Harmonization with retroharmonize
`Retroharmonize_article.Rmd`

## Article 2. Ex Post Harmonization And Data Integration of Cultural Access and Participation Surveys
`harmonized_cap.Rmd`

