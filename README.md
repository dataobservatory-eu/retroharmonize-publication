# retroharmonize-publication

This repo is created to make two publications. Please use the following structure.

## Folders

**root** - The two articles, `.bib` bibliography files, and `yml` files for markdown conversions, plus reproducbile `docx`, `pdf`, `epub` versions. Work in the `Rmd` markdown files. If you do not write R code, just ignore the R code chunks, and use it as a clean markdown text.

**bib** - please save here individual BibTex entries.  The consolidated entries will should be placed in one of the main `.bib` files in the root folder. The pandoc / knitr / RStudio workflow can have hickuups with bib files, so try to save individual files first in `bib/xyz.bib`. The literature review items should be added to the `surveyharmonization.bib` file, making sure that at least the title={}, author={} and the year={} fields are not empty, i.e. if there is only a date ={2022-05-06} field, you create a year = {2022} field, too.

**not_included** - user's scrap directory, excluded by `.gitignore`.  Please put your non-synchronized scaps and code doodles here.

**data-raw** - data as downloaded, received, as a starting point of our reproducible work. You will find here 5 CAP surveys.

**use_cases** - `.Rmd` files for the individual use cases.

**R** - R code written for the publications.  It is better to write stand-alone R codes, and put final 'chunks' into the `.Rmd` files.

**data** - Final data outputs that will be placed in the articles.

## Article 1. Ex Post Survey Harmonization with retroharmonize
`Retroharmonize_article.Rmd`

## Article 2. Ex Post Harmonization And Data Integration of Cultural Access and Participation Surveys
`harmonized_cap.Rmd`

## Contributor Covenant Code of Conduct

You must abide the [CONTRIBUTOR COVENANT CODE OF CONDUCT](https://www.contributor-covenant.org/version/2/1/code_of_conduct/) pledging to make participation in our community a harassment-free experience for everyone, regardless of age, body size, visible or invisible disability, ethnicity, sex characteristics, gender identity and expression, level of experience, education, socio-economic status, nationality, personal appearance, race, caste, color, religion, or sexual identity and orientation. (See [translations](https://www.contributor-covenant.org/translations/) and [FAQ](https://www.contributor-covenant.org/faq/).)