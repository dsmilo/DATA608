# Indicators of Changing Neighborhoods

This folder contains the data and code used to prepare the [Shiny app](https://dsmilo.shinyapps.io/gentrification/) deployed for the DATA 608 final project.  This folder contains three items:
  - `gentrification`: prepared data, code, and markdown files used for app
  - `dataprep.R`: documentation of data prepration to create final data used in app
  - **data:** contains data files used and file metadata

To replicate this analysis, note that two things are required:
  - a [developer key](http://api.census.gov/data/key_signup.html) for the US Census API
  - the development version of the `plotly` package:
  `devtools::install_github(“ropensci/plotly”)`
