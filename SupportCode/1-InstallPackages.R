# checks that required packages are installed, installs them if not, loads all required packages

packages <- c("magrittr",
              "RCurl",
              "rmarkdown",
              "flexdashboard",
              "MMWRweek",
              "jsonlite",
              "httr",
              "treemap",
              "leaflet",
              "devtools",
              "haven",
              "plotly",
              "rpivotTable",
              "eulerr",
              "reactable",
              "DT",
              "keyring",
              "ggraph",
              "igraph",
              "plyr",
              "widyr",
              "tidytext",
              "splitstackshape",
              "xtable",
              "openxlsx",
              "janitor",
              "lubridate",
              "dplyr",
              "readr",
              "jpeg",
              "d3treeR",
              "Rnssp")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# packages not on CRAN
if ("Rnssp" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github("cdcgov/Rnssp")
}

if ("d3treeR" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github("timelyportfolio/d3treeR")
}

# Packages loading
# invisible(sapply(packages, library, character.only = TRUE))
