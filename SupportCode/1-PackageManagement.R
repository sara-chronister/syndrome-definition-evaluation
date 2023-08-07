#### Install and load packages --------------------------------------

#### Setup for install and loading via pacman -------------------
pacman_installed <- "pacman" %in% rownames(installed.packages())
if (pacman_installed == FALSE) {
  install.packages("pacman")
}
library(pacman)

#### CRAN packages -----------------
packages_cran <- c(
  "magrittr",
  "RCurl",
  "rmarkdown",
  "flexdashboard",
  "MMWRweek",
  "jsonlite",
  "httr",
  "treemap",
  "leaflet",
  "devtools",
  "plotly",
  "eulerr",
  "reactable",
  "DT",
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
  "jpeg"
)

p_load(packages_cran, update = TRUE, character.only = TRUE)

#### Packages not on CRAN -------------------
if ("Rnssp" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github("cdcgov/Rnssp")
  library(Rnssp)
}

if ("d3treeR" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github("timelyportfolio/d3treeR")
  library(d3treeR)
}