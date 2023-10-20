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
  "fs",
  "MMWRweek",
  "jsonlite",
  "httr",
  "treemap",
  "leaflet",
  "devtools",
  "plotly",
  "eulerr",
  "reactable",
  "readxl",
  "DT",
  "ggraph",
  "igraph",
  "plyr",
  "widyr",
  "tidytext",
  "splitstackshape",
  "xtable",
  "janitor",
  "lubridate",
  "dplyr",
  "readr",
  "jpeg",
  "tidyr",
  "stringr"
)

p_load(packages_cran, update = FALSE, character.only = TRUE)

#### Packages not on CRAN -------------------
if ("Rnssp" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github("cdcgov/Rnssp")
  library(Rnssp)
} else {
  library(Rnssp)
}

if ("d3treeR" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github("timelyportfolio/d3treeR")
  library(d3treeR)
} else {
  library(d3treeR)
}