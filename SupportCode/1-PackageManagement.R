#### Install and load packages --------------------------------------

#### Setup for install and loading via pacman -------------------
pacman_installed <- "pacman" %in% rownames(installed.packages())
if (pacman_installed == FALSE) {
  install.packages("pacman")
}
library(pacman)

#### CRAN packages -----------------
pacman::p_load(
  plyr, 
  devtools, 
  dplyr, 
  DT, 
  eulerr, 
  flexdashboard, 
  fs, 
  ggraph, 
  httr, 
  igraph, 
  irr, 
  janitor, 
  jpeg, 
  jsonlite, 
  leaflet, 
  lubridate, 
  magrittr, 
  MMWRweek, 
  plotly, 
  RCurl, 
  reactable, 
  readr, 
  readxl, 
  rmarkdown, 
  splitstackshape, 
  stringr, 
  tidyr, 
  tidytext, 
  treemap, 
  widyr, 
  writexl, 
  xtable,
  update = FALSE)

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