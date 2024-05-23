#### Install and load packages --------------------------------------

#### Setup for install and loading via pacman -------------------
if ("renv" %in% rownames(installed.packages()) == FALSE) {install.packages("renv")}
if ("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")}

#### Load renv packages --------------------------------------
# renv::restore()

#### Load packages -----------------
pacman::p_load(
  dplyr,
  DT,
  eulerr,
  forcats,
  fs,
  gtsummary,
  janitor,
  lubridate,
  magrittr,
  plotly,
  readr,
  readxl,
  rmarkdown,
  Rnssp,
  splitstackshape,
  stringr,
  tictoc,
  tidyr,
  tidytext,
  writexl,
  xtable,
  zoo,
  update = FALSE)

#### Update renv (if new packages utilizes) -----------------
# renv::snapshot() # UNCOMMENT THIS CODE & RUN IF THE WORKFLOW USES NEW PACKAGES!