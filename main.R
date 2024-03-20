# =======================================================================
# Title: main.R

# Description: 
# Runs the syndrome evaluation toolkit.

# =======================================================================

# Change -----

## Define Number of Queries Being Evaluated (def1, def2, def3)
n_queries_eval <- 3

# Set Up -----

## Load ESSENCE Credentials
myProfile <- load("~/myProfile.rda")

## Install/Load Packages & Custom Functions
files.source <- list.files("Scripts\\SupportCode", pattern = "\\.R")
sapply(paste0("Scripts\\SupportCode\\",files.source),source)

## Read in all DefinitionInformationTable excel sheets into a single, named list.
DefinitionInformation <- multiplesheets(fname = "DefinitionInformationTable.xlsx")

# Source Scripts -----

## 1) Parameter Script (No need to edit, pulls information from DefinitionInformation.xlsx)
source(here::here("Scripts", "1_parameters.R"))

## 2) Data Pull & Cleaning Script
source(here::here("Scripts", "2_pull_data.R"))
  
## 3) Validation Review
source(here::here("Scripts", "3_validation_review.R"))
