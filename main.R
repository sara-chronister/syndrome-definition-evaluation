# =======================================================================
# Title: main.R

# Description: 
# Runs the syndrome evaluation toolkit.

# =======================================================================

# Set Up -----

## Install/Load Packages & Custom Functions
files.source <- list.files("Scripts\\SupportCode", pattern = "\\.R")
sapply(paste0("Scripts\\SupportCode\\",files.source),source)

## Read in all DefinitionInformationTable excel sheets into a single, named list.
DefinitionInformation <- multiplesheets(fname = "DefinitionInformationTable.xlsx")

## Define Number of Queries Being Evaluated
n_queries_eval <- 2

# Source Scripts -----

## 1) Parameter Script (No need to edit, pulls information from DefinitionInformation.xlsx)
source(here::here("Scripts", "1_parameters.R"))

## 2) Data Pull & Cleaning Script
if(!file_exists(paste0(params$output_folder,"Data.RData"))){
  source(here::here("Scripts", "2_pull_data.R"))
  
}else{load(paste0(params$output_folder,"Data.RData"))}

## 3) Render Template