# =======================================================================
# Title: main.R

# Description: 
# Runs the syndrome evaluation toolkit.

# =======================================================================

#  ***USERS: CHANGE n_queries_eval BASED ON NEEDS*** -----

## Define Number of Queries Being Evaluated (def1, def2, def3)
n_queries_eval <- 3

if(n_queries_eval > 3){stop("This tool can only analyze up to 3 syndromes simultaneously. Please change n_queries_eval to a value <= 3.")}


# Set Up -----

## Load ESSENCE Credentials
myProfile <- load("~/myProfile.rda")

## Install/Load Packages & Custom Functions
files.source <- list.files("Scripts\\SupportCode", pattern = "\\.R")
sapply(paste0("Scripts\\SupportCode\\",files.source),source)

## Read in all DefinitionInformationTable excel sheets into a single, named list.
DefinitionInformation <- multiplesheets(fname = "DefinitionInformationTable.xlsx")



# Source Scripts -----
tictoc::tic("Syndrome Evaluation Toolkit") # Start Timing - Entire Toolkit

## 1) Parameter Script (No need to edit, pulls information from DefinitionInformation.xlsx)
source(here::here("Scripts", "1_parameters.R"))

## 2) Data Pull & Cleaning Script
source(here::here("Scripts", "2_process_data.R"))

## 3) Render Evaluation Report
rmarkdown::render(input = "Scripts/Syndrome Evaluation Report Template.Rmd",
                  output_dir = params$filepaths$output,
                  output_file = paste0("Syndrome Evaluation Report.html"),
                  params = params)


tictoc::toc() # End Timing - Entire Toolkit