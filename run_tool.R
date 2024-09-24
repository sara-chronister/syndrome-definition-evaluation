# =======================================================================
# Title: run_tool.R

# Description: 
# Runs the syndrome evaluation toolkit.

# Instructions: 
# Update line 19 (n_defs_eval) to the number of definitions you are evaluating (max 3)
# Only change the name of the "DefinitionInformationTable.xlsx" file if you saved it under a different name
# Switch timed to TRUE (or T) if you would like to see how long the process took
# See troubleshooting steps below if you experience any errors

# =======================================================================

## Install/Load Packages, myProfile Credentials, & Custom Functions
files.source <- list.files("Scripts/SupportCode", pattern = "\\.R")
sapply(paste0("Scripts/SupportCode/",files.source),source)

run_evaluation(n_defs_eval = 2, # check before running - MAX 3
               def_info_table = "DefinitionInformationTable.xlsx",
               timed = FALSE)

# =======================================================================

# Troubleshooting:

# If you are having an error with myProfile or need to update myProfile to your new password:
  # Option 1: Uncomment the following line, which will open a popup for you to enter your information (remember to check the box to "Save Profile to Home Directory" and use the ".rda" option) - Requires Rnssp package, skip to Option 2 if Rnssp is not installed
    # Rnssp:::create_user_profile_gui()
  # Option 2: go to "Scripts/SupportCode/2-UserCredentials.R" and follow the instructions beginning on Line 12

