# checks that required packages are installed, installs them if not, loads all required packages

packages <- c("tidyverse","MMWRweek","jsonlite","httr","plotly","eulerr","reactable","keyring","ggraph","igraph","plyr","widyr","tidytext","splitstackshape","readxl","lubridate","dplyr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
