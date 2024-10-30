## Load ESSENCE Credentials
tryCatch({load("~/myProfile.rda")}, # Load myProfile credentials
         
         # If myProfile credentials are not available at the home directory, regenerate them! 
         error = function(e) {Rnssp::create_user_profile_gui()}) # Note: Select .rda option! 


## Manual instructions below if needed
## https://cdn.ymaws.com/www.cste.org/resource/resmgr/docs/RStudio_ESSENCE_API_Guide_J.html#RStudio_-_Securely_Saving_AMC_Credentials


## Highlight the lines between the dashes below (*not* including the dashes) and un-comment using the shortcut ctrl+shift+c
## While those lines are still highlighted, run them (*do not make any changes to the code*) using the shortcut ctrl+enter
## remember to re-comment the lines after you have entered your updated password and saved it (using the same shortcut ctrl+shift+c)

#---
# if ("devtools" %in% rownames(installed.packages()) == FALSE) {
#   install.packages("devtools")
# }
# if ("Rnssp" %in% rownames(installed.packages()) == FALSE) {
#   devtools::install_github("cdcgov/Rnssp")
# }
# library(Rnssp)
# 
# myProfile <- Credentials$new(
#   username = askme("Enter your username: "),
#   password = askme()
# )
# 
# save(myProfile, file = "~/myProfile.rda")
#---

# load("~/myProfile.rda")
  
# IF your organization uses proxy settings, enter the information in the lines below and uncomment lines 33-36 (leave uncommented)
# look for proxy settings (windows) under settings > network and internet > proxy and look for the "Address" for url and "Port" for port
# the username and password are your network username and password, which you set in the key_set function above where service = "local"
# httr::set_config(use_proxy(url = "http://proxy.maricopa.gov", # enter in the quotes
#                            port = 8080, # replace 8080 with your port number
#                            username = key_list("local")[1,2], 
#                            password = key_get("local", key_list("local")[1,2])))
