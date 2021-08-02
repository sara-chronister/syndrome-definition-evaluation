library(keyring)

# leave the key_set lines commented out unless this is the first time running it
# if this is the first time running this script, uncomment the following lines, replace the username in the username = "" with your username for the respective services (local = computer, essence = ESSENCE), run the lines, enter your passwords for the respective services, and recomment the lines
# key_set(service = "local", username = "")
# key_set(service = "essence", username = "")

# IF your organization uses proxy settings, enter the information in the lines below and uncomment lines 9-12 (leave uncommented)
# look for proxy settings (windows) under settings > network and internet > proxy and look for the "Address" for url and "Port" for port
# the username and password are your network username and password, which you set in the key_set function above where service = "local"
# httr::set_config(use_proxy(url = "", # enter in the quotes
#                            port = 0000, # replace 0000 with your port number
#                            username = key_list("local")[1,2], 
#                            password = key_get("local", key_list("local")[1,2])))
