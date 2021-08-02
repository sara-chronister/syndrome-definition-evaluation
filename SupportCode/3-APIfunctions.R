

get_csv <- function(url) {
  api_response <- GET(url, authenticate(key_list("essence")[1,2],
                                        key_get("essence",key_list("essence")[1,2])))
  data_details <- content(api_response, type="text/csv")
  rm(api_response)
  return(data_details)
}


set_dates <- function(url, start = (Sys.Date()-7), end = Sys.Date()) {
  
  # pull out the end date from the url
  old_end <- regmatches(url, regexpr('endDate=.+?&', url)) %>% str_replace(.,"endDate=","") %>% str_replace(.,"&","")
  # format the new end date you specify when calling the function
  new_end <- format(as.Date(end), "%e%b%Y")
  # replace the old end date with the new end date
  url <- gsub("[[:space:]]", "", str_replace(url, old_end, new_end))
  
  # pull out the start date from the url
  old_start <- regmatches(url, regexpr('startDate=.+?&', url)) %>% str_replace(.,"startDate=","") %>% str_replace(.,"&","")
  # format the new start date you specify when calling the function
  new_start <- str_trim(format(as.Date(start), "%e%b%Y"))
  # replace the old start date with the new start date
  url <- gsub("[[:space:]]", "", str_replace(url, old_start, new_start))
  
  return(url)
}


return_longterm_query <- function(url, loop_start, loop_end, by = 14) {
  
  #### Setup for loop ####
  # function to create a data frame with the start and end dates you are trying to use 
  # default is 14 day intervals, but you can specify any number of days in the by argument when you call the function 
  loop_start <- as.Date(loop_start)
  loop_end <- as.Date(loop_end)
  
  loop_dates <- data.frame(start = seq.Date(from = loop_start, to = loop_end, by = by)) %>%
    mutate(end = start+(by-1)) %>%
    mutate(end = ifelse((loop_end>start&end>loop_end),loop_end,end)) %>%
    mutate(end = as.Date.numeric(end, origin = "1970-01-01"))
  
  # initiate a blank list to store the output from the loop
  output_list <- list()
  
  #### Loop over multiple time frames ####
  # loop to change the dates, get csv from url, and store output in a list
  for (i in 1:nrow(loop_dates)) {
    
    # update the to the i set of start and end dates in the url
    url_update <- set_dates(url, start = loop_dates$start[i], end = loop_dates$end[i])
    # generate the output from the i set of start and end dates
    new_output <- get_csv(url = url_update) %>%
      mutate_all(as.character)
    # store the results from the i set of start and end dates as the i element in a list
    output_list[[i]] <- new_output
    
  }
  
  #### Result ####
  # collapse results of the list generated in the output above into a data frame
  all_data <- plyr::ldply(output_list, bind_rows)
  
  return(all_data)
  
}


get_ts_json <- function(url) { 
  api_response <- GET(url, authenticate(key_list("essence")[1,2],
                                        key_get("essence",key_list("essence")[1,2]))) 
  if (api_response$status_code==401) {
    warning("API Status Code 401: ESSENCE password has expired. Please go to amc.syndromicsurveillance.org to reset.")
  } else {
    if (api_response$status_code==404) {
      warning("API Status Code 404: Check the ESSENCE API for errors. If necessary, re-copy the API from ESSENCE to ensure it is formatted correctly.")
    } else {
      api_response_json <- content(api_response, as = "text")
      api_data <- fromJSON(api_response_json)
      timeseries_data <- api_data$timeSeriesData 
      timeseries_data <- timeseries_data %>% select(-altText, -details) %>%
        mutate(date = as.Date(date))
      rm(api_response)
      return(timeseries_data)
    }
  }
}
