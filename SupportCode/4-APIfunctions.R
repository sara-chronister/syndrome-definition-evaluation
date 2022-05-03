
#### time series data table ####
## adapted from https://cdn.ymaws.com/www.cste.org/resource/resmgr/docs/RStudio_ESSENCE_API_Guide_J.html#Time_Series_Data_Table

get_ts_data <- function(url, start_date = NULL, end_date = NULL) {
  
  if (str_detect(url, "timeSeries\\?", negate = TRUE)) {
    
    writeLines('Error: Incorrect API URL for get_ts_graph function. Copy/paste the "Data" URL from the Time Series result page.')
    
  } else {
    
    url_new <- change_dates(url, start_date, end_date)
    
    api_response <- myProfile$get_api_response(url_new)
    api_response_json <- content(api_response, as = "text")
    api_data <- fromJSON(api_response_json) %>%
      extract2("timeSeriesData")
    
    return(api_data)
    
  }
  
}


#### time series graph ####
## https://cdn.ymaws.com/www.cste.org/resource/resmgr/docs/RStudio_ESSENCE_API_Guide_J.html#Time_Series_Graph_from_ESSENCE

get_ts_graph <- function(url, start_date = NULL, end_date = NULL) {
  
  if(str_detect(url, "timeSeries/graph", negate = TRUE)) {
    writeLines('Error: Incorrect API URL for get_ts_graph function. Copy/paste the "Graph (PNG)" URL from the Time Series result page.')
  } else {
    url_new <- change_dates(url, start_date, end_date)
    api_png <- myProfile$get_api_tsgraph(url_new)
    knitr::include_graphics(api_png$tsgraph)
  }
    
}


#### table builder results ####
## https://cdn.ymaws.com/www.cste.org/resource/resmgr/docs/RStudio_ESSENCE_API_Guide_J.html#Table_Builder_Results

get_table <- function(url, start_date = NULL, end_date = NULL) {
  
  url_new <- change_dates(url, start_date, end_date)
  
  if(str_detect(url,"csv\\?")) {
    api_data <- myProfile$get_api_data(url_new, fromCSV = TRUE)
  } else {
    api_data <- myProfile$get_api_data(url_new)
  }
  
}

#### data details (line level) ####
## https://cdn.ymaws.com/www.cste.org/resource/resmgr/docs/RStudio_ESSENCE_API_Guide_J.html#Data_Details_(line_level)

get_details <- function(url, start_date = NULL, end_date = NULL) {
  
  if (str_detect(url, "dataDetails")) {
    url_new <- change_dates(url, start_date, end_date)
    if (str_detect(url_new, "csv\\?")) {
      api_data <- myProfile$get_api_data(url_new, fromCSV = TRUE)
    } else {
      api_data <- myProfile$get_api_data(url_new) %>%
        extract2("dataDetails")
    }
  } else {
    writeLines("Error: Mismatched function and API. Either use the API from the Data Details result page or select the appropriate function for your API.")
  }
  
}

#### data details (line level long term) ####

get_longterm_details <- function(url, start_date = NULL, end_date = NULL, by = 14) {
  
  ## Setup for loop ##
  # function to create a data frame with the start and end dates you are trying to use 
  # default is 14 day intervals, but you can specify any number of days in the by argument when you call the function 
  loop_start <- as.Date(start_date)
  loop_end <- as.Date(end_date)
  
  loop_dates <- data.frame(start = seq.Date(from = loop_start, to = loop_end, by = by)) %>%
    mutate(end = start+(by-1)) %>%
    mutate(end = ifelse((loop_end>start&end>loop_end),loop_end,end)) %>%
    mutate(end = as.Date.numeric(end, origin = "1970-01-01"))
  
  # initiate a blank list to store the output from the loop
  output_list <- list()
  
  ## Loop over multiple time frames ##
  # loop to change the dates, get csv from url, and store output in a list
  for (i in 1:nrow(loop_dates)) {
    
    # update the to the i set of start and end dates in the url
    url_update <- change_dates(url, start_date = loop_dates$start[i], end_date = loop_dates$end[i])
    # generate the output from the i set of start and end dates
    new_output <- get_details(url = url_update) %>%
      mutate_all(as.character)
    # store the results from the i set of start and end dates as the i element in a list
    output_list[[i]] <- new_output
    
  }
  
  ## Result ##
  # collapse results of the list generated in the output above into a data frame
  all_data <- plyr::ldply(output_list, bind_rows)
  
  return(all_data)
  
}


#### summary stats ####
## https://cdn.ymaws.com/www.cste.org/resource/resmgr/docs/RStudio_ESSENCE_API_Guide_J.html#Summary_Stats

get_summary <- function(url, start_date = NULL, end_Date = NULL) {
  
  if (str_detect(url, "summaryData")) {
    url_new <- change_dates(url, start_date, end_date)
    if (str_detect(url_new, "csv\\?")) {
      api_data <- myProfile$get_api_data(url_new, fromCSV = TRUE)
    } else {
      api_data <- myProfile$get_api_data(url_new) %>%
        extract2("summaryData")
    }
  } else {
    writeLines('Error: Mismatched function and API. Either use the "Graph (PNG)" URL from the Time Series result page or select the appropriate function for your API.')
  }
  
}


#### alert list detection table ####
## https://cdn.ymaws.com/www.cste.org/resource/resmgr/docs/RStudio_ESSENCE_API_Guide_J.html#Alert_List_Detection_Table

get_alert <- function(url, start_date = NULL, end_Date = NULL) {
  
  if (str_detect(url, "alerts")) {
    url_new <- change_dates(url, start_date, end_date)
    if (str_detect(url_new, "regionSyndromeAlerts")) {
      api_data <- myProfile$get_api_data(url_new) %>%
        extract2("regionSyndromeAlerts")
    } else {
      api_data <- myProfile$get_api_data(url_new) %>%
        extract2("hospitalSyndromeAlerts")
    }
  } else {
    writeLines('Error: Mismatched function and API. Either use the "Graph (PNG)" URL from the Time Series result page or select the appropriate function for your API.')
  }
  
}


get_essence_data <- function(url, start_date = NULL, end_date = NULL) {
  
  api_type <- str_extract(url,"(?<=api/).+(?=\\?)")
  
  url_new <- change_dates(url, start_date, end_date)
  
  if (api_type == "timeSeries") {
    api_response <- myProfile$get_api_response(url_new)
    api_response_json <- content(api_response, as = "text")
    api_data <- fromJSON(api_response_json) %>%
      extract2("timeSeriesData")
  } else if (api_type == "timeSeries/graph") {
    api_png <- myProfile$get_api_tsgraph(url_new)
    knitr::include_graphics(api_png$tsgraph)
  } else if (api_type == "tableBuilder") {
    api_data <- myProfile$get_api_data(url_new)
  } else if (api_type == "tableBuilder/csv") {
    api_data <- myProfile$get_api_data(url_new, fromCSV = TRUE)
  } else if (api_type == "dataDetails") {
    api_data <- myProfile$get_api_data(url_new) %>%
      extract2("dataDetails")
  } else if (api_type == "dataDetails/csv") {
    api_data <- myProfile$get_api_data(url_new, fromCSV = TRUE) 
  } else if (api_type == "summaryData") {
    api_data <- myProfile$get_api_data(url_new) %>%
      extract2("summaryData")
  } else if (api_type == "alerts/regionSyndromeAlerts") {
    api_data <- myProfile$get_api_data(url_new) %>%
      extract2("regionSyndromeAlerts")
  } else if (api_type == "alerts/hospitalSyndromeAlerts") {
    api_data <- myProfile$get_api_data(url_new) %>%
      extract2("hospitalSyndromeAlerts")
  } else {
    writeLines("Error: API did not work as expected. Please check your URL, dates, and password before trying again.")
  }
  
}
