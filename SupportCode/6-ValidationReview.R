#### Functions to Assist Validation Review ------------

## Add Date Components

add_date_components <- function(df, date_variable = "Date"){
  
  df_date <- df %>%
    mutate(Weekday = lubridate::wday(get(date_variable), week_start = 1, label = TRUE, abbr = FALSE), # Break date into Weekday (Word), Week, Month, and Year
           Week = lubridate::epiweek(get(date_variable)),
           Month = lubridate::month(get(date_variable)),
           Year = lubridate::year(get(date_variable)))
  
  return(df_date)
}

## Filter NA & Pull

pull_no_na <- function(df, variable){
  
  df_pulled <- df %>%
    filter(!is.na(get(variable))) %>% # Remove All NA rows for the variable (likely unfilled EXCEL rows)
    pull(get(variable)) # Get the value of the variable
  
  return(df_pulled)
}

## Create Definition Review Folder Name

name_def_review_folder <- function(definition){
  
  if(definition == "def1"){
    def_review_folder_name <- paste0("Def1 (",def1_name,")")
    
  }else if(definition == "def2"){
    def_review_folder_name <- paste0("Def2 (",def2_name,")")
    
  }else if(definition == "def3"){
    def_review_folder_name <- paste0("Def3 (",def3_name,")")
  }
  
  return(def_review_folder_name)
}

## Get Sample (RS or SRS)

get_sample <- function(df, sample_metric, sample_value, 
                       strat_sample = FALSE, strat_vars = NULL){
  
  set.seed(1234) # Setting Seed to promote reproducibility of random/stratified-random sampling
  
  if(sample_metric == "proportion"){
    
    if(strat_sample == TRUE){ # STRATIFIED RANDOM SAMPLING BY PROPORTION
      
      sample <- df %>% slice_sample(.data = ., prop = sample_value, by = strat_vars)
      
    }else if(strat_sample == FALSE){ # RANDOM SAMPLING BY PROPORTION
      
      sample <- df %>% slice_sample(.data = ., prop = sample_value)
    }
    

    }else if(sample_metric == "number"){
    
    if(strat_sample == TRUE){ # STRATIFIED RANDOM SAMPLING BY COUNTS
      
      sample <- df %>% slice_sample(.data=., n = sample_value, by = strat_vars)
      
    }else if(strat_sample == FALSE){ # RANDOM SAMPLING BY COUNTS
      
      sample <- df %>% slice_sample(.data = ., n = sample_value)
    }
  }
  
  # Arrange Records & Remove Unnecessary Variables
  sample <- sample %>% 
    arrange(Date) %>%
    select(-starts_with("def"))
  
  # Return sample
  return(sample)
}

