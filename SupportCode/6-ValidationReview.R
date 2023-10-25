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
    def_review_folder_name <- paste0("Def1 (",def1_short,")")
    
  }else if(definition == "def2"){
    def_review_folder_name <- paste0("Def2 (",def2_short,")")
    
  }else if(definition == "def3"){
    def_review_folder_name <- paste0("Def3 (",def3_short,")")
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
    

    }else if(sample_metric == "n"){
    
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

## Check Appropriate Evaluation Template

check_eval_template <- function(eval_template, review_defs){
  
  n_review_defs <- length(review_defs) # Original vector of review definitions
  
  if(eval_template == "OneDef"){
    
    review_defs <- review_defs[!review_defs %in% c("def2", "def3")]
    
  }else if(eval_template == "TwoDef"){
    
    review_defs <- review_defs[review_defs != "def3"]
  }
  
  n_review_defs_clean <- length(review_defs) # Vector of review definitions after exclusions
  
  # Check to see if function filtered out any review definitions (requiring a message)
  if(n_review_defs_clean < n_review_defs){
    
    message(paste0("You have requested manual review for more definitions than this Evaluation template supports. The number of definitions for manual review have been subset to: ", paste0(review_defs, collapse = ", "),". ", "If you would like to manually review all the definitions specified, please consider using an appropriate Evaluation template."))
  }
  
  # Return review definitions
  return(review_defs)
}
