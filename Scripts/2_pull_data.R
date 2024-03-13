# =======================================================================
# Title: 2_pull_data.R

# Description: 
# Pull, clean, and save data as .RData files within respective output files.

# =======================================================================

# Define Analysis Parameters -----

## Define params list
params <- list()

## Define Parameters

### Number of Queries to Evaluate
params$n_queries_eval <- n_queries_eval

#### Output Folder
params$output_folder <- case_when(
  params$n_queries_eval == 1 ~ "Output/OneDef/",
  params$n_queries_eval == 2 ~ "Output/TwoDefs/",
  params$n_queries_eval == 3 ~ "Output/ThreeDefs/")

fs::dir_create(params$output_folder) # Create Folder

### Dates & Geographies
params$start_date <- DefinitionInformation[["Setup"]]$StartDate %>% as.Date()
params$end_date <- DefinitionInformation[["Setup"]]$EndDate %>% as.Date()
params$jurisdiction <- DefinitionInformation[["Setup"]]$Jurisdiction

### ESSENCE Fields to Pull & Analyze

#### All Fields to Pull in API
params$fields <-  DefinitionInformation[["SelectFields"]] %>%
  mutate(includeAPI = ifelse(IncludeForValidationReview =="Yes"| IncludeForTextAnalysis =="Yes", "Yes", "No")) %>%
  filter(includeAPI == "Yes")

#### All Fields (as API URL)
params$fields_api_url <- params$fields %>%
  pull(ESSENCEfields) %>%
  paste0(., collapse = "&field=") %>%
  paste0("&field=",.)

#### Text Mining
params$fields_text_analysis <- params$fields %>%
  filter(IncludeForTextAnalysis =="Yes" & (is.na(Notes)| str_detect(Notes, "Not eligible", negate=TRUE))) %>% # Some fields are not eligible for text mining (see Notes)
  pull(ESSENCEfields)

#### Validation Review (Line-Level Records)
params$fields_validation_review <- params$fields %>%  
  filter(IncludeForValidationReview =="Yes") %>%
  pull(ESSENCEfields)

### Data Cleaning
params$deduplicate_ddx <- DefinitionInformation[["ValidationReviewInformation"]] %>% pull_no_na(df=., variable = "DeduplicateDDx")



# Pull Data -----
## Description: Create a list for each definition evaluated

for(i in 1:params$n_queries_eval){
  
  ### Create Definition-Specific Storage List
  defX_list <- list()
  
  ## Set Up -----
  
  ### The DefinitionInformation tab of the DefinitionInformation excel file
  defX_list$setup$info <- DefinitionInformation[["DefinitionInformation"]] %>% 
    filter(defX == paste0("def",i))
  
  ### Full Name (column B) and Abbreviation (column C) of the definition 
  defX_list$setup$name <- defX_list$info$Syndrome
  defX_list$setup$short <- defX_list$info$Abbreviation
  
  ### ESSENCE syntax of the definition (column F) & definition elements
  defX_list$setup$structure <- clean_query_essence(defX_list$setup$info$Structure)
  defX_list$setup$structure_print <- defX_list$setup$info$Structure %>%
    str_replace_all(., pattern = "\\^", replacement = "\\\\^") %>%
    str_replace_all(., pattern = "\\_", replacement = "\\\\_") # Prevents R Markdown from interpreting "_" as start/end of bold commands.
  defX_list$setup$elements <- defX_list$setup$structure$`Syndrome Element`  # Individual definition elements
  
  ### Definition ESSENCE API 
  defX_list$setup$url <- defX_list$setup$info$API %>%
    Rnssp::change_dates(url=., start_date = params$start_date, end_date = params$end_date) %>% # Start and end dates from Setup tab.
    paste0(., params$fields_api_url) # ESSENCE fields from SelectFields tab. "Yes" in either IncludeForValidationReview (column B) or IncludeForTextAnalysis (column C).
    
    
  ## Results -----
  
  ### Pull API Data (DataDetails)
  defX_list$results$raw <- get_longterm_details(url = defX_list$setup$url, 
                                                start_date = params$start_date, end_date = params$end_date)
  ### Clean API Data (DataDetails) 
  defX_list$results$clean <- defX_list$results$raw %>%
    distinct(C_BioSense_ID, .keep_all = TRUE) %>% # Remove duplicate records (if applicable)
    mutate(!!paste0("def",i) := 1, # Flag to indicate these results matched this definition
           Date = as.Date(Date, format='%m/%d/%Y')) %>% # Convert Date to Date type variable.
    mutate(across(
      .cols = where(is.character),
      .fns = ~ifelse(. == "none", NA, .))) %>%
    select(Date, C_BioSense_ID, everything()) # Reorder variables

  if(params$deduplicate_ddx == TRUE){defX_list$results$clean <- defX_list$results$clean %>% dedup_dx(df=., keep_raw=FALSE)}
  
  
  ## Analysis -----
  
  ### Total Number of Results
  defX_list$analysis$total <- nrow(defX_list$results$clean)
  defX_list$analysis$total_pretty <- format(defX_list$analysis$total, big.mark = ",", scientific = FALSE) ## Pretty Version
  
  ### Definition Elements detected in free-text field of interest (default = CCDDParsed; update later to apply to fields that were not used in the query)
  defX_list$analysis$elements_detected <- defX_list$results$clean %>%
    select(C_BioSense_ID, all_of(params$fields_validation_review)) %>%
    detect_elements(data = ., defX_list$setup$elements, text_field = "CCDDParsed") %>%
    janitor::adorn_totals(where = "row")
  
  # rename the list to match the number of the definition
  assign(paste0("def",i,"_list"), defX_list)
}


# Compare Data -----







# Save Data -----

## .RData Files
if(params$n_queries_eval == 1){
  save(params, def1_list, file = paste0(params$output_folder,"Data.RData")) 
  
}else if(params$n_queries_eval == 2){
  save(params, def1_list, def2_list, file = paste0(params$output_folder,"Data.RData")) 
  
}else if(params$n_queries_eval == 3){
  save(params, def1_list, def2_list, def3_list, file = paste0(params$output_folder,"Data.RData")) 
}

## .csv Files