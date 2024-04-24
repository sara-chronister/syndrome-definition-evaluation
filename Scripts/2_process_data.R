# =======================================================================
# Title: 2_pull_data.R

# Description: 
# Pull, clean, and save data as .RData files within respective 
# output files.

# =======================================================================

# Pull & Clean Data -----

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
  
  ### Detect Elements Fields - Correct Labeling to Align with ESSENCE Variable Names
  defX_list$setup$detect_elements_fields <- str_split(defX_list$setup$info$Fields, pattern = "\\,") %>% 
    unlist() %>% 
    str_remove_all(., pattern = "\\bor\\b|\\band\\b") %>%
    str_trim(., side = "both") %>%
    str_replace_all(., pattern=" ", replacement = "\\_") %>%
    tibble(Fields =.) %>%
    ### Correct Labeling to Align with ESSENCE Variable Names ###
    mutate(Fields = case_when(Fields == "Discharge_Diagnosis" ~ "DischargeDiagnosis",
                              Fields == "Chief_Complaint_History" ~ "ChiefComplaintUpdates",
                              TRUE ~ Fields)) %>%
    pull(Fields)
  
  defX_list$setup$detect_elements_fields_regex <- paste(defX_list$setup$detect_elements_fields, collapse="|")
    
  
  ## Results -----
  
  ### Pull API Data (DataDetails)
  defX_list$results$raw <- get_essence_data(url = defX_list$setup$url, start_date = params$start_date, end_date = params$end_date)
  
  ### Clean API Data (DataDetails) 
  defX_list$results$clean <- defX_list$results$raw %>%
    distinct(C_BioSense_ID, .keep_all = TRUE) %>% # Remove duplicate records (if applicable)
    mutate(!!paste0("def",i) := 1, # Flag to indicate these results matched this definition
           Date = as.Date(Date, format='%m/%d/%Y')) %>% # Convert Date to Date type variable.
    mutate(across(
      .cols = where(is.character),
      .fns = ~ifelse(. == "none", NA, .))) %>%
    clean_demographics(df=.) %>%
    clean_clinical_information(df=.) %>%
    clean_freetext(df=., keep_raw = FALSE, parse_abbrev = FALSE, text_case = "original") %>% # Prep's free-text for text mining
    select(Date, C_BioSense_ID, everything()) # Reorder variables
  
  #### Deduplicate DDx Codes
  if(params$deduplicate_ddx == TRUE){defX_list$results$clean <- defX_list$results$clean %>% dedup_dx(df=., keep_raw=FALSE)}
  
  ### Create Timeseries Summary
  defX_list$results$timeseries <- defX_list$results$clean %>%
    count(Date, name = "Visits") %>%
    filter(between(Date, params$start_date, params$end_date)) %>%
    full_join(params$all_dates) %>%
    mutate(Syndrome = defX_list$setup$info$Abbreviation) %>%
    replace_na(list(Visits = 0))
  
  ## Analysis -----
  
  ### Total Number of Results
  defX_list$analysis$total <- nrow(defX_list$results$clean)
  defX_list$analysis$total_pretty <- format(defX_list$analysis$total, big.mark = ",", scientific = FALSE) ## Pretty Version
  
  ### Definition Elements detected in free-text field(s) of interest
  list_detect_elements <- list()
  
  #### For each field (stored as a separate list element), the definition is applied to, search for the presence (0/1) of each query syntax component
  for(j in seq_along(defX_list$setup$detect_elements_fields)){
    list_detect_elements[[j]] <- detect_elements(data = defX_list$results$clean, 
                                                 terms = defX_list$setup$elements,
                                                 text_field = defX_list$setup$detect_elements_fields[j])
  }
  
  #### Combine all field-specific detect elements into one data frame
  defX_list$analysis$elements_detected <- reduce(.x = list_detect_elements, 
           .f = left_join, by = "C_BioSense_ID") %>%
    mutate(TruePositive = NA) %>%
    mutate(across(
      .cols = where(is.numeric),
      .fns = ~ ifelse(is.na(.), 0, .))) %>%
    select(C_BioSense_ID, TruePositive, all_of(defX_list$setup$detect_elements_fields),
           everything())
  
  #### Elements Detected Table
  defX_list$analysis$elements_detected_table <- defX_list$analysis$elements_detected %>%
    pivot_longer(cols = where(is.numeric), names_to = "Syndrome Element", values_to = "Match") %>%
    mutate(Field = str_extract(`Syndrome Element`, pattern = defX_list$setup$detect_elements_fields_regex),
           `Syndrome Element` = str_remove_all(`Syndrome Element`, defX_list$setup$detect_elements_fields_regex), # Remove DE Variable Prefix
           `Syndrome Element` = str_replace_all(`Syndrome Element`, "\\."," "), # Remove punctuation
           `Syndrome Element` = str_sub(`Syndrome Element`, start = 2)) %>% # Remove final _ after DE Variable Prefix
    full_join(defX_list$setup$structure) %>% # Full Join Syndrome Element Classification Crosswalk (DDx vs Free-text)
    mutate(`Syndrome Element` = ifelse(`Element Type` == "Diagnosis Code", 
                                       str_to_sentence(`Syndrome Element`), `Syndrome Element`)) %>%
    group_by(Field, `Syndrome Element`, `Element Type`) %>%
    summarize(Matches = sum(Match, na.rm = TRUE)) %>%
    arrange(desc(Matches))
    
  # rename the list to match the number of the definition
  assign(paste0("def",i,"_list"), defX_list) ## Consider using params$queries_
}

# Assess Definition Overlap -----

## Full Join Data
if(params$n_queries_eval == 2){
  
  Overlap <- full_join(def1_list$results$clean, def2_list$results$clean) 
  
}else if(params$n_queries_eval == 3){
  
  Overlap <- full_join(def1_list$results$clean, def2_list$results$clean) %>%
    full_join(., def3_list$results$clean)
}

## Convert NAs to 0's and Sum DefX Categories
if(params$n_queries_eval > 1){
 
  Overlap <- Overlap %>%
    mutate(across(
      .cols = starts_with("def", ignore.case = FALSE),
      .fns = ~ifelse(is.na(.), 0, .))) %>%
    mutate(Total_Defs = rowSums(select(., starts_with("def", ignore.case = FALSE)))) %>% # add all defX columns together 
    select(-starts_with("def"), starts_with("def"), Total_Defs) # Ensure def# and Total_Def columns are last. 
}

## Rename Def# Variables to Query Abbreviations
if(params$n_queries_eval == 2){
  
  Overlap <- Overlap %>%
    rename(!!params$queries_abbrev[1] := def1,
           !!params$queries_abbrev[2] := def2)
  
}else if(params$n_queries_eval == 3){
  
  Overlap <- Overlap %>%
    rename(!!params$queries_abbrev[1] := def1,
           !!params$queries_abbrev[2] := def2,
           !!params$queries_abbrev[3] := def3)
}
  
## Create Combinations Variable to Categorize Overlap
if(params$n_queries_eval > 1){
 
  Overlap$Definitions <- apply(Overlap[,params$queries_abbrev], 
                               MARGIN = 1, function(data) paste(names(which(data == 1)), collapse = ", ")) 
}
         

# Store Data -----
syndrome_eval_list <- list()


## Store Individual Queries within syndrome_eval_list
for(i in 1:params$n_queries_eval){
  syndrome_eval_list[[i]] <- get(paste0("def",i,"_list"))
}

names(syndrome_eval_list) <- params$queries_abbrev

## Add Overlap DF
if(params$n_queries_eval > 1){
  
  syndrome_eval_list[[params$overlap_name]] <- Overlap %>%
    select(Date, C_BioSense_ID, all_of(params$queries_abbrev), Total_Defs, Definitions, everything()) # Reorder columns
}

## Add Row Totals (via Overlap DF)
if(params$n_queries_eval == 1){
  
  syndrome_eval_list$defs_total <- nrow(syndrome_eval_list[[1]]$results$clean)
    
}else if(params$n_queries_eval > 1){syndrome_eval_list$defs_total <- nrow(syndrome_eval_list[[params$overlap_name]])}

syndrome_eval_list$defs_total_pretty <- format(syndrome_eval_list$defs_total, big.mark = ",", scientific = FALSE)


# Save Data -----

## All Data (.RData)
save(params, syndrome_eval_list, file = paste0(params$filepaths$output,paste(params$queries_abbrev, collapse="_"),".RData"))

## Matched Elements
for(i in 1:params$n_queries_eval){
  
  syndrome_eval_list[[i]]$analysis$elements_detected %>%
    write.csv(file = paste0(params$filepaths$matched_elements, params$queries_abbrev[i]," Matched Elements.csv"))
}


## Definition Overlap (.csv)
if(params$n_queries_eval > 1){
  
  ### Pull out all unique combinations into individual data frames
  definition_overlap_list <- split(syndrome_eval_list[[params$overlap_name]], syndrome_eval_list[[params$overlap_name]]$Definitions)
  
  ### Add name prefixes indicating the number of definitions in a combination
  names(definition_overlap_list) <- case_when(
    str_count(names(definition_overlap_list), pattern = "\\,") == 0 ~ paste0("1_",names(definition_overlap_list)),
    str_count(names(definition_overlap_list), pattern = "\\,") == 1 ~ paste0("2_",names(definition_overlap_list)),
    str_count(names(definition_overlap_list), pattern = "\\,") == 2 ~ paste0("3_",names(definition_overlap_list)))
  
  ### Clean data frame names
  names(definition_overlap_list) <- names(definition_overlap_list) %>%
    str_remove_all(., pattern = " ") %>%
    str_replace_all(., pattern = "\\,", replacement = "\\_")
  
  ### Save all data frames in list
  lapply(names(definition_overlap_list), function(name) { write_csv(definition_overlap_list[[name]], 
                                                                    file = paste0(params$filepaths$definition_overlap,name,".csv"))})
  
  ## All Visits (Regardless of Matching/Overlap)
  syndrome_eval_list[[params$overlap_name]] %>%
    write.csv(file = paste0(params$filepaths$definition_overlap, "All Visits.csv"))
}



## Format & Save Validation Review Data

if(params$validation_review$enable_validation_review == TRUE){
  
  vr_list <- list()
  
  for(i in 1:params$n_queries_eval){
    
    # Step 1: Formatting Data for Reviewers
    
    vr_list[[i]] <- syndrome_eval_list[[i]]$results$clean %>%
      add_date_components(df=.) %>% # Generate Date Components (Date --> Weekday, Week, Month, Year)
      get_sample(df=.,
                 sample_metric = params$validation_review$SampleMetric,
                 sample_value = params$validation_review$SampleValue,
                 strat_sample = params$validation_review$StratifiedSample,
                 strat_vars = params$validation_review$StratifiedVariables) %>% # SAMPLE
      mutate(Review_Rating = NA,
             Notes = NA) %>%
      ## REORDER VARIABLES: BSI, REVIEWER VARS, EVERYTHING ELSE
      select(Date, C_BioSense_ID,
             Review_Rating, Notes,
             everything(),
             -Weekday, -Week, -Month, -Year) %>%
      ## REMOVE ALL CHARACTERS WHICH MAY CAUSE XML ERRORS WITH EXCEL FILES
      mutate(across(.cols = where(is.character), 
                    .fns = ~ str_replace_all(string=., pattern = "[^[:graph:]]", replacement = " "))) # Replace all non-printable characters with a space (to prevent XML encoding issues when writing EXCEL files). 
    
    
    # Step 3: Save Review Data (1 copy for each reviewer examining each definition)
    
    for(j in 1:params$validation_review$n_reviewers){ # for each reviewer specified (j) create reviewer specific subfolder (within each definition being reviewed)
      
      vr_list[[i]] %>%
        writexl::write_xlsx(x=., path = paste0(params$filepaths$validation_review, 
                                          "/",params$queries_abbrev[i],
                                          "/1_Reviewed_Data/Reviewer_",j,
                                          "/Reviewer_",j,"_Data.xlsx"))
    }
    
    # Step 4: Set up Consensus Review Code for each definition's Validation Review Folder
    
    # Step 4c: Copy Validation Summary R Markdown code template(s) to each definition validation review folder.
    
    if(params$validation_review$n_reviewers == 1){ # Consensus Review not feasible with 1 reviewer --> Singular Validation_Summary.Rmd
      
      file.copy(from = "Scripts/SupportCode/Validation_Review/Validation_Summary.Rmd",
                to = paste0(params$filepaths$validation_review,"/",params$queries_abbrev[i]))
      
    }else if(params$validation_review$n_reviewers > 1){ # Only include Post-Consensus Review RMD if there are enough reviewers to do Consensus Review.
      
      # Pre Consensus Review Code Template
      file.copy(from = "Scripts/SupportCode/Validation_Review/Validation_Summary_Pre_Consensus_Review.Rmd",
                to = paste0(params$filepaths$validation_review,"/",params$queries_abbrev[i]))
      
      # Post Consensus Review Code Template
      file.copy(from = "Scripts/SupportCode/Validation_Review/Validation_Summary_Post_Consensus_Review.Rmd",
                to = paste0(params$filepaths$validation_review,"/",params$queries_abbrev[i]))
    }
  }
}

# Clean Up -----
rm(list = ls(pattern = "filename|Overlap|list_detect_elements|definition_overlap_list|defX_list"))
