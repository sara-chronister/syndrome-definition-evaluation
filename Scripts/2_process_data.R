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
  defX_list_simple <- list() # To store only cleaned DataDetails & Elements Detected (for easy access)
  
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
    str_replace_all(., pattern = "\\_", replacement = "\\\\_") # Prevents R Markdown from interpreting "_" as start/end of bold commands
  defX_list$setup$elements <- defX_list$setup$structure$`Syndrome Element`  # Individual definition elements
  
  ### Definition ESSENCE API 
  defX_list$setup$url <- defX_list$setup$info$API %>%
    Rnssp::change_dates(url=., start_date = params$start_date, end_date = params$end_date) %>% # Start and end dates from Setup tab.
    paste0(., params$fields_api_url) # ESSENCE fields from SelectFields tab. "Yes" in either IncludeForValidationReview (column B) or IncludeForTextAnalysis (column C).
  
  ### Fields Query Definitions Are Applied To
  defX_list$setup$detect_elements_fields <- str_split(defX_list$setup$info$Fields, pattern = "\\,") %>% 
    unlist() %>% 
    str_remove_all(., pattern = "\\bor\\b|\\band\\b") %>%
    str_trim(., side = "both") %>%
    str_replace_all(., pattern=" ", replacement = "\\_") %>%
    tibble(Fields =.) %>%
    ### Correct Labeling to Align with ESSENCE Variable Names ###
    mutate(Fields = case_when(Fields == "Discharge_Diagnosis" ~ "DischargeDiagnosis",
                              Fields == "Chief_Complaint_History" ~ "ChiefComplaintUpdates",
                              TRUE ~ Fields)) %>% # Correct Labeling to Align with ESSENCE Variable Names
    pull(Fields)
  
  defX_list$setup$detect_elements_fields_regex <- paste(defX_list$setup$detect_elements_fields, collapse="|")
    
  
  ## Results -----
  
  ### Pull API Data (DataDetails)
  defX_list$results$raw_datadetails <- get_essence_data(url = defX_list$setup$url, start_date = params$start_date, end_date = params$end_date)
  
  ### Clean API Data (DataDetails) 
  defX_list$results$clean_datadetails <- defX_list$results$raw_datadetails %>%
    distinct(C_BioSense_ID, .keep_all = TRUE) %>% # Remove duplicate records (if applicable)
    mutate(!!paste0("def",i) := 1, # Flag to indicate these results matched this definition
           Date = as.Date(Date, format='%m/%d/%Y')) %>% # Convert Date to Date type variable.
    mutate(across(
      .cols = where(is.character),
      .fns = ~ifelse(. == "none", NA, .))) %>% # Convert "none" to NA
    clean_demographics(df=.) %>%
    clean_clinical_information(df=.) %>%
    clean_freetext(df=., keep_raw = FALSE, parse_abbrev = FALSE, text_case = "original") %>% # Preps free-text fields for text mining
    select(Date, C_BioSense_ID, everything()) # Reorder variables
  
  #### Deduplicate DDx Codes
  if(params$deduplicate_ddx == TRUE){
    defX_list$results$clean_datadetails <- defX_list$results$clean_datadetails %>% 
      dedup_dx(df=., keep_raw=FALSE)
    }
  
  ### Create Timeseries Summary
  defX_list$results$timeseries <- defX_list$results$clean_datadetails %>%
    count(Date, name = "Visits") %>%
    filter(between(Date, params$start_date, params$end_date)) %>%
    full_join(params$all_dates) %>%
    mutate(Syndrome = defX_list$setup$info$Abbreviation) %>%
    replace_na(list(Visits = 0))
  
  ## Analysis -----
  
  ### Total Number of Results
  defX_list$analysis$total <- nrow(defX_list$results$clean_datadetails) # Number of DataDetails records (per definition)
  defX_list$analysis$total_pretty <- format(defX_list$analysis$total, big.mark = ",", scientific = FALSE) # Number of DataDetails records (formatted)
  
  ### Definition Elements detected in free-text field(s) of interest
  list_detect_elements <- list()
  
  #### For each field (stored as a separate list element) the definition is applied to, search for the presence (0/1) of each query syntax component
  
  suppressMessages({
    
    for(j in seq_along(defX_list$setup$detect_elements_fields)){
      list_detect_elements[[j]] <- detect_elements(data = defX_list$results$clean_datadetails, 
                                                   terms = defX_list$setup$elements,
                                                   text_field = defX_list$setup$detect_elements_fields[j])}
    
    #### Combine all field-specific detect elements into one data frame
    defX_list$analysis$elements_detected <- reduce(.x = list_detect_elements, .f = left_join, by = "C_BioSense_ID") %>%
      mutate(TruePositive = NA) %>%
      mutate(across(
        .cols = where(is.numeric),
        .fns = ~ ifelse(is.na(.), 0, .))) %>%
      select(C_BioSense_ID, TruePositive, all_of(defX_list$setup$detect_elements_fields), everything())

  })

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
    
  
  # Output Data (List Named to Match the Number of the Definition)
  assign(paste0("def",i,"_list"), defX_list)
  
  # Create a Simplified Output Data List
  defX_list_simple$clean_datadetails <- defX_list$results$clean_datadetails
  defX_list_simple$elements_detected <- defX_list$analysis$elements_detected
  
  # Output Data (List Named to Match Query Abbreviation)
  assign(params$queries_abbrev[i],defX_list_simple)
  
  # Free Up Memory/RAM
  gc()
}

rm(defX_list, defX_list_simple) # Remove placeholder lists (duplicate, have already been renamed to new objects)

# Assess Definition Overlap (Across All Visits) -----

## Full Join Data
if(params$n_queries_eval == 2){
  
  All_Visits <- full_join(def1_list$results$clean_datadetails, def2_list$results$clean_datadetails) 
  
}else if(params$n_queries_eval == 3){
  
  All_Visits <- full_join(def1_list$results$clean_datadetails, def2_list$results$clean_datadetails) %>%
    full_join(., def3_list$results$clean_datadetails)
}

## Convert NAs to 0's and Sum DefX Categories
if(params$n_queries_eval > 1){
 
  All_Visits <- All_Visits %>%
    mutate(across(
      .cols = starts_with("def", ignore.case = FALSE),
      .fns = ~ifelse(is.na(.), 0, .))) %>%
    mutate(Total_Defs = rowSums(select(., starts_with("def", ignore.case = FALSE)))) %>% # add all defX columns together 
    select(-starts_with("def"), starts_with("def"), Total_Defs) # Ensure def# and Total_Def columns are last. 
}

## Rename Def# Variables to Query Abbreviations
if(params$n_queries_eval == 2){
  
  All_Visits <- All_Visits %>%
    rename(!!params$queries_abbrev[1] := def1,
           !!params$queries_abbrev[2] := def2)
  
}else if(params$n_queries_eval == 3){
  
  All_Visits <- All_Visits %>%
    rename(!!params$queries_abbrev[1] := def1,
           !!params$queries_abbrev[2] := def2,
           !!params$queries_abbrev[3] := def3)
}
  
## Create Definitions Variable to Categorize A Visit Record by What Combination of Definitions It Is Detected By (Overlap)
if(params$n_queries_eval > 1){
 
  All_Visits$Definitions <- apply(All_Visits[,params$queries_abbrev], 
                               MARGIN = 1, function(data) paste(names(which(data == 1)), collapse = ", ")) 
}


# Store Data -----
syndrome_eval_list <- list()


## Store Individual Queries within syndrome_eval_list
for(i in 1:params$n_queries_eval){
  syndrome_eval_list[[i]] <- get(paste0("def",i,"_list"))
}

## Rename List Elements using Query Abbreviations
names(syndrome_eval_list) <- params$queries_abbrev


## Add Overlap DF
if(params$n_queries_eval > 1){
  
  syndrome_eval_list[[params$allvisits_name]] <- All_Visits %>%
    select(Date, C_BioSense_ID, all_of(params$queries_abbrev), Total_Defs, Definitions, everything()) # Reorder columns
}

## Add Row Totals (via Overlap DF)
if(params$n_queries_eval == 1){
  
  syndrome_eval_list$defs_total <- nrow(syndrome_eval_list[[1]]$results$clean_datadetails)
    
}else if(params$n_queries_eval > 1){syndrome_eval_list$defs_total <- nrow(syndrome_eval_list[[params$allvisits_name]])}

syndrome_eval_list$defs_total_pretty <- format(syndrome_eval_list$defs_total, big.mark = ",", scientific = FALSE)


## Rename All Visits Object
if(params$n_queries_eval > 1){assign(params$allvisits_name, All_Visits)} # All_Visits --> object named Query Abbreviations separated by _


# Save Data -----

## Only save the objects that exist in the global environment (if they don't exist, continue to save the ones that do). Source: https://stackoverflow.com/questions/69742005/saving-r-objects-conditional-on-whether-they-exist)
save(list = intersect(ls(), c("params", "syndrome_eval_list", 
                              params$queries_abbrev, # Our Easy Access Data ($clean_datadetails & $elements_detected) for all definitions
                              params$allvisits_name)), # All Visits data frame show how the definitions apply (and possibly overlap) within pulled records
     file = paste0(params$filepaths$output,paste(params$queries_abbrev, collapse="_"),".RData"))


## Matched Elements
for(i in 1:params$n_queries_eval){
  
  syndrome_eval_list[[i]]$analysis$elements_detected %>%
    write.csv(file = paste0(params$filepaths$matched_elements, params$queries_abbrev[i]," Matched Elements.csv"))
}


## Definition All Visit Comparison (.csv)
if(params$n_queries_eval > 1){
  
  ### Pull out all unique combinations into individual data frames
  definition_comparison_list <- split(syndrome_eval_list[[params$allvisits_name]], syndrome_eval_list[[params$allvisits_name]]$Definitions)
  
  ### Add name prefixes indicating the number of definitions in a combination
  names(definition_comparison_list) <- case_when(
    str_count(names(definition_comparison_list), pattern = "\\,") == 0 ~ paste0("1_",names(definition_comparison_list)),
    str_count(names(definition_comparison_list), pattern = "\\,") == 1 ~ paste0("2_",names(definition_comparison_list)),
    str_count(names(definition_comparison_list), pattern = "\\,") == 2 ~ paste0("3_",names(definition_comparison_list)))
  
  ### Clean data frame names
  names(definition_comparison_list) <- names(definition_comparison_list) %>%
    str_remove_all(., pattern = " ") %>%
    str_replace_all(., pattern = "\\,", replacement = "\\_")
  
  ### Save all data frames in list
  lapply(names(definition_comparison_list), function(name) { write_csv(definition_comparison_list[[name]], 
                                                                    file = paste0(params$filepaths$definition_comparison,name,".csv"))})
  
  ## All Visits (Regardless of Matching/Overlap)
  syndrome_eval_list[[params$allvisits_name]] %>%
    write.csv(file = paste0(params$filepaths$definition_comparison, "All Visits.csv"))
}



## Format & Save Validation Review Data

if(params$validation_review$enable_validation_review == TRUE){
  
  vr_list <- list()
  
  for(i in 1:params$n_queries_eval){
    
    # Step 1: Formatting Data for Reviewers
    
    vr_list[[i]] <- syndrome_eval_list[[i]]$results$clean_datadetails %>%
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
rm(list = ls(pattern="def\\d_list|defX_list|All_Visits|list_detect_elements|vr_list|definition_comparison_list")) # Remove unneeded objects