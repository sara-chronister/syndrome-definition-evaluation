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
  
    
  ## Results -----
  
  ### Pull API Data (DataDetails)
  defX_list$results$raw <- get_essence_data(url = defX_list$setup$url, 
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
  
  #### Deduplicate DDx Codes
  if(params$deduplicate_ddx == TRUE){defX_list$results$clean <- defX_list$results$clean %>% dedup_dx(df=., keep_raw=FALSE)}
  
  
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
  defX_list$analysis$elements_detected <- bind_cols(list_detect_elements) %>%
    rename(C_BioSense_ID = C_BioSense_ID...1) %>%
    select(-matches(".*\\..*")) %>% # Remove duplicate variables
    mutate(across(
      .cols = where(is.numeric),
      .fns = ~ ifelse(is.na(.), 0, .)))
  
  # rename the list to match the number of the definition
  assign(paste0("def",i,"_list"), defX_list) ## Consider using params$queries_
}
rm(i,j)


# Assess Definition Overlap -----

## Full Join Data
if(params$n_queries_eval == 2){
  
  Overlap <- full_join(def1_list$results$clean, def2_list$results$clean) 
  
}else if(params$n_queries_eval == 3){
  
  Overlap <- full_join(def1_list$results$clean, def2_list$results$clean) %>%
    full_join(., def3_list$results$clean)
}

## Convert NAs to 0's and Sum DefX Categories
Overlap <- Overlap %>%
  mutate(across(
    .cols = starts_with("def", ignore.case = FALSE),
    .fns = ~ifelse(is.na(.), 0, .))) %>%
  mutate(Total_Defs = rowSums(select(., starts_with("def", ignore.case = FALSE)))) %>% # add all defX columns together 
  select(-starts_with("def"), starts_with("def"), Total_Defs) # Ensure def# and Total_Def columns are last.

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
Overlap$Definitions <- apply(Overlap[,params$queries_abbrev], 
                              MARGIN = 1, function(data) paste(names(which(data == 1)), collapse = ", "))
         

# Store Data -----
syndrome_eval_list <- list()


## Store Individual Queries within syndrome_eval_list
for(i in 1:params$n_queries_eval){
  syndrome_eval_list[[i]] <- get(paste0("def",i,"_list"))
}

names(syndrome_eval_list) <- params$queries_abbrev

## Add Overlap DF
if(params$n_queries_eval > 1){
  
  overlap_name <- paste0(paste(params$queries_abbrev, collapse="_"), "_Overlap")
  
  syndrome_eval_list[[overlap_name]] <- Overlap %>%
    select(Date, C_BioSense_ID, all_of(params$queries_abbrev), Total_Defs, Definitions, everything()) # Reorder columns
}

## Add Row Totals (via Overlap DF)
syndrome_eval_list$defs_total <- nrow(syndrome_eval_list$Overlap)
syndrome_eval_list$defs_total_pretty <- format(syndrome_eval_list$defs_total, big.mark = ",", scientific = FALSE)



# Save Data -----

## All Data (.RData)
save(params, syndrome_eval_list, file = paste0(params$filepaths$output,paste(params$queries_abbrev, collapse="_"),".RData"))

## Matched Elements
for(i in 1:params$n_queries_eval){
  
  syndrome_eval_list[[i]]$analysis$elements_detected %>%
    write.csv(file = paste0(params$filepaths$matched_elements, params$queries_abbrev[i]," Matched Elements.csv"))
}
rm(i)


## Definition Overlap (.csv)

# Match Only 1 Definition
if(params$n_queries_eval > 1){ # If only 1 definition is evaluated use "All Defs Visits.csv"
  for(i in 1:params$n_queries_eval){
    
    syndrome_eval_list[[overlap_name]] %>%
      filter(Definitions == params$queries_abbrev[i]) %>%
      write.csv(file = paste0(params$filepaths$definition_overlap,"1_", params$queries_abbrev[i], " Only Visits.csv"))
  }
}


## Match Only 2 Definitions
if(params$n_queries_eval == 3){ # 2 definition combinations are only of interest if evaluating 3 definitions simultaneously. 1 definition being evaluated (combinations not possible); 2 definitions being evaluated used "All Defs Visits.csv"
  
  ### Identify All Unique 2 Definition Combinations
  combinations <- syndrome_eval_list[[overlap_name]] %>% 
    filter(Total_Defs == 2) %>% 
    distinct(Definitions) %>%
    pull(Definitions)
  
  ### Create 2 Definition Combination File Names
  combination_filenames <- combinations %>%
    str_to_upper(.) %>%
    str_replace_all(string = ., pattern = ",", replacement = " and")
  
  ### For Loop: Filter & Write CSV for all Unique Combinations
  for(i in seq_along(combinations)){
    
    syndrome_eval_list[[overlap_name]] %>%
      filter(Definitions == combinations[i]) %>%
      write.csv(file = paste0(params$filepaths$definition_overlap,"2_", combination_filenames[i], " Visits.csv"))
  }
  rm(i)
}


## Match All Definitions
all_matched_filename <- syndrome_eval_list[[overlap_name]] %>% 
  filter(Total_Defs == params$n_queries_eval) %>% 
  distinct(Definitions) %>%
  pull(Definitions)%>%
  str_to_upper(.) %>%
  str_replace_all(string = ., pattern = ",", replacement = " and")

### Write CSV
syndrome_eval_list[[overlap_name]] %>%
  filter(Total_Defs == params$n_queries_eval) %>%
  write.csv(file = paste0(params$filepaths$definition_overlap,params$n_queries_eval,"_", all_matched_filename, " Visits.csv"))


# All Visits (Regardless of Matching/Overlap)
syndrome_eval_list[[overlap_name]] %>%
  write.csv(file = paste0(params$filepaths$definition_overlap, "All Visits.csv"))

# Clean Up -----
rm(list = ls(pattern = "filename"))