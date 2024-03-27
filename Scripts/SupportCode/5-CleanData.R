## Functions to clean different variables

## Demographics -----
clean_demographics <- function(df){
  
  # Step 0: Clean Data Frame
  df_clean <- df
  
  # Step 1: Clean Demographic Variables
  
  ## Step 1a: Sex
  if("Sex" %in% names(df_clean)){
    
    df_clean <- df_clean %>%
      mutate(Sex = case_when(
        Sex == "M" ~ "Male",
        Sex == "F" ~ "Female",
        Sex == "U" ~ "Unknown"),
        Sex = factor(Sex, levels = c("Female", "Male", "Unknown")))
    }
  
  ## Step 1b: Race
  if("c_race" %in% names(df_clean)){
    
    df_clean <- df_clean %>%
      mutate(Race = ifelse(c_race %in% c("Not Categorized", "Not Reported or Null", "Other Race", "Unknown"), "Unknown", c_race),
             Race = factor(Race, levels = c("American Indian or Alaska Native",
                                            "Asian", "Black or African American",
                                            "Multiracial", "Native Hawaiian or Other Pacific Islander",
                                            "White", "Unknown")))
  }
  
  ## Step 1c: Ethnicity
  if("c_ethnicity" %in% names(df_clean)){
    
    df_clean <- df_clean %>%
      mutate(Ethnicity = ifelse(c_ethnicity %in% c("Not Categorized", "Not Reported or Null", "Unknown"), "Unknown", c_ethnicity),
             Ethnicity = factor(Ethnicity, levels = c("Hispanic or Latino", "Not Hispanic or Latino", "Unknown")))
    
  }
  
  ## Step 1d: Age
  if("Age" %in% names(df_clean)){
    
    df_clean <- df_clean %>%
      mutate(Age = as.numeric(Age),
             Age = ifelse(Age < 0, NA, Age))
  }
  
  ## Step 1e: Regions
  if("Region" %in% names(df_clean)){
    
    df_clean <- df_clean %>%
      mutate(Region = str_remove(Region, pattern = "WA\\_"))
  }
  
  if("HospitalRegion" %in% names(df_clean)){
    
    df_clean <- df_clean %>%
      mutate(HospitalRegion = str_remove(HospitalRegion, pattern = "WA\\_"))
  }
  
  return(df_clean)
}


## Clinical Information -----
clean_clinical_information <- function(df){
  
  # Step 0: Clean Data Frame
  df_clean <- df
  
  # Step 1: Clean Clinical Information Variables
  
  ## Step 1a: Initial_Temp_Calc
  if("Initial_Temp_Calc" %in% names(df_clean)){
    df_clean <- df_clean %>% mutate(Initial_Temp_Calc = as.numeric(Initial_Temp_Calc))
  }
  
  ## Step 1b: Initial_Pulse_Oximetry_Calc
  if("Initial_Pulse_Oximetry_Calc" %in% names(df_clean)){
    df_clean <- df_clean %>% 
      mutate(Initial_Pulse_Oximetry_Calc = as.numeric(Initial_Pulse_Oximetry_Calc),
             Initial_Pulse_Oximetry_Calc = ifelse(Initial_Pulse_Oximetry_Calc < 0, NA, Initial_Pulse_Oximetry_Calc))
  }
  
  ## Step 1c: MinutesFromVisitToDischarge
  if("MinutesFromVisitToDischarge" %in% names(df_clean)){
    df_clean <- df_clean %>%
      mutate(MinutesFromVisitToDischarge = as.numeric(MinutesFromVisitToDischarge),
             MinutesFromVisitToDischarge = ifelse(MinutesFromVisitToDischarge <0, NA, MinutesFromVisitToDischarge))
  }
  
  ## Step 1d: C_Death
  if("C_Death" %in% names(df_clean)){
    df_clean <- df_clean %>%
      mutate(C_Death = case_when(
        C_Death == "Yes" ~ 1,
        C_Death == "No" ~ 0))
  }

  return(df_clean)
}


## Discharge Diagnoses -----

#### Deduplicate DischargeDiagnosis Variables ####
dedup_dx <- function(df, keep_raw = FALSE){
  
  # Step 0: Create Cleaned Data Frame
  df_clean <- df
  
  # Step 1: Identify the variables present within the data frame
  all_ddx_cleaning_vars <- c("DischargeDiagnosis", "DDParsed", "Diagnosis_Combo",
                             "CCDD", "CCDDParsed")
  
  ddx_vars <- intersect(all_ddx_cleaning_vars, names(df)) # All ddx vars in present in df (for cleaning)
  
  # Step 2: (TOGGLE) Keep uncleaned variables too? 
  if(keep_raw == TRUE){
    
    df_clean <- df_clean %>%
      mutate(across(.cols = all_of(ddx_vars),
                    .fns = list(raw = ~.)))
  }
  
  
  # [Clean] Diagnosis_Combo
  if("Diagnosis_Combo" %in% ddx_vars){
    ## Description: Within Diagnosis_Combo, there are ";" directly in front of ICD-10 CM codes (ex: ";F32.A") as well as within the attached, free-text definitions. This is a problem that must be addressed before segmenting the string into a vector of substrings.
    
    ## Approach: Match ONLY the ";" in front of the ICD-10 CM codes, and turn into a "|". Then segment the string into a vector of substrings using "|" (to avoid any problematic mix ups).
    
    df_clean <- df_clean %>%
      mutate(Diagnosis_Combo = str_replace_all(Diagnosis_Combo,
                                               pattern = ";([:alpha:]{1}[:alnum:]{2}\\.*[:alnum:]*)", # Look for ";" and the ICD-10 CM immediately following it. 
                                               replacement = "|\\1")) # Replace ";" for "|", keep the same ICD-10 CM
    
    # Note: In the replacement, "\\1" is a back-reference to the ICD-10 CM pattern surrounded by () following the ";" in the pattern. For more information on regex back-references, see: https://www.regular-expressions.info/backref.html
  }
  
  
  
  # Step 2: Deduplicate Discharge Diagnosis Codes (DischargeDiagnosis, DDParsed, Diagnosis_Combo Only)
  
  ## Description: If one of the 3 variables are in the data frame, it's split pattern will be added and then 
  ## the variable will be de-duplicated in the for-loop below. If 0/3 variables aren't present, for-loop won't
  ## run.
  
  ddx_vars_split_pattern <- c()
  
  if("DischargeDiagnosis" %in% ddx_vars){ddx_vars_split_pattern <- c(ddx_vars_split_pattern, ";")}
  if("DDParsed" %in% ddx_vars){ddx_vars_split_pattern <- c(ddx_vars_split_pattern, ";")}
  if("Diagnosis_Combo" %in% ddx_vars){ddx_vars_split_pattern <- c(ddx_vars_split_pattern, "\\|")}
  
  if(!is.null(ddx_vars_split_pattern)){
    
    for(i in seq_along(ddx_vars_split_pattern)){
      
      # 2a: Split string into character vectors of substrings
      df_clean <- df_clean %>%
        mutate(!!paste0(ddx_vars[i],"_list") := str_split(get(ddx_vars[i]), 
                                                          pattern = ddx_vars_split_pattern[i])) # Split string by ddx_vars_split_pattern
      
      # 2b: Clean character vectors of substrings (remove "", and duplicates) Remove character vector elements that are "". (Typically beginning/end of DischargeDiagnosis and DDParsed)
      
      df_clean <- df_clean %>%
        mutate(!!paste0(ddx_vars[i],"_list") := lapply(get(paste0(ddx_vars[i],"_list")), 
                                                       function(vec) unique(vec[vec != ""]))) # Remove duplicates and ""
      
      # 2c: Re-collapse de-duplicated ddx codes into a single string
      df_clean <- df_clean %>%
        mutate(!!paste0(ddx_vars[i]) := sapply(get(paste0(ddx_vars[i],"_list")), 
                                               function(list) paste(list, collapse = ";"))) # Collapse substrings together with ";"
    }
    
  }
  
  
  # [Clean] DischargeDiagnosis & DDParsed
  
  ## Description: Add leading & trailing ";" to DischargeDiagnosis and DDParsed.
  
  if("DischargeDiagnosis" %in% ddx_vars){
    
    df_clean <- df_clean %>% 
      mutate(DischargeDiagnosis = ifelse(DischargeDiagnosis != "NA" & !is.na(DischargeDiagnosis), 
                                         paste0(";",DischargeDiagnosis,";"), NA))} # Add ";" unless NA
  
  if("DDParsed" %in% ddx_vars){
    
    df_clean <- df_clean %>% 
      mutate(DDParsed = ifelse(DDParsed != "NA" & !is.na(DDParsed), 
                               paste0(";",DDParsed,";"), NA))} # Add ";" unless NA
  
  
  
  # Step 3: Deduplicate Discharge Diagnosis Codes (CCDD, CCDDParsed)
  
  # 3a: Quick-Method (Deduplicated DischargeDiagnosis & DDParsed already available)
  
  ## Description: Split CC-DD, and append the CC portion on to the already de-duplicated DischargeDiagnosis (DD) portion from above. 
  
  if("CCDD" %in% ddx_vars & "DischargeDiagnosis" %in% ddx_vars){
    
    df_clean <- df_clean %>%
      tidyr::separate(data = ., col = CCDD, into = c("CCParsed", "DD_remove"), sep = "\\|") %>% # Split by "|" separating CC and DD
      mutate(CCDD = ifelse(!is.na(DischargeDiagnosis),
                           paste(CCParsed, DischargeDiagnosis, sep = " | "), paste(CCParsed,"|"))) %>% # Combine CCParsed with previously de-duped DischargeDiagnosis
      select(-CCParsed, -DD_remove) # Remove raw component variables (de-duped CCDD created)
  }
  
  if("CCDDParsed" %in% ddx_vars & "DDParsed" %in% ddx_vars){
    
    df_clean <- df_clean %>%
      tidyr::separate(dat = ., col = CCDDParsed, into = c("CCParsed", "DD_remove"), sep = "\\|") %>% # Split by the "|" separating CC and DD
      mutate(CCDDParsed = ifelse(!is.na(DDParsed),
                                 paste(CCParsed, DDParsed, sep = " | "), paste(CCParsed,"|"))) %>% # Combine CCParsed with previously de-duped DDParsed
      select(-CCParsed, -DD_remove) # Remove raw component variables (de-duped CCDD created)
  }
  
  
  #3b: Long-Method (Deduplicated DischargeDiagnosis & DDParsed NOT already available)
  
  if("CCDD" %in% ddx_vars & !"DischargeDiagnosis" %in% ddx_vars){
    
    # Step 1: Split CC-DD, make DD into a character vector of substrings (individual ICD-10 CM)
    df_clean <- df_clean %>%
      tidyr::separate(data =., col = CCDD, into = c("CCParsed", "DD"), sep = "\\|") %>% # Split by "|" separating CC and DD
      mutate(across(
        .cols = c(CCParsed, DD), 
        .fns = ~ifelse(.x == "", 
                       NA, str_trim(.x, side = "both")))) %>% # convert "" to NA, trim whitespace
      mutate(DD_list = str_split(DD, pattern = ";")) # Split DD string into character vector (substrings) by ";"
    
    # Step 2: Remove duplicates and ""
    df_clean$DD_list <- lapply(df_clean$DD_list, function(vec) unique(vec[vec != ""])) # Remove duplicates and ""
    
    # Step 3: Recombine DD (now de-duplicated)
    df_clean$DD <- sapply(df_clean$DD_list, function(list) paste(list, collapse = ";")) # Recombine DD substrings (";" between each)
    
    # Step 4: Recombine CC-DD (now de-duplicated)
    df_clean <- df_clean %>%
      mutate(DD = ifelse(!is.na(DD) & DD != "NA", 
                         paste0(";",DD,";"), NA), # Add starting/ending ";" on DD
             CCDD = ifelse(!is.na(DD),
                           paste(CCParsed, DD, sep = " | "), paste(CCParsed,"|"))) %>% # Recombine CCParsed and DD (now deduplicated)
      select(-DD, -CCParsed,  -DD_list) # Remove raw components (CCDD deduplicated created)
    
  }
  
  if("CCDDParsed" %in% ddx_vars & !"DDParsed" %in% ddx_vars){
    
    # Step 1
    df_clean <- df_clean %>%
      tidyr::separate(data =., col = CCDDParsed, into = c("CCParsed", "DDParsed"), sep = "\\|") %>% # Split by "|" separating CC and DD
      mutate(across(
        .cols = c(CCParsed, DDParsed), 
        .fns = ~ifelse(.x == "", 
                       NA, str_trim(.x, side = "both")))) %>% # convert "" to NA, trim whitespace
      mutate(DDParsed_list = str_split(DDParsed, pattern = ";")) # Split DD string into character vector (substrings) by ";"
    
    # Step 2
    df_clean$DDParsed_list <- lapply(df_clean$DDParsed_list, function(vec) unique(vec[vec != ""])) # Remove duplicates and ""
    
    # Step 3
    df_clean$DDParsed <- sapply(df_clean$DDParsed_list, function(list) paste(list, collapse = ";")) # Recombine DD substrings (";" between each)
    
    # Step 4
    df_clean <- df_clean %>%
      mutate(DDParsed = ifelse(!is.na(DDParsed) & DDParsed != "NA", 
                               paste0(";",DDParsed,";"), NA), # Add starting/ending ";" on DD
             CCDDParsed = ifelse(!is.na(DDParsed),
                                 paste(CCParsed, DDParsed, sep = " | "), paste(CCParsed,"|"))) %>% # Recombine CCParsed and DD (now deduplicated)
      select(-CCParsed, -DDParsed, -DDParsed_list) # Remove components (CCDDParsed deduplicated has been created)
    
  }
  
  
  # Step 4: Clean Up & Return Data to Global Environment
  
  df_clean <- df_clean %>% select(-ends_with("_list")) # remove list variables
  
  if(keep_raw == TRUE){
    
    df_clean <- df_clean %>% select(-ends_with("_raw"), everything()) # Make "_raw" variables appear last. 
    
  }else if(keep_raw == FALSE){
    
    df_clean <- df_clean %>% select(-ends_with("_raw"))
  }
  
  return(df_clean)
}


## Free-Text -----

#### ChiefComplaintUpdates ####
clean_ChiefComplaintUpdates <- function(data = my_file) {
  data2 <- data %>%
    dplyr::select(ChiefComplaintUpdates) %>%
    mutate(ChiefComplaintUpdates = str_replace_all(ChiefComplaintUpdates, "[[:cntrl:]]|<BR>|[?.!???'+):@]|\\|", "")) %>%
    mutate(ChiefComplaintUpdates = str_replace_all(ChiefComplaintUpdates,"\\{[[:digit:]]\\}", "")) %>%
    mutate(ChiefComplaintUpdates = str_replace_all(ChiefComplaintUpdates,";|\\\\|\\/", " ")) %>%
    mutate(ChiefComplaintUpdates = str_trim(ChiefComplaintUpdates, side = "both")) %>%
    mutate(ChiefComplaintUpdates = toupper(ChiefComplaintUpdates)) %>%
    mutate(ChiefComplaintUpdates = str_replace_all(ChiefComplaintUpdates, "PT", "PATIENT")) %>%
    mutate(number_chars_updates = str_count(ChiefComplaintUpdates),
           number_words_ccupdates = str_count(ChiefComplaintUpdates, boundary("word"))) %>%
    mutate(
      number_words_ccupdates = case_when(ChiefComplaintUpdates == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_words_ccupdates))),
      number_chars_updates = case_when(ChiefComplaintUpdates == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_chars_updates)))) 
  
  data3 <- dplyr::select(.data = data,-ChiefComplaintUpdates)
  
  data <- bind_cols(data3, data2)
  
  return(data)
}

#### ChiefComplaintOrig ####
clean_ChiefComplaintOriginal <- function(data = my_file) {
  data2 <- data %>%
    dplyr::select(ChiefComplaintOrig) %>%
    mutate(ChiefComplaintOrig = str_replace_all(ChiefComplaintOrig, "[[:cntrl:]]", "")) %>%
    mutate(ChiefComplaintOrig = str_replace_all(ChiefComplaintOrig, "[[:alnum:]];[[:alnum:]]", " ")) %>%
    mutate(ChiefComplaintOrig = str_trim(ChiefComplaintOrig, side = "both")) %>%
    mutate(ChiefComplaintOrig = if_else(nchar(ChiefComplaintOrig)==2, "NA", ChiefComplaintOrig)) %>%
    mutate(ChiefComplaintOrig = toupper(ChiefComplaintOrig)) %>%
    mutate(ChiefComplaintOrig = str_replace_na(ChiefComplaintOrig, replacement = "NA")) %>%
    mutate(ChiefComplaintOrig = str_replace_all(ChiefComplaintOrig, " PT | PT|PT ", " PATIENT ")) %>%
    mutate(number_chars_orig = str_count(ChiefComplaintOrig),
           number_words_ccorig = str_count(ChiefComplaintOrig, boundary("word"))) %>%
    mutate(
      number_words_ccorig = case_when(ChiefComplaintOrig == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_words_ccorig))),
      number_chars_orig = case_when(ChiefComplaintOrig == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_chars_orig)))) 
  
  data3 <- dplyr::select(.data = data,-ChiefComplaintOrig)
  
  data <- bind_cols(data3, data2)
  
  return(data)
}

#### DischargeDiagnosis ####
clean_DischargeDiagnosis <- function(data = my_file) {
  data2 <- data %>%
    dplyr::select(DischargeDiagnosis) %>%
    mutate(DischargeDiagnosis = str_replace_all(DischargeDiagnosis, "[[:cntrl:]]|<BR>|[?.!???'+):]", "")) %>%
    mutate(DischargeDiagnosis = str_replace_all(DischargeDiagnosis, "\\\\|([a-zA-Z])/([\\d])|([\\d])/([a-zA-Z])|([a-zA-Z])/([a-zA-Z])|([\\d])/([\\d])|;", " ")) %>%
    mutate(DischargeDiagnosis = str_trim(DischargeDiagnosis, side = "both")) %>%
    mutate(DischargeDiagnosis = if_else(nchar(DischargeDiagnosis)<=2, "NA", DischargeDiagnosis)) %>%
    mutate(DischargeDiagnosis = str_replace_na(DischargeDiagnosis, replacement = "NA")) %>%
    mutate(number_chars_dx = str_count(DischargeDiagnosis),
           number_words_dx = str_count(DischargeDiagnosis, boundary("word"))) %>%
    mutate(
      number_chars_dx = case_when(DischargeDiagnosis == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_chars_dx))),
      number_words_dx = case_when(DischargeDiagnosis == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_words_dx)))) 
  
  data3 <- dplyr::select(.data = data,-DischargeDiagnosis)
  
  data <- bind_cols(data3, data2)
  
  return(data)
}

#### CCDD ####
clean_CCDD <- function(data = my_file) {
  data2 <- data %>%
    dplyr::select(CCDD) %>%
    mutate(CCDD = str_replace_all(CCDD, "[[:cntrl:]]|<BR>|[?.!???'+):]", "")) %>%
    mutate(CCDD = str_replace_all(CCDD, "\\\\|;|([a-zA-Z])/([\\d])|([\\d])/([a-zA-Z])|([a-zA-Z])/([a-zA-Z])|([\\d])/([\\d])|\\W", " ")) %>%
    mutate(CCDD = str_trim(CCDD, side = "both")) %>%
    mutate(CCDD = if_else(nchar(CCDD)<=2, "NA", CCDD)) %>%
    mutate(CCDD = str_replace_na(CCDD, replacement = "NA")) %>%
    mutate(number_chars_CCDD = str_count(CCDD),
           number_words_CCDD = str_count(CCDD, boundary("word"))) %>%
    mutate(
      number_chars_CCDD = case_when(CCDD == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_chars_CCDD))),
      number_words_CCDD = case_when(CCDD == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_words_CCDD)))) 
  
  data3 <- dplyr::select(.data = data,-CCDD)
  
  data <- bind_cols(data3, data2)
  
  return(data)
}

#### ChiefComplaintParsed ####
clean_ChiefComplaintParsed <- function(data = my_file) {
  data2 <- data %>%
    dplyr::select(ChiefComplaintParsed) %>%
    mutate(ChiefComplaintParsed = str_replace_all(ChiefComplaintParsed, "[[:cntrl:]]", "")) %>%
    mutate(ChiefComplaintParsed = str_trim(ChiefComplaintParsed, side = "both")) %>%
    mutate(ChiefComplaintParsed = if_else(nchar(ChiefComplaintParsed)==2, "NA", ChiefComplaintParsed)) %>%
    mutate(ChiefComplaintParsed = str_replace_na(ChiefComplaintParsed, replacement = "NA")) %>%
    mutate(number_chars_parsed = str_count(ChiefComplaintParsed),
           number_words_parsed = str_count(ChiefComplaintParsed, boundary("word"))) %>%
    mutate(
      number_words_parsed = case_when(ChiefComplaintParsed == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_words_parsed))),
      number_chars_parsed = case_when(ChiefComplaintParsed == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_chars_parsed)))) 
  
  data3 <- dplyr::select(.data = data,-ChiefComplaintParsed)
  
  data <- bind_cols(data3, data2)
  
  return(data)
}

#### Admit_Reason_Combo ####
clean_Admit_Reason_Combo <- function(data = my_file) {
  data2 <- data %>%
    dplyr::select(Admit_Reason_Combo) %>%
    mutate(Admit_Reason_Combo = str_replace_all(Admit_Reason_Combo, "[[:cntrl:]]", "")) %>%
    mutate(Admit_Reason_Combo = str_trim(Admit_Reason_Combo, side = "both")) %>%
    mutate(Admit_Reason_Combo = if_else(nchar(Admit_Reason_Combo)==2, "NA", Admit_Reason_Combo)) %>%
    mutate(Admit_Reason_Combo = toupper(Admit_Reason_Combo)) %>%
    mutate(Admit_Reason_Combo = str_replace_na(Admit_Reason_Combo, replacement = "NA")) %>%
    mutate(Admit_Reason_Combo = str_replace_all(Admit_Reason_Combo, "PT", "PATIENT")) %>%
    mutate(number_chars_admit = str_count(Admit_Reason_Combo),
           number_words_admit = str_count(Admit_Reason_Combo, boundary("word"))) %>%
    mutate(
      number_words_admit = case_when(Admit_Reason_Combo == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_words_admit))),
      number_chars_admit = case_when(Admit_Reason_Combo == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_chars_admit)))) 
  
  data3 <- dplyr::select(.data = data,-Admit_Reason_Combo)
  
  data <- bind_cols(data3, data2)
  
  return(data)
}

#### TriageNotesOrig ####
clean_TriageNotesOrig <- function(data = my_file) {
  data2 <- data %>%
    dplyr::select(TriageNotesOrig) %>%
    mutate(TriageNotesOrig = str_replace_all(TriageNotesOrig, "[[:cntrl:]]", "")) %>%
    mutate(TriageNotesOrig = str_trim(TriageNotesOrig, side = "both")) %>%
    mutate(TriageNotesOrig = if_else(nchar(TriageNotesOrig)==2, "NA", TriageNotesOrig)) %>%
    mutate(TriageNotesOrig = toupper(TriageNotesOrig)) %>%
    mutate(TriageNotesOrig = str_replace_na(TriageNotesOrig, replacement = "NA")) %>%
    mutate(TriageNotesOrig = str_replace_all(TriageNotesOrig, "PT", "PATIENT")) %>%
    mutate(number_chars_triage = str_count(TriageNotesOrig),
           number_words_triage = str_count(TriageNotesOrig, boundary("word"))) %>%
    mutate(
      number_words_admit = case_when(TriageNotesOrig == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_words_triage))),
      number_chars_admit = case_when(TriageNotesOrig == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_chars_triage)))) 
  
  data3 <- dplyr::select(.data = data,-TriageNotesOrig)
  
  data <- bind_cols(data3, data2)
  
  return(data)
}