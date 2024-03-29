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
  
  if("AgeGroup" %in% names(df_clean)){
    
    df_clean <- df_clean %>%
      mutate(AgeGroup = ifelse(AgeGroup == "65-1000", "65+", AgeGroup))
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

### Deduplicate DischargeDiagnosis Variables ###
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

### Parse Abbreviations ###
parse_abbrev <- function(text, abbrev_crosswalk){
  
  # Step 1) Set up Abbreviation Crosswalk
  names(abbrev_crosswalk) <- paste0("\\b",names(abbrev_crosswalk),"\\b") # Add "\\b" to names of abbrev_parsed (add word boundaries, used for pattern matching).
  
  # Step 2) Set Up Parsed Text Object
  parsed_text <- text
  
  # Step 3) Parse Abbreviations
  for(i in seq_along(abbrev_crosswalk)){
    
    parsed_text <- str_replace_all(parsed_text, 
                                   pattern = regex(names(abbrev_crosswalk)[i], ignore_case = TRUE),
                                   replacement = unname(abbrev_crosswalk)[i])
  }
  
  # Step 3) Return Parsed Text to Global Environment
  return(parsed_text)
}

### Clean Free Text ###
clean_freetext <- function(df, keep_raw = FALSE, parse_abbrev = FALSE, na_label = NA, text_case = "original"){
  
  # Step 0: Create Cleaned Data Frame
  df_clean <- df
  
  # Step 1: Identify the variables present within the data frame.
  all_freetext_cleaning_vars <- c("ChiefComplaintOrig", "ChiefComplaintUpdates", "ChiefComplaintParsed", 
                                  "DischargeDiagnosis", "CCDD", "Admit_Reason_Combo", "TriageNotesOrig")
  
  freetext_vars <- intersect(all_freetext_cleaning_vars, names(df)) # All free text cleaning vars present in df (for cleaning)
  
  # Step 2: (TOGGLE) Keep uncleaned variables too?
  if(keep_raw == TRUE){
    
    df_clean <- df_clean %>%
      mutate(across(.cols = all_of(freetext_vars), 
                    .fns = list(raw = ~.)))
  }
  
  # Step 3: Parse Abbreviations 
  if(parse_abbrev == TRUE){
    
    # ADD ABBREVATIONS TO BE PARSED HERE ("ABBREV" = "PARSED")
    abbreviation_crosswalk <- c("pt" = "patient", 
                                "px" = "pain",
                                "pn" = "pain",
                                "dx" = "diagnosis",
                                "hx" = "history",
                                "fx" = "fracture",
                                "rx" = "prescription",
                                "sx" = "symptom",
                                "sob" = "shortness of breath",
                                "N/V" = "nausea and vomiting",
                                "loc" = "loss of consciousness",
                                "ams" = "altered mental status",
                                "abd" = "abdominal",
                                "glf" = "ground level fall",
                                "c\\/o" = "complains of",
                                "f\\/u" = "follow up",
                                "inj" = "injury",
                                "od" = "overdose",
                                "mvc" = "motor vehicle collision",
                                "mva" = "motor vehicle accident",
                                "lac" = "laceration",
                                "acc" = "accident",
                                "poss" = "possible",
                                "bilat" = "bilateral",
                                "l" = "left",
                                "lt" = "left",
                                "lft" = "left",
                                "r" = "right",
                                "rt" = "right")
    
    df_clean <- df_clean %>%
      mutate(across(
        .cols = all_of(freetext_vars), ~parse_abbrev(.x, abbreviation_crosswalk)
      )) 
  }
  
  # Step 4: Cleaning Steps Applied Equally to All Fields
  
  # Process:
  # 4a) Define Replacement & Removal characters
  # 4b) Replace characters (via replacement_vec)
  # 4c) Remove characters (via removal_vec)
  # 4d) Handle NAs
  # 4e) Trim white space and remove unnecessary punctuation (if applicable)
  # 4f) Convert long spaces to a single space
  # 4g) Adjust case of text (default - mixed, UPPERCASE, lowercase)
  
  # Step 4a: Define Removal and Replacement patterns 
  
  # ADD CHARACTERS TO REPLACE WITH A SPACE (" ") HERE
  replacement_vec <- c(";", 
                       "\\\\", 
                       "\\/", 
                       "\\\\n",
                       "([a-zA-Z])/([\\d])", 
                       "([\\d])/([a-zA-Z])")
  
  # ADD CHARACTERS TO REMOVE HERE
  removal_vec <- c("[[:cntrl:]]", # Control Characters
                   "<BR>", # Linebreaks 
                   "<.*?>", # Remove XML Characters (Problem in TriageNotesOrig) Source: https://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r
                   "\\{[[:digit:]]\\}", # {#} (ChiefComplaintUpdates)
                   "[?.!'+()):@|]") # Any of these punctuation
  
  
  df_clean <- df_clean %>%
    # 4b) Replace Characters (replacement_vec)
    mutate(across(
      .cols = all_of(freetext_vars),
      .fns = ~str_replace_all(., pattern = paste0(replacement_vec, collapse = "|"), replacement = " ")
    )) %>%
    # 4c) Remove Characters (removal_vec)
    mutate(across(
      .cols = all_of(freetext_vars),
      .fns = ~str_remove_all(., pattern = paste0(removal_vec, collapse = "|"))
    )) %>%
    # 4d) Handle NA's (Convert segments to na_label)
    mutate(across(
      .cols = all_of(freetext_vars),
      .fns = ~ifelse(. %in% c(NA, "NA", "none", "NONE") | nchar(.) <= 2, na_label, .)
    )) %>%
    # 4e) Trim extra white space
    mutate(across(
      .cols = all_of(freetext_vars),
      .fns = ~str_trim(., side = "both")
    )) %>%
    # 4f) Remove unnecessarily long spaces (2+ spaces+ --> 1 space)
    mutate(across(
      .cols = all_of(freetext_vars),
      .fns = ~str_replace_all(., pattern = "\\s{2,}", replacement = " ")
    ))
  
  # 4g) (TOGGLE) Adjust case of text
  if(text_case == "upper"){
    
    df_clean <- df_clean %>%
      mutate(across(
        .cols = all_of(freetext_vars),
        .fns = ~str_to_upper(.)
      ))
    
  }else if(text_case == "lower"){
    
    df_clean <- df_clean %>%
      mutate(across(
        .cols = all_of(freetext_vars),
        .fns = ~str_to_lower(.)
      ))
  }
  
  # Return Clean DF 
  return(df_clean)
}