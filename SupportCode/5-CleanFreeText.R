## Functions to clean different free text variables

library(tidytext);library(dplyr)

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
    mutate(Admit_Reason_Combo = str_replace_all(Admit_Reason_Combo, "\.", "")) %>%
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
