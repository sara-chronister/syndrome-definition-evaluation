

clean_query_essence <- function(query) {
  
  clean_query <- data.frame(Element = query) %>%
    mutate(Element = str_to_lower(Element)) %>%
    mutate(Element = str_replace_all(Element, ",andnot,\\^.*?\\^|,andnot,\\(.*?\\)|\\|", "")) %>%
    # mutate(Element = ifelse(str_detect(Element, "[[:alnum:]]\\^[[:alnum:]]"), str_replace(Element, "")))
    mutate(Element = str_replace_all(Element, "(?<=[[:alnum:]])\\^(?=[[:alnum:]])", ",or,")) %>%
    mutate(Element = str_replace_all(Element, "!","^")) %>%
    mutate(Element = str_replace_all(Element,",and,",",or,")) %>%
    mutate(Element = str_replace_all(Element,",or,","|")) %>%
    mutate(Element = str_replace_all(Element, "_", "[.]")) %>%
    cSplit(., splitCols = "Element", sep = "|", type.convert = FALSE) %>%
    pivot_longer(cols = starts_with("Element"), values_to = "Element") %>%
    mutate(Element = str_replace_all(Element,"\\[;/ \\]|\\[;/\\]","")) %>%
    mutate(Element = str_replace_all(Element,"\\)|\\(|\\^|,|;|/|(?<!\\[)\\.","")) %>%
    mutate(Type = case_when(
      str_detect(Element," v[[:digit:]]") ~ "CCDD Category (see ESSENCE)",
      str_detect(Element,"[[:digit:]]") ~ "Diagnosis Code",
      str_detect(Element, "[[:digit:]]", negate = TRUE) ~ "Syndrome Term")) %>%
    select(-name, `Syndrome Element` = Element, `Element Type` = Type) %>%
    dplyr::distinct()
  
  return(clean_query)
  
}

## Create 0/1 Indicators for the Presence of Definition Elements in Fields

detect_elements <- function(data, terms, text_field) {
  
  terms_colnames <- str_replace_all(terms," ",".")
  
  terms_detected_setup <- data %>%
    select(C_BioSense_ID, field = !!text_field) %>%
    mutate(field = str_to_lower(field))
  
  terms_detected_list <- list()
  
  for (i in seq_along(terms)) {
    
    terms_detected_list[[i]] <- terms_detected_setup %>%
      mutate(term = str_detect(field,terms[i]),
             term = ifelse(term==TRUE, 1,0))
    
    names(terms_detected_list[[i]]) <- c("C_BioSense_ID",text_field,
                                         paste0(text_field,"_",terms_colnames[i]))
  }
  
  terms_detected <- purrr::reduce(terms_detected_list, full_join) %>%
    select(C_BioSense_ID, text_field, everything()) %>%
    distinct()
  
  return(terms_detected)  
}
