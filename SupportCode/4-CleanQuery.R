

clean_query_essence <- function(query) {
  
  clean_query <- data.frame(Element = query) %>%
    mutate(Element = str_to_lower(Element)) %>%
    mutate(Element = str_replace_all(Element, ",andnot,\\^.*?\\^|,andnot,\\(.*?\\)|\\|", "")) %>%
    mutate(Element = str_replace_all(Element, "!","^")) %>%
    mutate(Element = str_replace_all(Element,",and,",",or,")) %>%
    mutate(Element = str_replace_all(Element,",or,","|")) %>%
    cSplit(., splitCols = "Element", sep = "|", type.convert = FALSE) %>%
    pivot_longer(cols = starts_with("Element"), values_to = "Element") %>%
    mutate(Element = str_replace_all(Element,"\\[;/ \\]|\\[;/\\]","")) %>%
    mutate(Element = str_replace_all(Element,"\\)|\\(|\\^|,|;|/|\\.","")) %>%
    mutate(Type = case_when(
      str_detect(Element,"v[[:digit:]]") ~ "CCDD Category (see ESSENCE)",
      str_detect(Element,"[[:digit:]]") ~ "Diagnosis Code",
      str_detect(Element, "[[:digit:]]", negate = TRUE) ~ "Syndrome Term")) %>%
    select(-name, `Syndrome Element` = Element, `Element Type` = Type) %>%
    dplyr::distinct()
  
  return(clean_query)
  
}


detect_elements <- function(data, terms, text_field) {
  
  terms_colnames <- str_replace_all(terms," ",".")
  
  terms_detected_setup <- data %>%
    select(EssenceID,field = !!text_field) %>%
    mutate(field = str_to_lower(field),
           TruePositive = "")
  
  terms_detected_list <- list()
  
  for (i in 1:length(terms)) {
    terms_detected_list[[i]] <- terms_detected_setup %>%
      dplyr::mutate(term = str_detect(field,terms[i])) %>%
      dplyr::mutate(term = ifelse(term==TRUE,1,0))
    
    names(terms_detected_list[[i]]) <- c("EssenceID",text_field,"TruePositive",paste("element",terms_colnames[i],sep="_"))
  }
  
  terms_detected <- purrr::reduce(terms_detected_list,full_join) %>%
    select(EssenceID,TruePositive,text_field,everything())
  return(terms_detected)  
  
}
