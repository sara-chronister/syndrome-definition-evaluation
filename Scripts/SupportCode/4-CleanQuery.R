

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

detect_elements <- function(df, text_field, terms, id_field = "C_BioSense_ID", group_name = NULL) {
  
  terms <- stringr::str_to_lower(terms)
  terms_colnames <- stringr::str_replace_all(terms," ",".")
  
  terms_detected_setup <- df %>%
    dplyr::select(id = !!id_field, field = !!text_field) %>%
    dplyr::mutate(field = stringr::str_to_lower(field))
  
  terms_detected_list <- list()
  
  for (i in 1:length(terms)) {
    terms_detected_list[[i]] <- terms_detected_setup %>%
      dplyr::mutate(term = stringr::str_detect(field,terms[i])) %>%
      dplyr::mutate(term = ifelse(term==TRUE,1,0))
    
    names(terms_detected_list[[i]]) <- c(id_field,text_field,paste(terms_colnames[i],"in",text_field,sep="_"))
  }
  
  terms_detected <- purrr::reduce(terms_detected_list,dplyr::full_join) %>%
    dplyr::select(tidyselect::all_of(id_field),tidyselect::everything(),-text_field) %>%
    dplyr::distinct()
  
  group_name <- group_name
  
  if (is.null(group_name)) {
    df_to_return <- dplyr::full_join(df, terms_detected, by = id_field)
  } else {
    group_name_added <- terms_detected
    group_name_added$sumTerm <- terms_detected %>%
      select(tidyselect::contains("in")) %>%
      rowSums()
    group_name_added <- group_name_added %>%
      dplyr::mutate(AnyTerm = ifelse(sumTerm>0,1,0))%>%
      dplyr::select(tidyselect::all_of(id_field), AnyTerm)
    colnames(group_name_added) <- c(id_field, group_name)
    df_to_return <- dplyr::full_join(df, group_name_added, by = id_field) %>%
      dplyr::full_join(terms_detected, by = id_field)
  }
  
  return(df_to_return)
  
}
