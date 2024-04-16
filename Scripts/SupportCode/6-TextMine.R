# Text Analysis

## Text Mine
textmine <- function(df, variables, n_grams, 
                     add_stop_words = NULL, 
                     top_n = 10, top_n_by_variable = TRUE, metric = "n"){
  
  # Resources Used to Build This Function:
  #0 : Suppress R Warnings: https://www.statology.org/suppress-warnings-in-r/
  # 1) For splitting n_grams flexibly into multiple pieces/columns for stop word filtering: https://stackoverflow.com/questions/18641951/splitting-a-dataframe-string-column-into-multiple-different-columns
  # 2) For filtering across many columns flexibly via filter(if_all()) -- see 35.4: https://www.r4epi.com/column-wise-operations-in-dplyr.html#across-with-filter
  # 3) For uniting a flexible number of n_gram broken segments back together (after stop word filtering) for text mining:  https://stackoverflow.com/questions/57539535/unite-columns-of-dataframe-by-using-wildcards-in-r
  # 4) For text mining workflow (specifically bind_tf_idf()): https://bookdown.org/Maxine/tidy-text-mining/tf-idf.html
  # 5) For creating function parameters which can be missing/not specified (top_n parameter): https://stackoverflow.com/questions/28370249/correct-way-to-specifiy-optional-arguments-in-r-functions
  # 6) For calling variables using a dplyr notation (ie.; df %>% select(var,...) instead of df %>% select("var",...); for metric parameter): https://stackoverflow.com/questions/48219732/pass-a-string-as-variable-name-in-dplyrfilter
  
  
  suppressWarnings({ ## Remove R Console Warnings even running the function
    
    ## Subset to Selected Free-Text variables
    df_tm <- df[,variables]
    
    ## Prepare Additional Stop Words for Filtering
    if(!missing(add_stop_words)){
      add_stop_words <- stringr::str_to_lower(add_stop_words) # Additional stop words to lower case (all n_grams are lower case in filtering step)
    }
    
    ## Unigrams ##
    if(n_grams == 1){
      
      df_tm <- df_tm %>%
        tidyr::pivot_longer(everything(), names_to = "Field", values_to = "free_text") %>% # All free-text variables pivoted to long format
        tidytext::unnest_tokens(tbl = .,
                                output = n_gram,
                                input = free_text,
                                token = "ngrams",
                                n = n_grams) %>% # Tokens extracted from free-text variables
        filter(
          !is.na(n_gram) & !n_gram %in% c("NA","na") # Remove NA's
          & !str_detect(n_gram, "\\b[:digit:]+\\b") # Remove individual numbers (that are not part of words/other text segments)
          & !n_gram %in% tidytext::stop_words$word  # Filter out tidytext package stop words. 
          & !n_gram %in% stringr::str_to_lower(Rnssp::rnssp_stopwords$word)) # Filter out Rnssp package stop words
      
      if(!missing(add_stop_words)){ # If add_stop_words is defined -- filter out n_grams by additional stop words.
        
        df_tm <- df_tm %>%
          filter(!n_gram %in% add_stop_words)
      }
      
      df_tm <- df_tm %>%
        mutate(n_gram = str_to_upper(n_gram),
               n_gram = str_remove_all(n_gram, pattern = "[[:punct:]]")) %>% # Convert N-grams back to all caps with no punctuation
        add_count(Field, name = "total_ngrams") %>% # Add total unique N-grams present "total_ngrams" (within each free-text field)
        count(Field, total_ngrams, n_gram, sort = TRUE) %>% # Get frequency counts "n" for each n-gram
        tidytext::bind_tf_idf(document = Field, 
                              term = n_gram, 
                              n= n) %>% # add tf, idf, tf_idf vars
        select(Field, n_gram, n, total_ngrams, tf, idf, tf_idf) # Re-order variables
      
      
      ## Bigrams,Trigrams,etc ##  
    }else if(n_grams > 1){
      
      df_tm <- df_tm %>%
        tidyr::pivot_longer(everything(), names_to = "Field", values_to = "free_text") %>%
        tidytext::unnest_tokens(tbl = .,
                                output = n_gram,
                                input = free_text,
                                token = "ngrams",
                                n = n_grams) %>%
        splitstackshape::cSplit(., "n_gram", " ") %>% # Flexibly split up n-gram into separate columns (for filtering of stop words). Source: https://stackoverflow.com/questions/18641951/splitting-a-dataframe-string-column-into-multiple-different-columns
        filter(  # Equivalent to mutate(across()). Source (35.4: Across with Filter: https://www.r4epi.com/column-wise-operations-in-dplyr.html)
          dplyr::if_all(
            .cols = contains("n_gram"),
            .fns = ~(!is.na(.x) 
                     & !.x %in% c("NA","na")
                     & !str_detect(.x, "\\b[:digit:]+\\b") # Remove individual numbers (that are not part of words/other text segments)
                     & !.x %in% tidytext::stop_words$word
                     & !.x %in% stringr::str_to_lower(Rnssp::rnssp_stopwords$word))
          )) # Conduct filter step flexibly across all n-gram broken (unigram) segments
      
      
      if(!missing(add_stop_words)){
        
        df_tm <- df_tm %>%
          filter(  # Equivalent to mutate(across()). Source (35.4: Across with Filter: https://www.r4epi.com/column-wise-operations-in-dplyr.html)
            dplyr::if_all(
              .cols = contains("n_gram"),
              .fns = ~(!.x %in% add_stop_words))
          )
      }
      
      df_tm <- df_tm %>%
        mutate( # Convert all broken n-gram (unigram) segements to all caps and no punctuation
          across(
            .cols = contains("n_gram"),
            .fns = ~ str_remove_all(str_to_upper(.x), pattern = "[[:punct:]]")
          )) %>%
        tidyr::unite(data =., col = n_gram, contains("n_gram"), sep = " ") %>% # Recombine broke n-gram (unigram) segements back together into the respective n-gram. Source (Unite with dplyr helper functions): https://stackoverflow.com/questions/57539535/unite-columns-of-dataframe-by-using-wildcards-in-r
        add_count(Field, name = "total_ngrams") %>%
        count(Field, total_ngrams, n_gram, sort = TRUE) %>%
        tidytext::bind_tf_idf(document = Field, 
                              term = n_gram, 
                              n= n) %>%
        select(Field, n_gram, n, total_ngrams, tf, idf, tf_idf)
    }
    
    
    ## (OPTIONAL) Order N-Grams by Term Frequency (High to Low), Grouped by variables. Additionally, subset to the top_n n-grams within each field.
    ## Arrange n_grams (highest to lowest) by specific metric: n, tf, tf_idf, etc.
    
    if(top_n_by_variable == TRUE){ # 
      
      df_tm <- df_tm %>%
        group_by(Field)
      
      if(!missing(top_n)){
        
        df_tm <- df_tm %>%
          slice_max(., !!sym(metric), n = top_n, with_ties = FALSE) %>%
          arrange(desc(!!sym(metric)), .by_group = TRUE)
      }
      
      df_tm <- df_tm %>%
        ungroup()
    }
    
    ## (OPTIONAL) Extract the Top # N-Grams for ALL variables
    if(top_n_by_variable == FALSE & !missing(top_n)){
      
      df_tm <- df_tm %>%
        slice_max(., !!sym(metric), n = top_n, with_ties = FALSE) %>%
        arrange(desc(!!sym(metric)))
      
    }
    
    ## Return df_tm
    return(df_tm)
    
  })
}

## Bind Text Mining Results Together Into 1 Summary Data Frame
bind_results_tm <- function(listDF, ngram){
  
  results <- bind_rows(listDF, .id = "def") %>%
    mutate(def = factor(def, levels = params$queries_abbrev),
           unit = ngram) %>% # To preserve ordering of defs (avoid alphabetic ordering)
    group_by(def, Field) %>%
    mutate(order = row_number(),
           order = forcats::fct_rev(as.factor(order))) %>%
    select(def, field = Field, order, everything())
  
  return(results)
}



## Plot Text Mining Outputs
plot_tm <- function(df){
  
  df %>%
    ggplot(data = .) +
    geom_bar(aes(x = n, y = order), stat="identity", fill = "#a4dba4") +
    geom_text(aes(x = 0, y = order, label = paste0(n_gram,'  (',n,")"), hjust = "left"), size = 3) + 
    facet_grid(field ~ def, switch = "y", scales = "free") +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      panel.grid = element_blank(),
      strip.text.y.left = element_text(angle = 0),
      panel.background = element_rect(fill = NA, color = "grey40"),
      axis.title = element_blank())
  
}
