# Evaluation Report Functions

## Definition Components Tables

create_def_component_tables <- function(df, def_num){
  
  component_table <- df %>%
    select(`Syndrome Element`, `Element Type`, Field, `Matched Visits` = Matches) %>%
    DT::datatable(., 
                  class='hover cell-border stripe',
                  caption = paste0(params$queries[def_num],' Definition Elements (by Number of Matched Records)'),
                  rownames=FALSE, escape = FALSE, 
                  filter = "top", 
                  extensions = c("Buttons", "KeyTable"), # https://rstudio.github.io/DT/extensions.html
                  options = list(
                    pageLength = -1, # show all elements (even those with 0 matched records)
                    # pageLength = nrow_1_match, # Show only elements with at least one matched record!
                    lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')), # Source: https://stackoverflow.com/questions/67484426/how-can-i-show-all-the-entries-with-datatable-in-r-dt-package
                    dom='Blfrt', # Source: https://datatables.net/reference/option/dom
                    keys = TRUE,
                    buttons = list(
                      list(extend = 'colvis', text = 'Filter Columns'),
                      list(extend = 'csv', text = 'Download CSV', filename = paste0(params$queries_abbrev[def_num],' Definition Elements Table')),
                      list(extend = 'excel', text = 'Download Excel', filename = paste0(params$queries_abbrev[def_num],' Definition Elements Table'))
                    )
                  )
                )
  
  return(component_table)
}
