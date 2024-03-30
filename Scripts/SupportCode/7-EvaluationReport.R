# Evaluation Report Functions

## Definition Components Tables

create_def_component_tables <- function(df, def_num){
  
  component_table <- df %>%
    DT::datatable(., 
                  class='hover cell-border stripe',
                  caption = paste0(params$queries[def_num],' Definition Elements (by Frequency of Occurrence)'),
                  rownames=FALSE, escape = FALSE, 
                  filter = "top", 
                  extensions = c("Buttons", "KeyTable", "FixedHeader"), # https://rstudio.github.io/DT/extensions.html
                  options = list(
                    pageLength = -1,
                    lengthMenu = list(c(5, 10, 25, 50, -1), c('5','10', '25', '50', 'All')), # Source: https://stackoverflow.com/questions/67484426/how-can-i-show-all-the-entries-with-datatable-in-r-dt-package
                    dom='Bfrtl', # Source: https://datatables.net/reference/option/dom
                    fixedHeader = TRUE,
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
