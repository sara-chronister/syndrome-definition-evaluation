
run_evaluation <- function(n_defs_eval,
                           def_info_table = "DefinitionInformationTable.xlsx",
                           timed = FALSE) {
  
  DefinitionInformation <- multiplesheets(fname = def_info_table)
  
  n_defs_eval <- n_defs_eval
  
  if(n_defs_eval > 3){stop("This tool can only analyze up to 3 syndromes simultaneously. Please change n_defs_eval to a value <= 3.")}
  
  if (timed == TRUE) {
    tictoc::tic("Syndrome Evaluation Toolkit") # Start Timing - Entire Toolkit
  }
  
  ## 1) Parameter Script (No need to edit, pulls information from DefinitionInformation.xlsx)
  source(here::here("Scripts", "1_parameters.R"))
  
  ## 2) Data Pull & Cleaning Script
  source(here::here("Scripts", "2_process_data.R"))
  
  ## 3) Render Evaluation Report
  rmarkdown::render(input = "Scripts/Syndrome Evaluation Report Template.Rmd",
                    output_dir = params$filepaths$output,
                    output_file = paste0("Syndrome Evaluation Report.html"),
                    params = params)
  
  if (timed == TRUE) {
    tictoc::toc() # End Timing - Entire Toolkit
  }
  
}









