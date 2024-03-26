# =======================================================================
# Title: 1_parameters.R

# Description: 
# Extract parameters from DefinitionInformation.xlsx, pull data, and save
# data as .RData files.

# =======================================================================

# Define params list  -----
params <- list()

# Define Analysis Parameters -----

### Number of Queries to Evaluate
params$n_queries_eval <- n_queries_eval

### Vector of Query Abbreviations to Evaluate
params$queries_abbrev <- DefinitionInformation[["DefinitionInformation"]]$Abbreviation[1:params$n_queries_eval]

### Dates & Geographies
params$start_date <- DefinitionInformation[["Setup"]]$StartDate %>% as.Date()
params$end_date <- DefinitionInformation[["Setup"]]$EndDate %>% as.Date()
params$jurisdiction <- DefinitionInformation[["Setup"]]$Jurisdiction

### ESSENCE Fields to Pull & Analyze

#### All Fields to Pull in API
params$fields <-  DefinitionInformation[["SelectFields"]] %>%
  mutate(includeAPI = ifelse(IncludeForValidationReview =="Yes"| IncludeForTextAnalysis =="Yes", "Yes", "No")) %>%
  filter(includeAPI == "Yes")

#### All Fields (as API URL)
params$fields_api_url <- params$fields %>%
  pull(ESSENCEfields) %>%
  paste0(., collapse = "&field=") %>%
  paste0("&field=",.)

#### Text Mining
params$fields_text_analysis <- params$fields %>%
  filter(IncludeForTextAnalysis =="Yes" & (is.na(Notes)| str_detect(Notes, "Not eligible", negate=TRUE))) %>% # Some fields are not eligible for text mining (see Notes)
  pull(ESSENCEfields)

#### Validation Review (Line-Level Records)
params$fields_validation_review <- params$fields %>%  
  filter(IncludeForValidationReview == "Yes") %>%
  pull(ESSENCEfields)

### Data Cleaning
params$deduplicate_ddx <- DefinitionInformation[["ValidationReviewInformation"]] %>% pull_no_na(df=., variable = "DeduplicateDDx")


### Validation Review
params$validation_review$enable_validation_review <- DefinitionInformation[["Setup"]]$ValidationReview

if(params$validation_review$enable_validation_review == TRUE){
  
  ## Identify # of Reviewers
  params$validation_review$n_reviewers <- DefinitionInformation[["ValidationReviewInformation"]]$ReviewerID %>% max()
  
  ## Identify Definitions to be Reviewed (def1, def2, def3)
  params$validation_review$validation_review_defs <- DefinitionInformation[["DefinitionInformation"]] %>%
    slice_head(., n = params$n_queries_eval) %>%
    pull(defX)
  
  
  ## Establish Sampling Parameters
  params$validation_review$SampleMetric <- DefinitionInformation[["ValidationReviewInformation"]] %>% 
    pull_no_na(df=., variable = "SampleMetric")
  
  params$validation_review$SampleValue <- DefinitionInformation[["ValidationReviewInformation"]] %>% 
    pull_no_na(df=., variable = "SampleValue")
  
  params$validation_review$StratifiedSample <- DefinitionInformation[["ValidationReviewInformation"]] %>% 
    pull_no_na(df=., variable = "StratifiedSample")
  
  params$validation_review$StratifiedVariables <- DefinitionInformation[["ValidationReviewInformation"]] %>% 
    pull_no_na(df=., variable = "StratifiedVariables") %>% # Pull StratifiedVariables (remove NAs)
    strsplit(., split = ",") %>% # Split single string "A, B" into a string vector c("A"," B") using the comma
    unlist() %>%  # Unlist (strsplit() puts new vector into a list)
    str_trim(., side="both") # Remove all whitespace on every string in the string vector: c("A", " B") --> c("A","B")
  
  # Establish Review Scale
  params$validation_review$ReviewScaleLow <- DefinitionInformation[["ValidationReviewInformation"]]$ReviewScaleLow[1]
  params$validation_review$ReviewScaleHigh <- DefinitionInformation[["ValidationReviewInformation"]]$ReviewScaleHigh[1]
}


# Create Filepaths  -----

#### Output Folder
params$filepaths$output <- case_when(
  params$n_queries_eval == 1 ~ "Output/OneDef/",
  params$n_queries_eval == 2 ~ "Output/TwoDefs/",
  params$n_queries_eval == 3 ~ "Output/ThreeDefs/")

#### Definition_Overlap
params$filepaths$definition_overlap <- paste0(params$filepaths$output,"Definition_Overlap/")

#### Matched_Elements
params$filepaths$matched_elements <- paste0(params$filepaths$output,"Matched_Elements/")

#### Validation_Review (Base Folder)
if(params$validation_review$enable_validation_review == TRUE){
  params$filepaths$validation_review <- paste0(params$filepaths$output,"Validation_Review/")
}

#### Create Folders
lapply(params$filepaths, dir_create) # Create Filepaths  


#### Validation_Review (Subfolders)

if(params$validation_review$enable_validation_review == TRUE){
  
  for(i in seq_along(params$queries_abbrev)){ # for each definition specified (i) create the following filepaths
    
    # Step 2: Create folders for each stage of a definition's validation review process
    fs::dir_create(paste0(params$filepaths$validation_review, "/",params$queries_abbrev[i],"/Resources"))
    fs::dir_create(paste0(params$filepaths$validation_review, "/",params$queries_abbrev[i],"/1_Reviewed_Data"))
    
    if(params$validation_review$n_reviewers > 1){ # Only create consensus data folders if > 1 reviewer.
      fs::dir_create(paste0(params$filepaths$validation_review, "/",params$queries_abbrev[i],"/2_Consensus_Data"))
    }
    
    # Step 3: Create subfolders for each reviewer examining a definition
    for(j in 1:params$validation_review$n_reviewers){ # for each reviewer specified (j) create reviewer specific subfolder (within each definition being reviewed)
      
      fs::dir_create(paste0(params$filepaths$validation_review, "/",params$queries_abbrev[i],"/1_Reviewed_Data/Reviewer_",j))
    }
  }
}
rm(i, j)