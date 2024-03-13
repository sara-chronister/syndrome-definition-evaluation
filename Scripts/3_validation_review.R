# =======================================================================
# Title: 3_validation_review.R

# Description: 
# Pull, clean, and save data as .RData files within respective 
# output files.

# =======================================================================



if(validation_review == TRUE){
  
  ## Create Validation Review File Directories
  
  for(i in seq_along(validation_review_defs)){ # for each definition specified (i) create the following filepaths
    
    # Step 1: Create folder name for each definition undergoing validation review
    def_review_folder <- name_def_review_folder(definition = validation_review_defs[i])
    
    # Step 2: Create folders for each stage of a definition's validation review process
    fs::dir_create(paste0(output_folder, "Validation_Review\\",def_review_folder,"\\Resources"))
    fs::dir_create(paste0(output_folder, "Validation_Review\\",def_review_folder,"\\1_Reviewed_Data"))
    
    if(n_reviewers > 1){ # Only create consensus data folders if > 1 reviewer.
      fs::dir_create(paste0(output_folder, "Validation_Review\\",def_review_folder,"\\2_Consensus_Data"))
    }
    
    # Step 3: Create subfolders for each reviewer examining a definition
    for(j in 1:n_reviewers){ # for each reviewer specified (j) create reviewer specific subfolder (within each definition being reviewed)
      
      fs::dir_create(paste0(output_folder, "Validation_Review\\",def_review_folder,"\\1_Reviewed_Data\\Reviewer_",j))
    }
  }
  rm(def_review_folder)
}
```