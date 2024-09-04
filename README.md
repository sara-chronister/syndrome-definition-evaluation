# Syndrome Definition Evaluation Toolkit

> [!NOTE]
> If you are planning to use this tool for syndrome development, I would recommed starting with the steps laid out in the [Syndrome Definition Guidance document](https://prod-knowledge-repository.s3-us-gov-west-1.amazonaws.com/references/SDC_Syndrome%20Definition%20Guidance%20document_FINAL.PDF) prior to starting with this tool. This document was developed by members of the NSSP Syndrome Definition Committee with the purpose of providing a recommended protocol for developing and testing a new syndrome definition. This tool is particularly useful and revelant to the "Refining the Syndrome" section of the document.

## Authors:
1. [Sara Chronister](sara.chronister@doh.wa.gov), Washington State Department of Health - Rapid Health Information NetwOrk (RHINO)
2. [Tyler Bonnell](tyler.bonnell@co.snohomish.wa.us), Snohomish County Health Department - Epidemiology & Informatics

## Background:
The purpose of this tool is to allow ESSENCE users to evaluate the data details (line level) results of one, two, or three syndrome definitions at a time. This project produces several important outputs: 
1. An **HTML report** that can be shared with others and opened using either Chrome or Firefox browsers. This document contains no identifiable data, only aggregate results, for security purposes.
2. A **.csv file** for each combination of definitions possible containing a subset of variables important in the manual review process. These files do contain identifiable information and should be handled accordingly.
3. A **.csv file** containing details about the definition elements (codes and terms) that can be used to evaluate the performance of individual elements in query results. These files do contain identifiable information and should be handled accordingly.
4. One or multiple **.xlsx file(s)** containing details about records captured by provided definitions that can be used to facilitate manual reviews and a consensus decision-making process to calculate query performance metrics by one or multiple reviewers. These files do contain identifiable information and should be handled accordingly.

## Instructions:

### 1) Download and set up files:
* Download the project files by clicking the green "Code" button and selecting "Download ZIP".
* Unzip the files to the location that you want the project files to be stored.

### 2) In Excel:
* Open the `"DefinitionInformationTable.xlsx"` file.
* **Follow the instructions for setting up the evaluation in all tabs.** Detailed information about each of the fields is in the top row. **DO NOT CHANGE THE LOCATIONS OF ANY OF THE FIELDS**. The program will fail if you do.
* Keep in mind that if you want to evaluate one definition, you must update the def1 row of the "DefinitionInformation" tab. For evaluation of two definitions, update the def1 and def2 rows, and for three definitions update all rows in the "DefinitionInformation" tab. 
* Save (without changing the name of the file) once you have all the information you need about your definitions.

### 3) In RStudio: 
* In your file location, open the `"SyndromeEvaluation.Rproj"` file.
* To run the tool, open `"run_tool.R"`.

> [!IMPORTANT]  
> * Edit the `n_queries_eval` object in the `"run_tool.R"` code (line 12) to either be `1`, `2`, or `3`. This will determine the number of definitions you are evaluating.
>   + `1` = toolkit evaluates def1, `2` = toolkit evaluates def1 and def2, `3` = toolkit evaluates def1, def2, and def3.
> * **Select all and run the entire code in `"run_tool.R"`. Wait for your report to run. When it is finished it will generate an HTML file that can be opened using any browser (except Internet Explorer) and can be shared with anyone via email, shared folder location, etc.**
>      + To select all the code, you can highlight everything with your mouse, or use the following shortcuts (Windows: ctrl+A, macOS: command+A).
>      + To run the code, you can click the "Run" button (top right corner of the R script window) or use the following shortcuts (Windows: ctrl+Enter, macOS: command+return).

## Accessing Toolkit Results:
* Result files from this tool will be available in the `Output` subfolder, and will be organized by the number of definitions you are evaluating.
   + When a One, Two, or Three definition syndrome definition evaluation is run, their results will be stored in the respective filepaths: `Output/OneDef/`, `Output/TwoDefs/`, `Output/ThreeDefs/`.
   + If a One, Two, or Three definition syndrome evaluation has not yet been run, these filepaths will not yet exist.

### Results Files & Folders
1. `Syndrome Evaluation Report.html` - The **main syndrome definition evaluation report** which includes syndrome definition syntax, volumes of emergency department visits, demographics, and the relative overlap between multiple syndromic definitions.
2. `[INSERT_CUSTOM_NAME].RData` - This `.RData` file will flexibly named by pasting together the abbreviations of the definitions being evaluated by the tool (see example below). It contains all of the data written to the `Definition_Comparison` and `Matched_Elements` subfolders, in a single, easy-to-access file for R users to access. This may be useful for R users to load instead of trying to work with many disparate `.xlsx` files.
   + Example: A 2 syndrome definition evaluation report is run for AIRQUAL (Air Quality-related Respiratory Illness v1) and ILI (Influenza Like Illness), the `.RData` file name would be `AIRQUAL_ILI.RData`.
4. `Definition_Comparison` subfolder - Contains `.xlsx` files of ESSENCE DataDetails records based on the syndrome definition(s) (singular or multiple overlap) they fall under. These may be useful for separate, custom analyses.
5. `Matched_Elements` subfolder - Contains `.xlsx` files of C_BioSense_IDs and multiple 0/1 indicator variables indicating whether individual syndrome definition syntax components were identified within each respective ESSENCE DataDetails record.
6. `Validation_Review` subfolder - Contains multiple files to support manual reviews and a consensus decision-making process to calculate query performance metrics by one or multiple reviewers (see more detail on `Validation Review` below).


## Validation Review:
**This tool also supports a linelist, consensus manual review process (referred to as Validation Review) to calculate performance/accuracy metrics of evaluated syndrome definitions. This section of the toolkit may be most helpful when you are finalizing a syndrome definition, and are hoping to calculate and disseminate performance/accuracy metrics of the syndrome definition to interested stakeholders.**

>[!TIP]
> Validation Review is set to run by default. To turn off the Validation Review, go to the "Setup" tab of `"DefinitionInformationTable.xlsx"` and set the value for Column G Row 3 to to FALSE.

### Instructions:
* After running the syndrome definition evaluation toolkit, navigate to the `Validation_Review` folder at the following filepath `Output/#Defs/Validation_Review/`.
  + `#Defs` refers to the number of syndrome definitions evaluated by the toolkit (i.e., `OneDef`, `TwoDefs`, or `ThreeDefs`).
* Within the `Validation_Review` folder, there will be separate subfolders for each of the syndrome definitions being evaluated. These subfolders will serve as separate Validation Review sandbozes for each syndrome definition, and they will be labelled using the syndrome definition's abbreviation.
  + Labelling Example: Air Quality-related Respiratory Illness v1 --> AIRQUAL 
  + **Users may choose whether they want to conduct Validation Reviews for all, some, or none of the specified definitions -- it is flexible!**
* Click on the Syndrome Definition folder of interest and begin a Validation Review.

> [!NOTE]  
> Example: I run a 2 syndrome definition report for AIRQUAL (Air Quality-related Respiratory Illness v1) and ILI (Influenza Like Illness). I only want to do a Validation Review for ILI (and not AIRQUAL). I will navigate to `Output/TwoDefs/Validation_Review/ILI` which will serve as my "home folder" for Validation Review.
>   + In this scenario, I can leave the `Output/TwoDefs/Validation_Review/AIRQUAL` folder alone as I do not want to conduct a Validation Review for that syndrome definition.
 
    
* Within the syndrome definition folder, navigate to the `1_Reviewer_Data` subfolder. For each reviewer, there will be a separate folder (`Reviewer_#`) and excel file (`Reviewer_#_Data.xlsx`) for their manual linelist review of ESSENCE DataDetails.
  + Reviewer IDs are specified in the `ReviewerID` and `ReviewerName` columns of the `ValidationReviewInformation` tab in `DefinitionInformationTable.xlsx` Excel file.
     
* Within the `Reviewer_#_Data.xlsx` file, each reviewer should assess all records independently and record:
  + Accuracy rating (in the `Review_Rating` column) for each row using the numeric scale set in Columns G and H of the `ValidationReviewInformation` tab in  `DefinitionInformationTable.xlsx` Excel file
  + (*Optional*) Contextual notes for that rating (in the `Notes` column)

* If there is **only 1 manual reviewer**:
  + Run `Validation_Summary.Rmd` found in `Output/#Defs/Validation_Review/DEFINITION_ABBREV` to generate a short report detailling the syndrome definition's performance/accuracy metrics.

* If there are **2 or more manual reviewers**:
  + Run `Validation_Summary_Pre_Consensus.Rmd` found in `Output/#Defs/Validation_Review/DEFINITION_ABBREV` to generate a short report detailing **preliminary performance/accuracy estimates** according to each manual reviewer, **and begin the Consensus Review process to adjudicate records where reviewers had differing `Review_Rating`'s.**
  + Conduct a Consensus Review Process by:
    + Open `Output/#Defs/Validation_Review/DEFINITION_ABBREV/2_Consensus_Data/Consensus_Data.xlsx`.
    + Discuss disagreements between manual reviewers (records where `Agreement = FALSE`).
      + The purpose of this step is to reach consensus on whether a visit is either a `True Positive` or a `False Positive` (rather than a number on a numeric rating scale). 
    + After coming to a consensus agreement for each record:
      + Update the `Review_Category_Consensus` column to indicate the final classification of the record(s).
        + The `Review Category_Consensus` column should only have values of either `True Positive`, or `False Positive`. Note that `Uncertain` is also an option, though it should be used sparingly and will only appear if there is a scale that starts and ends on an odd number, i.e., 1-3 or 1-5.
      + (*Optional*) You may record the conversations and/or information that caused reviewer(s) to change their initial rating in `Note_Consensus` (if this contextual information may be helpful in the future).
  + **The Consensus Review process is complete!** Save and exit `Consensus_Data.xlsx`.
  + Run `Validation_Summary_Post_Consensus.Rmd` to generate a short report detailing finalized consensus **performance/accuracy estimates** of the syndrome definition.
  + A final dataset (with all enter consensus review decisions) will be available at `Output/#Defs/Validation_Review/DEFINITION_ABBREV/2_Consensus_Data/Consensus_Data_Final.xlsx`.
