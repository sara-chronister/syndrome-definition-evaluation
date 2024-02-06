# Syndrome Definition Evaluation

## Background:
The purpose of this tool is to allow ESSENCE users to evaluate the data details (line level) results of one, two, or three syndrome definitions at a time. This project produces several important outputs: 
1. An **HTML report** that can be shared with others and opened using either Chrome or Firefox browsers. This document contains no identifiable data, only aggregate results, for security purposes.
2. A **csv file** for each combination of definitions possible containing a subset of variables important in the manual review process. These files do contain identifiable information and should be handled accordingly.
3. A **csv file** containing details about the definition elements (codes and terms) that can be used to evaluate the performance of individual elements in query results. These files do contain identifiable information and should be handled accordingly.
   
> [!NOTE]
> If you are planning to use this tool for syndrome development, I would recommed starting with the steps laid out in the [Syndrome Definition Guidance document](https://prod-knowledge-repository.s3-us-gov-west-1.amazonaws.com/references/SDC_Syndrome%20Definition%20Guidance%20document_FINAL.PDF) prior to starting with this tool. This document was developed by members of the NSSP Syndrome Definition Committee with the purpose of providing a recommended protocol for developing and testing a new syndrome definition. This tool is particularly useful and revelant to the "Refining the Syndrome" section of the document.

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
* In RStudio navigate to the Files tab (by default in the bottom right pane) and open the `"SupportCode"` folder.
  + Open the `"2-UserCredentials.R"` file and follow the instructions. Remember to comment out all lines again by adding a # at the beginning of each line when you have finished.
* Open either the `"Evaluation_OneDef.Rmd"`, `"Evaluation_TwoDefs.Rmd"`, or `"Evaluation_OneDef.Rmd"` file (based on the number of queries you wish to evaluate).
  + You can rename any of these files to reflect the definition(s) you are evaluating. This will be the file name of the HTML report generated.
* To run the report, look for a button at the top of the script that says `"Knit"` :yarn:. Click that button and wait for your report to run. When it is finished it will generate an HTML file that can be opened using any browser (except Internet Explorer) and can be shared with anyone via email, shared folder location, etc.

> [!TIP]
> * For subsequent uses, be sure your user credentials (ESSENCE and local, if necessary) are up to date. Update your credentials in the `"SupportCode/2-UserCredentials.R"` file as needed.

> [!IMPORTANT]
> * Result files with be stored in the `Output_` folder corresponding to the `Evaluation_#Defs.Rmd` you ran. (Example: If you run `Evaluation_OneDef.Rmd` then result files will be stored in `Output_OneDef`).

### 4) Validation Review:
**This tool also supports a linelist, consensus manual review process (referred to as Validation Review) to estimate accuracy metrics of syndrome definitions.**

>[!TIP]
> Validation Review is set to run by default. To turn off the Validation Review, go to the "Setup" tab of `"DefinitionInformationTable.xlsx"` and set the value for Column G Row 3 to to FALSE. 

* In the `Output_#Defs` folder there will be a `Validation_Review` folder
* Within the `Validation_Review` folder, navigate to the syndrome(s) of interest and then to the `1_Reviewed_Data` subfolder. For each reviewer, there will be a separate folder (`Reviewer_#`) and excel file (`Reviewer_#_Data.xlsx`) for their manual linelist review of ESSENCE DataDetails records.
* Within the excel file, each review should review all records independently and record:
  + Accuracy rating (in the `Review_Rating` column) for each row using the numeric scale set in Columns G and H of the `ValidationReviewInformation` tab in the `DefinitionInformationTable` Excel file
  + (*Optional*) contextual notes for that rating (in the `Notes` column)

* **1 reviewer**, run `Validation_Summary.Rmd` (found in `Output_#Defs/ValidationReview/DefX` folder) to generate a short report detailing the syndrome definition's accuracy.
* **2+ reviewers**, run `Validation_Summary_Pre_Consensus.Rmd` (found in `Output_#Defs/ValidationReview/DefX` folder) to generate a short report detailing **preliminary estimates** of the syndrome definition's accuracy, **and to begin the Consensus Review process to ajudicate records where reviewers had differing `Review_Rating`'s.**

* Navigate to `Output_#Defs/ValidationReview/2_Consensus_Data/Consensus_Data.xlsx` to discuss disagreements between reviewers (records where `Agreement = FALSE`).
* After coming to a consensus, update the `Review_Category_Consensus` column to indicate the final status of the record(s). (*Optional*) You may record the conversations and/or information that caused reviewer(s) to change their initial rating in `Note_Consensus` (if this contextual information may be helpful in the future).
  + At this stage of consensus review, the intent is to reach consensus on whether a visit is either a `True Positive` or a `False Positive` rather than a number on a numeric scale, therefore the `Review Category_Consensus` column should only have values of either `True Positive`, or `False Positive`. Note that `Uncertain` is also an option, though it should be used sparingly and will only appear if there is a scale that starts and ends on an odd number, i.e., 1-3 or 1-5. 
* **The Consensus Review process is complete, save and exit `Consensus_Data.xlsx`**
* Run `Validation_Summary_Post_Consensus.Rmd` to generate a short report detailing **final estimates** of the syndrome definition's accuracy.


  ### Output File Inventory

| Folder | File  | Description | 
| ------------- | ------------- | ------------- |
| `SupportCode` | | R scripts containing custom functions **to support loading ESSENCE credentials** as well as pulling/cleaning data for reports. |
| | `DefinitionInformationTable.xlsx`| Define evaluation process parameters and supply the syndrome definitions you wish to evaluate. |
| | `Evaluation_#Defs.Rmd` | R markdown report used to launch entire syndrome validation process. Choose the respective `.Rmd` template based on the number of syndromes you wish to evaluate. |
| | `Evaluation_#Defs.html` | Rendered R markdown report showcasing syndrome syntax, volumes of emergency department visits, and relative overlap between multiple syndromes. |
|`Output_#Defs` | *Filenames reflect syndrome abbrevations*  | Multiple linelist files of ESSENCE DataDetails records based on the syndrome definition(s) (singular or multiple overlap) they fall under. |
|`Output_#Defs/Matched_Elements` | *Filenames reflect syndrome abbreviations* | Mulitple files of C_BioSense_IDs and a matrix of 0/1 variables indicating the syndrome syntax components that were identified within the respective record. | 
|`Output_#Defs/Validation_Review`|*Nested subfolders supporting Validation Review for each syndrome being evaluated*||
|`Output_#Defs/Validation_Review/1_Reviewed_Data`|`Reviewer_#_Data.xlsx`| Contains separate validation review excel files for each reviewer. |
|`Output_#Defs/Validation_Review`|`Validation_Summary.Rmd` (1 reviewer only) or `Validation_Summary_Pre_Consensus.Rmd` (2+ reviewers only)| R Markdown report that calculates syndrome accuracy metrics (1 reviewer: final metrics, 2+ reviewers: preliminary metrics). For 2+ reviewers, it also generates `Consensus_Data.xlsx`. |
|`Output_#Defs/Validation_Review/2_Consensus_Data`|`Consensus_Data.xlsx`| Linelist file that facilitates consensus review/discussion of record(s) with disagreement between reviewers (records that have `Agreement = FALSE`. After coming to a consensus decision, the final status of the record is updated in `Review_Category_Consensus`. |
|`Output_#Defs/Validation_Review`|`Validation_Summary_Post_Consensus.Rmd` (2+ reviewers only)| R Markdown report that calculates final, consensus syndrome accuracy metrics. |

___
*For questions, ideas for improvement/collaboration, or attribution, please reach out to <sara.chronister@doh.wa.gov>.*
