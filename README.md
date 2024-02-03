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
* Open the `"DefinitionInformationTable.xlsx"` file
* Fill out all relevant information in all tabs. Detailed information about each of the fields is in the top row. **DO NOT CHANGE THE LOCATIONS OF ANY OF THE FIELDS**. The program will fail if you do. 
* Keep in mind that if you want to evaluate one definition, you must update the def1 row of the "DefinitionInformation" tab. For evaluation of two definitions, update the def1 and def2 rows, and for three definitions update all rows in the "DefinitionInformation" tab. 
* Save (without changing the name of the file) once you have all the information you need about your definitions.

### 3) In RStudio: 
* In your file location, open the `"SyndromeEvaluation.Rproj"` file.
* In RStudio navigate to the Files tab (by default in the bottom right pane) and open the `"SupportCode"` folder.
  + Open the `"3-UserCredentials.R"` file and follow the instructions. Remember to comment out all lines again by adding a # at the beginning of each line when you have finished.
* Open either the `"Evaluation_OneDef.Rmd"`, `"Evaluation_TwoDefs.Rmd"`, or `"Evaluation_OneDef.Rmd"` file (based on the number of queries you wish to evaluate).
  + You can rename any of these files to reflect the definition(s) you are evaluating. This will be the file name of the HTML report generated.
* To run the report, look for a button at the top of the script that says `"Knit"` :yarn:. Click that button and wait for your report to run. When it is finished it will generate an HTML file that can be opened using any browser (except Internet Explorer) and can be shared with anyone via email, shared folder location, etc.

> [!TIP]
> * For subsequent uses, be sure your user credentials (ESSENCE and local, if necessary) are up to date. Update your credentials in the `"SupportCode/2-UserCredentials.R"` file as needed.

> [!IMPORTANT]
> * Result files with be stored in the `Output_` folder corresponding to the `Evaluation_.Rmd` you ran. (Example: If you run `Evaluation_OneDef.Rmd` then result files will be stored in `Output_OneDef`).

### 4) Validation Review:
**This tool also supports a linelist, consensus manual review process (referred to as Validation Review) to estimate accuracy metrics of syndrome definitions.**

>[!TIP]
> Validation Review is toggled on/off in the "ValidationReviewInformation" tab of `"DefinitionInformationTable.xlsx"` (*default setting is on*). If it is toggled on, a `Validation_Review` subfolder will be present within the `Output_` folder for all the `Evaluation_.Rmd` reports you ran.

* Within `Validation_Review` navigate to the syndrome(s) of interest and then to the `1_Reviewed_Data` subfolder. For each reviewer, there will be a separate folder (`Reviewer_#`) and excel file (`Reviewer_1_Data.xlsx`) for their manual linelist review of ESSENCE DataDetails records.
* (Each reviewer) Within the excel file, review all records and record 1) syndrome definition accuracy ratings (in the `Review_Rating` column), and 2) (*Optional*) contextual notes for that rating (in the `Notes` column). 

> [!NOTE]
> `Review_Rating` accepts **numeric values** that denote whether records are "true positives" or accurately reflect the condition of interest. Review scales are defined in the "ValidationReviewInformation" tab of `"DefinitionInformationTable.xlsx"`. Some examples of review scales include:
> - 0-1 (0: False Positive | 1: True Positive)  **Default**
> - 1-3 (1: Unlikely | 2:  Uncertain | 3: Likely) 

* **1 reviewer**, run `Validation_Summary.Rmd` to generate a short report detailing the syndrome definition's accuracy.
* **2+ reviewers**, run `Validation_Summary_Pre_Consensus.Rmd` to generate a short report detailing **preliminary estimates** of the syndrome definition's accuracy, **and to begin the Consensus Review process to ajudicate records where reviewers had differing `Review_Rating`'s.**

* Navigate to `2_Consensus_Data/Consensus_Data.xlsx` to discuss disagreements between reviewers (records where `Agreement = FALSE`). After coming to a consensus, update `Review_Category_Consensus` to indicate the final status of the record(s). (Optional) You may record the conversations and/or information that caused reviewer(s) to change their initial rating in `Note_Consensus` (if this contextual information may be helpful in the future). **The Consensus Review process is complete, save and exit `Consensus_Data.xlsx`**
* Run `Validation_Summary_Post_Consensus.Rmd` to generate a short report detailing **final estimates** of the syndrome definition's accuracy.
___
*For questions, ideas for improvement/collaboration, or attribution, please reach out to <sara.chronister@doh.wa.gov>.*
