# Syndrome Definition Evaluation Toolkit

## Resources:
> [!NOTE]
> If you are planning to use this tool for syndrome development, I would recommed starting with the steps laid out in the [Syndrome Definition Guidance document](https://prod-knowledge-repository.s3-us-gov-west-1.amazonaws.com/references/SDC_Syndrome%20Definition%20Guidance%20document_FINAL.PDF) prior to starting with this tool. This document was developed by members of the NSSP Syndrome Definition Committee with the purpose of providing a recommended protocol for developing and testing a new syndrome definition. This tool is particularly useful and revelant to the "Refining the Syndrome" section of the document.

## Background:
The purpose of this tool is to allow ESSENCE users to evaluate the data details (line level) results of one, two, or three syndrome definitions at a time. This project produces several important outputs: 
1. An **HTML report** that can be shared with others and opened using either Chrome or Firefox browsers. This document contains no identifiable data, only aggregate results, for security purposes.
2. A **.xlsx file** for each combination of definitions possible containing a subset of variables important in the manual review process. These files do contain identifiable information and should be handled accordingly.
3. A **.xlsx file** containing details about the definition elements (codes and terms) that can be used to evaluate the performance of individual elements in query results. These files do contain identifiable information and should be handled accordingly.
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
>   * `1` = toolkit evaluates def1, `2` = toolkit evaluates def1 and def2, `3` = toolkit evaluates def1, def2, and def3.
> * **Select all and run the entire code in `"run_tool.R"`. Wait for your report to run. When it is finished it will generate an HTML file that can be opened using any browser (except Internet Explorer) and can be shared with anyone via email, shared folder location, etc.**
>      * To select all the code, you can highlight everything with your mouse, or use the following shortcuts (Windows: ctrl+A, macOS: command+A).
>      * To run the code, you can click the "Run" button (top right corner of the R script window) or use the following shortcuts (Windows: ctrl+Enter, macOS: command+return).

## Accessing the Results:
* Result files from this tool will be available in the `Output` subfolder, and will be organized by the number of definitions you are evaluating.
   * When a One, Two, or Three definition syndrome definition evaluation is run, their results will be stored in the respective filepaths: `Output/OneDef/`, `Output/TwoDefs/`, `Output/ThreeDefs/`.
   * If a One, Two, or Three definition syndrome evaluation has not yet been run, these filepaths will not yet exist.

### Validation Toolkit Results
1. `Syndrome Evaluation Report.html` - The **main syndrome definition evaluation report** which includes syndrome definition syntax, volumes of emergency department visits, demographics, and the relative overlap between multiple syndromic definitions.
2. `[INSERT_CUSTOM_NAME].RData` - This `.RData` file will flexibly named by pasting together the abbreviations of the definitions being evaluated by the tool (see example below). It contains all of the data written to the `Definition_Comparison` and `Matched_Elements` subfolders, in a single, easy-to-access file for R users to access. This may be useful for R users to load instead of trying to work with many disparate `.xlsx` files.
   * Example: A 2 syndrome definition evaluation report is run for AIRQUAL (Air Quality-related Respiratory Illness v1) and ILI (Influenza Like Illness), the `.RData` file name would be `AIRQUAL_ILI.RData`.
4. `Definition_Comparison` subfolder - Contains `.xlsx` files of ESSENCE DataDetails records based on the syndrome definition(s) (singular or multiple overlap) they fall under. These may be useful for separate, custom analyses.
5. `Matched_Elements` subfolder - Contains `.xlsx` files of C_BioSense_IDs and multiple 0/1 indicator variables indicating whether individual syndrome definition syntax components were identified within each respective ESSENCE DataDetails record.
6. `Validation_Review` subfolder - Contains multiple files to support manual reviews and a consensus decision-making process to calculate query performance metrics by one or multiple reviewers (see more detail on `Validation Review` below).


## Validation Review:
**This tool also supports a linelist, consensus manual review process (referred to as Validation Review) to calculate accuracy metrics of evaluated syndrome definitions.**

>[!TIP]
> Validation Review is set to run by default. To turn off the Validation Review, go to the "Setup" tab of `"DefinitionInformationTable.xlsx"` and set the value for Column G Row 3 to to FALSE. 
