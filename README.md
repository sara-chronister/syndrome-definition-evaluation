# Syndrome Definition Evaluation

## Background:

The purpose of this tool is to allow ESSENCE users to evaluate the data details (line level) results of one, two, or three syndrome definitions at a time. This project produces several important outputs: 
* An HTML file that can be shared with others and opened using either Chrome or Firefox browsers. This document contains no identifiable data, only aggregate results, for security purposes. 
* A csv file for each combination of definitions possible containing a subset of variables important in the manual review process. These files do contain identifiable information and should be handled accordingly. 
* A csv file containing details about the definition elements (codes and terms) that can be used to evaluate the performance of individual elements in query results. These files do contain identifiable information and should be handled accordingly. 

If you are planning to use this tool for syndrome development, I would recommed starting with the steps laid out in this document prior to starting with this tool: https://kr-drupal.syndromicsurveillance.org/developing-evaluating-and-disseminating-definitions-syndromic-surveillance-public-health-practice.

## Instructions:

### Download and set up files:
* Download the project files by clicking the green "Code" button and selecting "Download ZIP".
* Unzip the files to the location that you want the project files to be stored.

### In Excel:
* Open the "DefinitionInformationTable.xlsx" file
* Fill out all relevant information in all tabs. Detailed information about each of the fields is in the top row. DO NOT CHANGE THE LOCATIONS OF ANY OF THE FIELDS. The program will fail if you do. 
* Keep in mind that if you want to evaluate one definition, you must update the def1 row of the "DefinitionInformation" tab. For evaluation of two definitions, update the def1 and def2 rows, and for three definitions update all rows in the "DefinitionInformation" tab. 
* Save (without changing the name of the file) once you have all the information you need about your definitions.

### In RStudio: 

#### First time use:  
* In your file location, open the "SyndromeEvaluation.Rproj" file.
* In RStudio navigate to the Files tab (by default in the bottom right pane) and open the "SupportCode" folder.
  + Open the "3-UserCredentials.R" file and follow the instructions. **Remember to comment out all lines again by adding a # at the beginning of each line when you have finished.** 
* Open either the "Evaluation_OneDef.Rmd", "Evaluation_TwoDefs.Rmd", or "Evaluation_OneDef.Rmd" file.
  + You can rename any of these files to reflect the definition(s) you are evaluating. This will be the file name of the HTML report generated.
* To run the report, look for a button at the top of the script that says "Knit" with an icon that looks like a needle and ball of yarn next to it. Click that button and wait for your report to run. When it is finished it will generate an HTML file that can be opened using any browser (except Internet Explorer) and can be shared with anyone via email, shared folder location, etc.

#### Subsequent uses: 
* Be sure your user credentials (ESSENCE and local, if necessary) are up to date. Update them in the "SupportCode/2-UserCredentials.R" file as needed. 
* Run the report using the "Knit" button and share the files as necessary and appropriate.

___
*For questions, ideas for improvement/collaboration, or attribution, please reach out to <sara.chronister@doh.wa.gov>.*
