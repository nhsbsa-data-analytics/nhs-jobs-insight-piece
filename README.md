# int644-jobs-data-insight-piece

The insight piece blog can be found [here](https://nhsbsa-data-analytics.github.io/nhs-jobs-insight-piece/)

## Folder Structure

The NHS Jobs insight piece focuses on extracting technical skills from job descriptions for data profressional roles. We focus on Data Scientists, Data Engineers and Data Analysts. We identify the skills and cluster them into groups of similar roles. This could benefit the development of the National Competency Framework and aid in the recruitment and retention of Digital, Data, and Technology staff.

```
int644-jobs-insight-piece
├───R ............................
│   ├───markdown ................
|   |   ├───int644_insight_piece.qmd # The quarto document that creates the final report.
│   ├───skills_analysis ................
|   |   ├───data_loading.R # R script to load in all of the data
|   |   ├───functions.R # Script that contains all of the functions to analyse the data.
|   |   ├───load_packages.R # Set up script to load in all of the packages.
|   |   ├───preamble.R # Set up script to assign parameters.
|   |   ├───single_job_title.R # R script that computes the similarity between vacancies and clusters them into groups.
│   ├───text_processing ................
|   |   ├───extract_text.R # Extract text from the database.
|   |   ├───functions.R # Functions to help extract the skills from the text.
|   |   ├───identify_skills.R # R script to identify the skills within the text.
|   |   ├───load_packages.R # Set up script to load in all of the packages.


├───SQL ............................
│   ├───INT644_BASE__TABLE_PROCESS_2122.sql # Initial script to create a base table within the database for the 21/22 financial year data.
│   ├───INT644_BASE__TABLE_PROCESS_2223.sql # Initial script to create a base table within the database for the 22/23 financial year data.
│   ├───int644_jd_data_dp.sql # Script to combine the data tables together and filter to get the data professional roles only.

├───docs ............................
|   |index.html # The final report in html format. Open in a browser to view all of the features.


```

## Scripts

Scripts are run in the following order to create the final project report.

<br>

The following scripts can be found in the SQL folder.

1. **INT644_BASE_TABLE_PROCESS_2122.sql**
    - Creates the base table for the 21/22 fiscal year containing metadata for all of the vacancies. Also identifies technical roles based on matching criteria using in-house dictionaries.

  <br>  

2. **INT644_BASE_TABLE_PROCESS_2223.sql**
    - Creates the base table for the 22/23 fiscal year containing metadata for all of the vacancies.

    <br>

3. **int644_jd_data_dp.sql**
    - Creates a table containing job description binaries for the vacancies detected as technical and those that have the data in the correct format, i.e. PDF or DOCX

    <br>

The following scripts can be found in the R folder

4. **/text_processing/extract_text.R**
    - Pulls the file binaries from the database, converts them into a PDF or DOCX and then extracts out the text.

    <br>

5. **/text_processing/identify_skills.R**
    - Loads in the extracted text and identifies techincal skills based on the in-house dictionaries.
    
    <br>

The following generates the blog and figures.

6. **/markdown/int644_insight_piece.qmd**
    - Use this to reproduce the blog for int644




