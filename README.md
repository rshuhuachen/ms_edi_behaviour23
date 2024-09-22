# Equity, diversity and inclusion at Behaviour 2023

This is the public repository for all the data, code and figures presented in the manuscript titled "How can we make conferences more inclusive? Lessons from the International Ethological Congress" by Rebecca Chen *et al.* (in prep)

To get the repository to your local device, you can use the command (after ensuring git is installed):
`git clone `
and the project can be accessed easily by opening the `.Rproj` file.

You can find a detailed description of the directory below:

## Data
You will find all data used in the study (both behavioural and survey) in the `data` folder. The `extra_docs` folder contains a summary of the post-congress survey including all questions we asked, the Code of Conduct, the instructions given to the session host if they were part of our experimental manipulation, and the protocol used for collecting data during Q&A sessions.

The `metadata` folder contains a list of all countries presented by attendees at the congress and a list of countries-nationalities for processing our data. It also contains descriptions of the column names used for the clean post-congress data (post_survey_colnames.csv), the behavioural data collected during Q&A sessions in long format (questionasking_colnames.csv) and the behavioural data further processed which was used for analysis (e.g. merging data from multiple observers, questionasking_colnames_analysis.csv).

The `post_survey` folder contains the anonymised post-congress survey data (minus the qualitative feedback to ensure the data is 100% anonymous) and a summary of our qualitative analysis on the open question responses.

The `pre_survey`folder contains the data collected during abstract submission (not mentioned in the manuscript) and during registration.

The `question_asking` folder contains all data collected during Q&A sessions on question asking behaviour. Data collected during plenary sessions was kept separate (combined_session_talk_question_all_long_plenary.RData for raw data; plenary.RData for processed data after manual correction done in plenary_corrected_manual.xlsx). The long dataframe, containing all observations in long format can be found in combined_session_talk_question_all_long_withtreatment.RData, while the processed data used for analysis can be found in question_asking_data_condensed_for_analysis.RData.

## Scripts

You will find all main scripts used for analysis in quarto-format (similar to RMarkdown), which can be found in the sub-directory `./qmd` or on the html page here: 
All additional scripts used for creating additional figures can be found in the `scripts` directory, as well as the IOR analysis. 

All html files are found in `./docs` and results of the analysis are saved as tables in `./results`.

Two scripts can be found under `./scripts/question_asking/` titled 1_import_clean_results.R and 2_select_betweenobservers.R. These scripts are *not* functional in the current directory, as they import files that are not published due to them being raw and not anonymous. However, they are put in this directory as they were used to create the processed data used for analysis, and to be transparent in the way the data were cleaned.

## Plots

All plots created for the manuscript can be found under `./plots`.


