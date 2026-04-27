# Codes

This folder contains all the codes written and used on the *Hogares Saludables* project. The codes are divided into subfolders according to the order they should be executed for a full replication of the results. The subfolders are as follows:

- **00_get_data:** get the raw survey answers for both the baseline and endline.
- **01_cleaning:** clean the raw baseline data, changing wrongly submitted information (id number, phone number, address, and similar) and deleting some households.
- **02_processing:** build the final dataset by processing each data source. The survey is processed by type of characteristic (demographic, laboral, mental, income, and similar). This information is merged with the data from the interventions, the budget for the intervention (both the one assigned by Argos to the treatment households and the one estimated for the control households) and the geographical information calculated for each household based on their coordinates.
- **03_analysis:** get descriptive statistics, both tables and graphs, for the data. As well as perform some analysis on outcomes.
- **04_estimations:** perform the estimations for both papers: regressions, propensity score matching, differences in differences.
- **05_anonymization:** dataset with anonymized households.
- **06_ai_fotografias:** scripts that use AI models to classify and organize the photographic record of the intervention.
