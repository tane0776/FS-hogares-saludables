# 02_processing

This folder contains all the codes used to process the data from multiple sources and create a dataset with all the necessary information. The scripts should be run after the cleaning stage and before any analysis or estimation. The specific content of each file is as follows:

- **010_Data_prep.R:** merges the processed survey information with the intervention, budget and geographic datasets to build a single working file for later analysis.

The additional codes that process the variables by type are as follows:
- **003a_Vars_demograficas.R:** processes the individual survey and generates age, sex, race and schooling variables. This variables are then collapse into household level with different categories (level of schooling, age group, kinship). A set of additional variables that give the sex, age and schooling level of the household head.
- **003b_Vars_laborales.R:** constructs employment variables such as unemployment status, contract type and social security coverage from the individual survey and aggregates them to the household level.

There are three additional folders that contain the following codes:
- **old:** has additional codes that were used at some point but are no longer relevant to the current estimations. This includes different specifications for the regressions and lasso reduction.
- **src-aux:** contains one additional code that creates a general balance table and is used as a table template for the estimation codes.
