# Hogares saludables

This repository contains code and documentation for the *Hogares Saludables* program executed in Colombia by Argos together with EAFIT and MIT. The project evaluates the impact of housing improvements on several socioeconomic outcomes.

## Project overview
The analysis is written mainly in R with a few Python utilities. Each stage of the workflow (data retrieval, cleaning, processing, analysis and estimation) is organised into numbered folders under [`codes/`](codes/README.md).

## Requirements and setup
- **R** version 4.1 or higher.
- **R packages** used across scripts include `tidyverse`, `fixest`, `ranger`, `mice`, `lfe`, `ordinal`, `glue`, `readxl`, `stargazer` and others.
- **Python** 3 with packages `pandas`, `openai`, `Pillow`, `PyMuPDF`, `dotenv` and `tqdm` for the image classification utilities in `codes/06_ai_fotografias`.

Install the R packages with `install.packages()` and Python packages with `pip` or `conda`.

## How to run the code
1. Clone this repository and set your working directory to its root.
2. Install the required R and Python packages.
3. Execute the scripts in each subfolder of [`codes/`](codes/README.md) in numerical order:
   - `00_get_data`
   - `01_cleaning`
   - `02_processing`
   - `03_analysis`
   - `04_estimations`
   - `05_Anonymization`
   - `06_ai_fotografias`
4. Refer to the detailed READMEs in [`codes/`](codes/README.md), [`codes/02_processing`](codes/02_processing/README.md) and [`codes/04_estimations`](codes/04_estimations/README.md) for folder‑specific instructions.

## Repository structure
- **codes/** – scripts to obtain, clean, process and analyse the data.
- **data/** – raw and processed datasets (not distributed here).
- **documentation/** – dictionaries and literature review material.
- **papers/** – LaTeX slides and manuscripts.
- **results/** – output tables and figures from the estimations.

## Data availability
The original household survey data are confidential and therefore not included in this repository. Only derived or anonymised examples are provided.

## License
All source code in this repository is released under the [MIT License](LICENSE).

## Contributors
- [Juan Carlos Muñoz-Mora](https://github.com/jcmunozmora)
- Albert Saiz
- Gustavo García
- Daniela Mejía
- [Juliana Lalinde-Velásquez](https://github.com/julianalalindev)

## Contact
Questions about the files can be directed to <jmunozm1@eafit.edu.co>.
