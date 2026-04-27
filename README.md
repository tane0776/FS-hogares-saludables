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

## Food security extension
This repository extends the original *Hogares Saludables* analysis with a food security outcome, developed as a thesis project at EAFIT.

**What was added.** A household‑level food security index (FSI) was constructed from 12 survey items covering water and food storage access, nine coping‑strategy questions, and an income‑sufficiency measure. The index is estimated using a Generalised Partial Credit Model (IRT) via the `mirt` package, producing a standardised score FSI\_theta ~ N(0,1) used in regressions and a rescaled FSI ∈ \[0,1\] for reporting.

**How to identify the extension.** All scripts introduced for the food security thesis are contained in `food_security/` subfolders within each stage of the pipeline:

- [`codes/02_processing/food_security/`](codes/02_processing/food_security/) – FSI construction and FS final dataset.
- [`codes/03_analysis/food_security/`](codes/03_analysis/food_security/) – descriptive statistics for FSI.
- [`codes/04_estimations/paper 2/food_security/`](codes/04_estimations/paper%202/food_security/) – PSM, DiD and balance tables for the FS outcome.

Everything else in the repository belongs to the original project and was not modified.

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
