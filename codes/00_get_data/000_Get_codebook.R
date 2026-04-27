require(dplyr)
require(readxl)
require(xlsx)
options(encoding = "UTF-8")

source("src/src_aux/Aux_000_Get_Data.R")

### Get diccionario
diccionario_odk("project_documents/HS_ArgosEAFIT_Cuestionario_May2023 - V18.xlsx",
                "HS_Diccionario_Nov2023.xlsx") 

