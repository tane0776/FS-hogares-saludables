rm(list=ls())
library(tidyverse)
source("src/src_aux/Aux_001_Get_RawData.R")

form_ruODK <-"https://v2.proyectamos-odk.com/v1/projects/15/forms/eafit_hs.svc"
xlsform <- "project_documents/HS_ArgosEAFIT_Cuestionario_May2023 - V18.xlsx"

### Prepare lab
factors <- get_xlform(xlsform)$factors
var_lab <- get_xlform(xlsform)$var_lab
  ####
  ####----- Part 2 : Read The File
  ####
  
  ## Get Access to the server
  ruODK::ru_setup(
    svc = form_ruODK,
    un = "jcmunozmora@gmail.com",
    pw = "Helena123-",
    tz = "America/Bogota",
    verbose = FALSE
  )
  
  ## MEtadat
  meta <- ruODK::form_schema(odkc_version = ruODK::get_test_odkc_version())
  
  ### Tables to download
  srv <- ruODK::odata_service_get()
  
  ### Final Data set
  eafit_hs <- get_data_r(srv$name[1])
  # Fix one repeated name
  names(eafit_hs)[75] <- "cv17x"
  eafit_hs <- eafit_hs %>% mutate(hh_id=id)
  
  ## Modulo Mental
  eafit_hs_mental <- get_data_r(srv$name[3]) %>% 
                  mutate(hh_id=submissions_id,
                         ind_adulto=elg_salud,
                         ind_name=elg_nm) %>% 
                        dplyr::select(-id,-submissions_id)
  ## Modulo Caracteristicas
  eafit_hs_carac <- get_data_r(srv$name[2]) %>% 
            mutate(hh_id=submissions_id,ind_id=id,
                          ind_adulto=adulto,
                          ind_name=c1) %>% 
            dplyr::select(-id,-submissions_id)
  rm(factors,var_lab,meta,srv,form_ruODK,xlsform)
  
  eafit_hs <- read_csv("C:/Users/personal/Downloads/eafit_hs.csv")
  eafit_hs_mental <- read_csv("C:/Users/personal/Downloads/eafit_hs-repeat_mental.csv")
  eafit_hs_carac <- read_csv("C:/Users/personal/Downloads/eafit_hs-repeat_caracteristicas.csv")
  
  eafit_hs <- eafit_hs %>% mutate(hh_id=KEY,
                                  PARENT_KEY=KEY)
  
  eafit_hs_mental <-eafit_hs_mental %>% 
    mutate(hh_id=PARENT_KEY,
           ind_adulto=elg_salud,
           ind_name=elg_nm)
  
  eafit_hs_carac <- eafit_hs_carac %>% 
    mutate(hh_id=PARENT_KEY,
           ind_adulto=`ind0-adulto`,
           ind_name=`ind0-c1`)
  
  # Lee las hojas de un archivo Excel en diferentes dataframes
  dictionary_hs<- read_excel("codes/00_obtenencion_datos/Diccionario.xlsx", sheet = "HS")
  dictionary_hs_carac<- read_excel("codes/00_obtenencion_datos/Diccionario.xlsx", sheet = "HS-repeat_caracteristicas") 
  dictionary_hs_mental<- read_excel("codes/00_obtenencion_datos/Diccionario.xlsx", sheet = "Hs-repeat_mental") 

  # Filtra los dataframes para eliminar filas donde las claves estén vacías
  eafit_hs <- eafit_hs %>% filter(hh_id != "")
  eafit_hs_carac <- eafit_hs_carac %>% filter(hh_id != "")
  eafit_hs_mental <- eafit_hs_mental %>% filter(hh_id != "")
  
  # Filtra los dataframes para eliminar filas donde ciertos campos estén vacíos, sean "1" o "2"
  eafit_hs_carac <- eafit_hs_carac[!(eafit_hs_carac$c1 %in% c("","1","2")),]
  
  # Label HH - Level
  c <- as.data.frame(names(eafit_hs))
  c$variable <- c$`names(eafit_hs)`
  mm<- c %>% left_join(dictionary_hs) %>% 
          dplyr::select(variable,new_name) %>%
          dplyr::mutate(new_name=ifelse(is.na(new_name),
                                        variable,new_name))
  names(eafit_hs)[match(mm$variable, names(eafit_hs))] <- mm$new_name
  
  # Define el tratamiento y control basados en la fecha
  eafit_hs$start_p4<- as.Date(eafit_hs$start_p4)
  eafit_hs <- eafit_hs %>% dplyr::filter(start_p4>=as.Date("2023-06-01"))
  eafit_hs <- eafit_hs %>% mutate(tratamiento_control=
                                    ifelse(start_p4<=as.Date("2023-09-11"), 
                                           "1", "0"))
  eafit_hs <- eafit_hs %>% mutate(tratamiento_control=
                                    ifelse(start_p4==as.Date("2023-09-25") & 
                                             start_p10=="21620314", "1",
                                           tratamiento_control )) 
  
  write_rds(eafit_hs,"data/survey/01_originales/raw_HOGARES.rda")
  
  # Labels individual caracteristics
  c <- as.data.frame(names(eafit_hs_carac))
  c$variable <- c$`names(eafit_hs_carac)`
  mm<- c %>% left_join(dictionary_hs_carac) %>% 
    dplyr::select(variable,new_name) %>%
    dplyr::mutate(new_name=ifelse(is.na(new_name),
                                  variable,new_name))
  names(eafit_hs_carac)[match(mm$variable, names(eafit_hs_carac))] <- mm$new_name
  
  # Labels individual caracteristics
  c <- as.data.frame(names(eafit_hs_mental))
  c$variable <- c$`names(eafit_hs_mental)`
  mm<- c %>% left_join(dictionary_hs_mental) %>% 
    dplyr::select(variable,new_name) %>%
    dplyr::mutate(new_name=ifelse(is.na(new_name),
                                  variable,new_name))
  names(eafit_hs_mental)[match(mm$variable, names(eafit_hs_mental))] <- mm$new_name
  
  ### Merge
  eafit_hs_carac <- eafit_hs_carac[!duplicated(eafit_hs_carac[c("hh_id","ind_adulto","ind_name")]),]
  eafit_hs_mental <- eafit_hs_mental[!duplicated(eafit_hs_mental[c("hh_id","ind_adulto","ind_name")]),]
  
  eafit_ind <- eafit_hs_carac %>% left_join(eafit_hs_mental)
  write_rds(eafit_ind,"data/survey/01_originales/raw_INDIVIDUOS.rda")
  
