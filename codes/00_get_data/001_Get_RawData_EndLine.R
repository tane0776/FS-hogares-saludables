rm(list=ls())
library(tidyverse)
library(readxl)
library(haven)
library(Hmisc)

eafit_hs <- read_csv("data/survey/00_endline/eafit_hs_endline.csv")
eafit_hs_endline_DMT <- read_dta("C:/Users/personal/Downloads/eafit_hs_endline_DMT.dta")
colnames(eafit_hs_endline_DMT) <- c(colnames(eafit_hs),"QUITAR")
eafit_hs <- eafit_hs_endline_DMT
eafit_hs$QUITAR[is.na(eafit_hs$QUITAR)] <- 0
eafit_hs_mental <- read_csv("data/survey/00_endline/eafit_hs_endline-repeat_mental.csv")
eafit_hs_carac <- read_csv("data/survey/00_endline/eafit_hs_endline-repeat_caracteristicas.csv")

eafit_hs <- eafit_hs[eafit_hs$KEY != "uuid:339566d9-19ad-405c-8504-a856c5d8f548",]
eafit_hs_carac <- eafit_hs_carac[eafit_hs_carac$PARENT_KEY != "uuid:339566d9-19ad-405c-8504-a856c5d8f548",]
eafit_hs_mental <- eafit_hs_mental[eafit_hs_mental$PARENT_KEY != "uuid:339566d9-19ad-405c-8504-a856c5d8f548",]

eafit_hs <- eafit_hs[eafit_hs$KEY != "uuid:1f9cf54e-e561-44a1-98d7-2736eefcdae1",]
eafit_hs_carac <- eafit_hs_carac[eafit_hs_carac$PARENT_KEY != "uuid:1f9cf54e-e561-44a1-98d7-2736eefcdae1",]
eafit_hs_mental <- eafit_hs_mental[eafit_hs_mental$PARENT_KEY != "uuid:1f9cf54e-e561-44a1-98d7-2736eefcdae1",]

eafit_hs <- eafit_hs[eafit_hs$KEY != "uuid:e53b5bed-b278-4e06-84c9-979e0cbbd6c8",]
eafit_hs_carac <- eafit_hs_carac[eafit_hs_carac$PARENT_KEY != "uuid:e53b5bed-b278-4e06-84c9-979e0cbbd6c8",]
eafit_hs_mental <- eafit_hs_mental[eafit_hs_mental$PARENT_KEY != "uuid:e53b5bed-b278-4e06-84c9-979e0cbbd6c8",]

eafit_hs <- eafit_hs[eafit_hs$KEY != "uuid:3df536b5-d701-4149-b504-20fe435bc1e5",]
eafit_hs_carac <- eafit_hs_carac[eafit_hs_carac$PARENT_KEY != "uuid:3df536b5-d701-4149-b504-20fe435bc1e5",]
eafit_hs_mental <- eafit_hs_mental[eafit_hs_mental$PARENT_KEY != "uuid:3df536b5-d701-4149-b504-20fe435bc1e5",]

eafit_hs <- eafit_hs[eafit_hs$KEY != "uuid:324614fa-ce39-4fcf-a5a9-24a60805d0a1",]
eafit_hs_carac <- eafit_hs_carac[eafit_hs_carac$PARENT_KEY != "uuid:324614fa-ce39-4fcf-a5a9-24a60805d0a1",]
eafit_hs_mental <- eafit_hs_mental[eafit_hs_mental$PARENT_KEY != "uuid:324614fa-ce39-4fcf-a5a9-24a60805d0a1",]

quitar <- eafit_hs$KEY[eafit_hs$SubmitterName == "Daniela MejÃ­a" |
                            eafit_hs$SubmitterName == "prueba" |
                            eafit_hs$SubmitterName == "Prueba de escritorio_dmt" |
                            eafit_hs$SubmitterName == "prueba escritorio_dmt" |
                            eafit_hs$SubmitterName == "prueba_dmt" |
                            eafit_hs$SubmitterName == "Prueba_dmt_2"]

eafit_hs <- eafit_hs[eafit_hs$KEY %nin% quitar,]
eafit_hs_carac <- eafit_hs_carac[eafit_hs_carac$PARENT_KEY %nin% quitar,]
eafit_hs_mental <- eafit_hs_mental[eafit_hs_mental$PARENT_KEY %nin% quitar,]

#Organizar hogar extra de Medellín que quedó en el piloto
eafit_hs_extra <- read_csv("data/survey/00_endline/eafit_hs_endline_43639779.csv")
eafit_hs_extra <- eafit_hs_extra[eafit_hs_extra$cedula == 43639779,]
eafit_hs_extra$`hs_main-modI-enc3` <- NA
eafit_hs_extra$QUITAR <- 0
eafit_hs <- rbind(eafit_hs,eafit_hs_extra)

eafit_hs_mental_extra <- read_csv("data/survey/00_endline/eafit_hs_endline-repeat_mental_43639779.csv")
eafit_hs_mental_extra <- eafit_hs_mental_extra[eafit_hs_mental_extra$KEY == "uuid:4b66d623-d91d-40df-acb1-07818d21b412/hs_main/repeat_mental[1]",]
eafit_hs_mental <- rbind(eafit_hs_mental,eafit_hs_mental_extra)

eafit_hs_carac_extra <- read_csv("data/survey/00_endline/eafit_hs_endline-repeat_caracteristicas_43639779.csv")
eafit_hs_carac_extra <- eafit_hs_carac_extra[eafit_hs_carac_extra$KEY == "uuid:4b66d623-d91d-40df-acb1-07818d21b412/hs_main/modA/repeat_caracteristicas[1]",]
eafit_hs_carac <- rbind(eafit_hs_carac,eafit_hs_carac_extra)

#organizar información de adultos faltantes
eafit_hs_faltantes <- read_csv("data/survey/00_endline/PERSONAS FALTANTES/OCTUBRE 22/eafit_hs_endline_adultos.csv")

eafit_hs_faltantes <- eafit_hs_faltantes[eafit_hs_faltantes$SubmitterName != "Daniela Mejía",]
eafit_hs_faltantes <- eafit_hs_faltantes[eafit_hs_faltantes$cities != "Medellin",]
eafit_hs_faltantes <- eafit_hs_faltantes[eafit_hs_faltantes$cedula != 38973256 | eafit_hs_faltantes$`A0-A21` != "09:58:00.000-05:00",]
eafit_hs_faltantes <- eafit_hs_faltantes[eafit_hs_faltantes$cedula != 38671487 | eafit_hs_faltantes$`A0-A21` != "09:06:00.000-05:00",]
eafit_hs_faltantes <- eafit_hs_faltantes[eafit_hs_faltantes$cedula != 31972147 | eafit_hs_faltantes$`A0-A21` != "09:48:00.000-05:00",]
eafit_hs_faltantes$QUITAR <- 0

eafit_hs$change_info <- eafit_hs$cedula %in% eafit_hs_faltantes$cedula

eafit_hs_change <- eafit_hs[eafit_hs$change_info == T,]
eafit_hs <- eafit_hs[eafit_hs$change_info == F,]

eafit_hs_change <- eafit_hs_change %>% select(-all_of(c('hs_main-modA-hh_n',
                                                        'hs_main-modA-repeat_caracteristicas_count',
                                                        'hs_main-modA-adultos',
                                                        'hs_main-modA-ninos',
                                                        'hs_main-repeat_mental_count'
                                                        )))

eafit_hs_faltantes <- eafit_hs_faltantes[,c('cedula',
                                            'hs_main-modA-hh_n',
                                            'hs_main-modA-repeat_caracteristicas_count',
                                            'hs_main-modA-adultos',
                                            'hs_main-modA-ninos',
                                            'hs_main-repeat_mental_count',
                                            'hs_main-enc3')]
colnames(eafit_hs_faltantes)[7] <- "hs_main-enc3_extra"

eafit_hs_change <- merge(x = eafit_hs_change, y = eafit_hs_faltantes, by = "cedula")

eafit_hs$`hs_main-enc3_extra` <- ""

eafit_hs <- rbind(eafit_hs,eafit_hs_change)

eafit_hs_mental_faltantes <- read_csv("data/survey/00_endline/PERSONAS FALTANTES/OCTUBRE 22/eafit_hs_endline_adultos-repeat_mental.csv")
eafit_hs_mental_faltantes <- eafit_hs_mental_faltantes[eafit_hs_mental_faltantes$PARENT_KEY != "uuid:0513149d-de68-4912-bc4d-8922463c33f5" & !is.na(eafit_hs_mental_faltantes$elg_nm),]
eafit_hs_mental_faltantes <- eafit_hs_mental_faltantes[eafit_hs_mental_faltantes$PARENT_KEY != "uuid:8bbbc0c6-7c35-4797-a872-c241e8f2dcf5",]
eafit_hs_mental_faltantes <- eafit_hs_mental_faltantes[eafit_hs_mental_faltantes$PARENT_KEY != "uuid:a65ff6d1-fe8e-4eb7-92ad-1724281205ef" | (eafit_hs_mental_faltantes$elg_nm == "Walter Arley" &  eafit_hs_mental_faltantes$elg_salud == 1),]
eafit_hs_mental_faltantes <- eafit_hs_mental_faltantes[eafit_hs_mental_faltantes$PARENT_KEY != "uuid:84a8833c-93d1-4e06-a7b2-97ad3d836de4",]
eafit_hs_mental_faltantes <- eafit_hs_mental_faltantes[eafit_hs_mental_faltantes$PARENT_KEY != "uuid:977b30f0-6429-477c-910f-eed0b29f18c3" | (eafit_hs_mental_faltantes$elg_nm == "Gildardo antonio" &  eafit_hs_mental_faltantes$elg_salud == 1),]
eafit_hs_mental_faltantes <- eafit_hs_mental_faltantes[eafit_hs_mental_faltantes$PARENT_KEY != "uuid:cb563bad-274d-405f-a814-06a2ba3244a3",]
eafit_hs_mental_faltantes <- eafit_hs_mental_faltantes[eafit_hs_mental_faltantes$PARENT_KEY != "uuid:502414e6-338e-48fe-b64f-d943f4c40cd5",]
eafit_hs_mental_faltantes <- eafit_hs_mental_faltantes[eafit_hs_mental_faltantes$PARENT_KEY != "uuid:87864ae3-7f38-4f2b-a6de-0762bfb19dea",]

eafit_hs_mental <- rbind(eafit_hs_mental,eafit_hs_mental_faltantes)

eafit_hs_carac_faltantes <- read_csv("data/survey/00_endline/PERSONAS FALTANTES/OCTUBRE 22/eafit_hs_endline_adultos-repeat_caracteristicas.csv")
eafit_hs_carac_faltantes <- eafit_hs_carac_faltantes[eafit_hs_carac_faltantes$PARENT_KEY != "uuid:0513149d-de68-4912-bc4d-8922463c33f5",]
eafit_hs_carac_faltantes <- eafit_hs_carac_faltantes[eafit_hs_carac_faltantes$PARENT_KEY != "uuid:8bbbc0c6-7c35-4797-a872-c241e8f2dcf5",] 
eafit_hs_carac_faltantes <- eafit_hs_carac_faltantes[eafit_hs_carac_faltantes$PARENT_KEY != "uuid:a65ff6d1-fe8e-4eb7-92ad-1724281205ef" | eafit_hs_carac_faltantes$mem_id == "43643362-5",] 
eafit_hs_carac_faltantes <- eafit_hs_carac_faltantes[eafit_hs_carac_faltantes$PARENT_KEY != "uuid:84a8833c-93d1-4e06-a7b2-97ad3d836de4",] 
eafit_hs_carac_faltantes <- eafit_hs_carac_faltantes[eafit_hs_carac_faltantes$PARENT_KEY != "uuid:977b30f0-6429-477c-910f-eed0b29f18c3" | eafit_hs_carac_faltantes$mem_id == "43566435-5",]
eafit_hs_carac_faltantes <- eafit_hs_carac_faltantes[eafit_hs_carac_faltantes$PARENT_KEY != "uuid:cb563bad-274d-405f-a814-06a2ba3244a3",]
eafit_hs_carac_faltantes <- eafit_hs_carac_faltantes[eafit_hs_carac_faltantes$PARENT_KEY != "uuid:502414e6-338e-48fe-b64f-d943f4c40cd5",]
eafit_hs_carac_faltantes <- eafit_hs_carac_faltantes[eafit_hs_carac_faltantes$PARENT_KEY != "uuid:87864ae3-7f38-4f2b-a6de-0762bfb19dea",]

eafit_hs_carac <- eafit_hs_carac[eafit_hs_carac$mem_id != "43643362-5",]
eafit_hs_carac <- eafit_hs_carac[eafit_hs_carac$mem_id != "43566435-5",]
eafit_hs_carac_faltantes$jefe <- str_sub(eafit_hs_carac_faltantes$mem_id,start = -1)
eafit_hs_carac_faltantes <- eafit_hs_carac_faltantes[eafit_hs_carac_faltantes$jefe != 1,]

eafit_hs_carac_faltantes <- eafit_hs_carac_faltantes %>% select(-all_of(c('jefe')))

eafit_hs_carac <- rbind(eafit_hs_carac,eafit_hs_carac_faltantes)

eafit_hs_carac$PARENT_KEY[eafit_hs_carac$mem_id == "43643362-5"] <- "uuid:0928c02d-8061-4ca3-8a82-faecbf2eda56"
eafit_hs_carac$PARENT_KEY[eafit_hs_carac$mem_id == "43566435-5"] <- "uuid:1790ba42-d7a2-49a9-89a7-76a992865db1"

eafit_hs <- eafit_hs %>% mutate(hh_id=KEY,
                                PARENT_KEY=KEY)

eafit_hs_mental <-eafit_hs_mental %>% 
                  mutate(hh_id=PARENT_KEY,
                        ind_adulto=elg_salud,
                        ind_name=elg_nm)

eafit_hs_carac <- eafit_hs_carac %>% 
                  mutate(hh_id=PARENT_KEY,ind_id=mem_id,
                          ind_adulto=`ind0-adulto`,
                          ind_name=`ind0-c1`)

  # Lee las hojas de un archivo Excel en diferentes dataframes
  dictionary_hs<- read_excel("data/survey/00_endline/Diccionario_endline.xlsx", sheet = "HS")
  dictionary_hs_carac<- read_excel("data/survey/00_endline/Diccionario_endline.xlsx", sheet = "HS-repeat_caracteristicas") 
  dictionary_hs_mental<- read_excel("data/survey/00_endline/Diccionario_endline.xlsx", sheet = "Hs-repeat_mental") 

  # Filtra los dataframes para eliminar filas donde las claves estén vacías
  eafit_hs <- eafit_hs %>% filter(hh_id != "")
  eafit_hs_carac <- eafit_hs_carac %>% filter(hh_id != "")
  eafit_hs_mental <- eafit_hs_mental %>% filter(hh_id != "")
  
  # Label HH - Level
  c <- as.data.frame(names(eafit_hs))
  c$original_name2 <- c$`names(eafit_hs)`
  mm<- c %>% left_join(dictionary_hs,by = "original_name2") %>% 
          dplyr::select(original_name2,new_name) %>%
          dplyr::mutate(new_name=ifelse(is.na(new_name),
                                        original_name2,new_name))
  names(eafit_hs)[match(mm$original_name2, names(eafit_hs))] <- mm$new_name
  
  #Pegar información de tratamiento de los datos de linea base
  load("data/survey/02_depurados/HOGARES.rda")
  
  eafit_hs <- merge(x = eafit_hs, y = HOGARES[,c("start_p10","tratamiento_control","moda_cmh_p7","moda_cmh_p9","moda_cmh_p12","moda_cmh_p16")], by = "start_p10")
  
  pledge <- read_excel("data/pledge/01_original/Info_pledge_final.xlsx")
  pledge <- pledge[,c("CEDULA")]
  colnames(pledge) <- c("start_p10")
  pledge$pledge <- 3
  
  eafit_hs <- merge(x = eafit_hs, y = pledge, by = "start_p10", all.x = T)
  
  eafit_hs$pledge[eafit_hs$tratamiento_control == 0] <- 1
  eafit_hs$pledge[is.na(eafit_hs$pledge) & eafit_hs$tratamiento_control == 1] <- 2
 
  # Labels individual caracteristics
  c <- as.data.frame(names(eafit_hs_carac))
  c$original_name <- c$`names(eafit_hs_carac)`
  mm<- c %>% left_join(dictionary_hs_carac) %>% 
    dplyr::select(original_name,new_name) %>%
    dplyr::mutate(new_name=ifelse(is.na(new_name),
                                  original_name,new_name))
  names(eafit_hs_carac)[match(mm$original_name, names(eafit_hs_carac))] <- mm$new_name
  
  # Labels individual caracteristics
  c <- as.data.frame(names(eafit_hs_mental))
  c$original_name <- c$`names(eafit_hs_mental)`
  mm<- c %>% left_join(dictionary_hs_mental) %>% 
    dplyr::select(original_name,new_name) %>%
    dplyr::mutate(new_name=ifelse(is.na(new_name),
                                  original_name,new_name))
  names(eafit_hs_mental)[match(mm$original_name, names(eafit_hs_mental))] <- mm$new_name
  
  ### Merge
  eafit_hs_carac <- eafit_hs_carac[!duplicated(eafit_hs_carac[c("hh_id","ind_adulto","ind_name","mem_id")]),]
  eafit_hs_mental <- eafit_hs_mental[!duplicated(eafit_hs_mental[c("hh_id","ind_adulto","ind_name")]),]
  
  eafit_ind <- eafit_hs_carac %>% left_join(eafit_hs_mental,by = c("hh_id","PARENT_KEY","ind_adulto","ind_name"))
  
  eafit_ind$start_p10 <- as.numeric(sub("\\-.*", "",eafit_ind$mem_id))
  
  eafit_ind <- eafit_ind[!is.na(eafit_ind$`mem_check-mem_existe`),]
  
  #CREAR UN AUXILIARRRRRRRRRRRRRRRRRRR
  
  load("data/survey/02_depurados/CARACTERISTICAS.rda")
  
  individual <- read_delim("C:/Users/personal/Downloads/Individual.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  individual <- individual %>% group_by(CEDULA) %>% mutate(id_fam = row_number())
  
  individual$mem_id <- paste0(individual$CEDULA,"-",individual$id_fam)
  
  individual$NOMBRE...4[individual$mem_id == "1107096112-1"] <- "Jo Jonathan"
  individual$NOMBRE...4[individual$mem_id == "72298484-2"] <- "Germán David"
  
  individual$aux_name <- chartr("áéíóú","aeiou",tolower(trimws(individual$NOMBRE...4)))
  
  individual <- individual[,c("CEDULA","aux_name","EDAD","mem_id")]
  
  colnames(individual) <- c("start_p10","aux_name","moda_cmh_p9","mem_id")
  
  aux_nombre_sexo <- CARACTERISTICAS
  
  CARACTERISTICAS <- CARACTERISTICAS[,c("start_p10","moda_cmh_p5","moda_cmh_p9")]
  
  CARACTERISTICAS$moda_cmh_p5[CARACTERISTICAS$start_p10 == 1107096112 & CARACTERISTICAS$moda_cmh_p9 == 27] <- "Jo Jonathan"
  CARACTERISTICAS$moda_cmh_p5[CARACTERISTICAS$start_p10 == 72298484 & CARACTERISTICAS$moda_cmh_p9 == 19] <- "Germán David"
  
  CARACTERISTICAS$aux_name <- chartr("áéíóú","aeiou",tolower(trimws(CARACTERISTICAS$moda_cmh_p5)))
  
  CARACTERISTICAS <- CARACTERISTICAS[,c("start_p10","aux_name","moda_cmh_p9","moda_cmh_p5")]
  
  CARACTERISTICAS <- merge(x = CARACTERISTICAS, y = individual, by = c("start_p10","aux_name","moda_cmh_p9"))
  
  colnames(CARACTERISTICAS) <- c("cedula","aux_name","edad","mem_name","mem_id")
  
  cedulas_caracteristicas <- CARACTERISTICAS$mem_id
  cedulas_final <- eafit_ind$mem_id
  
  union <- intersect(cedulas_caracteristicas,cedulas_final)
  
  dif <- setdiff(cedulas_caracteristicas,union)
  
  dif <- data.frame(dif)
  colnames(dif) <- "mem_id"
  
  dif <- merge(dif,CARACTERISTICAS[,c("mem_id","mem_name","cedula")])
  
  viejos <- data.frame(matrix(NA,nrow(dif),ncol(eafit_ind)))
  colnames(viejos) <- colnames(eafit_ind)
  
  viejos$mem_id <- dif$mem_id
  viejos$mem_name <- dif$mem_name
  viejos$start_p10 <- dif$cedula
  
  viejos$`mem_check-mem_existe` <- 2
  
  eafit_ind <- rbind(eafit_ind,viejos)
  
  eafit_ind <- merge(x = eafit_hs[,c("start_p10","tratamiento_control","pledge","QUITAR","start_p13")], y = eafit_ind, by = c("start_p10"))
  
  aux_nombre_sexo <- aux_nombre_sexo[,c("mem_id","moda_cmh_p7","moda_cmh_p9","moda_cmh_p12","moda_cmh_p16")]
  colnames(aux_nombre_sexo) <- c("mem_id","moda_cmh_p7_base","moda_cmh_p9_base","moda_cmh_p12_base","moda_cmh_p16_base")
  
  eafit_ind <- merge(x = aux_nombre_sexo, y = eafit_ind, by = c("mem_id"), all.y = T)
  
  eafit_ind$moda_cmh_p7[is.na(eafit_ind$moda_cmh_p7)] <- eafit_ind$moda_cmh_p7_base
  eafit_ind$moda_cmh_p9[is.na(eafit_ind$moda_cmh_p9)] <- eafit_ind$moda_cmh_p9_base
  eafit_ind$moda_cmh_p16[is.na(eafit_ind$moda_cmh_p16)] <- eafit_ind$moda_cmh_p16_base
  
  eafit_ind$moda_cmh_p12 <- as.numeric(str_sub(eafit_ind$mem_id,-2))
  eafit_ind$moda_cmh_p12[eafit_ind$moda_cmh_p12 > 0] <- 0
  eafit_ind$moda_cmh_p12 <- as.numeric(str_sub(as.character(eafit_ind$moda_cmh_p12),-1))
  
  colnames(eafit_ind)[32] <- "moda_cmh_p18"
  
  eafit_ind <- eafit_ind %>% select(-all_of(c("moda_cmh_p7_base","moda_cmh_p9_base","moda_cmh_p12_base","moda_cmh_p16_base")))
  
  hogares_caidos <- read_excel("C:/Users/personal/Downloads/hogares_caidos.xlsx")
  
  eafit_ind$`mem_check-mem_existe`[eafit_ind$start_p10 %in% hogares_caidos$start_p10] <- NA
  
  eafit_hs <- eafit_hs[eafit_hs$QUITAR == 0,]
  eafit_ind <- eafit_ind[eafit_ind$QUITAR == 0,]
  
  #Arreglos manuales
  eafit_ind$`mem_check-mem_existe`[eafit_ind$mem_id == "2699661-2"] <- 2
  eafit_ind$`mem_check-mem_existe`[eafit_ind$mem_id == "32803484-5"] <- 2
  
  eafit_ind <- eafit_ind %>%
    filter(
      !(mem_id %in% c("71653892-2","32803448-6","1037570230-2","1045746691-6")))
  
  eafit_hs$moda_cmh_drop[eafit_hs$start_p10 == 32717266] <- 3
  eafit_hs$moda_cmh_drop[eafit_hs$start_p10 == 72428031] <- 3
  eafit_hs$moda_cmh_drop[eafit_hs$start_p10 == 1107090332] <- 3
  
  write_rds(eafit_hs,"data/survey/01_originales/raw_HOGARES_Endline.rda")
  write_rds(eafit_ind,"data/survey/01_originales/raw_INDIVIDUOS_Endline.rda")
  
  eafit_ind <- as.data.frame(eafit_ind)
  eafit_hs <- as.data.frame(eafit_hs)
  
  for(i in 1:ncol(eafit_ind)){
    if(length(class(eafit_ind[,i])) == 1) {
      if(class(eafit_ind[,i])=="factor") {
        eafit_ind[,i] <- as.numeric(eafit_ind[,i])
      }
    }
  }
  
  for(i in 1:ncol(eafit_hs)){
    if(length(class(eafit_hs[,i])) == 1) {
      if(class(eafit_hs[,i])=="factor") {
        eafit_hs[,i] <- as.numeric(eafit_hs[,i])
      }
    }
  }
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
  write.csv(eafit_hs, file="data/survey/01_originales/raw_HOGARES_Endline.csv", na="")
  write.csv(eafit_ind, file="data/survey/01_originales/raw_INDIVIDUOS_Endline.csv", na="")