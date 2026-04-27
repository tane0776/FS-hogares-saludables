# Elimina todos los objetos del espacio de trabajo
rm(list=ls())

# Carga las bibliotecas necesarias
pacman::p_load(dplyr,summarytools,readxl,openxlsx,tidyverse,glue,tidylog)

#SISTEMATIZACION REALIZADA POR ARGOS
interv_brrnqlla <- read_excel("data/intervention/01_originales/SISTEMATIZACION.xlsx",sheet = "Formato 1 Barranquilla")
interv_cali <- read_excel("data/intervention/01_originales/SISTEMATIZACION.xlsx",sheet = "Formato 2  Cali")
interv_mdlln <- read_excel("data/intervention/01_originales/SISTEMATIZACION.xlsx",sheet = "Fromato 3 Medellín")
interv_cntrl <- read_excel("data/intervention/01_originales/SISTEMATIZACION.xlsx",sheet = "Formato 4 (Control)")

#### Interventions
interv_brrnqlla <- interv_brrnqlla[!is.na(interv_brrnqlla$CEDULA),]
interv_cali <- interv_cali[!is.na(interv_cali$CEDULA),]
interv_mdlln <- interv_mdlln[!is.na(interv_mdlln$CEDULA),]

#Tratamiento
interv_brrnqlla$tipo <- 1
interv_cali$tipo <- 1
interv_mdlln$tipo <- 1
#Control
interv_cntrl$tipo <- 0

interv_mdlln$Categoría <- as.character(interv_mdlln$Categoría)
interv_cali$Categoría <- as.character(interv_cali$Categoría)

#### ----- Fix names to staderis
colnames(interv_cntrl)[27] <- "PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO"
colnames(interv_cntrl)[29] <- "Categoría"

interv_cntrl <- interv_cntrl %>% mutate(
  banio_enchapes_piso = case_match(
    `BAÑO ENCHAPES PISO`,
    "INTERVENCION TOTAL" ~ 2,
    "INTERVENCION PARCIAL" ~ 1,
    "NO NECESITA INTERVENCION" ~ 0,
    NA ~ 0
  ),
  banio_enchapes_muros = case_match(
    `BAÑO ENCHAPES MUROS`,
    "INTERVENCION TOTAL" ~ 2,
    "INTERVENCION PARCIAL" ~ 1,
    "NO NECESITA INTERVENCION" ~ 0,
    NA ~ 0
  )
)

interv_cntrl$aux <- pmax(interv_cntrl$banio_enchapes_muros,
                         interv_cntrl$banio_enchapes_piso)

interv_cntrl <- interv_cntrl %>% mutate(
  `BAÑO ENCHAPES PISO Y MUROS` = case_match(
    aux,
    2 ~ "INTERVENCION TOTAL",
    1 ~ "INTERVENCION PARCIAL",
    0 ~ "NO NECESITA INTERVENCION"
  )
)

colnames(interv_brrnqlla)[13] <- "BAÑO ENCHAPES PISO Y MUROS"
colnames(interv_brrnqlla)[14] <- "BAÑO GRIFERIAS DUCHA"
colnames(interv_brrnqlla)[15] <- "COCINA REVOQUES"
colnames(interv_brrnqlla)[16] <- "COCINA ENCHAPE DE SALPICADERO Y MESON EN CONCRETO"
colnames(interv_brrnqlla)[17] <- "COCINA POZUELO Y GRIFERIA"
colnames(interv_brrnqlla)[34] <- "PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO"

colnames(interv_cali)[28] <- "COCINA ENCHAPE DE SALPICADERO Y MESON EN CONCRETO"

colnames(interv_mdlln)[26] <- "COCINA ENCHAPE DE SALPICADERO Y MESON EN CONCRETO"

#### Find the common columns across cities
intervenciones <- intersect(colnames(interv_brrnqlla),intersect(colnames(interv_cali),intersect(colnames(interv_mdlln),colnames(interv_cntrl))))

interv_brrnqlla <- interv_brrnqlla[,colnames(interv_brrnqlla) %in% intervenciones]
interv_cali <- interv_cali[,colnames(interv_cali) %in% intervenciones]
interv_mdlln <- interv_mdlln[,colnames(interv_mdlln) %in% intervenciones]
interv_cntrl <- interv_cntrl[,colnames(interv_cntrl) %in% intervenciones]

#### <---- Build Data set of Intervenciones --->

intervenciones <- dplyr::bind_rows(interv_brrnqlla,interv_cali,interv_mdlln,interv_cntrl)
intervenciones <- intervenciones[!is.na(intervenciones$CEDULA),]

intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO`[intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "MEJORAMIENTO BAÑO Y COCINA"] <- "MEJORAMIENTO BAÑO COCINA"
intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO`[intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "MEJORAMIENTO BAÑO, PISO Y COCINA"] <- "MEJORAMIENTO PISO BAÑO COCINA"
intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO`[intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "MEJORAMIENTO BAÑO Y PISO"] <- "MEJORAMIENTO PISO BAÑO"

#### <---- Get data auxiliar to fix the data--->
aux_correcciones <- read_excel("data/intervention/01_originales/INFO_TRATAMIENTO_ARCOIRIS_DMT-INTERVENCIONES.xlsx", 
                                 sheet = "Hoja1")
aux_correcciones <- aux_correcciones[,c("Número_Documento_Titular","INTERVENCIONES")]

colnames(aux_correcciones) <- c("CEDULA","PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO")

aux_correcciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO`[aux_correcciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "BAÑOS"] <- "MEJORAMIENTO BAÑO"
aux_correcciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO`[aux_correcciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "BAÑOS PISOS COCINA"] <- "MEJORAMIENTO PISO BAÑO COCINA"
aux_correcciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO`[aux_correcciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "BAÑOS PISOS"] <- "MEJORAMIENTO PISO BAÑO"
aux_correcciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO`[aux_correcciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "BAÑOS COCINA"] <- "MEJORAMIENTO BAÑO COCINA"
aux_correcciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO`[aux_correcciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "PISOS"] <- "MEJORAMIENTO PISO"
aux_correcciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO`[aux_correcciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "PISOS COCINA"] <- "MEJORAMIENTO PISO COCINA"
aux_correcciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO`[aux_correcciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "COCINA"] <- "MEJORAMIENTO COCINA"


#### <---- Fix those with missing information --->
correcciones <- intervenciones[is.na(intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO`),]
correcciones <- correcciones$CEDULA

aux_correcciones <- aux_correcciones[aux_correcciones$CEDULA %in% correcciones,]

aux_intervenciones <- intervenciones[,c("CEDULA","PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO")]
aux_intervenciones <- aux_intervenciones[!is.na(aux_intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO`),]

aux_intervenciones <- rbind(aux_intervenciones,aux_correcciones)

#### <---- Get data auxiliar to fix the data--->
intervenciones <- intervenciones[,-c(16)] ### Borramos la información anterior

intervenciones <- merge(intervenciones,aux_intervenciones,by="CEDULA",all.x=T)

#### <---- Fix Intervencion Baño --->
intervenciones$intervencion_baño <- as.numeric(intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "MEJORAMIENTO BAÑO"
                                               | intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "MEJORAMIENTO BAÑO COCINA"
                                               | intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "MEJORAMIENTO PISO BAÑO"
                                               | intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "MEJORAMIENTO PISO BAÑO COCINA")

#### <---- Fix Intervencion COCINA --->
intervenciones$intervencion_cocina <- as.numeric(intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "MEJORAMIENTO BAÑO COCINA"
                                                 | intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "MEJORAMIENTO COCINA"
                                                 | intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "MEJORAMIENTO PISO BAÑO COCINA"
                                                 | intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "MEJORAMIENTO PISO COCINA")

#### <---- Fix Intervencion PISO --->
intervenciones$intervencion_piso <- as.numeric(intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "MEJORAMIENTO PISO"
                                               | intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "MEJORAMIENTO PISO BAÑO"
                                               | intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "MEJORAMIENTO PISO BAÑO COCINA"
                                               | intervenciones$`PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO` == "MEJORAMIENTO PISO COCINA")

rm(list=ls()[! ls() %in% c("intervenciones")])

intervenciones <- intervenciones[,c("CEDULA","MUNICIPIO","BARRIO","PISOS DEMOLICIONES","BAÑO REVOQUES",
                                    "BAÑO ENCHAPES PISO Y MUROS","BAÑO GRIFERIAS DUCHA","COCINA REVOQUES",
                                    "COCINA ENCHAPE DE SALPICADERO Y MESON EN CONCRETO","COCINA POZUELO Y GRIFERIA",
                                    "PRIORIZACION DE ACTIVIDADES PARA EL MEJORAMIENTO","Categoría","tipo",
                                    "intervencion_baño","intervencion_cocina","intervencion_piso")]

colnames(intervenciones)[4] <- "pisos_demoliciones"
colnames(intervenciones)[5] <- "baño_revoque"
colnames(intervenciones)[6] <- "baño_enchapes"
colnames(intervenciones)[7] <- "baño_griferia_ducha"
colnames(intervenciones)[8] <- "cocina_revoque"
colnames(intervenciones)[9] <- "cocina_enchape"
colnames(intervenciones)[10] <- "cocina_pozuelo_griferia"
colnames(intervenciones)[11] <- "intervencion"

# Replace missing intervention information with no intervention
intervenciones$pisos_demoliciones[is.na(intervenciones$pisos_demoliciones)] <- "NO NECESITA INTERVENCION"
intervenciones$baño_revoque[is.na(intervenciones$baño_revoque)] <- "NO NECESITA INTERVENCION"
intervenciones$baño_enchapes[is.na(intervenciones$baño_enchapes)] <- "NO NECESITA INTERVENCION"
intervenciones$baño_griferia_ducha[is.na(intervenciones$baño_griferia_ducha)] <- "NO NECESITA INTERVENCION"
intervenciones$cocina_revoque[is.na(intervenciones$cocina_revoque)] <- "NO NECESITA INTERVENCION"
intervenciones$cocina_enchape[is.na(intervenciones$cocina_enchape)] <- "NO NECESITA INTERVENCION"
intervenciones$cocina_pozuelo_griferia[is.na(intervenciones$cocina_pozuelo_griferia)] <- "NO NECESITA INTERVENCION"

intervenciones$baño_revoque[intervenciones$baño_revoque == "INTERVENCIÓN TOTAL"] <- "INTERVENCION TOTAL"
intervenciones$baño_revoque[intervenciones$baño_revoque == "NO NESECITA INTERVENCION"] <- "NO NECESITA INTERVENCION"

intervenciones <- intervenciones %>% mutate(
  necesidad_pisos_demoliciones = case_match(
    pisos_demoliciones,
    "INTERVENCION TOTAL" ~ 2,
    "INTERVENCION PARCIAL" ~ 1,
    "NO NECESITA INTERVENCION" ~ 0,
    NA ~ 0
  ),
  necesidad_baño_revoque = case_match(
    baño_revoque,
    "INTERVENCION TOTAL" ~ 2,
    "INTERVENCION PARCIAL" ~ 1,
    "NO NECESITA INTERVENCION" ~ 0,
    NA ~ 0
  ),
  necesidad_baño_enchapes = case_match(
    baño_enchapes,
    "INTERVENCION TOTAL" ~ 2,
    "INTERVENCION PARCIAL" ~ 1,
    "NO NECESITA INTERVENCION" ~ 0,
    NA ~ 0
  ),
  necesidad_baño_griferia_ducha = case_match(
    baño_griferia_ducha,
    "INTERVENCION TOTAL" ~ 2,
    "INTERVENCION PARCIAL" ~ 1,
    "NO NECESITA INTERVENCION" ~ 0,
    NA ~ 0
  ),
  necesidad_cocina_revoque = case_match(
    cocina_revoque,
    "INTERVENCION TOTAL" ~ 2,
    "INTERVENCION PARCIAL" ~ 1,
    "NO NECESITA INTERVENCION" ~ 0,
    NA ~ 0
  ),
  necesidad_cocina_enchape = case_match(
    cocina_enchape,
    "INTERVENCION TOTAL" ~ 2,
    "INTERVENCION PARCIAL" ~ 1,
    "NO NECESITA INTERVENCION" ~ 0,
    NA ~ 0
  ),
  necesidad_cocina_pozuelo_griferia = case_match(
    cocina_pozuelo_griferia,
    "INTERVENCION TOTAL" ~ 2,
    "INTERVENCION PARCIAL" ~ 1,
    "NO NECESITA INTERVENCION" ~ 0,
    NA ~ 0
  )
)

intervenciones <- intervenciones %>% mutate(
  necesidad_cocina = pmax(necesidad_cocina_revoque,necesidad_cocina_enchape,necesidad_cocina_pozuelo_griferia),
  necesidad_cocina_d = ifelse(necesidad_cocina > 0,1,0),
  necesidad_cocina_n = necesidad_cocina_revoque + necesidad_cocina_enchape + necesidad_cocina_pozuelo_griferia,
  necesidad_banio = pmax(necesidad_baño_revoque,necesidad_baño_enchapes,necesidad_baño_griferia_ducha),
  necesidad_banio_d = ifelse(necesidad_banio > 0,1,0),
  necesidad_banio_n = necesidad_baño_revoque + necesidad_baño_enchapes + necesidad_baño_griferia_ducha,
  necesidad_piso = pmax(necesidad_pisos_demoliciones),
  necesidad_piso_d = ifelse(necesidad_piso > 0,1,0),
  necesidad_piso_n = necesidad_pisos_demoliciones
)  

#### <---- Presupuestos --->
presupuesto <- read_excel("data/intervention/01_originales/PRESUPUESTO_INTEVENCIONES.xlsx")

presupuesto <- presupuesto[,c("Cedula","Ciudad","Barrio","p_intervencion_piso","p_intervencion_baño","p_intervencion_cocina","p_intervencion_directa","p_intervencion_total")]

presupuesto$p_intervencion_piso <- as.numeric(presupuesto$p_intervencion_piso)
presupuesto$p_intervencion_baño <- as.numeric(presupuesto$p_intervencion_baño)
presupuesto$p_intervencion_cocina <- as.numeric(presupuesto$p_intervencion_cocina)
presupuesto$p_intervencion_directa <- as.numeric(presupuesto$p_intervencion_directa)
presupuesto$p_intervencion_total <- as.numeric(presupuesto$p_intervencion_total)

presupuesto <- na.omit(presupuesto)

interv_presupuesto <- merge(x = intervenciones, y = presupuesto, by.x = "CEDULA", by.y = "Cedula", all.x =  T)

table(interv_presupuesto$baño_revoque)
write_rds(interv_presupuesto,"data/intervention/03_procesados/intervenciones_raw.rds")

if(file.exists("data/intervention/03_procesados/estimated_budgets.rds")) {
  budgets <- readRDS("data/intervention/03_procesados/estimated_budgets.rds")
} else {
  source("codes/02_procesamiento/005_Estimate_Budget.R")
  budgets <- readRDS("data/intervention/03_procesados/estimated_budgets.rds")
}

##### <----- Clean Variables ----> 
interv_presupuesto <- interv_presupuesto %>% select(CEDULA,`Categoría`,
                                                    pisos_demoliciones,
                                                    baño_revoque,baño_griferia_ducha,baño_enchapes,
                                                    cocina_revoque,cocina_pozuelo_griferia,cocina_enchape,
                                                    necesidad_cocina,necesidad_cocina_d,necesidad_cocina_n,
                                                    necesidad_banio,necesidad_banio_d,necesidad_banio_n,
                                                    necesidad_piso,necesidad_piso_d,necesidad_piso_n,
                                                    contains("intervencion_")) 
#interv_presupuesto <- interv_presupuesto %>% select(-p_intervencion_directa)

names(interv_presupuesto) <- c("CEDULA", "Interv_category",
                               "pisos_demoliciones",
                               "baño_revoque","baño_griferia_ducha","baño_enchapes",
                               "cocina_revoque","cocina_pozuelo_griferia","cocina_enchape",
                               "necesidad_cocina","necesidad_cocina_d","necesidad_cocina_n",
                               "necesidad_banio","necesidad_banio_d","necesidad_banio_n",
                               "necesidad_piso","necesidad_piso_d","necesidad_piso_n",
                               "Interv_bath","Interv_kitchen","Interv_floor",
                                "Interv_budget_floor",
                                "Interv_budget_bath",
                                "Interv_budget_kitchen",
                                "Interv_budget_subtotal",
                                "Interv_budget_total")
# Categoria (level of urgencia)

#table(interv_presupuesto$Barrio,interv_presupuesto$tra)
#View(interv_presupuesto[interv_presupuesto$Barrio=="0",])


#### New vars
interv_presupuesto <- interv_presupuesto %>% mutate(
                                    ### Total intervention
                                    Interv_n=Interv_bath+Interv_floor+Interv_kitchen,
                                    ### Floor+Bath
                                    Interv_bath_floor = ifelse(Interv_bath == 1 & Interv_floor == 1,1,0),
                                    ### Floor+Kitchen
                                    Interv_kitchen_floor = ifelse(Interv_kitchen == 1 & Interv_floor == 1,1,0),
                                    ### Bath+Kitchen
                                    Interv_bath_kitchen = ifelse(Interv_kitchen == 1 & Interv_bath == 1,1,0),
                                    ### All
                                    Interv_all = ifelse(Interv_kitchen == 1 & Interv_bath == 1 & Interv_floor == 1,1,0),
                                    Interv_budget_floor=Interv_budget_floor/1000000,
                                    Interv_budget_bath=Interv_budget_bath/1000000,
                                    Interv_budget_kitchen=Interv_budget_kitchen/1000000,
                                    Interv_budget_subtotal=Interv_budget_subtotal/1000000,
                                    Interv_budget_total=Interv_budget_total/1000000)


interv_presupuesto <- interv_presupuesto %>% mutate(
  Interv_bath_type = case_when(baño_revoque == "NO NECESITA INTERVENCION" & baño_griferia_ducha == "NO NECESITA INTERVENCION" ~ "NO NECESITA INTERVENCION",
                               baño_revoque == "INTERVENCION TOTAL" & baño_griferia_ducha == "INTERVENCION TOTAL" ~ "INTERVENCION TOTAL",
                               TRUE ~ "INTERVENCION PARCIAL"
                               ),
  Interv_kitchen_type = case_when(cocina_revoque == "NO NECESITA INTERVENCION" & cocina_pozuelo_griferia == "NO NECESITA INTERVENCION" ~ "NO NECESITA INTERVENCION",
                                  cocina_revoque == "INTERVENCION TOTAL" & cocina_pozuelo_griferia == "INTERVENCION TOTAL" ~ "INTERVENCION TOTAL",
                                  TRUE ~ "INTERVENCION PARCIAL"
                                  ),
  Interv_floor_type = case_when(pisos_demoliciones == "NO NECESITA INTERVENCION" ~ "NO NECESITA INTERVENCION",
                                pisos_demoliciones == "INTERVENCION TOTAL" ~ "INTERVENCION TOTAL",
                                TRUE ~ "INTERVENCION PARCIAL"
                                )
  )

write_rds(interv_presupuesto,"data/intervention/03_procesados/intervenciones.rds")
