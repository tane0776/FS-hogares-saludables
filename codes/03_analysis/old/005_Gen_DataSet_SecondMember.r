##############
# This code creates the data set for the phone call 
# 
# Created: Santiago Navas y JC Munoz
#
# Created: 25/11/23
##############

### Instructions: 
# 
# Sample: HS:252 Control Puro = 300 (es aleatorio de notros) 825 * 2 --> línea base y final
# * 825*2 = 1625*38080 = 6188000
# - Solo adultos - A nivel individual
# 
# 
# 
# Base
#     - PARENT_KEY.x: ID HH
#     - personal_key:ID Individuo
#     - ID_H: Consecutivo HH
#     - ID_I: Consecutivo Individuo
#     - start_p13: Ciudad
#     - tratamiento_control: tratamiento_control
#     - mode_sm_p5: Si Estaba para reponder salud mental
#     - mode_sm_p6: Si Quizo responder salud Mental
#     - moda_cmh_p12: Relación con el jefe de hogar
#     - moda_cmh_p5: Nombre
#     - Telefono 1:moda_cmh_p62
#     - Telefono 2: moda_cmh_p63
#     

rm(list = ls())
set.seed(12345)
### -- 00 -- Load packages --- ####
  pacman::p_load(readr,readxl,dplyr,glue,openxlsx)

### --- 01 -- Load data --- ####
  load('output/CARACTERISTICAS.rda')

### --- 02 -- Prepare data for selection --- ####
  ### Get only adults (moda_cmh_p9 >= 18) y only household head (moda_cmh_p12==1)
  second_member<- CARACTERISTICAS[as.numeric(CARACTERISTICAS$moda_cmh_p9)>=18 & CARACTERISTICAS$moda_cmh_p12=="1",]
  
  #### Create the three main grups (HS:252 Control Puro = 300 (es aleatorio de notros) 825 * 2)
  
  # Grupo 1: HS+Pledge = 252
  muestra<- rbind(data.frame(PARENT_KEY =read.xlsx('input/List_Gottigen_Nov2023.xlsx')[,2],
                             #### Grupo 1 -> pledge + HS (strengths=2), Grupo 2 -> HS =0 (strengths=1)
                             strengths=read.xlsx('input/List_Gottigen_Nov2023.xlsx')[,3]+1),
                ### Gen Grupo III --> Create the pure control by 
                data.frame(PARENT_KEY = sample(second_member[second_member$tratamiento_control=="0",]$PARENT_KEY.x, 300),
                           #### Grupo 3 -> pledge + HS = 1, Grupo 2 -> HD =0
                            strengths=0))
  # Check composition
  table(muestra$strengths)

### --- 02 -- Get the dataset --- ####
  # Get only households members in the selected househols
    second_member <- merge(CARACTERISTICAS,muestra,by.x=c("PARENT_KEY.x"),by.y=c("PARENT_KEY"))
    # Delete no selected households  
    second_member <- second_member[!is.na(second_member$strengths),]
    table(second_member$strengths)
  
### --- 03 -- Prepare data --- ####

    ### Prepare Indixes
    second_member <- second_member %>% 
                      # Simplifying the ID
                      mutate(PARENT_KEY.x = as.factor(PARENT_KEY.x),
                      ID_H = sprintf("%04d", as.numeric(PARENT_KEY.x))) %>%
                      # Individual
                      group_by(ID_H) %>%
                      mutate(ID_I = paste0(ID_H,"-", sprintf("%02d", row_number())))


    ### Prepare data
    out <- second_member %>%
        ### Delete those members that already answer questions
        # **${elg_nm}** se encuentra presente para responder una preguntas sobre su estado emocional?
        ### Only those that are ELEGIBLES amd WERE not present during the interview
        dplyr::filter(mode_sm_p5 == 2) %>%
        ### Prepare variables
        dplyr::transmute(PARENT_KEY.x, personal_key, ID_H, ID_I,
            ## Ciudad (mode_sm_p5)
            ciudad = recode(start_p13, "1" = "Medellin", "2" = "Cali", "3" = "Barranquilla"),
            ## Tratamiento BID
            tipo = recode(as.character(strengths), "0" = "Control", "1" = "HS", "2" = "HS+Pledge"),
            # Parentezco Jefe de hogar
            parentezco = recode(as.character(moda_cmh_p12),
                "1" = "Jefe (a) del hogar",
                "2" = "Pareja, esposo (a), cónyuge, compañero(a) ", 
                "3" = "Hijo(a) hijastro(a)",
                "4" = "Nieto (a)",
                "5" = "Padre, madre, padrastro y madrastra",
                "6" = "Suegro o suegra ",
                "7" = "Hermano (a), hermanastro (a)",
                "8" = "Yerno, nuera"
            ),
            ## Nombre
            nombre=moda_cmh_p5,
            ### Celular
            celular=moda_cmh_p62,
            ### Otro número
            celular_otro=moda_cmh_p63)
  
      ### Base datos callcenter
      write.xlsx(out, "output_bid/HS_BID_Second_Member_CallCenter.xlsx")
      
      ### For work
      write_rds(out[,c("PARENT_KEY.x","personal_key","ID_H","ID_I" )], "output_bid/HS_BID_Second_Member.rds")
      
      ### BD - ODK
      write.csv(out[,c("ID_I","nombre")], "output_bid/HS_ArgosEAFIT_Cuestionario_SegundaPersona_list_n.csv")
      ### List
      write.csv(out %>% transmute(name=ID_I,label=ID_I,nm=nombre), "output_bid/HS_ArgosEAFIT_Cuestionario_SegundaPersona_list.csv",
                row.names=FALSE)
  
