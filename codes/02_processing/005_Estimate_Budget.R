# Elimina todos los objetos del espacio de trabajo
rm(list=ls())
#Estimacion de interv_presupuestos de control con Random forest
options(repos = c(CRAN = "https://cloud.r-project.org/"))
install.packages("randomForest")
library(randomForest)
set.seed(123)

##### <---- Open data --->
interv_presupuesto <- readRDS("data/intervention/03_procesados/intervenciones_raw.rds")

#Si se desea eliminar a los que no requieren intervencion antes de estimar
#interv_interv_presupuesto <- interv_interv_presupuesto[interv_interv_presupuesto$intervencion != "No aplica",]

interv_presupuesto$baño_revoque[interv_presupuesto$baño_revoque == "INTERVENCIÓN TOTAL"] <- "INTERVENCION TOTAL"
interv_presupuesto$baño_revoque[interv_presupuesto$baño_revoque == "NO NESECITA INTERVENCION"] <- "NO NECESITA INTERVENCION"

for(i in 4:8) {
  #Corregir informacion de los controles a los que no les aplica la intervencion
  #interv_presupuesto[is.na(interv_presupuesto[,i]) & interv_presupuesto$intervencion != "No aplica",i] <- "No info"
  interv_presupuesto[is.na(interv_presupuesto[,i]) & interv_presupuesto$intervencion != "No aplica",i] <- "NO NECESITA INTERVENCION"
  interv_presupuesto[is.na(interv_presupuesto[,i]) & interv_presupuesto$intervencion == "No aplica",i] <- "NO NECESITA INTERVENCION"
  interv_presupuesto[,i] <- factor(interv_presupuesto[,i],levels=c('NO NECESITA INTERVENCION','INTERVENCION PARCIAL','INTERVENCION TOTAL'))
}

interv_presupuesto$MUNICIPIO[interv_presupuesto$MUNICIPIO == "Baranquilla"] <- "BARRANQUILLA"
interv_presupuesto$MUNICIPIO[interv_presupuesto$MUNICIPIO == "Cali"] <- "CALI"
interv_presupuesto$MUNICIPIO[interv_presupuesto$MUNICIPIO == "Medellin"] <- "MEDELLIN"
interv_presupuesto$MUNICIPIO <- as.factor(interv_presupuesto$MUNICIPIO)

intervenciones_presupuesto_necesidades <- interv_presupuesto

#Cambiar valor a reflejar intervencion designada
interv_presupuesto$baño_revoque[interv_presupuesto$intervencion_baño == 0] <- 'NO NECESITA INTERVENCION'
interv_presupuesto$baño_enchapes[interv_presupuesto$intervencion_baño == 0] <- 'NO NECESITA INTERVENCION'
interv_presupuesto$baño_griferia_ducha[interv_presupuesto$intervencion_baño == 0] <- 'NO NECESITA INTERVENCION'

interv_presupuesto$cocina_revoque[interv_presupuesto$intervencion_cocina == 0] <- 'NO NECESITA INTERVENCION'
interv_presupuesto$cocina_enchape[interv_presupuesto$intervencion_cocina == 0] <- 'NO NECESITA INTERVENCION'
interv_presupuesto$cocina_pozuelo_griferia[interv_presupuesto$intervencion_cocina == 0] <- 'NO NECESITA INTERVENCION'

interv_presupuesto$pisos_demoliciones[interv_presupuesto$intervencion_piso == 0] <- 'NO NECESITA INTERVENCION'

intervenciones_presupuesto <- interv_presupuesto[!is.na(interv_presupuesto$p_intervencion_directa),]
intervenciones_no_presupuesto <- interv_presupuesto[is.na(interv_presupuesto$p_intervencion_directa),]

intervenciones_presupuesto$fake <- 0

#Añadir casos en los que no se tenga intervención
n_adicionales <- 50
for(i in 1:n_adicionales){
  if(i < (n_adicionales/3)){
    mpio <- "MEDELLIN"
  } else if(i < (n_adicionales/3)*2){
    mpio <- "CALI"
  } else{
    mpio <- "BARRRANQUILLA"
  }
  intervenciones_presupuesto <- rbind(intervenciones_presupuesto,c(0,mpio,"NINGUNO","NO NECESITA INTERVENCION",
                             "NO NECESITA INTERVENCION","NO NECESITA INTERVENCION",
                             "NO NECESITA INTERVENCION","NO NECESITA INTERVENCION",
                             "NINGUNA",0,0,0,0,0,"NINGUNO","NINGUNO",0,0,0,0,0,1))
}

intervenciones_presupuesto$p_intervencion_total <- as.numeric(intervenciones_presupuesto$p_intervencion_total)
intervenciones_presupuesto$CEDULA <- as.numeric(intervenciones_presupuesto$CEDULA)
intervenciones_presupuesto$tipo <- as.numeric(intervenciones_presupuesto$tipo)
intervenciones_presupuesto$intervencion_baño <- as.numeric(intervenciones_presupuesto$intervencion_baño)
intervenciones_presupuesto$intervencion_cocina <- as.numeric(intervenciones_presupuesto$intervencion_cocina)
intervenciones_presupuesto$intervencion_piso <- as.numeric(intervenciones_presupuesto$intervencion_piso)
intervenciones_presupuesto$p_intervencion_piso <- as.numeric(intervenciones_presupuesto$p_intervencion_piso)
intervenciones_presupuesto$p_intervencion_baño <- as.numeric(intervenciones_presupuesto$p_intervencion_baño)
intervenciones_presupuesto$p_intervencion_cocina <- as.numeric(intervenciones_presupuesto$p_intervencion_cocina)
intervenciones_presupuesto$p_intervencion_directa <- as.numeric(intervenciones_presupuesto$p_intervencion_directa)
intervenciones_presupuesto$fake <- as.numeric(intervenciones_presupuesto$fake)

#Survey dataset
survey <- readRDS("data/survey/03_procesados/hogares_encuesta_procesada.rds")

intervenciones_presupuesto <- merge(intervenciones_presupuesto,survey[,c("n_bedrooms","n_baths","start_p10")],by.x = "CEDULA",by.y = "start_p10",all.x = T)
intervenciones_presupuesto <- intervenciones_presupuesto[!(is.na(intervenciones_presupuesto$n_baths) & intervenciones_presupuesto$fake == 0),]
intervenciones_no_presupuesto <- merge(intervenciones_no_presupuesto,survey[,c("n_bedrooms","n_baths","start_p10")],by.x = "CEDULA",by.y = "start_p10")

intervenciones_presupuesto_necesidades <- merge(intervenciones_presupuesto_necesidades,survey[,c("n_bedrooms","n_baths","start_p10")],by.x = "CEDULA",by.y = "start_p10",all.x = T)

intervenciones_presupuesto$n_bedrooms[intervenciones_presupuesto$fake == 1] <- round(mean(intervenciones_presupuesto$n_bedrooms[intervenciones_presupuesto$fake == 0]))
intervenciones_presupuesto$n_baths[intervenciones_presupuesto$fake == 1] <-round(mean(intervenciones_presupuesto$n_baths[intervenciones_presupuesto$fake == 0]))

##### <---- Random Forest  --->
  #Entrenamiento del modelo
  smp_size <- floor(0.75 * nrow(intervenciones_presupuesto))
  train_ind <- sample(seq_len(nrow(intervenciones_presupuesto)), size = smp_size)

  train <- intervenciones_presupuesto[train_ind,]
  train <- na.omit(train)
  test <- intervenciones_presupuesto[-train_ind,]

  ### Estimating the model
  rf_model <- randomForest(p_intervencion_directa ~ pisos_demoliciones +
                             baño_enchapes + baño_revoque + baño_griferia_ducha +
                             cocina_revoque + cocina_pozuelo_griferia + cocina_enchape +
                             n_bedrooms + n_baths,
                          data = train,
                          ntree = 500,
                          mtry = 3)
  
  ##### <---- Open data --->
  predicted_prices <- predict(rf_model, test)
  ### Predicted prices
  test$pred <- predicted_prices
  mse <- mean((predicted_prices - test$p_intervencion_directa)^2)
  print(paste("Mean Squared Error: ", mse))
  
  plot(test$pred, test$p_intervencion_directa)
  abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)
  
  ### Predecir precios basados en la intervencion diseñada de Argos
  predicted_prices <- predict(rf_model, intervenciones_no_presupuesto)
  intervenciones_no_presupuesto$p_intervencion_directa <- predicted_prices
  
  intervenciones_presupuesto$p_intervencion_directa_est <- predicted_prices
  intrat_moneed<-ggplot(  intervenciones_presupuesto, aes(x=p_intervencion_directa, y=p_intervencion_directa_est)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x) +
    labs(
      x = "Interior Investment Deficit",
      y = "Interior Investment Deficit: Random Forest Prediction"
    )
  ggsave("results/tables/regs/in_budget_pred.png", plot = intrat_moneed)
  
  
  plot(intervenciones_presupuesto$p_intervencion_directa, intervenciones_presupuesto$p_intervencion_directa_est)
  abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)
  
  
  intervenciones_presupuesto$p_intervencion_directa <- intervenciones_presupuesto$p_intervencion_directa
  intervenciones_presupuesto <- intervenciones_presupuesto[intervenciones_presupuesto$fake == 0,]
  
  intervenciones_presupuesto <- intervenciones_presupuesto[,c("CEDULA","p_intervencion_directa")]
  intervenciones_no_presupuesto <- intervenciones_no_presupuesto[,c("CEDULA","p_intervencion_directa")]
  
  interv_presupuesto <- dplyr::bind_rows(intervenciones_presupuesto,intervenciones_no_presupuesto)
  
  names(interv_presupuesto) <- c("CEDULA","Interv_budget_subtotal_est")
  
  interv_presupuesto$Interv_budget_subtotal_est <- interv_presupuesto$Interv_budget_subtotal_est/1000000
  
  ### Predecir presupuesto basado en necesidades
  intervenciones_presupuesto_necesidades$p_intervencion_directa <- NA
  
  predicted_prices <- predict(rf_model, intervenciones_presupuesto_necesidades)
  intervenciones_presupuesto_necesidades$p_intervencion_directa <- predicted_prices

  intervenciones_presupuesto_necesidades<- intervenciones_presupuesto_necesidades[,c("CEDULA","p_intervencion_directa")]
  names(intervenciones_presupuesto_necesidades) <- c("CEDULA","Interv_budget_needs_subtotal_est")
  
  intervenciones_presupuesto_necesidades$Interv_budget_needs_subtotal_est <- intervenciones_presupuesto_necesidades$Interv_budget_needs_subtotal_est/1000000
  
  presupuestos <- merge(x = interv_presupuesto, y = intervenciones_presupuesto_necesidades, by = "CEDULA")
  
  write_rds(presupuestos,"data/intervention/03_procesados/estimated_budgets.rds")

  # data_reg <- rbind(train,test)
  # 
  # 
  # 
  # r1 <- lm(p_intervencion_total ~ pisos_demoliciones +  baño_revoque +  baño_griferia_ducha
  #          + cocina_revoque + cocina_pozuelo_griferia,
  #          data = data_reg)
  # 
  # datos <- rbind(intervenciones_no_interv_presupuesto,intervenciones_presupuesto)
  # datos$pred <- predict(r1,datos)
  # 
  # r_2 <- lm(p_intervencion_total ~ pisos_demoliciones +  baño_revoque +
  #             baño_griferia_ducha + cocina_revoque + cocina_pozuelo_griferia + 0,
  #           data = data_reg)
  # 
  # datos$pred2 <- predict(r_2,datos)
  # 
  
#### <---- CHECK ---->

  #Estandarizacion del interv_presupuesto
  #General
  #interv$Interv_budget_total_est_centered <- scale(interv$Interv_budget_total_est, center = 0)

  #A nivel de ciudad
  #interv <- interv %>%
  #  group_by(MUNICIPIO) %>%
   # mutate(ds_p_intervencion_directa_mpio = scale(p_intervencion_directa, center = 0))

  #A nivel de barrio
  #interv_interv_presupuesto <- interv_interv_presupuesto %>%
  # group_by(BARRIO) %>%
  #  mutate(ds_p_intervencion_directa_mpio = scale(p_intervencion_directa, center = 0))

