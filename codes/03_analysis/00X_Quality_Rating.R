# Elimina todos los objetos del espacio de trabajo
rm(list=ls())

# Carga las bibliotecas necesarias
pacman::p_load(readxl)

#devtools::install_github("elbersb/tidylog")
rating_miguel <- read_excel("data/quality rating (pictures)/Quality rating test_Miguel.xlsx")
rating_miguel <- rating_miguel[,1:6]
colnames(rating_miguel) <- c("id","floor","bath","kitchen","fachada","household")
colnames(rating_miguel)[2:6] <- paste(colnames(rating_miguel)[2:6],"miguel",sep="_")

rating_miguel$bath_miguel <- as.numeric(rating_miguel$bath_miguel)
rating_miguel$kitchen_miguel <- as.numeric(rating_miguel$kitchen_miguel)

rating_miguel$bath_miguel[is.na(rating_miguel$bath_miguel)] <- 0
rating_miguel$kitchen_miguel[is.na(rating_miguel$kitchen_miguel)] <- 0

rating_nicholas <- read_excel("data/quality rating (pictures)/Quality rating test_Nicholas.xlsx")
rating_nicholas <- rating_nicholas[,1:6]
colnames(rating_nicholas) <- c("id","floor","bath","kitchen","fachada","household")
colnames(rating_nicholas)[2:6] <- paste(colnames(rating_nicholas)[2:6],"nicholas",sep="_") 

rating <- merge(x = rating_miguel, y = rating_nicholas, by = "id")

#Correlaciones
corr_floor <- cor(rating$floor_miguel, rating$floor_nicholas)
corr_bath <- cor(rating$bath_miguel, rating$bath_nicholas)
corr_kitchen <- cor(rating$kitchen_miguel, rating$kitchen_nicholas)
corr_fachada <- cor(rating$fachada_miguel, rating$fachada_nicholas)
corr_household <- cor(rating$household_miguel, rating$household_nicholas)

correlaciones <- data.frame(variable = c("Piso","Baño","Cocina","Fachada","Hogar"),
                             correlacion = c(corr_floor,corr_bath,corr_kitchen,corr_fachada,corr_household))

round(cor(rating[2:11]),digits = 2)

rating_sample_info <- read_excel("rating_sample_miguel_nicholas.xlsx")

rating_info <- merge(x = rating, y = rating_sample_info, by.x = "id", by.y = "Consecutivo")

ds <- readRDS("data/datasets/HS_FinalDS_Jun2024.rds")
ds <- ds[,c("start_p10","Interv_bath","Interv_floor","Interv_kitchen")]

rating_info <- merge(x = rating_info, y = ds, by.x = "Cédula", by.y = "start_p10")

rating_info$mean_floor <- rowMeans(rating_info[,c(3,8)])
rating_info$mean_bath <- rowMeans(rating_info[,c(4,9)])
rating_info$mean_fachada <- rowMeans(rating_info[,c(6,11)])
rating_info$mean_kitchen <- rowMeans(rating_info[,c(5,10)])
rating_info$mean_household <- rowMeans(rating_info[,c(7,12)])

corr_household_miguel <- cor(rating_info$`# de intervenciones`,rating_info$household_miguel)
corr_household_nicholas <- cor(rating_info$`# de intervenciones`,rating_info$household_nicholas)
corr_household_mean <- cor(rating_info$`# de intervenciones`,rating_info$mean_household)

correlaciones_household <- data.frame(variable = c("Rating de hogar vs # de intervenciones (Miguel)","Rating de hogar vs # de intervenciones (Nicholas)","Rating de hogar vs # de intervenciones (Promedio)"),
                            correlacion = c(corr_household_miguel,corr_household_nicholas,corr_household_mean))

corr_floor_miguel <- cor(rating_info$`# de intervenciones`,rating_info$floor_miguel)
corr_floor_nicholas <- cor(rating_info$`# de intervenciones`,rating_info$floor_nicholas)
corr_floor_mean <- cor(rating_info$`# de intervenciones`,rating_info$mean_floor)

correlaciones_floor <- data.frame(variable = c("Rating de piso vs intervención de piso (Miguel)","Rating de piso vs intervención de piso (Nicholas)","Rating de piso vs intervención de piso (Promedio)"),
                                  correlacion = c(corr_floor_miguel,corr_floor_nicholas,corr_floor_mean))

corr_bath_miguel <- cor(rating_info$`# de intervenciones`,rating_info$bath_miguel)
corr_bath_nicholas <- cor(rating_info$`# de intervenciones`,rating_info$bath_nicholas)
corr_bath_mean <- cor(rating_info$`# de intervenciones`,rating_info$mean_bath)


correlaciones_bath <- data.frame(variable = c("Rating de baño vs intervención de baño (Miguel)","Rating de baño vs intervención de baño (Nicholas)","Rating de baño vs intervención de baño (Promedio)"),
                                 correlacion = c(corr_bath_miguel,corr_bath_nicholas,corr_bath_mean))

corr_kitchen_miguel <- cor(rating_info$`# de intervenciones`,rating_info$kitchen_miguel)
corr_kitchen_nicholas <- cor(rating_info$`# de intervenciones`,rating_info$kitchen_nicholas)
corr_kitchen_mean <- cor(rating_info$`# de intervenciones`,rating_info$mean_kitchen)

correlaciones_kitchen <- data.frame(variable = c("Rating de cocina vs intervención de cocina (Miguel)","Rating de cocina vs intervención de cocina (Nicholas)","Rating de cocina vs intervención de cocina (Promedio)"),
                                    correlacion = c(corr_kitchen_miguel,corr_kitchen_nicholas,corr_kitchen_mean))