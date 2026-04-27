set.seed(123)

library(mice)
library(ggplot2)
library(cowplot)

#Corregir precios de arriendo
hs_main$arriendo[hs_main$arriendo < 50000] <- hs_main$arriendo[hs_main$arriendo < 50000]*1000

hs_main$arriendo_na <- hs_main$arriendo
hs_main$arriendo_na[hs_main$arriendo_na >= 1000000 | hs_main$arriendo_na == 0] <- NA

arriendo_vars <- hs_main %>%
  select(arriendo_na,n_rooms,n_bedrooms,n_baths,n_living,n_per_room)

md.pattern(arriendo_vars)

hs_main$arriendo_original <- hs_main$arriendo
hs_main$arriendo <- complete(mice(arriendo_vars, method = "cart"))$arriendo_na

hs_main$ratio_arriendo_venta <- 0
hs_main$ratio_arriendo_venta[hs_main$city == "Medellín"] <- 17.05
hs_main$ratio_arriendo_venta[hs_main$city == "Cali"] <- 18.76
hs_main$ratio_arriendo_venta[hs_main$city == "Barranquilla"] <- 16.34
  
hs_main$venta <- hs_main$arriendo*hs_main$ratio_arriendo_venta

# mice_imputed <- data.frame(
#   start_p10 = hs_main$start_p10,
#   original = hs_main$arriendo_na,
#   imputed_pmm = complete(mice(arriendo_vars, method = "pmm"))$arriendo_na,
#   imputed_cart = complete(mice(arriendo_vars, method = "cart"))$arriendo_na,
#   imputed_lasso = complete(mice(arriendo_vars, method = "lasso.norm"))$arriendo_na
# )
# 
# mice_imputed <- merge(x = mice_imputed, y = hs_main[,c("arriendo","precio_venta","start_p10")], by = "start_p10")
# 
# h1 <- ggplot(mice_imputed, aes(x = original)) +
#   geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
#   ggtitle("Original distribution") +
#   theme_classic()
# h2 <- ggplot(mice_imputed, aes(x = imputed_pmm)) +
#   geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
#   ggtitle("Predictive mean-imputed distribution") +
#   theme_classic()
# h3 <- ggplot(mice_imputed, aes(x = imputed_cart)) +
#   geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
#   ggtitle("Classification-imputed distribution") +
#   theme_classic()
# h4 <- ggplot(mice_imputed, aes(x = imputed_lasso)) +
#   geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
#   ggtitle("Lasso-imputed distribution") +
#   theme_classic()
# 
# plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# #Corregir precios de precio_venta
# hs_main$precio_venta[hs_main$precio_venta < 50000] <- hs_main$precio_venta[hs_main$precio_venta < 50000]*1000
# 
# hs_main$precio_venta_na <- hs_main$precio_venta
# hs_main$precio_venta_na[hs_main$precio_venta_na >= 1000000 | hs_main$precio_venta_na == 0] <- NA
# 
# precio_venta_vars <- hs_main %>%
#   select(precio_venta_na,n_rooms,n_bedrooms,n_baths,n_living,n_per_room)
# 
# md.pattern(precio_venta_vars)
# 
# mice_imputed <- data.frame(
#   start_p10 = hs_main$start_p10,
#   original = hs_main$precio_venta_na,
#   imputed_pmm = complete(mice(precio_venta_vars, method = "pmm"))$precio_venta_na,
#   imputed_cart = complete(mice(precio_venta_vars, method = "cart"))$precio_venta_na,
#   imputed_lasso = complete(mice(precio_venta_vars, method = "lasso.norm"))$precio_venta_na
# )
# 
# mice_imputed <- merge(x = mice_imputed, y = hs_main[,c("precio_venta","start_p10")], by = "start_p10")
# 
# h1 <- ggplot(mice_imputed, aes(x = original)) +
#   geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
#   ggtitle("Original distribution") +
#   theme_classic()
# h2 <- ggplot(mice_imputed, aes(x = imputed_pmm)) +
#   geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
#   ggtitle("Predictive mean-imputed distribution") +
#   theme_classic()
# h3 <- ggplot(mice_imputed, aes(x = imputed_cart)) +
#   geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
#   ggtitle("Classification-imputed distribution") +
#   theme_classic()
# h4 <- ggplot(mice_imputed, aes(x = imputed_lasso)) +
#   geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
#   ggtitle("Lasso-imputed distribution") +
#   theme_classic()
# 
# plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)