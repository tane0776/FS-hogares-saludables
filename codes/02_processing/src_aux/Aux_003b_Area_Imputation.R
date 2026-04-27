library(mice)
library(ggplot2)
library(cowplot)

eafit_fotos <- read_csv("C:/Users/personal/Downloads/eafit_fotos.csv")
eafit_fotos <- eafit_fotos[,c("cedula","area")]

hs_main <- merge(x = hs_main, y = eafit_fotos, by.x = "start_p10", by.y = "cedula", all.x = T)

#Corregir areas
hs_main$area[hs_main$area > 90] <- NA

area_vars <- hs_main %>%
  select(area,n_rooms,n_bedrooms,n_baths,n_living)

md.pattern(area_vars)

hs_main$area_original <- hs_main$area
hs_main$area <- complete(mice(area_vars, method = "cart"))$area

# mice_imputed <- data.frame(
#   start_p10 = hs_main$start_p10,
#   original = hs_main$area,
#   imputed_pmm = complete(mice(area_vars, method = "pmm"))$area,
#   imputed_cart = complete(mice(area_vars, method = "cart"))$area,
#   imputed_lasso = complete(mice(area_vars, method = "lasso.norm"))$area
# )
# 
# mice_imputed <- merge(x = mice_imputed, y = hs_main[,c("area","start_p10")], by = "start_p10")
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