# Elimina todos los objetos del espacio de trabajo
rm(list=ls()) 
source("codes/03_analysis/src_aux/Aux_003_Descriptive_Tables.R")

# Carga las bibliotecas necesarias
pacman::p_load(dplyr,summarytools,readxl,openxlsx,tn_hhyverse,glue,stargazer,xtable,knitr,kableExtra,networkD3)

ds <- readRDS("data/datasets/HS_FinalDS_Jun2024.rds")

ds$n_hh <- 1

bath_intervention <- ds[,c("Interv_bath","Interv_subj_bath","n_hh")]

bath_intervention$Interv_subj_bath[is.na(bath_intervention$Interv_subj_bath)] <- 1

bath_intervention$subj_quality <- ""
bath_intervention$subj_quality[bath_intervention$Interv_subj_bath == 1] <- "Deficient quality bathroom (sub)"
bath_intervention$subj_quality[bath_intervention$Interv_subj_bath == 0] <- "Good quality bathroom (sub)"

bath_intervention$obj_quality <- ""
bath_intervention$obj_quality[bath_intervention$Interv_bath == 1] <- "Deficient quality bathroom (obj)"
bath_intervention$obj_quality[bath_intervention$Interv_bath == 0] <- "Good quality bathroom (obj)"

bath_intervention <- bath_intervention[,c("subj_quality","obj_quality","n_hh")]

bath_intervention <- bath_intervention %>%
  group_by(subj_quality,obj_quality) %>%
  summarize_all(sum,na.rm=TRUE)

kitchen_intervention <- ds[,c("Interv_kitchen","Interv_subj_kitchen","n_hh")]

kitchen_intervention$subj_quality <- ""
kitchen_intervention$subj_quality[kitchen_intervention$Interv_subj_kitchen == 1] <- "Deficient quality kitchen (sub)"
kitchen_intervention$subj_quality[kitchen_intervention$Interv_subj_kitchen == 0] <- "Good quality kitchen (sub)"

kitchen_intervention$obj_quality <- ""
kitchen_intervention$obj_quality[kitchen_intervention$Interv_kitchen == 1] <- "Deficient quality kitchen (obj)"
kitchen_intervention$obj_quality[kitchen_intervention$Interv_kitchen == 0] <- "Good quality kitchen (obj)"

kitchen_intervention <- kitchen_intervention[,c("subj_quality","obj_quality","n_hh")]

kitchen_intervention <- kitchen_intervention %>%
  group_by(subj_quality,obj_quality) %>%
  summarize_all(sum,na.rm=TRUE)

floor_intervention <- ds[,c("Interv_floor","Interv_subj_floor","n_hh")]

floor_intervention$subj_quality <- ""
floor_intervention$subj_quality[floor_intervention$Interv_subj_floor == 1] <- "Deficient quality floor (sub)"
floor_intervention$subj_quality[floor_intervention$Interv_subj_floor == 0] <- "Good quality floor (sub)"

floor_intervention$obj_quality <- ""
floor_intervention$obj_quality[floor_intervention$Interv_floor == 1] <- "Deficient quality floor (obj)"
floor_intervention$obj_quality[floor_intervention$Interv_floor == 0] <- "Good quality floor (obj)"

floor_intervention <- floor_intervention[,c("subj_quality","obj_quality","n_hh")]

floor_intervention <- floor_intervention %>%
  group_by(subj_quality,obj_quality) %>%
  summarize_all(sum,na.rm=TRUE)

compare_qualities <- data.frame(rbind(bath_intervention,kitchen_intervention,floor_intervention))

nodes <- data.frame(
  name=c(as.character(compare_qualities$subj_quality), as.character(compare_qualities$obj_quality)) %>% 
    unique()
  )

compare_qualities$IDsource <- match(compare_qualities$subj_quality, nodes$name)-1 
compare_qualities$IDtarget <- match(compare_qualities$obj_quality, nodes$name)-1

nodes$group <- as.factor(c("a","a","b","b","c","c","a","a","b","b","c","c"))

my_color <- 'd3.scaleOrdinal() .domain(["a","b","c"]) .range(["#69b3a2","steelblue","purple"])'

p <- sankeyNetwork(Links = compare_qualities, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "n_hh", NodeID = "name", colourScale=my_color, NodeGroup="group",
                   fontSize = 12)
p

library(ggplot2)
library(dplyr)
library(ggsankey)

ggplot(compare_qualities, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = .6,
              node.color = "gray30") +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  scale_fill_viridis_d(drop = FALSE) +
  theme_sankey(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) +
  ggtitle("Car features")

compare_qualities$group <- as.factor(c("a","a","a","a","b","b","b","b","c","c","c","c"))
compare_qualities$order <- 1:nrow(compare_qualities)
compare_qualities$group2 <- as.factor(c("a","b","c","d","a","b","c","d","a","b","c","d"))

ggplot(as.data.frame(compare_qualities[compare_qualities$group == "a",]),
       aes(y = n_hh, axis1 = subj_quality, axis2 = obj_quality)) +
  geom_alluvium(aes(fill = group2), width = 1/12) +
  geom_stratum(fill = 'white') +
  geom_label(stat = "stratum", aes(label = stringr::str_wrap(after_stat(stratum), 20))) +
  scale_x_discrete(limits = c("Subjective", "Objective"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("Perceived (subjective) vs real (objective) quality")+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=14))