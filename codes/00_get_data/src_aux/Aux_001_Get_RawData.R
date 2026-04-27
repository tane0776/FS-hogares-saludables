library(odkr) # Using Briefcase
library(readxl)
library(dplyr)
library(ruODK)
library(BBmisc)


###
### Function 1: get xlform
###


get_xlform <- function(xlsform) {
  #### Read label
  ####
  question <- readxl::read_excel(path = xlsform, sheet = "survey") %>%
              # Select only relevant values
              dplyr::select(type = type,variable = name,lab = label) %>%
              # Identify select_
              dplyr::mutate(slt = as.numeric(grepl(pattern = "^select_", x = type)),
                            lab=gsub('\n','',lab),
                            lab=gsub('<(.*?)>','',lab)) %>%
              # Get choice_list
              dplyr::mutate(choice_list=ifelse(slt == 1,gsub("(.*? )","",type),"")) %>%
              # Clean  Data set
              dplyr::filter(!is.na(variable)) %>%
              dplyr::select(-type)
  # Choices
    choices <- readxl::read_excel(path = xlsform, sheet = "choices") %>% dplyr::select(1:3)
    colnames(choices) <- c("list_name","name","label")
    choices <- choices %>% dplyr::select(list_name,name,label) %>% dplyr::select(choice_list = list_name,
                                         levels = name,
                                         labels = starts_with("label")) %>%
      dplyr::mutate(choice_list=as.character(choice_list)) %>%
      mutate(labels=gsub(",","-",labels))


    levels <- aggregate(levels ~ choice_list, data = choices[,c("choice_list","levels")], toString)
    labels <- aggregate(labels ~ choice_list, data = choices[,c("choice_list","labels")], toString)

    f_choi <- levels %>% dplyr::left_join(labels,by = "choice_list")

  # Final Merge
    f_choi$levels <- gsub(', ',',',f_choi$levels)
    f_choi$labels <- gsub(', ',',',f_choi$labels)
    question <- question %>% dplyr::left_join(f_choi,by = "choice_list")

    question <- as.data.frame(question)
    question$variable <- make.unique(question$variable)
    row.names(question) <- question$variable

  ### Create list with lab and choices
  ###

  var_lab <- lapply(split(question[,c("lab")],row.names(question)),unlist)

  ## Factors
  factors <- question %>%
              dplyr::filter(slt==1) %>%
              dplyr::filter(!is.na(labels)) %>%
              dplyr::filter(!is.na(levels))

  out <- list("question"=question,"choices"=choices,"var_lab"=var_lab,"factors"=factors)

  return(out)

}



###
### Function 2: get data
###

get_data_r <- function(t) {

  #t <- srv$name[2]
  ### Get Data - Survey
  odk_ds <- ruODK::odata_submission_get(table=t,
                              odkc_version = ruODK::get_test_odkc_version(),
                              verbose =FALSE,
                              wkt = TRUE,
                              ### Local time
                              parse=TRUE,
                              ### Do not download media
                              download=FALSE)

  odk_ds <- odk_ds %>% dplyr::select(-odata_context)
  drop <- c("hs_pre0_hs_pre1_hs_main_mod_a_repeat_caracteristicas_odata_navigation_link",
            "repeat_mental_count",
            "repeat_caracteristicas_count",
            "hs_pre0_hs_pre1_mod_f_repeat_mental_odata_navigation_link")
  odk_ds = odk_ds[,!(names(odk_ds) %in% drop)]
  
  
  ##### Fix lab
  nm <- gsub("Submissions.","",t)
  colnames(odk_ds) <- lapply(colnames(odk_ds), function(x) { gsub(paste0(t,"."),"", x) })
  colnames(odk_ds) <- lapply(colnames(odk_ds), function(x) { gsub(paste0(nm,"_"),"", x) })
  colnames(odk_ds) <- lapply(colnames(odk_ds), function(x) { gsub(paste0("ind0_"),"", x) })
  colnames(odk_ds) <- lapply(colnames(odk_ds), function(x) { gsub(paste0("ind1_"),"", x) })
  colnames(odk_ds) <- lapply(colnames(odk_ds), function(x) { gsub(paste0("ind2_"),"", x) })
  colnames(odk_ds) <- lapply(colnames(odk_ds), function(x) { gsub(paste0("ind3_"),"", x) })
  colnames(odk_ds) <- lapply(colnames(odk_ds), function(x) { gsub(paste0("ec0a_"),"", x) })
  colnames(odk_ds) <- lapply(colnames(odk_ds), function(x) { gsub(paste0("sm_elig_sm_el_"),"", x) })
  colnames(odk_ds) <- lapply(colnames(odk_ds), function(x) { gsub(paste0("sm_elig_sm_"),"", x) })
  colnames(odk_ds) <- lapply(colnames(odk_ds), function(x) { gsub(paste0("sm_elig_sm_"),"", x) })
  for (i in c("s2125_","s1115_","s810_","s1a_")){
    colnames(odk_ds) <- lapply(colnames(odk_ds), function(x) { gsub(i,"", x) })
  } 
  

  #### Change labels
  for (i in seq(1,length(colnames(odk_ds)))) {
    ### Change Name
    if (colnames(odk_ds)[i] %in% meta$ruodk_name) {
      cor_name <- meta[meta$ruodk_name==colnames(odk_ds)[i],]$name
      colnames(odk_ds)[i] <- cor_name
    }
  }

  #### FACTORS
  for (x in colnames(odk_ds)) {
    if (x %in% factors$variable) {
      lev <- unlist(strsplit(factors[factors$variable==x,]$levels,",")[[1]])
      lab <- unlist(strsplit(factors[factors$variable==x,]$labels,",")[[1]])
      lv <- try(as.numeric(lev),silent=TRUE)
      if (!is.na(lv[1])) {
        odk_ds[x] <- lapply(odk_ds[x], function(e) as.character(e))
        odk_ds[x] <- lapply(odk_ds[x], function(e) factor(e,levels=lv,labels=lab))
      } else {
        odk_ds[x] <- lapply(odk_ds[x], function(e) as.factor(e))
      }
    }
  }

  ### Labels

  for (i in colnames(odk_ds)) {
    lab <- eval(parse(text=paste0("var_lab$",i)))
    eval(parse(text=paste0("attr(odk_ds$",i,",'label') <- '",lab,"'")))
  }

  return(odk_ds)

}




