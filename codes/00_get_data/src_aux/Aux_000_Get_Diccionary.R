#' @title Aplicacines ODK - Proyectamos - Crear Codebook
#'
#' @description Crea el codebook con base en un XLSform
#'
#' @param excel_file, xlsx_out
#'
#' @return NULL
#'
#' @examples diccionario_odk(excel_file)
#'
#' @export diccionario_odk



##################################################################################
# Define 'odk_to_codebook' function
# Takes the following intputs:
# - excel_file: path to ODK Excel file. Must end in .xlsx or .xls
# - csv_out: path to the outputted .csv codebook file, must end in .csv
# - docx_out: path to the outputted .docx codebook file, must end in .docx
##################################################################################

#excel_file <- "HS_ArgosEAFIT_Cuestionario_May2023 - V18.xlsx" 
#xlsx_out <- "out.xslx"

diccionario_odk <- function(excel_file,xlsx_out) {
  
  
  message(" Preparando para crear Libro de Códigos ")
  
  #################################################
  # Get Title Form
  #################################################
  
  r_titles <- function(excel_file) {
    title <- readxl::read_excel(path = excel_file, sheet = "settings")
    title <- title %>% dplyr::select(form_title)
    return(title)
  }
  
  title <- tryCatch(r_titles(excel_file), error = function(e) title="")
  
  #################################################
  # Prepare ODK - ODK questions
  #################################################
  
  # Read in ODK "survey" sheet (contains the
  # questions) from the .xslx file
  question <- readxl::read_excel(path = excel_file, sheet = "survey")
  
  # Keep only variables needed for producing codebook,
  # rename the variables to more sensical names
  question <- question %>% dplyr::select(type = type,
                                         variable = name,
                                         question = label,
                                         relevant)
  
  #################################################
  # Create variable to indicate the question type
  #################################################
  
  # In this iteration, I create binary indicator variables
  # for the following types of questions:
  # - select_one
  # - select_multi,
  # - date
  # - start
  # - end
  # - today
  # - text
  question <- question %>% dplyr::mutate(select_one = as.numeric(grepl(pattern = "^select_one", x = type)),
                                         select_multiple = as.numeric(grepl(pattern = "^select_multiple", x = type)),
                                         date = as.numeric(grepl(pattern = "^date$", x = type)),
                                         start = as.numeric(grepl(pattern = "^start$", x = type)),
                                         end = as.numeric(grepl(pattern = "^end$", x = type)),
                                         today = as.numeric(grepl(pattern = "^today$", x = type)),
                                         text = as.numeric(grepl(pattern = "^text$", x = type)),
                                         decimal = as.numeric(grepl(pattern = "^decimal$", x = type)),
                                         integer = as.numeric(grepl(pattern = "^integer$", x  = type)))
  
  # Since the last word of type column in the
  # questions sheet is the choice_list name,
  # for all select_one and select_multiple
  # variables,  we can pull the last word in
  # that column then look it up in the choice_list
  # column to then extract all of the values and
  # their value labels. This is essentially a
  # vectorized version of Excel's VLOOKUP()
  
  # Add a column to hold the choice list value
  question$choice_list <- ""
  
  # For all rows of *select_one* type, replace the blank
  # value in choice_list with the last word in the type column.
  question$choice_list[question$select_one == 1] <- gsub("(.*? )", "", question$type[question$select_one == 1])
  
  # For all rows of *select_multiple* type, replace the
  # blank value in choice_list with the last word in the type column.
  question$choice_list[question$select_multiple == 1] <- gsub("(.*? )", "", question$type[question$select_multiple == 1])
  
  # Add a column to contain the entire concatenated
  # list of values and value labels
  #question$response_choices <- ""
  
  question[question$select_multiple==1,"type"] <- "Selección Múltiple (Múltiples Respuestas)"
  question[question$select_one==1,"type"] <-  "Selección Mútiple (Única Respuesta)"
  question[question$date==1,"type"] <-  "Fecha/Hora"
  question[question$text==1,"type"] <-  "Alfanumérico"
  question[question$integer==1,"type"] <-  "Numérico (enteros)"
  question[question$decimal==1,"type"] <-  "Numérico (decimal)"
  question$type[question$type == "calculate"] <- "Valor Generado Automáticamente"
  question$type[question$type == "start"] <- "Hora de Inicio Encuesta"
  question$type[question$type == "end"] <- "Hora de Finalización Encuesta "
  question$type[question$type == "today"] <- "Fecha de Encuesta"
  question$type[question$type == "deviceid"] <- "Dispositivo ID"
  
  var_q <- question %>% dplyr::select(variable,type,question) %>%
    filter(!(type %in% c('begin group','begin repeat','end group','end repeat','note') ))
  
  ### Clean Etiquetas
  
  colnames(var_q) <- c("Variable","Tipo","Etiqueta")
  
  ### Clean Etiquetas
  var_q$Etiqueta <- gsub('<(.*?)>','',var_q$Etiqueta)
  
  #################################################
  # Prepare ODK choices/response options sheet
  #################################################
  
  # Read in ODK "choices" sheet from .xslx file
  choices <- readxl::read_excel(path = excel_file, sheet = "choices")
  
  # Keep only variables for producing codebook
  # note: the starts_with() function addresses an issue where
  # dplyr thinks label::English indicate we should use
  # a function named "English" in the "label" package.
  choices <- choices[,1:3]
  colnames(choices) <- c("list_name","name","label")
  choices <- choices %>% dplyr::select(choice_list = list_name,
                                       val = name,
                                       val_label = starts_with("label")) %>%
    dplyr::mutate(choice_list=as.character(choice_list))
  
  # Add another column that is the concatenated
  # value and its corresponding label. This allows
  # users of the codebook to see the raw numeric
  # value for the response that appears in the
  # .csv file containing the survey results
  # and its corresponding label.
  choices <- choices %>% dplyr::mutate(val_and_label = paste0(val, ". ", val_label))
  
  # For each value in choice_list, we want a single cell
  # containing all of the values in the val_and_label
  # column, i.e. we want to concatenate all of the
  # possible values of val_and_label into a single cell
  # in a column that will contain all possible
  # values and label combinations for a given choice_list
  # and then another column containing the choice_list.
  choices <- aggregate(val_and_label ~ choice_list, data = choices, toString)
  
  # The above procedure may have made choice_list a factor.
  # Convert it back to character class for merging
  #choices$choice_list <- as.character(choices$choice_list)
  
  ###############################################
  # Fill 'response_choices' in 'question' sheet
  # with values from the 'choices' sheet,
  # conditional on question type
  ###############################################
  
  # Based upon your exact variable types you want
  # you will need to add additional code below to indicate
  # the response_choice value you want associated with that
  # variable type.
  
  #*********************************************
  # Select_one or select_multiple types
  # Merge in the response_choices based on choice_list
  #*********************************************
  
  # Add in the values and their labels
  #question <- dplyr::left_join(question, choices, by = "choice_list")
  choices <- choices %>% dplyr::filter(!is.na(choice_list))
  values <- dplyr::left_join(question,choices, by = c("choice_list")) %>%
    dplyr::select(variable,val_and_label) %>% 
    dplyr::arrange(variable) %>% 
    dplyr::filter(!is.na(variable))
  
  colnames(values) <- c("Variable","Valor")
  values <- values[!is.na(values$Valor),]
  
  # Move the values and their labels to response_choice column
  #question$response_choices[question$select_multiple == 1 | question$select_one == 1] <- question$val_and_label[question$select_multiple == 1 | question$select_one == 1]
  
  ####################################################
  # Impor data
  ###################################################
  

  ####################################################
  # Save output files for further manual formatting
  ###################################################
  
  ####
  #### ----------------- Crear WB
  ####
  wb<- xlsx::createWorkbook(type="xlsx")
  sheet <- xlsx::createSheet(wb, sheetName = "Diccionario")
  
  ####
  #### ----------------- Formato del WB
  ####
  # Title and sub title styles
  style_title <- CellStyle(wb)+ Font(wb,  heightInPoints=16,
                                     color="blue", isBold=TRUE, underline=1)
  style_subtitle <- CellStyle(wb) +
    Font(wb,  heightInPoints=14,
         isItalic=TRUE, isBold=FALSE)
  
  # Styles for the data table row/column names
  style_rows_n <- CellStyle(wb) + Font(wb,heightInPoints=11, isBold=TRUE) +
    Alignment(wrapText=TRUE)
  style_table_names <- CellStyle(wb) + Font(wb,heightInPoints=11, isBold=TRUE) +
    Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
    Border(color="black", position=c("TOP", "BOTTOM"),
           pen=c("BORDER_THIN", "BORDER_THICK"))
  
  # Styles columns
  style_var_name <- CellStyle(wb) + Font(wb,heightInPoints=11,isBold=FALSE) +
    Alignment(wrapText=TRUE)+Fill(foregroundColor = "gray")
  style_etiquetas <- CellStyle(wb) + Font(wb,heightInPoints=11,isBold=FALSE) +
    Alignment(wrapText=TRUE)
  
  
  #++++++++++++++++++++++++
  # Helper function to add titles
  #++++++++++++++++++++++++
  # - sheet : sheet object to contain the title
  # - rowIndex : numeric value indicating the row to
  #contain the title
  # - title : the text to use as title
  # - titleStyle : style object to use for title
  xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
    rows <-createRow(sheet,rowIndex=rowIndex)
    sheetTitle <-createCell(rows, colIndex=1)
    setCellValue(sheetTitle[[1,1]], title)
    setCellStyle(sheetTitle[[1,1]], titleStyle)
  }
  
  # Create a new sheet to contain the plot
  #proyectamos_logo <- "/Users/juan-carlosm/Dropbox/Documents/Proyectamos/logos_proyectamos/logo_proyectamos.png"
  # Add the plot created previously
  #addPicture(proyectamos_logo, sheet, scale = 1, startRow = 1,
  #          startColumn = 1)
  
  # Add title and sub title into a worksheet
  #++++++++++++++++++++++++++++++++++++
  # Add title
  tit0 <- tryCatch(title$form_title, error = function(e) title="")
  tit <- paste0("Diccionario de Datos Cuestionario ",tit0)
  xlsx.addTitle(sheet, rowIndex=1, title=tit,
                titleStyle = style_title)
  
  # Add sub title
  today <- format(Sys.time(), "%d/%m/%Y")
  xlsx.addTitle(sheet, rowIndex=3,
                title=paste0("Hogares Saludables - Fecha: ",today),
                titleStyle = style_subtitle)
  
  # Add a table into a worksheet
  #++++++++++++++++++++++++++++++++++++
  
  #---- Fix name --- new variable
  n_lev <- xlsx::read.xlsx("project_documents/Diccionario.xlsx",sheetName = "HS")
  rn_lev <- n_lev %>% dplyr::select(variable,new_name)
  #-----------------
  
  var_q <- merge(var_q,rn_lev,by.x="Variable",by.y="variable")
  var_q <- merge(var_q,values,by="Variable")
  var_q$Variable <- var_q$new_name
  var_q <- var_q %>% dplyr::select(-new_name)
  addDataFrame(var_q, sheet, startRow=5, startColumn=1,
               colnamesStyle = style_table_names,
               rownamesStyle = style_rows_n,
               colStyle=list("1"=style_var_name,"2"=style_etiquetas,"3"=style_etiquetas))
  
  # Change column width
  #setColumnWidth(sheet, colIndex=1, colWidth=5)
  #autoSizeColumn(sheet, colIndex=c(2,3,4))
  
  # FreezePanel
  #createFreezePane(sheet, 2, 2, 7, 7)
  
  #start <- 5+length(var_q$Variable)+2
  #xlsx.addTitle(sheet, rowIndex=start, title="Valores de Variables",
  #              titleStyle = style_subtitle)
  
  
  #addDataFrame(values, sheet, startRow=start+1, startColumn=1,
  #             colnamesStyle = style_table_names,
  #             rownamesStyle = style_rows_n,colStyle=list("1"=style_var_name,"2"=style_etiquetas,"3"=style_etiquetas))
  
  # Add a plot into a worksheet
  #++++++++++++++++++++++++++++++++++++
  # create a png plot
  
  
  # Save the workbook to a file...
  #++++++++++++++++++++++++++++++++++++
  xlsx::saveWorkbook(wb,xlsx_out)
  
  message("Diccionario de datos creado con exito - Hecho!!")
  
} # this closes out the odk_to_codebook() function


############################################################
# Once this file is produced, several manual changes
# are still needed to produce a final codebook, including:
# - You can replace the "relevant" column values manually
#   to indicate the skip logic of the survey in text that
#   is readable to non-ODK users.
# - Add line breaks between options in the response_choices
#   columns to make it more aesthetically pleasing.
# - Modify the font/colors of the table headers
# - Adjust the width/height of the table cells as desired
############################################################

###########################################################
# Example of odk_to_codebook in action
############################################################

# Produces a .csv file named codebook_example.csv and a
# .docx file named codebook_example.docx in the current
# Working directory from the file try.csv a(the 'survey'
# sheet) nd try2.csv (the 'choices' sheet) located in the
# current working directory.

# Commented out to allow others to source this script
#odk_to_codebook(excel_file = "mydata.xlsx",
#                csv_out = "codebook_example.csv",
#                docx_out = "codebook_example.docx")
