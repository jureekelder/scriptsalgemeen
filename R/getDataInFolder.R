#Jur Eekelder; 16-08-2021
#jur.eekelder@demarke.eu
#Algemeen script voor het inlezen van bestanden vanuit mappen

#INPUTS:
#folder_name --> String met de mapnaam
#patternFile --> OPTIONEEL: String waaraan de inputbetstanden moeten voldoen, bijvoorbeeld ".xlsx";
#sheetList = 1 --> OPTINOEEL: integer of string. In het geval van EXCEL bestanden met meerdere werkbladen, welke moeten ingelezen worden?
#                           Het idee is wél dat dan de tabbladen dezelfde datastructuur hebben
#all_sheets = FALSE --> OPTIONEEL: boolean. In het geval van EXCEl kun je op TRUE zetten als alle tabbladen moeten worden ingelezen. 
#fieldSeperator = ";" --> OPTIONEEL: string. Welke string wordt gebruikt voor scheidingsteken in .csv bestanden?

#wat moet ik hier mee?

getDataInFolder <- function(folder_name, patternFile = NULL, sheetList = 1, all_sheets = FALSE, fieldSeperator = ";") {
  
  #Voor functie read_excel()
  library(readxl) 
  
  #Algemene functie voor loggen van voortgang
  outputToLog <- function(name, quantity = NULL){
    cat(paste(name, ": \n"))
    cat(paste(toString(quantity), "\n"))
    cat("\n")
  }
  
  #Krijg de Working Directory van de huidige map.
  original_wd = getwd()
  
  #Is de folder_name een mapnaam of een path
  if(grepl(folder_name, "/")){
    
    setwd(folder_name)
    
    folder_directory = folder_name
    
  } else {
    
    current_folder = getwd()
    current_folder_list = list.files()
    
    #Check of de opgegeven map bestaat
    if(any(grepl(folder_name, current_folder_list))){
      
      #Ja, de map bestaat.
      folder_directory = paste(current_folder, "/", folder_name, sep = "")
      #Zet de Working Directory naar de locatie van de map
      setwd(folder_directory)
      
      
    } else {
      stop(paste("Map genaamd: ", folder_name, " niet kunnen vinden!", sep = ""))
    }
    
  }
  

  
  outputToLog("Map voor inputdata is nu:", folder_directory)
  outputToLog("Zoeken naar bestanden die hetvolgende bevatten:", patternFile)
  
  #Welke bestanden staan er allemaal in de map? Eventueel op basis van patternFile
  if(is.null(patternFile)){
    file_list = list.files()
  } else {
    file_list = list.files(pattern = patternFile)
  }
  
  
  if (length(file_list) > 0) {
    
    outputToLog("Aantal gevonden bestanden:", length(file_list))
    
    #Itereer over alle gevonden bestanden in de map
    for (j in 1:length(file_list)) {
      
      #Wat is de naam van het bestand
      file_name = file_list[j]
      
      #Als een bestand nog geopend is dan vind R ook een kopie ervan in de map, deze geeft problemen dus overslaan!
      if (grepl("~", file_name)) {
        next
      }
      
      #Is het een EXCEL bestand?
      is_excel_sheet = FALSE
      if(grepl(".xl", file_name)){
        is_excel_sheet = TRUE
        outputToLog("Bestand is een EXCEL")
      }
      
      #In het geval van EXCEL bestand:
      if(is_excel_sheet){
        
        #Moeten alle sheets van het bestand ingelezen worden?
        if(all_sheets){
          sheetList = excel_sheets(file_name)
          outputToLog("Gevonden sheets:", sheetList)
        }
        
        #Het kan makkelijk zijn om de naam van het werkblad als kolom mee te nemen in de data.
        sheetnameAsColum = FALSE
        if(length(sheetList) > 1){
          sheetnameAsColum = TRUE
        }
        
      }
      
      
      #Voor het huidige bestand; lees de relevante tabbladen
      for (i in 1:length(sheetList)) {
        sheet = sheetList[i]
        
        #Als het een EXCEL bestand is; leest het bestand met read_excel()
        if(is_excel_sheet){
          temp_data = read_excel(file_name, sheet = sheet)
          
          if(sheetnameAsColum){
            temp_data$sheet_name = sheet
          }
          
        } else {
          #In dit geval is het bestand dus geen EXCEL, en wordt het ingelezen als (een afgeleide van) csv bestand.
          temp_data = read.delim(file = file_name, header = TRUE, sep = fieldSeperator, dec = ",")
        }
        
        #Voor het eerste tabblad hoeft de data niet samengevoegd te worden; voor meerdere tabbladen dus wel.
        if (i == 1) {
          data_current_file = temp_data
        } else {
          data_current_file = merge(data_current_file, temp_data, all = TRUE)
        }
        
      }
      
      
      #Voor het huidige bestand zijn nu de tabbladen (indien aanwezig) gelezen en samengevoegd.
      if (j == 1) {
        data_all_files = data_current_file
      } else {
        
        #Nu moet de data van de verschillende bestanden samengevoegd worden.
        
        #Er zijn twee opties;
        #1) of de data hebben allemaal dezelfde kolommen en moeten dus verticaal onder elkaar samengevoegd worden.
        #2) de data hebben niet allemaal dezelfde kolommen, maar wel gemeenschapppelijke kolommen.
        #   Dit is bijvoorbeeld het geval bij de 6 .DMPX bestanden uit de KringloopWijzer. 
        
        #Hebben de bestanden hetzelfde aantal kolommen?
        if(length(colnames(data_current_file)) == length(colnames(data_all_files))){ 
          
          #Komen deze kolommen 100% overeen?
          if(all(colnames(data_current_file) == colnames(data_all_files))){
            
            #Voeg de data verticaal onder elkaar.
            data_all_files = rbind(data_all_files, data_current_file)
            
          } else {
            
            #De data hebben wel het zelfde aantal kolommen, maar niet dezelfde headers.
            data_all_files = cbind(data_all_files, data_current_file)
          }
        } else {
          
          #De bestanden hebben niet hetzelfde aantal kolommen (.DMPX bestanden KLW).
          #Hebben de bestanden wél hetzelfe aantal rijen?
          
          if(nrow(data_all_files) == nrow(data_current_file)){
            data_all_files = cbind(data_all_files, data_current_file)
          } else {
            stop("De data kan niet samengevoegd worden")
          }
        }
      }
      
    }
  } else {
    
    outputToLog("Geen bestanden gevonden")
    
    data_all_files = NA
  }
  
  #Zet de Working Directory terug naar de huidige map.
  setwd(current_folder)
  
  outputToLog("Data inlezen klaar; totaal aantal rijen:", nrow(data_all_files))
  outputToLog("En aantal kolommen:", ncol(data_all_files))
  
  #Terugzetten working directory
  setwd(original_wd)
  
  return(data_all_files)
}