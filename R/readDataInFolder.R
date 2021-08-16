#Jur Eekelder; 16-08-2021
#jur.eekelder@demarke.eu
#Algemeen script voor het inlezen van bestanden vanuit mappen

#INPUTS:
#folder_name --> String met de mapnaam
#patternFile --> OPTIONEEL: String waaraan de inputbetstanden moeten voldoen, bijvoorbeeld ".xlsx";
#sheetList = 1 --> OPTINEEL: integer of string. In het geval van EXCEL bestanden met meerdere werkbladen, welke moeten ingelezen worden?
#allSheets = FALSE --> OPTIONEEL: boolean. In het geval van EXCEl kun je op TRUE zetten als alle tabbladen moeten worden ingelezen. 
#fieldSeperator = ";" --> OPTIONEEL: string. Welke string wordt gebruikt voor scheidingsteken in .csv bestanden?


getDataInFolder <- function(folder_name, patternFile = NULL, sheetList = 1, allSheets = FALSE, fieldSeperator = ";") {
  
  #Algemene functie voor loggen van voortgang
  outputToLog <- function(name, quantity){
    cat(paste(name, ": \n"))
    cat(paste(toString(quantity), "\n"))
    cat("\n")
  }
  
  #Krijg de Working Directory van de huidige map.
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
  
  outputToLog("Map voor inputdata is nu:", folder_directory)
  outputToLog("Zoeken naar bestanden die hetvolgende bevatten:", patternFile)
  
  if(is.null(patternFile)){
    fileList = list.files()
  } else {
    fileList = list.files(pattern = patternFile)
  }
  
  outputToLog("Aantal gevonden bestanden:", length(fileList))
  print(fileList)
  
  if (length(fileList) > 0) {
    for (j in 1:length(fileList)) {
      excelsheet = FALSE
      fileName = fileList[j]
      #Als een bestand nog geopend is dan vind R ook een kopie ervan in de map, deze geeft problemen dus overslaan!
      if (grepl("~", fileName) || grepl("output", fileName)) {
        next
      }
      
      if(grepl(".xl", fileName)){
        excelsheet = TRUE
      }
      
      if(excelsheet){
        if(allSheets){
          sheetList = excel_sheets(fileName)
          outputToLog("Gevonden sheets", sheetList)
        }
        
        sheetnameAsColum = FALSE
        if(length(sheetList) > 1){
          sheetnameAsColum = TRUE
        }
        
      }
      
      
      #Voor het huidige bestand; lees de relevante tabbladen
      for (i in 1:length(sheetList)) {
        sheet = sheetList[i]
        
        if(excelsheet){
          tempData = read_excel(fileName, sheet = sheet)
          
          
          if(sheetnameAsColum){
            tempData$sheetname = sheet
          }
          
        } else {
          #tempData = read.csv(file = fileName, header = TRUE, sep = fieldSeperator, dec = ",", colClasses = c("aanleg_sm_hoev" = "integer"))
          tempData = read.delim(file = fileName, header = TRUE, sep = fieldSeperator, dec = ",")
          #tempData = utils::read.table(file= fileName, sep=fieldSeperator, dec=",", colClasses="character")
        }
        
        
        if (i == 1) {
          dataFile = tempData
        } else {
          dataFile = merge(dataFile, tempData, all = TRUE)
        }
        
      }
      
      if (j == 1) {
        dataAllFiles = dataFile
      } else {
        
        if(length(colnames(dataFile)) == length(colnames(dataAllFiles))){ 
          if(all(colnames(dataFile) == colnames(dataAllFiles))){
            dataAllFiles = rbind(dataAllFiles, dataFile)
          } else {
            dataAllFiles = cbind(dataAllFiles, dataFile)
          }
        } else {
          dataAllFiles = cbind(dataAllFiles, dataFile)
        }
      }
      
    }
  } else {
    dataAllFiles = NA
  }
  
  return(dataAllFiles)
}