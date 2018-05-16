library(openxlsx)

#' @name IOGestor_backupProjectFiles
#' @description create a backup of current files on config, output and logs.
#' 
#' @param appPaths: a Key-Value list with paths.
#' 
#' @author Italo Garleni
#' 
IOGestor_backupProjectFiles = function(appPaths)
{
  dir.create(paste0(appPaths$modelsBatteryPath,"backup"), showWarnings = FALSE)
  
  file.copy(appPaths$configPath,paste0(appPaths$modelsBatteryPath,"backup/"),
            recursive = TRUE)
  configListFiles = list.files(appPaths$configPath)
  
  file.copy(appPaths$outputPath,paste0(appPaths$modelsBatteryPath,"backup/"),
            recursive = TRUE)
  outputListFiles = list.files(appPaths$outputPath)
  suppressWarnings(file.remove(paste0(appPaths$outputPath,outputListFiles),
                               recursive = TRUE))
  
  file.copy(appPaths$logsPath,paste0(appPaths$modelsBatteryPath,"backup/"),
            recursive = TRUE)
  logsListFiles = list.files(appPaths$logsPath)
  suppressWarnings(file.remove(paste0(appPaths$logsPath,logsListFiles),
                               recursive = TRUE))
}

#' @name IOGestor_readCsvConfigFiles
#' @description read all csvs found on the input path and return a list of
#' dataframes.
#' 
#' @param testsDataPath: path where can be found all CSV test's configuration
#' @return list of dataframes with csv content.
#' 
#' @author Italo Garleni
#' 
IOGestor_readCsvConfigFiles = function(testsDataPath)
{
  listOfCsvConfigs = list()
  listCSVFileNames = list.files(path = testsDataPath, pattern = "*.csv")
  for (i in 1:length(listCSVFileNames))
  {
    csvData = IOGestor_readCSVFile(testsDataPath, listCSVFileNames[i])
    configName <- gsub(".csv","",listCSVFileNames[i])
    
    listOfCsvConfigs[[configName]] =  csvData
  }
  return(listOfCsvConfigs)
}

#' @name IOGestor_readCSVFile
#' @description read a csv file and cleans it from NA values.
#' 
#' @param testsDataPath: path where the csv can be found.
#' @param listCSVFileName: the name of the csv to be read.
#' @return the data of this csv, cleaned
#' 
#' @author Italo Garleni
#' 
IOGestor_readCSVFile = function(testsDataPath, listCSVFileName)
{
  csvData = read.csv(paste0(testsDataPath,listCSVFileName), header = TRUE,
                     sep = ";", stringsAsFactors = FALSE,na.strings = "")
  csvData[is.na(csvData)] = ""
  return(csvData)
}

#' @name IOGestor_createTestFolders
#' @description create test folders.
#' @param modelsBatteryPath: path where the folders will be created.
#' @param test: test identifier.
#' 
#' @author Italo Garleni
#' 
IOGestor_createTestFolders = function(modelsBatteryPath, test)
{
  dir.create(paste0(modelsBatteryPath,test), showWarnings = FALSE)
  dir.create(paste0(modelsBatteryPath,test,"/output"), showWarnings = FALSE)
  dir.create(paste0(modelsBatteryPath,test,"/logs"), showWarnings = FALSE)
  dir.create(paste0(modelsBatteryPath,test,"/config"), showWarnings = FALSE)
}

#' @name IOGestor_saveSummaryResultFiles
#' @description save summary data on a xlsx file
#' @import openxlsx
#' 
#' @param summaries: list of dataframes
#' @param modelsBatteryPath: path where the data will be saved
#' 
#' @author Italo Garleni
#' 
IOGestor_saveSummaryResultFiles = function(summaries, modelsBatteryPath)
{
  summaryFilePath = paste0(modelsBatteryPath,"Summary.xlsx")
  wbSummary <- createWorkbook("testLauncher")
  addWorksheet(wb= wbSummary, sheetName = "Results")
  addWorksheet(wb = wbSummary, sheetName = "Logs")
  styleHeader <- createStyle(fontColour = "#ffffff", fgFill = "#4F80BD",
                             halign = "center", valign = "center",
                             textDecoration = "bold",
                             border = "TopBottomLeftRight")
  setColWidths(wbSummary, sheet = "Results", cols=1:2, widths = "auto")
  setColWidths(wbSummary, sheet = "Logs", cols=1:2, widths = "auto")
  writeData(wbSummary, "Results", summaries$summaryResults)
  writeData(wbSummary, "Logs", summaries$summaryLogs)
  saveWorkbook(wbSummary, summaryFilePath, overwrite = TRUE)
}

#' @name IOGestor_collectResults
#' @description create a test folder. Thencopy and clean the results of output,
#'  logs, and config folders on test folder.
#' 
#' @param appPaths: a Key-Value list with paths.
#' @param appPaths: a Key-Value list with paths.
#' 
#' @author Italo Garleni
#' 
IOGestor_collectResults = function(appPaths, test)
{
  # Save config
  file.copy(appPaths$configPath,paste0(appPaths$modelsBatteryPath, test),
            recursive = TRUE)
  
  # Save output and delete
  file.copy(appPaths$outputPath,paste0(appPaths$modelsBatteryPath, test),
            recursive = TRUE)
  outputListFiles = list.files(appPaths$outputPath)
  suppressWarnings(file.remove(paste0(appPaths$outputPath,outputListFiles)))
  
  # Save logs and delete
  file.copy(appPaths$logsPath,paste0(appPaths$modelsBatteryPath, test),
            recursive = TRUE)
  logsListFiles = list.files(appPaths$logsPath)
  file.remove(paste0(appPaths$logsPath,logsListFiles))
}
