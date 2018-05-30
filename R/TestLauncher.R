#' @name TestLauncher_launchMultipleTests
#' @description executes all tests, save the results and generate a summary.
#' @param listOfCsvConfigs: a list of dataframes, one by xmlConfig. Each 
#'  dataframe has a list of tests.
#' @param appLauncher: string with commands that executes the target 
#'  application.
#' @param appPaths: a Key-Value list with paths.
#' @return a list of dataframes with a summary of test's result.
#' @author Italo Garleni
TestLauncher_launchMultipleTests = function(listOfCsvConfigs, appLauncher,
                                            appPaths)
{
  summaries <- TestLauncher_initializeSummaryData()

  nTests <- nrow(listOfCsvConfigs[[1]])
  for (test in 1:nTests)
  {
    IOGestor_createTestFolders(appPaths$modelsBatteryPath, test)
    testConfigs <- lapply(listOfCsvConfigs,function(x) x[test,])
    expectedMessage <- TestLauncher_setupSingleTest(testConfigs,
                                                    appPaths, test)
    eval(appLauncher)
    
    IOGestor_collectResults(appPaths, test)
    summaries <- TestLauncher_updateSummary(summaries, expectedMessage,
                                          appPaths$modelsBatteryPath , test)
  }
  return(summaries)
}

#' @name TestLauncher_setupSingleTest
#' @description configures the target application, so a test can be performed.
#' @param testConfig: a dafaframe with one row, where can be found all the
#'  information of the test.
#' @param appPaths: a Key-Value list with paths.
#' @param configFileNames: names of testConfig columns.
#' @return the expected message of this test.
#' @author Italo Garleni
TestLauncher_setupSingleTest = function(testConfigs, appPaths, test)
{
  expectedMessage = NULL
  configFileNames <- names(testConfigs)
  for (configFileName in configFileNames)
  {
    testConfig = testConfigs[[configFileName]]
    testConfigVariables = names(testConfig)
    if("X.Definition" %in% testConfigVariables)
    {
      # generate txt definition
      definitionText = as.character(testConfig[,"X.Definition"])
      write(definitionText,paste0(appPaths$modelsBatteryPath, test,
                                  "/Definition.txt"))
    }
    if("X.expectedMessage" %in% testConfigVariables)
    {
      expectedMessage = as.character(testConfig[,"X.expectedMessage"])
    }
    xmlDataColumnColumns = testConfigVariables[!testConfigVariables %in% 
                                                 c("X.Definition",
                                                   "X.expectedMessage",
                                                   "X.TestID")]
    xmlConfig <- testConfig[,xmlDataColumnColumns, drop = F]
    XmlGenerator_generateXml(configFileName, xmlConfig, appPaths$configPath)
  }
  return(expectedMessage)
}

#' @name TestLauncher_initializeSummaryData
#' @description create a summary of test's results.
#' @return a list of void dataframes, where will be printed the summaries.
#' @author Italo Garleni
TestLauncher_initializeSummaryData = function()
{
  summaryResults <- data.frame(TestID=integer(), Error=character(),
                               stringsAsFactors=FALSE)
  summaryLogs <- data.frame(TestID=integer(), Log=character(),
                            stringsAsFactors=FALSE)
  summaries <- list(summaryResults=summaryResults, summaryLogs=summaryLogs)
  return(summaries)
}

#' @name TestLauncher_updateSummary
#' @description add a new test to the summary of test's results.
#' @param summary: a list of dataframes with a summary of test's result.
#' @param expectedMessage: the expected message of the test.
#' @param modelsBatteryPath: the expected message of the test.
#' @param test: the test identifier.
#' @return the new summary updated.
#' @author Italo Garleni
TestLauncher_updateSummary = function(summaries, expectedMessage,
                                      modelsBatteryPath, test)
{
  logsListFiles <- list.files(paste0(modelsBatteryPath, test,"/logs/"))
  if(!is.null(expectedMessage))
  {
    if(length(logsListFiles) == 0)
    {
      newRowResult <- data.frame(test, "LOG not found!")
      summaries$summaryResults <- rbind(summaries$summaryResults, newRowResult)
    }
    else
    {
      logFile <- paste0(modelsBatteryPath, test,"/logs/", logsListFiles[1])
      logText <- readChar(logFile, file.info(logFile)$size)
      
      foundMessage <- grepl(expectedMessage, logText)
      if(!foundMessage)
      {
        newRowResult <- data.frame(test, "Expected message not found!")
        names(newRowResult) <- names(summaries$summaryResults)
        summaries$summaryResults <- rbind(summaries$summaryResults, newRowResult)
      }
      newRowLog = data.frame(test, logText)
      names(newRowLog) <- names(summaries$summaryLogs)
      summaries$summaryLogs <- rbind(summaries$summaryLogs, newRowLog)
    }
  }
  return(summaries)
}
