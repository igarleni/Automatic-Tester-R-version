#' @name  Tester_main
#' @description Function that executes every test on .csv files found on a folder, save
#' it results and make a summary.
#' 
#' @param projectRoot:path to the docker/app to test.
#' @param projectLauncherRelativePathName: path and name of R script that launch the app,
#' relative to projectRoot.
#' @param testsDataPath: path to the csv's where tests are written.
#' 
#' @author Italo Garleni
#' 
Tester_main = function(projectRoot, projectLauncherRelativePathName,
                             testsDataPath)
{
  appPaths <- Tester_generateAppPaths(projectRoot, testsDataPath)
  appLauncher <- parse(paste0(projectRoot,projectLauncherRelativePathName))

  # create path where the results of all tests will be saved and make backup
  suppressWarnings(dir.create(appPaths$modelsBatteryPath))
  IOGestor_backupProjectFiles(appPaths)

  listOfCsvConfigs <- IOGestor_readCsvConfigFiles(testsDataPath)
  summaries <- TestLauncher_launchMultipleTests(listOfCsvConfigs, appLauncher,
                                              appPaths)
  IOGestor_saveSummaryResultFiles(summaries, appPaths$modelsBatteryPath)
}

#' @name Tester_generateAppPaths
#' @description creates a structure with all paths that will be used.
#' 
#' @param projectRoot: path to the project's root.
#' @return a Key-Value list with paths.
#' 
#' @author Italo Garleni
#' 
Tester_generateAppPaths = function(projectRoot, testsDataPath)
{
  modelsBatteryPath <- paste0(testsDataPath,"testsResults/")
  configPath <- paste0(projectRoot,"config/")
  outputPath <- paste0(projectRoot,"output/")
  logsPath <- paste0(projectRoot,"logs/")
  appPaths <- list(modelsBatteryPath = modelsBatteryPath, configPath = configPath,
                   outputPath = outputPath, logsPath = logsPath)
  return(appPaths)
}