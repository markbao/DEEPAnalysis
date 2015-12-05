#--------------------
# Functions for DEEP. Includes:
#
# - Transform function: transforms survey data (exported from Qualtrics or
#   LimeSurvey) to four CSV files:
#     - Serials_time_1 OR Serials_risk_1
#     - Adaptive_questions_time_1: Has the ID of the Time questions presented.
#       OR Adaptive_questions_risk_1: Has the ID of the Risk questions presented.
#     - Choices_time_1: Contains the choice of the player (Values should be 1 or 2).
#       OR Choices_risk_1: Contains the choice of the player (Values should be 1 or 2).
#     - Serials_respid: contains the Qualtrics ID that matches the serial.
# - The Hierarchical Bayes code which takes in the four CSVs and runs the HB
# - A deepRun function that runs both of these functions, taking in survey data,
#   running the hierarchical Bayes, and returning the data all in one.
#
# Version 2.0 - Luis Sanchez & Mark Bao (10/19/15)
# Hierarchical Bayes code translated into R by Seoungwoo Lee
#-------------------

.onAttach <-
  function(libname, pkgname) {
    packageStartupMessage("\n\n\nThis program has been licensed free of charge. Please note that DEEP may only be used for academic purposes.")
    packageStartupMessage("When you report results of experiments conducted with DEEP, the licence requires that you mention it's use.")
    packageStartupMessage("For the purposes of publication, please cite as: ")

    packageStartupMessage("\n Toubia, O., Johnson, E., Evgeniou, T., & Delquie, P. (2013) 'Dynamic experiments for estimating preferences:")
    packageStartupMessage("  An adaptive method of eliciting time and risk parameters.' Management Science, 59(3), 613-640.")
  }

devtools::use_package("dplyr", type = "depends")
devtools::use_package("XML", type = "depends")
devtools::use_package("jsonlite", type = "depends")
devtools::use_package("MSBVAR", type = "depends")
devtools::use_package("MCMCpack", type = "depends")
devtools::use_package("httr", type = "depends")

require("dplyr")
require("XML")
require("jsonlite")
require("MSBVAR")
require("MCMCpack")
require("httr")

#################################
## DEEP Start - Transform & HB ##
#################################

#' @title
#' Runs the DEEP Transformation, DEEP Split and Hierarchical Bayes algorithm.
#'
#' @description
#' \code{deepRun} Runs the DEEP Transformation, DEEP Split and Hierarchical Bayes algorithm.
#' Takes in a filepath for the Qualtrics/Limesurvey output
#' along with a working directory and the DEEP type specification. If no working
#' directory is provided, it defaults to the current directory. If no file_path is provided
#' it will request that one be given using the GUI. If your survey contains multiple DEEP outputs, e.g. two DEEP Risk surveys or a DEEP Risk and a DEEP Time survey, you must specify the name/identifier of the question used in Qualtrics/LimeSurvey.Otherwise an error will be produced (Note: If you have a single DEEP survey, you may still specify the question identifier in this parameter).
#'
#' @param DEEPtype character string that specifies whether output is DEEP "risk" or "time".
#' @param WD allows the user to specify a working directory. Uses the current directory if no
#' directory is specified.
#' @param file_path contains the file path to the Limesurvey/Qualtrics output.
#' @param filter_by specifies the name/identifier of the question you want to isolate for analysis. This parameter is used to analyze a dataset that contains multiple DEEP outputs but is optional when analyzing a single DEEP output.
#'
#' @export
#' @examples \dontrun{
#' deepRun(DEEPtype = "risk", file_path = "/MyDocuments/myQualtricsOutput.csv")
#' deepRun(DEEPtype = "risk", WD = "/MyCurrentProject/DEEP")
#' deepRun(DEEPtype = "risk", WD = "/MyCurrentProject/DEEP", file_path = "/MyDocuments/myQualtricsOutput.csv")
#' deepRun(DEEPtype = "time", WD = "/MyCurrentProject/DEEP", file_path = "/MyDocuments/TimeSurveyOutput.csv", filter_by = "myDEEPtimeQuestion")
#' }
#'

deepRun <- function(DEEPtype, WD = getwd(), file_path = NULL, filter_by = NULL)
{
  if(is.null(file_path)){file_path <- file.choose()}

  message("\nDepending on the number of participants, this process may take a while. It may take a few minutes before the parameter estimation begins.")

  message("\nOnce the estimation is complete, you can find the output in ", WD)
  Sys.sleep(10)

  deepTransform(DEEPtype = DEEPtype, WD = WD, file_path = file_path, filter_by)

  if(tolower(DEEPtype) == "time"){deepTimeHB()}
  if(tolower(DEEPtype) == "risk"){deepRiskHB()}

  message("\nRemember that DEEP may only be used for academic purposes. \nIf you use DEEP please include the following citation at the time of publication:
          \n Toubia, O., Johnson, E., Evgeniou, T., & Delquié, P. (2013). Dynamic experiments for estimating preferences: \n An adaptive method of eliciting time and risk parameters. Management Science, 59(3), 613-640.")
}
