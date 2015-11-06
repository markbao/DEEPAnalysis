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
#   runs the heirarchical Bayes, and returning the data all in one.
#
# Version 2.0 - Luis Sanchez & Mark Bao (10/19/15)
# Hierarchical Bayes code translated into R by Seoungwoo Lee
#-------------------

devtools::use_package("dplyr")
devtools::use_package("XML")
devtools::use_package("jsonlite")
devtools::use_package("MSBVAR")
devtools::use_package("MCMCpack")

library(dplyr)
library(XML)
library(jsonlite)

library(MSBVAR)
library(MCMCpack)


#################################
## DEEP Start - Transform & HB ##
#################################

#' @title
#' Runs the DEEP Transformation, DEEP Split and Hierarchical Bayes algorithm.
#'
#' @description
#' \code{deepRun} Takes in a filepath for the Qualtrics/Limesurvey output
#' along with a working directory and the DEEP type specification. If no working
#' directory is provided, it defaults to the current directory. If no file_path is provided
#' it will request that one ge given using the GUI.
#'
#' @param DEEPtype character string that specifies whether output is DEEP "risk" or "time".
#' @param WD allows the user to specify a working directory. Uses the current directory if no directory is specified.
#' @param file_path contains the file path to the Limesurvey/Qualtrics output.
#'
#' @export
#' @examples \dontrun{
#' deepRun(DEEPtype = "risk", file_path = "/MyDocuments/myQualtricsOutput.csv")
#' deepRun(DEEPtype = "risk", WD = "/MyCurrentProject/DEEP")
#' deepRun(DEEPtype = "risk", WD = "/MyCurrentProject/DEEP", file_path = "/MyDocuments/myQualtricsOutput.csv")
#' }
#'

deepRun <- function(DEEPtype, WD = getwd(), file_path = NULL)
{
  if(is.null(file_path)){file_path <- file.choose()}

  deepTransform(DEEPtype = DEEPtype, WD = WD, file_path = file_path)

  if(tolower(DEEPtype) == "time"){deepTimeHB()}
  if(tolower(DEEPtype) == "risk"){deepRiskHB()}

}
