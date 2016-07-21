##################################
#### Base Transform Functions ####
##################################

#' @title
#' Reads file extension and helps Identify whether file is XML or CSV.
#'
#' @description
#' \code{strEndsWith} searches for a substring ("needle") at the end of a larger string ("haystack").
#' It returns a boolean TRUE/FALSE if it finds the substring within the string.
#' Note: if the substring contains more characters than the string, it defaults to FALSE.
#'
#' @param haystack string input, used for the file_path
#' @param needle string input, used for the file extension.
#'
#' @return boolean TRUE or FALSE to indicate if the file extension (the "needle") matches the filepath (the "haystack").
#'
#' @examples \dontrun{
#' strEndsWith(haystack = "myQualtricsOutput.csv", needle = "csv")
#' }
#'

strEndsWith <- function(haystack, needle)
{
  hl <- nchar(haystack)
  nl <- nchar(needle)
  if(nl>hl)
  {
    return(F)
  } else
  {
    return(substr(haystack, hl-nl+1, hl) == needle)
  }
}





#' @title
#' Splits Participant Data into Hierarchical Bayes inputs
#'
#' @description
#' \code{deepSplit} subsets the "transformed" Limesurvey or Qualtrics DEEP output, after it has been loaded
#' to a dataframe, and exports four CSV files into the current working directory that will be read by the Hierarchical Bayes. In addition, it creates a CSV containing the time (in seconds) that each participant spent on each question.
#' Note: Correct specification of the DEEP type is required for proper naming of files.
#'
#' @param raw_data dataframe containing DEEP risk/time output
#' @param DEEPtype character string that specifies whether output is DEEP "risk" or "time".
#'
#' @return Writes four CSV files named according to what DEEP type was specified.
#' @export
#' @examples \dontrun{
#' deepSplit(rawdata = = df, DEEPtype = "risk")
#' }
#'

deepSplit <- function(raw_data, DEEPtype)
{
  # Rename first column (Qualtrics IDs) and clean useless columns or NAs
  colnames(raw_data)[1] <- "Q_ResponseID" # Renames first column

  clean_data <- raw_data %>%   #removes extraneous Qualtrics output
    dplyr::select(matches("Q"))

  clean_data <- na.omit(clean_data)

  clean_data$serial<-seq.int(nrow(clean_data))

  # Create  a serials file for HB
  write.table(clean_data$serial, paste("serials_",DEEPtype,"_1.csv",sep=""),
              row.names=FALSE, col.names=FALSE, sep=',')

  # Write a combination of Response Id and serial for later merging
  write.table(clean_data %>% dplyr::select(Q_ResponseID, serial), "serials_respid.csv",
              row.names=FALSE, col.names=TRUE, sep=',')

  # writes table with the Adaptive Question IDs
  adap_q <- clean_data %>%
    dplyr::select(matches("Q\\d+ID$"))

  # Skips last question ID and exports as CSV
  adap_q[, 1:(ncol(adap_q)-1)] %>%
    write.table(paste("adaptive_questions_",DEEPtype,"_1.csv",sep=""), row.names=FALSE, col.names=FALSE, sep=',')

  # write the choices
  choices <- clean_data %>% dplyr::select(matches("Answer"))
  choices[, 1:(ncol(choices)-1)] %>%
    write.table(paste("choices_",DEEPtype,"_1.csv",sep=""), row.names=FALSE, col.names=FALSE, sep=',')

  # write the Timing
  timing <- clean_data %>% dplyr::select(serial, matches("Timing"))
  timing[, 1:(ncol(timing)-1)] %>%
    write.table(paste("Qtiming_DEEP",DEEPtype,".csv",sep=""), row.names=FALSE, col.names=TRUE, sep=',')

}



#' @title
#' Transforms JSON DEEP output into a readable dataframe
#'
#' @description
#' \code{deepTransform} Reads the filetype of the Qualtrics/Limesurvey output and parses
#' the JSON into a dataframe. Depending on the DEEP type, it sends it to the function \code{deepSplit}
#' to create the inputs for the Hierarchical Bayes. If a specific survey question has been specified, it only transforms that part of the dataset.
#'
#' @param DEEPtype character string that specifies whether output is DEEP "risk" or "time".
#' @param WD allows the user to specify a working directory. Uses the current directory if no directory is specified.
#' @param file_path contains the file path to the Limesurvey/Qualtrics output.
#' @param filter_by specifies the name/identifier of the question you want to isolate for analysis. This parameter is used to analyze a dataset that contains multiple DEEP outputs but is optional when analyzing a single DEEP output.
#'
#' @return a dataframe containing the parsed JSON with the Participant responses
#' @return a CSV is exported with the unshrunken parameter estimates
#' @export
#' @examples \dontrun{
#' deepTransform(DEEPtype = "risk", file_path = "/Documents/output.xml")
#' deepTransform(DEEPtype = "time", file_path = "/Documents/output.xml", filter_by = "myDEEPtimeQuestion")
#' }
#'

# Takes Raw Participant Data from JSON format and adapts it. Feeds it to deepSplit.
deepTransform <- function(DEEPtype, WD = getwd(), file_path, filter_by = NULL, collaborate)
{
  setwd(WD) #sets working directory

  file_path <- gsub("\\", "/", file_path, fixed = T)
  # Check format of file and load it in

  if (strEndsWith(file_path, "xml")) {
    # Load XML
    survey_data <- XML::xmlToDataFrame(file_path, stringsAsFactors = FALSE)
  } else if (strEndsWith(file_path, "csv")) {
    # Load CSV
    survey_data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)

    # Change Headers, Eliminate duplicate Columns & Delete first row
    colnames(survey_data) = survey_data[1, ]
    survey_data <- survey_data[-1,]
    survey_data <- survey_data[ , !duplicated(colnames(survey_data))]
  } else {
    stop("Invalid file. File must be an XML or CSV.")
  }


  # Expand out JSON fields json[]["id"] and json[]["answer"] to Q1ID and Q1Answer
  # For each row, we need to set Q[i]ID and Q[i]Answer
  survey_data_converted <- survey_data

  if(!is.null(filter_by))
  {
    survey_data_converted <- dplyr::select(survey_data_converted, ResponseID, matches(filter_by))

    #Handle NA
    survey_data_converted[survey_data_converted==""]  <- NA
    survey_data_converted <- na.omit(survey_data_converted)
  }


  for (rowIterator in 1:nrow(survey_data_converted))
  {
    # Get each row
    row <- survey_data_converted[rowIterator,]

    # Find the row with the DEEP JSON in it using grep
    # TODO: figure out what happens when multiple rows have JSON
    json <- row[which(apply(row, 2, function(x) any(grep("\\[\\{\\},\\{", x))))]
    if(length(json)!=0)
    {
      json <- sapply(json, function(x) x[1])
      
      # Parse the JSON
      partJSON <- jsonlite::fromJSON(json)
      
      # Remove first row ('step 0')
      partJSON <- partJSON[-1,]
      
      # Renumber rows
      row.names(partJSON) <- 1:nrow(partJSON)
      
      
      ###### Retrieve Unshrunken estimates #######
      
      if(tolower(DEEPtype) == "time")
      {
        betaColName <- "Unshrunken_Beta"
        # Check if the Beta column exists; if not, create it.
        if (!(betaColName %in% colnames(survey_data_converted)))
        {
          survey_data_converted[betaColName] <- 0
          survey_data_converted[rowIterator, betaColName] <- partJSON$beta[nrow(partJSON)]
        }
        else
        {
          survey_data_converted[rowIterator, betaColName] <- partJSON$beta[nrow(partJSON)]
        }
        
        drColName <- "Unshrunken_DiscountRate"
        # Check if the Discount rate column exists; if not, create it.
        if (!(drColName %in% colnames(survey_data_converted)))
        {
          survey_data_converted[drColName] <- 0
          survey_data_converted[rowIterator, drColName] <- partJSON$discountRate[nrow(partJSON)]
        }
        else
        {
          survey_data_converted[rowIterator, drColName] <- partJSON$discountRate[nrow(partJSON)]
        }
      }
      
      if(tolower(DEEPtype) == "risk")
      {
        alphaColName <- "Unshrunken_Alpha"
        # Check if the Alpha column exists; if not, create it.
        if (!(alphaColName %in% colnames(survey_data_converted)))
        {
          survey_data_converted[alphaColName] <- 0
          survey_data_converted[rowIterator, alphaColName] <- partJSON$alpha[nrow(partJSON)]
        }
        else
        {
          survey_data_converted[rowIterator, alphaColName] <- partJSON$alpha[nrow(partJSON)]
        }
        
        sigmaColName <- "Unshrunken_Sigma"
        # Check if the Sigma column exists; if not, create it.
        if (!(sigmaColName %in% colnames(survey_data_converted)))
        {
          survey_data_converted[sigmaColName] <- 0
          survey_data_converted[rowIterator, sigmaColName] <- partJSON$sigma[nrow(partJSON)]
        }
        else
        {
          survey_data_converted[rowIterator, sigmaColName] <- partJSON$sigma[nrow(partJSON)]
        }
        
        lambdaColName <- "Unshrunken_Lambda"
        # Check if the Beta column exists; if not, create it.
        if (!(lambdaColName %in% colnames(survey_data_converted)))
        {
          survey_data_converted[lambdaColName] <- 0
          survey_data_converted[rowIterator, lambdaColName] <- partJSON$lambda[nrow(partJSON)]
        }
        else
        {
          survey_data_converted[rowIterator, lambdaColName] <- partJSON$lambda[nrow(partJSON)]
        }
      }
      
      # Iterate through each row (each step) in partJSON
      for (stepIterator in 1:nrow(partJSON))
      {
        stepRow <- partJSON[stepIterator,]
        
        # Generate the name for the ID column, which is Q[step]ID
        idColumnName <- paste("Q",stepIterator,"ID", sep="")
        
        # Check if the ID column for this step exists; if not, create it
        if (!(idColumnName %in% colnames(survey_data_converted)))
        {
          survey_data_converted[idColumnName] <- 0
        }
        
        # Store the ID in the [rowID, "Q[step]ID"]
        # So for the ID in row 2, question 5, it should store it in [2, "Q5ID"]
        if (!(is.na(stepRow$id)))
        {
          survey_data_converted[rowIterator, idColumnName] <- stepRow$id
        }
        
        # Generate the name for the Answer column, which is Q[step]Answer
        answerColumnName <- paste("Q",stepIterator,"Answer", sep="")
        
        # Check if the Answer column for this step exists; if not, create it
        if (!(answerColumnName %in% colnames(survey_data_converted)))
        {
          survey_data_converted[answerColumnName] <- 0
        }
        
        # Store the answer in the corresponding row/column
        if (!(is.na(stepRow$answer))) {
          survey_data_converted[rowIterator, answerColumnName] <- stepRow$answer
        }
        
        # Generate the name for the Timing column, which is Q[step]Timing
        timeColumnName <- paste("Q",stepIterator,"Timing", sep="")
        
        # Check if the Timing column for this step exists; if not, create it
        if (!(timeColumnName %in% colnames(survey_data_converted)))
        {
          survey_data_converted[timeColumnName] <- 0
        }
        
        # Store the time in the corresponding row/column
        if (!(is.na(stepRow$responseTime))) {
          survey_data_converted[rowIterator, timeColumnName] <- stepRow$responseTime
        }
      }
      
    }
    else{survey_data_converted[rowIterator,] <- NA}
    
  }

  unshrunkenEstimates <- dplyr::select(survey_data_converted, matches("Response"), matches("Unshrunken"))
  write.csv(unshrunkenEstimates, "Unshrunken_Parameter_Estimates.csv", row.names = F)

  if(collaborate){deepContribute(file_path)}
  if(tolower(DEEPtype) == "time"){deepSplit(survey_data_converted, tolower(DEEPtype))}
  if(tolower(DEEPtype) == "risk"){deepSplit(survey_data_converted, tolower(DEEPtype))}
}


# Contributes to DEEP Parameter Estimation Repository
deepContribute <- function(file_path)
{
  timeStamp <- Sys.time()
  timeStamp <- gsub(":", ".", timeStamp)
  timeStamp <- gsub(" ", "_", timeStamp)
  dropFolder <- "incoming"
  #create file name for server upload
  fileName <- strsplit(file_path,"/")
  fileName <- fileName[[1]][length(fileName[[1]])]
  fileName <- paste(timeStamp, fileName, sep="__")

  location <- paste("sftp://deepstore.decisionsciences.columbia.edu:22",dropFolder, fileName, sep="/")

  ftpUpload(what = file_path, to = location, verbose = FALSE, userpwd = "deepstore:7dcb9y7X9io4ruPseR834K66u")
}


