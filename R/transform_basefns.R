##################################
#### Base Transform Functions ####
##################################

#' @title
#' Reads file extension and helps Identify whether file is XML or CSV.
#'
#' @description
#' \code{strEndsWith} searches for a substring ("needle") within a larger string ("haystack").
#' It returns a boolean TRUE/FALSE if it finds the substring within the string.
#' Note: if the substring contains more characters than the string, it defaults to FALSE.
#'
#' @param haystack string input, used for the file_path
#' @param needle string input, used for the file extension.
#'
#' @return boolean TRUE or FALSE to indicate if the file extension (the "needle") was found in the filepath (the "haystack").
#' @export
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
#' to a dataframe, and exports four CSV files into the current working directory that will be read by the Hierarchical Bayes.
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

}

#' @title
#' Transforms JSON DEEP output into a readable dataframe
#'
#' @description
#' \code{deepTransform} Reads the filetype of the Qualtrics/Limesurvey output and parses
#' the JSON into a dataframe. Depending on the DEEP type, it sends it to the function \code{deepSplit}
#' to create the inputs for the Hierarchical Bayes.
#'
#' @param DEEPtype character string that specifies whether output is DEEP "risk" or "time".
#' @param WD allows the user to specify a working directory. Uses the current directory if no directory is specified.
#' @param file_path contains the file path to the Limesurvey/Qualtrics output.
#'
#' @return a dataframe containing the parsed JSON with the Participant responses
#' @export
#' @examples \dontrun{
#' deepTransform(DEEPtype = "risk", file_path = "/Documents/output.xml")
#' }
#'

# Takes Raw Participant Data from JSON format and adapts it. Feeds it to deepSplit.
deepTransform <- function(DEEPtype, WD = getwd(), file_path)
{
  setwd(WD) #sets working directory

  file_path <- gsub("\\", "/", file_path, fixed = T)
  # Check format of file and load it in

  if (strEndsWith(file_path, "xml")) {
    # Load XML
    survey_data <- xmlToDataFrame(file_path, stringsAsFactors = FALSE)
  } else if (strEndsWith(file_path, "csv")) {
    # Load CSV
    survey_data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)

    # Delete first row
    survey_data <- survey_data[-1,]
  } else {
    stop("Invalid file. File must be an XML or CSV.")
  }


  # Expand out JSON fields json[]["id"] and json[]["answer"] to Q1ID and Q1Answer
  # For each row, we need to set Q[i]ID and Q[i]Answer
  survey_data_converted <- survey_data

  for (rowIterator in 1:nrow(survey_data_converted))
  {
    # Get each row
    row <- survey_data_converted[rowIterator,]

    # Find the row with the DEEP JSON in it using grep
    # TODO: figure out what happens when multiple rows have JSON
    json <- row[which(apply(row, 2, function(x) any(grep("\\[\\{\\},\\{", x))))]
    json <- sapply(json, function(x) x[1])

    # Parse the JSON
    partJSON <- fromJSON(json)

    # Remove first row ('step 0')
    partJSON <- partJSON[-1,]

    # Renumber rows
    row.names(partJSON) <- 1:nrow(partJSON)

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
    }
  }

  if(tolower(DEEPtype) == "time"){deepSplit(survey_data_converted, tolower(DEEPtype))}
  if(tolower(DEEPtype) == "risk"){deepSplit(survey_data_converted, tolower(DEEPtype))}
}


