# Author: Haifeng Liu
# This program is to define the common functions used for manipulating VCDB data
# Input:
# Output:


library(verisr)
library(ggplot2)  # For ploting bar charts
library(data.table)

######################## Function Definitions ######################################################
# Get the summary of a enum from a vcdb data
GetEnumStat <- function(data, enumName, plotName,
                        frequent= TRUE, freqThreshold = 0.02) {
  # Computes the value distribution of a variable or an Enum in a VCDB data table.
  # One enum could corresponds to multiple vcdb variables
  #
  # Args:
  #   data: A (sub) data set of VCDB.
  #   enumName: The name of VCDB enum.
  #   plotName: The name of barchart.
  #   frequent: If TRUE, only summarize the frequently presented enum values. Default is TRUE.
  #   freqThreshold: the threshold for frequency of enum value that needs to be summarized. Only
  #     valid when frequent is TRUE
  #
  # Returns:
  #   The data table with summary of enum value statistics.
  xEnum <- getenum(data, enumName)
  #print(xEnum)
  xTab <-data.table(xEnum)
  if (frequent == TRUE) {
    fTab <- xTab[freq> freqThreshold]
    print(simplebar(fTab, plotName))
    result <- fTab
  } else {
    print(simplebar(xTab, plotName))
    result <- xTab
  }
  return(result)
}

# Plot the result of a enum summary in the form of bat chat
GplotEnumSummary <- function(enumVariable) {
  gg <- ggplot(enumVariable, aes(x=enum, y=x))
  gg <- gg + geom_bar(stat="identity", fill="steelblue")
  gg <- gg + coord_flip() + theme_bw()
  print(gg)
}

# Explore the given set of vcdb to summarize the statistics of key columns
ExploreVcdbData <- function(vcdb) {
  ## Explore the incidents in vcdb

  # Get confirmed incidents
  confirmedDbIdx <- vcdb[["security_incident.Confirmed"]]
  print(confirmedDbIdx)
  confirmedDb <- vcdb[confirmedDbIdx == TRUE]
  dim(confirmedDb)

  ## Query the incidents with estimated loss
  dbWithOverallAmountLoss <- vcdb[impact.overall_amount != ""]
  dim(dbWithOverallAmountLoss)
  dbWithMinAmountLoss <- vcdb[impact.overall_min_amount != ""]
  dim(dbWithMinAmountLoss)
  dbWithMaxAmountLoss <- vcdb[impact.overall_max_amount != ""]
  dim(dbWithMaxAmountLoss)


  # get the employeenum of victims
  xEnum <- getenum(vcdb, "victim.employee_count")
  print(xEnum)
  xTab <-data.table(xEnum)
  #bigCountry <- countryTab[freq>0.02]
  print(simplebar(xTab, "Employee counts of victims"))

  GetEnumStat(vcdb, "victim.country", "Victim Countries")
  GetEnumStat(vcdb, "victim.revenue", "Victim Revenues")
  GetEnumStat(vcdb, "asset.variety", "Comprimised Assets", frequent=FALSE)
  GetEnumStat(vcdb, "asset.hosting", "Assets Hosting", frequent=FALSE)
  GetEnumStat(vcdb, "asset.cloud", "Assets Cloud", frequent=FALSE)
  GetEnumStat(vcdb, "asset.ownership", "Assets Owenership", frequent=FALSE)
  GetEnumStat(vcdb, "asset.accessibility", "Assets Accessibility", frequent=FALSE)
  GetEnumStat(vcdb, "attribute.confidentiality.data_disclosure", "Data Disclosure", frequent=FALSE)
  GetEnumStat(vcdb, "attribute.confidentiality.data.amount", "Data Disclosure Amount", frequent=FALSE)
  GetEnumStat(vcdb, "attribute.integrity.variety", "Integrity Variety", frequent=FALSE)
  GetEnumStat(vcdb, "attribute.availability.variety", "Availability Variety", frequent=FALSE)
}

# Load the vcdb data from a file folder
LoadVcdb <- function(vcdbDir) {
  # Args:
  #   vcdbDir: The name of folder which includes all vcdb jason files.
  #     #
  # Returns:
  #   The data table containing all extracted incidents.

  # may optionally load a custom json schema file.
  if (interactive()) { # show progress bar if the session is interactive
    vcdb <- json2veris(vcdbDir, progressbar=TRUE)
  } else {
    vcdb <- json2veris(vcdbDir)
  }
  return(vcdb)
}

# Check the column names with specific regular expression
CheckColNamesWithReg <- function(regExp, vcdb) {
  # Args:
  #   regExp: The regular expression that needs to be satisfied for the column names.
  #   vcdb: the vcdb data table that contains the columns.
  #     #
  # Returns:
  #   The vector of column names that match with the given regular expression
  cNames <- colnames(vcdb)
  targetCols <- c()
  for (cName in cNames) {

    if ((grepl(regExp, cName) == TRUE) ){
      #print(cName)
      targetCols <- append(targetCols, cName)
    }
  }
  return(targetCols)
}

# Extract the column names with specific regular expression that have at least
# one TRUE values in the given vcdb data
GetColNamesWithRegAndTrue <- function(regExp, vcdb) {
  # Args:
  #   regExp: The regular expression that needs to be satisfied for the column names.
  #   vcdb: the vcdb data table that contains the columns.
  #     #
  # Returns:
  #   The vector of column names that match with the given regular expression
  regNames <- CheckColNamesWithReg(regExp, vcdb)
  result <- c()
  for (cName in regNames) {
    if (any(vcdb[[cName]]) == TRUE) {
      print(cName)
      result <- append(result, cName)
    }
  }
  return(result)
}

# Compute mean absolute error
MAE <- function(actual, predicted) {

  mean(abs(actual-predicted))
}

# Function that returns Root Mean Squared Error
RMSE <- function(actual, predicted) {

  error <- actual-predicted
  sqrt(mean(error^2))
}

# Function to compute R2 of two vectors
R2 <- function(actual, predicted) {
  1 - (sum((actual-predicted )^2)/sum((actual-mean(actual))^2))
}
####### End of Function definitions ####################################################

