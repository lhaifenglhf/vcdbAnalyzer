# Author: Haifeng Liu
# This program is to curate the cyber incident data dowloaded from VCDB
# Input: the file folder of VCDB
# Output: the csv data file with incident loss


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

# Explore the given set of vcdb to summary the statistics of key columns
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
  cNames <- colnames(vcdb)
  targetCols <- c()
  for (cName in cNames) {

    if ((grepl(regExp, cName) == TRUE) ){
      print(cName)
      targetCols <- append(targetCols, cName)
    }
  }
}
####### End of Function definitions ####################################################

# Execution of the program
vcdb.dir <- "../../Github/VCDB/data/json/"
vcdb <- LoadVcdb(vcdb.dir)
dim(vcdb)
summary(vcdb)
plot(vcdb)

#ext.variety <- getenum(vcdb, "actor.external.variety")
#print(ext.variety)
#print(simplebar(ext.variety, "Variety of Hacking Actions"))



# Get the confirmed incident data with not null loss amount
confirmedLossDb <- vcdb[impact.overall_amount != "" & security_incident.Confirmed == TRUE]
#confirmedLossDb <- vcdb[impact.overall_amount != ""]

# Construct the list of columns of interest for manupilating data
#
targetCols = c("victim.victim_id", "victim.industry", "victim.industry.name", "victim.orgsize.Small", "victim.orgsize.Large",
               "victim.region",  "victim.revenue.amount", "victim.locations_affected",
               "impact.overall_amount","impact.loss.amount", "impact.iso_currency_code.USD"
              )
cNames <- colnames(vcdb)
for (cName in cNames) {

  if ((grepl("^victim.employee_count", cName) == TRUE) | (grepl("^victim.country", cName) == TRUE)
      | (grepl("^imapct.loss.rating", cName) == TRUE)){
    targetCols <- append(targetCols, cName)
  }
}

lossData <- confirmedLossDb[, targetCols, with = FALSE]
#summary(lossData)
# Write the filterd data set into a CSV
fileName = "outFiles/lossData.csv"
fwrite(lossData, fileName)


