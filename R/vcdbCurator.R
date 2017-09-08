# Author: Haifeng Liu
# This program is to curate the cyber incident data extracted from VCDB
# Input: the csv file of VCDB data (not full columns)
# Output: the csv data file which is ready to be used for building an analysis model


source("./R/vcdbUtils.R")

# Load the source data from a vcdb file
sourceDataFile <- "dataFiles/lossData.csv"
sourceData <- fread(sourceDataFile)


# Specify the source columns that will be handled
sourceCols = c("victim.industry.name", "victim.country.US", "victim.orgsize.Small",
               "victim.orgsize.Large",  "impact.overall_amount")

cNames <- colnames(sourceData)
for (cName in cNames) {

  if ((grepl("^victim.employee_count", cName) == TRUE)
      | (grepl("^impact.iso_currency_code", cName) == TRUE)){
    sourceCols <- append(sourceCols, cName)
  }
}

# Get the list of currencys that presented in the data with true values
currencyNames <- GetColNamesWithRegAndTrue("^impact.iso_currency_code", sourceData)
# "impact.iso_currency_code.EUR" , "impact.iso_currency_code.GBP"
# "impact.iso_currency_code.USD"

exchangeRate <- list(1.19, 1.3)
names(exchangeRate) <- c("impact.iso_currency_code.EUR", "impact.iso_currency_code.GBP")

# Update the loss amount values to USD according to currency exchange rate
convertCount = 0
allCurrencyFalse = 0
for (i in 1:nrow(sourceData)) {
  rowData <- sourceData[i]
  loss <- rowData$impact.overall_amount
  if (rowData$impact.iso_currency_code.USD == FALSE) {  # change loss amount to USD value
    if (rowData$impact.iso_currency_code.EUR == TRUE) {
      loss <- loss * exchangeRate$impact.iso_currency_code.EUR
      convertCount <- convertCount  + 1
      cat("Converted row:", i,  "\n")
    } else {
      if (rowData$impact.iso_currency_code.GBP == TRUE) {
        loss <- loss * exchangeRate$impact.iso_currency_code.GBP
        convertCount <- convertCount  + 1
        cat("Converted row: ", i,  "\n")
      } else {
        allCurrencyFalse <- allCurrencyFalse + 1
        cat("Unknown currency at row: ", i, " US: ", rowData$victim.country.US,"\n")
        #stop(rowData)
      }
    }
  }
  sourceData[i]$impact.overall_amount <- loss

}

# Remove currency columns
curatedCols <- sourceCols[grepl("^impact.iso_currency_code", sourceCols) == FALSE]
curatedData <- sourceData[, curatedCols, with=FALSE]


# Write the curated data back to a csv file
curatedDataFile <- "dataFiles/curatedData.csv"
fwrite(curatedData, curatedDataFile)

