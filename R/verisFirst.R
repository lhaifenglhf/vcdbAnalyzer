library(verisr)
vcdb.dir <- "../../Github/VCDB/data/json/"
# may optionally load a custom json schema file.
if (interactive()) { # show progress bar if the session is interactive
  vcdb <- json2veris(vcdb.dir, progressbar=TRUE)
} else {
  vcdb <- json2veris(vcdb.dir)  
}
class(vcdb)
dim(vcdb)
summary(vcdb)

library(ggplot2)
plot(vcdb)

ext.variety <- getenum(vcdb, "actor.external.variety")
print(ext.variety)

gg <- ggplot(ext.variety, aes(x=enum, y=x))
gg <- gg + geom_bar(stat="identity", fill="steelblue")
gg <- gg + coord_flip() + theme_bw()
print(gg)

print(simplebar(ext.variety, "Variety of Hacking Actions"))

##Try filter

# see the docs on data.table for getting columns like this
ddfilter <- vcdb[["attribute.confidentiality.data_disclosure.Yes"]]
webfilter <- vcdb[["action.hacking.vector.Web application"]]
# now we can combine with | or & ("or" and "and" respectively)
# to filter incidents with confirmed data loss and web vector:
ddweb <- ddfilter & webfilter

cat("Confirmed data loss events:", sum(ddfilter), "\n")
cat("Hacking vector of web apps:", sum(webfilter), "\n")
cat("Both data loss and web app:", sum(ddweb), "\n")

##Special names added to verisr object
###
### actor will return top level actor categories
### action will return top level action categories
### asset.variety will return top level asset categories
### attribute will return top level asset categories
### victim.industry2 will return the first 2 digits of the NAICS code
### victim.industry3 same, first 3 digits
### victim.orgsize returns “Large” and “Small” enumerations
### pattern returns the patterns (see DBIR 2014) each line is classified as

## Querying Multiple Enumerations
a2 <- getenumby(vcdb, c("action", "asset.variety"))
head(a2)

## Confirmed incidents
confirmedIncidentsEnum <- getenum(vcdb, "security_incident")
print(confirmedIncidentsEnum)

## Confirmed incidents data
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

confirmedLossDb <- vcdb[impact.overall_amount != "" & security_incident.Confirmed == TRUE]
confirmedLossDb <- vcdb[impact.overall_amount != ""]
## construct the list of columns of interest
#
targetCols = c("victim.victim_id", "victim.industry", "impact.overall_amount", "impact.iso_currency_code.USD", "victim.revenue.amount", "victim.location_affected")
cNames <- colnames(vcdb)
for (cName in cNames) {
  
  if ((grepl("^victim.employee_count", cName) == TRUE) | (grepl("^victim.country", cName) == TRUE)){
    targetCols <- append(targetCols, cName)
  }
}

lossData <- confirmedLossDb[, targetCols, with = FALSE]

## Write the filterd data set into a CSV
library(data.table)
fileName = "outFiles/lossData.csv"
fwrite(lossData, fileName)
# Bank of the West

## Explore the incidents in vcdb

# get the subset of industries with larger fraction over 10% ammong all incidents
industryEnum <- getenum(vcdb, "victim.industry")
print(industryEnum)
industryTab <-data.table(industryEnum)
bigIndustry <- industryTab[freq>0.02]
print(simplebar(bigIndustry, "VCDB primary industry of victims"))
#print(simplebar(ind, "Primary industry of victims"))

# get the country of victims
countryEnum <- getenum(vcdb, "victim.country")
print(countryEnum)
countryTab <-data.table(countryEnum)
bigCountry <- countryTab[freq>0.02]
print(simplebar(bigCountry, "VCDB countries of victims"))

# get the employeenum of victims
xEnum <- getenum(vcdb, "victim.employee_count")
print(xEnum)
xTab <-data.table(xEnum)
#bigCountry <- countryTab[freq>0.02]
print(simplebar(xTab, "Employee counts of victims"))

getEnumStat(vcdb, "victim.country", "Victim Countries")
getEnumStat(vcdb, "victim.revenue", "Victim Revenues")
getEnumStat(vcdb, "asset.variety", "Comprimised Assets", frequent=FALSE)
getEnumStat(vcdb, "asset.hosting", "Assets Hosting", frequent=FALSE)
getEnumStat(vcdb, "asset.cloud", "Assets Cloud", frequent=FALSE)
getEnumStat(vcdb, "asset.ownership", "Assets Owenership", frequent=FALSE)
getEnumStat(vcdb, "asset.accessibility", "Assets Accessibility", frequent=FALSE)
getEnumStat(vcdb, "attribute.confidentiality.data_disclosure", "Data Disclosure", frequent=FALSE)
getEnumStat(vcdb, "attribute.confidentiality.data.amount", "Data Disclosure Amount", frequent=FALSE)
getEnumStat(vcdb, "attribute.integrity.variety", "Integrity Variety", frequent=FALSE)
getEnumStat(vcdb, "attribute.availability.variety", "Availability Variety", frequent=FALSE)

######################## Function Definitions ######################################################
## Define the function of getting bar statitics of a specific enum  in a vcdb data set
getEnumStat <- function(data, enumName, plotName, frequent= TRUE, freqThreshold = 0.02) {
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
  print(result)
}