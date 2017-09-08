# Author: Haifeng Liu
# This program is to extract the cyber incident data dowloaded from VCDB
# Input: the file folder of VCDB
# Output: the csv data file with incident loss

source("./R/vcdbUtils.R")


# Execution of the program

vcdb.dir <- "../../Github/VCDB/data/json/"
vcdb <- LoadVcdb(vcdb.dir)
dim(vcdb)
summary(vcdb)


#ext.variety <- getenum(vcdb, "actor.external.variety")
#print(ext.variety)
#print(simplebar(ext.variety, "Variety of Hacking Actions"))



# Get the confirmed incident data with not null loss amount
confirmedLossDb <- vcdb[impact.overall_amount != "" & security_incident.Confirmed == TRUE]
#confirmedLossDb <- vcdb[impact.overall_amount != ""]

# Construct the list of columns of interest for manupilating data
#
targetCols = c("victim.victim_id", "victim.industry", "victim.industry.name", "victim.country.US",
               "victim.orgsize.Small", "victim.orgsize.Large", "victim.region",
               "victim.locations_affected", "impact.overall_amount","impact.loss.amount"
              )
cNames <- colnames(vcdb)
for (cName in cNames) {

  if ((grepl("^victim.employee_count", cName) == TRUE) | (grepl("^imapct.loss.rating", cName) == TRUE)
      | (grepl("^impact.iso_currency_code", cName) == TRUE)){
    targetCols <- append(targetCols, cName)
  }
}

lossData <- confirmedLossDb[, targetCols, with = FALSE]
#summary(lossData)
# Write the filterd data set into a CSV
fileName = "dataFiles/lossData.csv"
fwrite(lossData, fileName)



