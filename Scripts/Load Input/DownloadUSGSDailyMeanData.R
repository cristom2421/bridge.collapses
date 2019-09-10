#LoadUSGS Daily Mean Flow Data
#November 13, 2018
#Written by Cristopher Montalvo
#Script to download daily mean flow data from USGS
#The end date is the "year before the current one"-12-31


DownloadUSGSDMData <- function(STAID,Code, BeginDate = "1900-01-01", stat, service) {
  
  require(dataRetrieval)
  require(dplyr)
  require(tidyr)
  
 
  EndDate   <- Sys.Date()-365
  EndDate <- unique(format.Date(EndDate, "%Y"))
  EndDate <- paste(EndDate, "12","31",sep="-")
  
  datadm <- readNWISdata(siteNumbers = STAID, parameterCd = Code, 
                           startDate = BeginDate, endDate = EndDate,
                           statCd = stat,
                           service = service)
  
datadm <- select(datadm, -c("agency_cd","tz_cd"))
names.cols <- c("STAID", "Date", "Flow","DataTypeFlag")
datadm <- setNames(datadm, nm = names.cols)
datadm <- as.data.frame(datadm)
datadm <- drop_na(datadm,"Date")
datadm <- drop_na(datadm,"Flow")
}