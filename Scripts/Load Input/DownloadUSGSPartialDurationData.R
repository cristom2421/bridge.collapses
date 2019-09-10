#Download USGS Partial Duration Data
#June 24, 2019
#Written by Cristopher Montalvo
#Script to download partial duration based on daily mean flow data from USGS
#The end date is the "year before the current one"-12-31


DownloadPartDurData <- function(STAID, BeginDate = "1900-01-01") {
  
  require(dataRetrieval)
  require(dplyr)
  
  EndDate   <- Sys.Date()-365
  EndDate <- unique(format.Date(EndDate, "%Y"))
  EndDate <- paste(EndDate, "12","31",sep="-")
  
  datapd <- readNWISdata(siteNumbers = STAID, parameterCd = "00060", 
                         startDate = BeginDate, endDate = EndDate,
                         statCd = "00003",
                         service = "dv")
  
  datapd <- select(datapd, -c("agency_cd","tz_cd"))
  names.cols <- c("STAID", "Date", "Flow","DataTypeFlag")
  datapd <- setNames(datapd, nm = names.cols)
  datapd <- as.data.frame(datapd)
  datapd <- drop_na(datapd,"Date")
  datapd <- drop_na(datapd,"Flow")
  minval <- min(datapd$Flow)
  maxval <- max(datapd$Flow)
  datapd$Flow <- sort(datapd$Flow, decreasing = FALSE)
  vals <- quantile(datapd$Flow, c(0.05, 0.10,0.25,0.50,0.75,0.90,0.95))
  Pt <- c("min:","po5:","p10:","p25:","p50:","p75:","p90:","p95:","max:")
  Val <- c(minval,vals,maxval)
  ExcVal <- c(0,0.05,0.10,0.25,0.50,0.75,0.90,0.95,1.00)
  
  PartDur<- data.frame(Pt,Val,ExcVal)
}
