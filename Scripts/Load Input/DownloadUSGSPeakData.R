DownloadUSGSPeakData <- function(STAID,DateFail) {
  require(dataRetrieval)
  require(dplyr)
  require(tidyr)
  
  peakData <- readNWISpeak(STAID)
  oldnames <- c("site_no","peak_dt","peak_va","peak_cd")
  newnames <- c("STAID","Date","Flow","Agency_cd")
  peakData <- rename_at(peakData,vars(oldnames), ~ newnames)
  peakData <-  select(peakData,STAID,Date,Flow,Agency_cd)
  peakData <- drop_na(peakData,"Date")
  peakData <- drop_na(peakData,"Flow")
  peakData$Year <- as.integer(format.Date(peakData$Date, "%Y"))
  peakData <- complete(peakData, Year = seq(min(Year), max(Year)), fill = list(Flow = 0))
  peakData$STAID <- STAID
  peakData$Date = if_else(is.na(peakData$Date), as.Date(paste0(peakData$Year, "-01-01")), peakData$Date)
  
  peakData <- as.data.frame(peakData)
  YearFail <- as.integer(unique(format.Date(DateFail, "%Y")))
  IndexFail <- which(peakData$Year==YearFail)
  
  startDate <- as.Date(paste(YearFail, "01","01",sep="-"))
  endDate <- as.Date(paste(YearFail, "12","31",sep="-"))
  
  dataif <- readNWISdata(siteNumbers = STAID, parameterCd = "00060", 
                         startDate = startDate, endDate = endDate,
                         service = "iv")
  dataif$dateTime <- ifelse(is.na(dataif$dateTime),
                            NA,
                            as.Date(dataif$dateTime))
  AnnualMaxIF <- max(dataif$X_00060_00000)
  indexMaxIF <- which.max(dataif$X_00060_00000)
  
  if(peakData$Flow[IndexFail] == 0) warning('Flow obtained from Instantenous Data')
  QFail <- ifelse(peakData$Flow[IndexFail]>0,
                                     peakData$Flow[IndexFail],
                                     AnnualMaxIF)
  
  QFail <- ifelse(is.na(QFail),
                  0,
                  peakData$Flow[IndexFail])
  
  peakData$Flow[IndexFail] <- QFail
  
  peakData$Date <- as.Date(peakData$Date)
  peakData$Year <- NULL
  
  print(peakData)
 
}