#Partial Duration Analysis
#June 26, 2019
#Written by Cristopher Montalvo
#Takes a dataframe of Partial Duration Data


PartialDurationAnalysis <- function(df.PartDur,df.flow,QfailD,QfailP,colSuff) {
  
  require(Hmisc)
  df.out <- data.frame(row.names = 1)
  
  #Flow and Excedance Values
  PartialFlow <- df.PartDur$Val
  PartialExc <- 1-df.PartDur$ExcVal
  HasInstPeak <- !is.na(QfailP)
  
  
  Count <- sum( df.PartDur$Val  < QfailD )
  nPartialDur <- sum(df.flow$Flow <= QfailD)
  PartialExcSite <- c(1 - 1e-05, PartialExc[2:8], 1/nPartialDur)
  
  log_flow <- log(PartialFlow) 
  if (any(-1*log_flow==Inf)) log_flow[-1*log_flow==Inf] <- NA_real_
  log_freq <- log(PartialExcSite)
  log_freq <- log_freq[!is.na(log_flow)]
  log_flow <- log_flow[!is.na(log_flow)]
  log_Fail <- log(QfailD)
  ExceedDurFail <- exp(approxExtrap(log_flow,log_freq,log_Fail)$y)
  T_Fail <- 1/ExceedDurFail/365.25
  
  if (HasInstPeak){
    log_Fail <- log(QfailP)  
    ExceedDurFailip <- exp(approxExtrap(log_flow,log_freq,log_Fail)$y)
    T_FailIP <- 1/ExceedDurFailip/365.25
    
  }
  df.out[,paste("EXCEED_PARTIAL_D",colSuff,sep="_")] <- ExceedDurFail
  df.out[,paste("T_Fail","D","PARTDUR",colSuff,sep="_")] <- T_Fail
  df.out[,paste("EXCEED_PARTIAL_I/P",colSuff,sep="_")] <- ExceedDurFailip
  df.out[,paste("T_Fail","I/P","PARTDUR",colSuff,sep="_")] <- T_FailIP
  
  print(df.out)
}
  
  