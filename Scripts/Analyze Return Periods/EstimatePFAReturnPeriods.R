# Function to estimae the return periods of maximum and other important events in a flow history
# using log-log interpolation of input results of a Peak Flood Analysis (Flow Frequency Analysis)
# Based on a input dataframe of flow-exceedence values
# Written by Madeleine Flint, 2018-07-06
# Last Updated on 2019-05-28 by Cristopher Montalvo

# Capable of returning a vector of values of different Q if names are used in Q to indicate types

EstimatePFAReturnPeriod <- function(Q = c(FAIL = NA_real_, FAILPM = NA_real_,FAILYRMAX=NA_real_,
                                       MAX = NA_real_,MAXPREFAIL=NA_real_, Q100 = NA_real_, Q500 = NA_real_),STAID, df.PFA, # Input df.PFA should have columns "Flow" and "Freq"
                                 colSuff = NA){   # column name suffix (e.g., "B17CD_USGSD")
  
  require(Hmisc)
  df.out <- data.frame(row.names = 1)
  
  TYPES = names(Q)
  if(is.null(TYPES)){
    warning('Input a named vector for Q to return with column names')
    TYPES <- "GENERIC"
    names(Q) <- "GENERIC"
  }
  
  log_freq <- log(df.PFA[[STAID]]$Freq[!is.na(df.PFA[[STAID]]$Freq)])  
  log_flow <- log(df.PFA[[STAID]]$Flow[!is.na(df.PFA[[STAID]]$Freq)])
  
  # Take care of all standard exceedence values
  for(type in TYPES[!grepl("[[:digit:]]",TYPES)]){
    log_Val    <- log(Q[type])  
    if (all(log_Val < range(log_flow)) | all(log_Val > range(log_flow))) warning(paste0('Q is outside of range for type ',type, ', extrapolation used.'))
    ExceedProb <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
    Tval       <- max(1/ExceedProb,1)
    df.out[,paste("T",type,colSuff,sep="_")] <- Tval
  }
  
  # 'Design' values, Q100 and Q500
  for(type in TYPES[grepl("Q[[:digit:]]{3}",TYPES)]){
    Tx         <- gsub("[[:alpha:]]","",type)
    TxIndex  <- switch(Tx,
                         "500" = which(df.out$Freq==0.002),
                         "100" = which(df.out$Freq==0.01)
                         )
    Qx         <- df.PFA[[STAID]]$Flow[TxIndex]
    df.out[,paste(type,colSuff,sep="_")] <- Qx
  }
  
  # Confidence intervals
  if(any(grepl("10",TYPES)) & !any(grepl("10",colnames(df.PFA)))){
    error('Cannot compute confidence interval return periods without values in df.PFA')
  }
  else{
    log_freq10  <- log(df.PFA[[STAID]]$Freq10[!is.na(df.PFA[[STAID]]$Freq10)])
    log_freq90  <- log(df.PFA[[STAID]]$Freq90[!is.na(df.PFA[[STAID]]$Freq90)])
    log_flow10  <- log(df.PFA[[STAID]]$Flow[!is.na(df.PFA[[STAID]]$Freq10)])
    log_flow90  <- log(df.PFA[[STAID]]$Flow[!is.na(df.PFA[[STAID]]$Freq90)])
    for(type in TYPES[grepl("[[:digit:]]{1}5",TYPES)]){
      CI         <- gsub("_","",gsub("[[:alpha:]]","",type), fixed = TRUE)
      col        <- colnames(df.PFA[[STAID]])[grepl(CI,colnames(df.PFA[[STAID]]))]
      log_freq  <- log(df.PFA[[STAID]][,col][!is.na(df.PFA[df.PFA[[STAID]],col])])
      log_flow  <- log(df.PFA[[STAID]]$Flow[!is.na(df.PFA[[STAID]][,col])])
      log_Val    <- log(Q[type])

      ExceedProb <- exp(approxExtrap(log_flow,log_freq,log_Val)$y)
      Tval       <- max(1/ExceedProb,1)
      df.out[,paste("T",type,colSuff,sep="_")] <- Tval
    }
  }
  print(df.out)
}

  
 
