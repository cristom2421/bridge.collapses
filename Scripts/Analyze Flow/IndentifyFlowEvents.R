# Function to identify the maximum and other important events in a flow history
# Written by Madeleine Flint, 2018-07-05
# Results used to identify return periods of these events
#input df.flow should have columns "Date" (in format Date) and "Flow"

IdentifyFlowEvents <- function(df.flow, TYPES = c("FAIL", "MAX","MAXPREFAIL", "FAILPM","FAILYRMAX"),
                               DateFail, DateBuilt,STAID, nDays = 1, # days +/- actual fail date
                               colSuff = NA,   # column name suffix (e.g., "D_USGS")
                               FLAG.comment = TRUE ,   # if a column of comments is to be made
                               FLAG.pm = TRUE, # if the +/- value is to substitute for FAIL
                               FLAG.exceeded = TRUE,
                               ID){ # count of the number of times flow exceeded
  require(tidyr)
  require(plyr)

    if (any(grepl("FAIL",TYPES)) & (class(DateFail)!="Date")){
        # convert to date
    DateFail <- as.Date.numeric(DateFail, origin = "1970-01-01", format = "%Y-%m-%d")
    if(class(DateFail)!="Date") warning('A failure year or date of class Date must be provided to evaluate FAIL-related flows')
  } 
  if (("FAILPM" %in% TYPES) & (is.na(nDays))) error('A number of days before/after the collapse date is required
                                                   to evaluate FAILPM flows')
  
  # if (("MAXPREFAIL" %in% TYPES) & (class(DateBuilt)!="Date")) warning('If DateBuilt not provided MAXPREFAIL will
                                                                      # use all values from the beginning of the flow record')
  if (!("Date" %in% colnames(df.flow[[STAID]])) | !("Flow" %in% colnames(df.flow[[STAID]]))) error('df.flow must have columns Date and Flow')
  
  
  
  
  
# Setup
  FailYear     <- format.Date(DateFail,"%Y")
  df.flow[[STAID]]$Year <- format.Date(df.flow[[STAID]]$Date,"%Y", origin = "1970-01-01")
  HasFailDate  <- ifelse(format.Date(DateFail,"%m-%d")=="01-01", # will need to change if any fail dates are actually 1/1
                         FALSE,
                         TRUE)

  df.out       <- data.frame(row.names = "1")#row.names = TYPES)
  
  # Analyze each data type
  for(type in TYPES){
    indexBegin[type]  <- switch(type,
                                FAIL       = ifelse(HasFailDate,
                                                    which(df.flow[[STAID]]$Date == DateFail),
                                                    which(df.flow[[STAID]]$Year == FailYear)[1]),
                                FAILPM     = indexBegin["FAIL"] - nDays, # assumes that have already solved for FAIL
                                FAILYRMAX  = which(df.flow[[STAID]]$Year == FailYear)[1],
                                MAX        = 1,
                                MAXPREFAIL = 1)
                                # MAXPREFAIL = ifelse(!is.na(DateBuilt),
                                #                     which(df.flow[[STAID]]$Year == format(DateBuilt,"%Y"))[1],
                                #                     1)
                                
    indexEnd[type]  <- switch(type,
                              FAIL       = ifelse(HasFailDate,
                                                  which(df.flow[[STAID]]$Date == DateFail),
                                                  which(df.flow[[STAID]]$Year == as.character(as.integer(FailYear) + 1))[1]-1),
                              FAILPM     = indexEnd["FAIL"] + nDays, # assumes that have already solved for FAIL
                              FAILYRMAX  = which(df.flow[[STAID]]$Year == as.character(as.integer(FailYear) + 1))[1]-1,
                              MAX        = nrow(df.flow[[STAID]]),
                              MAXPREFAIL = ifelse(HasFailDate,
                                                indexEnd["FAIL"],
                                                  ifelse("FAIL" %in% TYPES,
                                                         which(df.out[,paste0("DATE_FAIL_",colSuff)] == df.flow[[STAID]]$Date), 
                                                         NA)) # will have to wait until estimated fail date
                              )
    #index <- which.max(df.flow[[STAID]]$Flow[indexBegin[type]:indexEnd[type]])+indexBegin[type]-1
    index       <- seq (indexBegin[type],indexEnd[type])[which.max(df.flow[[STAID]]$Flow[c(seq(indexBegin[type],indexEnd[type]))])]
    # Miscellaneous corrections
   
     if(is.null(nrow(index)) & type == "FAIL" & HasFailDate){ # fail date not present in record
      warning('Failure date not available in record')
      index     <- which.min(abs(df.flow[[STAID]]$Date - DateFail))
      print(index)
      print(FLAG.comment)
      if(FLAG.comment){
        DaysRecorded <- sum(df.flow[[STAID]]$Year == FailYear)
        print(DaysRecorded)
        if(DaysRecorded < 350) warning('Fewer than 350 days recorded in failure year')
        df.out[,paste("COMMENT_FAIL",colSuff,sep="_")] <-paste("Closest Q reading at",
                                                               DateFail-df.flow[[STAID]]$Date[index],
                                                               "days from failure, with",
                                                               DaysRecorded,
                                                               "flows recorded in failure year.",sep=" ")
        
      }
    }
    df.out[,"Date"] <- df.flow[[STAID]][index,"Date"]
    df.out[,"Flow"]  <- df.flow[[STAID]][index,"Flow"]

    if(type=="FAIL" & !HasFailDate){
        indexEnd["MAXPREFAIL"] <- index
        DaysRecorded <- sum(df.flow[[STAID]]$Year == FailYear)
        if(DaysRecorded < 350) warning('Fewer than 350 days recorded in failure year')
        if(FLAG.comment){
          df.out[,paste("COMMENT_FAIL",colSuff,sep="_")] <- paste("Fail date estimated from max flow in failure year,",
                                                                   "with",
                                                                   DaysRecorded,
                                                                   "flows recorded in failure year.",sep=" ")
          }
      }
  
    if(type=="FAILPM"){
      colnames(df.out)[colnames(df.out)=="Date"] <- paste0("DATE_",type,nDays,"_",colSuff)
      colnames(df.out)[colnames(df.out)=="Flow"] <- paste0("Q_",type,nDays,"_",colSuff)
      if(HasFailDate &
         FLAG.pm & 
         (paste("DATE_FAIL",colSuff,sep="_")!=paste0("DATE_FAILPM",nDays,"_",colSuff))){
        df.out[,paste("Q",type,colSuff,sep="_")] <- df.out[,paste0("Q_FAILPM",nDays,"_",colSuff)]
        if(FLAG.comment){
          df.out[,paste("COMMENT_FAIL",colSuff,sep="_")] <- paste0("Fail flow from +/-",nDays,"from given fail date.")
        }
      } 
    }
    # put in correct column names
    colnames(df.out)[colnames(df.out)=="Date"] <- paste("DATE",type,colSuff,sep="_")
    colnames(df.out)[colnames(df.out)=="Flow"] <- paste("Q",type,colSuff,sep="_")
    
  }

  if(FLAG.exceeded){
    ExceedsFlowIndex <- which(df.flow[[STAID]]$Flow >= df.out[1,paste("Q_FAIL",colSuff,sep="_")])
    ExceedCounts     <- ifelse(length(ExceedsFlowIndex)>1, 
                               sum(ExceedsFlowIndex[2:length(ExceedsFlowIndex)]-ExceedsFlowIndex[1:(length(ExceedsFlowIndex)-1)]>1),
                               1)
    df.out[,paste("COUNT_Q_FAIL_EXCEEDED",colSuff,sep="_")] <- ExceedCounts
    df.out[,paste("ID")] <- ID
  }
  print(df.out)
  }
 
  # IDs 1004 and 1390 have <365 days recorded
  # 1004, Hatchie River, none after April 4 (large event April 1, with known failure date, so OK)
  # 1390, Battle Run, no data starting July 11. Assumed failure date 6/27, major flash-flooding, so pretty confident got the biggest flow of the year. Put an asterisk on.
  
 
  # df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==1004,"COMMENT_Q_FAIL_USGS"] <- paste(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==1004,"COMMENT_Q_FAIL_USGS"],
  #                                                                            "<365days recorded - but avail on known fail date")
  # df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==1390,"COMMENT_Q_FAIL_USGS"] <- paste(df.Fail.NBI.Gage[df.Fail.NBI.Gage$ID==1390,"COMMENT_Q_FAIL_USGS"],
  #                                                                            "191 days recorded, no record 5 days after major flash flood")
  
  