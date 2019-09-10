# Function to load flood frequency analysis results from output file
# Last Updated 2-24-2019
# Supports HEC-SSP v2? Bulletin 17B analysis, PeakFQ v4.2 B17B, and PeakFQ v7.2 B17C
LoadFFA <- function(filename,folder.out, TYPE,
                    colStandard = TRUE){ # standardize column output names

  ls.colNames <- list("PKFQ72_17C" = c("ANN_EXC_PROB",  "EMA_REG",	"EMA_STAT",	"LOG_VAR_REG",	"5PCT_CONF_REG",	"95PCT_CONF_REG"),
                      "PKFQSA_17C" = c("ANN.EXC.PROB",  "EMA Est",	"V[log(EMA)]",	"At-Site Est",	"10PCT CI Low",	"90PCT CI High"),
                      "HEC_17B"    = c("Ordinate",  "FREQ",	"FLOW Computed Curve",	"FLOW Expected Prob Curve",	"FLOW 5Pct Conf",	"FLOW 95Pct Conf",	
                                       "FLOW Observed (Median)",	"FLOW Historic Data",	"FLOW High Outlier",	"FLOW Low Outlier"),
                      "PKFQ42_17B" = c("ANN_EXC_PROB",  "BULL17B_EST",	"SYSTEM_RECORD",	"EXPECTED_PROB",	"5PCT_CONF",	"95PCT_CONF"),
                      "PARTDUR"    = c("Val","Exc"))
  colNames <- ls.colNames[[TYPE]]
  
  colsSwitch <- list(FlowCols   = c("EMA Est.","EMA_REG", "FLOW Computed Curve", "BULL17B_EST","Val"),
                     FreqCols   = c("ANN.EXC.PROB","ANN_EXC_PROB","FREQ","Exc"),
                     Flow05Cols = c("5PCT_CONF_REG", "5PCT_CONF"),
                     Freq10Cols = c("10PCT CI Low"),
                     Freq90Cols = c("90PCT CI High"),
                     Flow95Cols = c("95PCT_CONF_REG", "95PCT_CONF"),
                     CI10Cols = c("CI-Low"),
                     CI90Cols = c("CI-High"))
  
  colsOut <- c(FlowCols   = "Flow",
               FreqCols   = "Freq",
               Flow05Cols = c("Flow05"),
               Flow95Cols = c("Flow95"),
               Freq10Cols = c("Freq10"),
               Freq90Cols = c("Freq90"),
               CI10Cols = c("CI-Low"),
               CI90Cols = c("CI-High"))
               
  if(grepl("PeakFQ72_17C",TYPE)){ # 7.2 version, non-batch
    PFA <- read.fwf(file.path(folder.out,filename),
                    77,
                    header=FALSE,
                    sep = "\n",
                    stringsAsFactors = FALSE)
    skip <- grep("TABLE 4",PFA$V1)[1] + 5
    PFA <- read.fwf(file.path(folder.out,filename),
                    77,
                    header=FALSE,
                    sep = "\n",
                    n = 15,
                    skip=skip,
                    stringsAsFactors = FALSE)
    PFA <- as.data.frame(sapply(1:nrow(PFA_17C_VICRg[[i]]),
                                function(j) gsub("--","NA",PFA[j,])), stringsAsFactors = FALSE)
    PFA <- as.data.frame(t(sapply(1:nrow(PFA), function(j) as.numeric(unlist(strsplit(PFA[j,], "[[:space:]]+")))[2:7] )))
  }
  
  
  if(grepl("PKFQSA_17C",TYPE)){ 
    data <- readLines(file.path(folder.out,filename),warn = F)
    lines.asterisk <- grep("[[:digit:]]{2}[*]{13}",data)
    #data[lines.asterisk] <- sub(paste0(rep("*",13),collapse=""),paste0(paste0(rep(" ",11),collapse = ""),"NA"),data[lines.asterisk],fixed=T)
    data[lines.asterisk] <- sub("[[:punct:]]{13}","           NA",data[lines.asterisk])
    skip <- 2 + grep("Ann. Exc. Prob.  EMA Est.", data)[1]
    n <- grep("EMA-Q 0.0000001", data)[1]
    temp <- strsplit(gsub("[[:space:]]{3,}"," ",data[skip:n]),"[[:space:]]{1}")
    temp <- lapply(temp, function(x) x[!x == ""])
    PFA <- as.data.frame(t(sapply(1:length(temp), function(i) unlist(temp[[i]]), simplify = T)), stringsAsFactors = F)
    PFA <- as.data.frame(apply(PFA[,2:7],MARGIN = 2, as.numeric),stringsAsFactors=F)
  }
  
  if(grepl("PeakFQ42_17B",TYPE)){ # older batch version
    skip <- 83
    PFA <- read.fwf(file.path(folder.out,filename),
                    77,
                    header=FALSE,
                    sep = "\n",
                    n = 15,
                    skip=skip,
                    stringsAsFactors = FALSE)
    PFA <- as.data.frame(t(sapply(1:nrow(PFA), function(j) as.numeric(unlist(strsplit(PFA[j,], "[[:space:]]+")))[2:7] )))
  }
  
  if(grepl("HEC_17B_P",TYPE)){ # HEC-SSP ANALYSIS USING PEAK DATA (TAB OUTPUT)
    skip <- 5
    PFA <- read.table(file.path(folder.out,filename),
                      header=FALSE,na.strings="NA",sep = "\t",
                      skip=skip, col.names = colNames,
                      stringsAsFactors = FALSE)
  }
  
  if(grepl("HEC_17B_D",TYPE)){ # HEC-SSP ANALYSIS USING DAILY MEAN DATA
    skip <- 3
    colClasses <- c()
    PFA    <- read.table(file.path(folder.out,filename),
                         header=FALSE,na.strings="NA",sep="\t",
                         skip=skip, col.names = colNames,
                         stringsAsFactors = FALSE)
    skip   <- skip + grep("TYPE",PFA[,1])[1] + 1
    PFA    <- read.table(file.path(folder.out,filename),
                         header=FALSE,na.strings="NA",sep="\t",
                         skip=skip, col.names = colNames,
                         colClasses = c("integer", rep("numeric",9)),
                         stringsAsFactors = FALSE)
    # probGages <- gage_sites[sapply(gage_sites, function(i) HEC_PFA_AnMaxDayMean[[i]][1,1]=="Units")]
    # for (i in probGages){
    #   # HEC_PFA_AnMaxDayMean[[i]] <- HEC_PFA_AnMaxDayMean[[i]][3:nrow(HEC_PFA_AnMaxDayMean[[i]]),]
    #   colClasses <- sapply(colnames(HEC_PFA_AnMaxDayMean[[i]]), function(j) class(HEC_PFA_AnMaxDayMean[[i]][,j]))
    #   cols       <- colnames(HEC_PFA_AnMaxDayMean[[i]])[!(colClasses %in% (c("numeric","integer")))]
    #   HEC_PFA_AnMaxDayMean[[i]][,cols] <- sapply(cols, function(j) as.numeric(HEC_PFA_AnMaxDayMean[[i]][,j]))
    # }
  }
  if(grepl("PARTDUR",TYPE)){
    ExcVals <- c(0,0.05,0.10,0.25,0.5,0.75,0.90,0.95,1)
    PFA <- read.table(file.path(folder.out,filename),
                          sep = " ",
                          skip = 5,
                          stringsAsFactors = FALSE,
                          colClasses = c("character","numeric"),
                          col.names = colNames)
    PFA$ExcVal <- ExcVals
    nPart      <- read.table(file.path(folder.out,filename),
                             sep = " ",
                             skip = 2,
                             stringsAsFactors = FALSE,
                             nrows = 1,
                             colClasses = c("character","numeric"),
                             col.names = colNames)[2]
    PFA <- PFA[,2:3]
    attr(PFA,nPart = nPart)
  }
  
  # Now setup column names
  colnames(PFA) <- colNames
  colClasses    <- names(colsSwitch)
  if(colStandard){
    for(col in colClasses){
      colsIn <- unlist(colsSwitch[[col]])
      colnames(PFA)[sapply(colNames, function(c) any(grepl(c, colsIn)))] <- colsOut[col]
    }
    PFA <- PFA[,colnames(PFA) %in% colsOut]
  }
  print(PFA)
}
