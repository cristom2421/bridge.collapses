PerformMatch <- function(MatchEntry,            # 1-row data.frame for which to find matches
                         TargetData,            # data.frame of potential match targets
                         MatchType,             # type of matching (bin, stream, road, route, gage, dam)
                         maxStringDist = 3,     # for approximate string matching, max distance that will count as approximately matched
                         capCandPct    = 0.5,   # if number of Candidate Matches is > capCandPct*nMatchRows, return "-"
                         capCandN      = 200,   # if number of Candidate Matches is > capCandN, return "-"
                         VERBOSE       = FALSE){# status updates print to screen if TRUE

# setup and checks---------------------------------
MatchRowNames <- rownames(TargetData)
nMatchRows <- nrow(TargetData)

MatchTypes <- c("bin", "stream", "road", "route", "gage", "dam")
if(all(!(grepl(MatchType,MatchTypes)))){
  warning("Match type not supported.")
  stop()
}
MatchSuffix <- c(bin    = "B",
                 road   = "R",
                 stream = "S",
                 route  = "T")[MatchType]
MatchQual   <- c(exact = "e",
                 full  = "f",
                 partial = "p",
                 fuzzy = "z")

# determine column names for type of data specified and set up---------------------------------
MatchColOrigNames <-      c(bin     = "BIN",
                            stream  = "FEAT_UND",
                            road    = "LOCATION",
                            route   = "LOCATION",
                            gage    = "FEAT_UND",
                            dam     = "FEAT_UND")
MatchColNames <-       list(bin     = c("BIN","BIN_NUM","BIN_NUM"),
                            stream  = c("FEAT_UND","STREAM_NAME_1", "STREAM_TYPE_1","STREAM_NAME_2", "STREAM_TYPE_2"),
                            road    = c("LOCATION","ROAD_NAME", "ROAD_TYPE"),
                            route   = c("ROUTE_NAME_1", "ROUTE_NAME_2","ROUTE_TYPE_1", "ROUTE_TYPE_2"),
                            gage    = c("STREAM_NAME", "STREAM_TYPE"),
                            dam     = c("STREAM_NAME", "STREAM_TYPE"))
MatchTargetColNames <- list(bin     = c("STRUCTURE_NUMBER_008","BIN_NUM", "ROUTE_NUMBER_005D"),
                            stream  = rep("FEATURES_DESC_006A", 5),
                            road    = rep("FACILITY_CARRIED_007", 3),
                            route   = c("ROUTE_NUMBER_005D","ROUTE_NUMBER_005D", "ROUTE_PREFIX_ITEM5B", "ROUTE_PREFIX_ITEM5B"), #ITEM5D
                            gage    = c("STREAM_NAME_GAGE", "STREAM_TYPE_GAGE"),
                            dam     = c("STREAM_NAME_DAM", "STREAM_TYPE_DAM"))

MatchColOrig   <- MatchColOrigNames[[MatchType]]
MatchTypes     <- MatchColNames[[MatchType]]
MatchToTypes   <- MatchTargetColNames[[MatchType]]
nMatchTypes    <- length(MatchTypes)

# check that data is present ---------------------------------
CandidateMatchRows <- ifelse(MatchEntry[1,MatchColOrig]!="" & !is.na(MatchEntry[1,MatchColOrig]),
                            1,
                            NA)
if(is.na(CandidateMatchRows)){
  if (VERBOSE==TRUE)print("  no data in original field")
  CandidateMatchRows <- as.character(-1*nMatchRows)
  return(CandidateMatchRows)
  stop()
}

# Loop to perform matches, from highest quality to lowest---------------------------------
for (i in 1:nMatchTypes){
  Marker <- i
  if(is.na(MatchEntry[1,MatchTypes[i]]) | MatchEntry[1,MatchTypes[i]] == "" | is.null(MatchEntry[1,MatchTypes[i]])){ 
    BoolRowMatch <- FALSE
    next
  }
  if(VERBOSE) print(paste0("  Matching ",MatchTypes[i], " to ",MatchToTypes[i]))
  MatchMarker <- paste0(Marker,MatchQual["exact"])
  string  <- as.character(MatchEntry[1,MatchTypes[i]])
  if (VERBOSE) print("    Checking for exact string")
  BoolRowMatch <- grepl(string, TargetData[,MatchToTypes[i]],fixed = TRUE)
  
  
  # all "words" in string matched in order
  MatchMarker <- paste0(Marker,MatchQual["full"])
  string  <- as.character(MatchEntry[1,MatchTypes[i]])
  pattern <- ifelse(MatchType!="bin",
                    paste0("\\<",paste0(unlist(strsplit(string," ")), collapse="\\> \\<"),"\\>"),
                    paste0(unlist(strsplit(string," ")), collapse="[[:alnum:]]*"))
  if (VERBOSE) print("    Checking for all parts of string, ordered")
  BoolRowMatch <- grepl(pattern, TargetData[,MatchToTypes[i]])
  
  # if full name did not yield match, try split string
  if (!any(BoolRowMatch)){
    MatchMarker <- paste0(Marker,MatchQual["partial"])
    SplitString  <- unlist(strsplit(string, " "))
    SplitString  <- gsub("[[:punct:]]","",SplitString)
    nStrings     <- length(SplitString)
    if (nStrings > 1){
      if (VERBOSE) print("    Checking for individual parts of string, unordered")
      if (MatchType!="bin"){
        BoolRowMatch <- sapply(1:nStrings, 
                               function(l) grepl(paste0("\\<",SplitString[l],"\\>"),TargetData[,MatchToTypes[i]]),
                               simplify = TRUE)
      }
      else{
        BoolRowMatch  <- sapply(1:nStrings, 
                                function(l) grepl(SplitString[l],TargetData[,MatchToTypes[i]]),
                                simplify = TRUE)
      }
      if (class(BoolRowMatch)!="data.frame") {BoolRowMatch <- as.data.frame(BoolRowMatch)} 
      nMatchesRow <- apply(BoolRowMatch, MARGIN = 1, sum, na.rm = TRUE)
      maxMatches  <- max(nMatchesRow, na.rm = TRUE)
      if(maxMatches > 0){
        BoolRowMatch <- nMatchesRow == maxMatches # only keep rows with highest number of matches
      }
      else BoolRowMatch <- FALSE
    }
  }
  # if no matches on split strings, try fuzzy matching
  if (!any(BoolRowMatch)){ 
    MatchMarker <- paste0(Marker,MatchQual["fuzzy"])
    if (VERBOSE) print(paste0("    Checking for string distance less than ",maxStringDist) )
    stringDist  <- stringdist(string,TargetData[,MatchToTypes[i]], method = "dl") -
                            abs(nchar(string) - nchar(TargetData[,MatchToTypes[i]]))
      minDist      <- min(stringDist, na.rm = TRUE)
      if(minDist <= maxStringDist){
        BoolRowMatch <- stringDist == minDist # only keep rows with lowest string distance
      }
      else BoolRowMatch <- FALSE
    }
  if(any(BoolRowMatch)) break
}

# record matches: return -nMatchRows if no or very many matches
CandidateMatchRows <- MatchRowNames[BoolRowMatch]
nMatches <- length(CandidateMatchRows)
if(nMatches==0 | nMatches > capCandPct*nMatchRows | nMatches > capCandN){
  CandidateMatchRows <- as.character(-1*nMatchRows)
  if(VERBOSE) print("    No matches found")
}
else CandidateMatchRows <- paste0(MatchMarker, MatchSuffix, ".",CandidateMatchRows)
return(CandidateMatchRows)
}


