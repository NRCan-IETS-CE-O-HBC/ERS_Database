
# R-Start: draft housing stock analysis script. 

DebugConn <- file("debug-msgs.txt","wt")

# Standard output
stream_out <- function(msgs=c()){
  for (i in 1:length(msgs) ){
    cat(msgs[i])
  }
}

# Debugging output
debug_out <- function(msgs=c()){
    if ( DebugSwitch ){   
	  writeLines(msgs, sep="" ,DebugConn)
    }
}

debug_vector <- function(myVector=c()){
    if ( DebugSwitch ){

	  i=0
	  OutVector = c("")
	  for (i in 1:length(myVector) ){
	    OutVector[i+1] = paste("   - ",myVector[i], sep="")
      }
	  OutVector[i+2] = "\n"
	  
      writeLines(OutVector, DebugConn )
	 
	}
}



# Header 
sayHello <- function(){ 
  string_time <- format(Sys.time(), "%a %b $Y %d %X ")
  #debug_out(c('\n\nERS-DB-Analysis.R debugging output. Run started at: ', string_time,' \n\n'))
  stream_out(c("\n"))
  stream_out(c('================== ERS-DataMerge-R ======================\n'))
  stream_out(c('This R script scans the ERS databases in .csv format, and merges \n'))
  stream_out(c('them into a single format. \n\n'))
}


#============= Configuration ========================

# should be a cmd-line arguement
DebugSwitch <- TRUE 

sayHello()

gPathToERSOld       = 'C:/Users/aferguso/Google\ Drive/NRCan\ work/NRCan-Optimization-Results/ERS\ Database/August_2017/EGH-retrofit-design-heat-loss-short.csv'
gPathToERSOld       = 'C:/Users/aferguso/Google\ Drive/NRCan\ work/NRCan-Optimization-Results/ERS\ Database/August_2017/EGH-retrofit-design-heat-loss.csv'
gPathToERSNewEStar  =  'C:/Users/aferguso/Google\ Drive/NRCan\ work/NRCan-Optimization-Results/ERS\ Database/March\ 2018/nh_evaluations_all_ers_estar.csv'
gPathToERSR2000     =  'C:/Users/aferguso/Google\ Drive/NRCan\ work/NRCan-Optimization-Results/ERS\ Database/March\ 2018/r2000_all.csv'



stream_out(c(" - Parsing ERS NH + EStar data from (",gPathToERSNewEStar , ")..."))
myRawData <- read.csv(file=gPathToERSNewEStar , header=TRUE, sep = ",", stringsAsFactors=FALSE)
stream_out (c(" done. raw data contains ",nrow(myRawData), " rows, and ", ncol(myRawData), " columns.\n\n"))

DataNStar <- subset( myRawData, stringsAsFactors=FALSE )
myRawData <- c()

stream_out(c(" - Parsing ERS R2000 data from (",gPathToERSR2000  , ")..."))
myRawData <- read.csv(file=gPathToERSR2000  , header=TRUE, sep = ",", stringsAsFactors=FALSE)
stream_out (c(" done. raw data contains ",nrow(myRawData), " rows, and ", ncol(myRawData), " columns.\n\n"))
DataR2000 <- subset( myRawData, stringsAsFactors=FALSE )
myRawData <- c()

stream_out(c(" - Parsing ERS existing house data from (",gPathToERSOld  , ")..."))
myRawData <- read.csv(file=gPathToERSOld   , header=TRUE, sep = ",", stringsAsFactors=FALSE)
stream_out (c(" done. raw data contains ",nrow(myRawData), " rows, and ", ncol(myRawData), " columns.\n\n"))
DataRetro <- subset( myRawData,  stringsAsFactors=FALSE )
myRawData <- c()

# MAP known aliases
DataNStar$POSTALCODE <- DataNStar$Postal.Code
DataNStar$DataSource <- "ERS-NH+ESTAR"

DataR2000$POSTALCODE <- DataR2000$Postal.code
DataR2000$DataSource <- "R-2000"

DataRetro$DataSource  <- "ERS-Existing"


DataRetro$PROGRAMNAME <- "?"


# =========================================
# First match and merge NH+EStar and R2000
stream_out("\n ================================================ \n")
stream_out(" STEP 1: Merge NH+EStar & R-20000 into AllNewHousing \n")


# Get column name sayHello
cols_R2000 <- colnames( DataR2000 )
cols_NStar <- colnames( DataNStar )


# Get locations of columns that match, set mismatches to -1 
FoundR2000 = match (cols_R2000, cols_NStar, -1 ) 
FoundNStar = match (cols_NStar, cols_R2000, -1 )

stream_out("\n\nMatches between NH+EStar & R-20000 ..................\n")
cols_R2000 [ FoundR2000 != -1 ] 

stream_out("\n\nUnmatched columns from R2000 ..................\n")
cols_R2000 [ FoundR2000 == -1 ] 

stream_out("\n\nUnmatched columns from ESTAR ..................\n")
cols_NStar [ FoundNStar == -1 ] 

DataMatchedR2000 <- subset ( DataR2000, select = cols_R2000[ FoundR2000 != -1 ] )
DataMatchedNStar <- subset ( DataNStar, select = cols_NStar[ FoundNStar != -1 ] ) 

DataMergeNew <- rbind( DataMatchedR2000, DataMatchedNStar ) 
DataMergeNew$EVAL_TYPE <- "NewHousing"

# =========================================
# Next merge new housing + old housing 

stream_out("\n ================================================ \n")
stream_out(" STEP 2: Merge AllNewHousing & Retrofit  \n")


cols_AllNew <- colnames ( DataMergeNew ) 
cols_Retro  <- colnames ( DataRetro ) 

FoundAllNew <- match ( cols_AllNew, cols_Retro , -1 ) 
FoundRetro  <- match ( cols_Retro , cols_AllNew, -1 ) 


stream_out("\n\nMatches between New homes (R-2000, EGH-NH + EStar ) & Retrofit ....\n")
cols_AllNew  [ FoundAllNew != -1 ] 

stream_out("\n\nUnmatched columns from New homes (R-2000, EGH-NH + EStar ) ........\n")
cols_AllNew  [ FoundAllNew == -1 ] 

stream_out("\n\nUnmatched columns from Retrofit ...................................\n")
cols_Retro [ FoundRetro == -1 ] 

DataMatchedAllNew <-subset ( DataMergeNew, select = cols_AllNew[ FoundAllNew != -1 ]  ) 
DataMatchedAllNew<- subset ( DataRetro, select = cols_Retro[  FoundRetro != -1 ] ) 

DataMergeAll <- rbind (DataMatchedAllNew, DataMatchedAllNew) 

stream_out ("\n\n Writing out merged data (ERS-merged.csv)...")
write.csv(DataMergeAll, file = "ERS-merged.csv")	  
stream_out (" done.\n")

q()

