
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
    if ( debug ){   
	  writeLines(msgs, sep="" ,DebugConn)
    }
}

debug_vector <- function(myVector=c()){
    if ( debug ){

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
  string_time <- format(Sys.time(), "%a %b %d %X %Y")
  debug_out(c('\n\n(ERS-DB-Analysis.R debugging output)', string_time,' \n\n'))
  
  stream_out(c('This is the draft housing stock analysis script. \n \n\n'))
}


#============= Configuration ========================

# should be a cmd-line arguement
debug = 1

# Location of the CEUD data source. Also could be a cmd line arguement
#gPathToCEUD = "C:\\Users\\jpurdy\\Google Drive\\NRCan-Optimization-Results\\EIP-Technology-Forecasting\\CEUD-Data\\CEUD-translator-txt.csv"
#gPathToCEUD = "C:\\Users\\aferguso\\Google Drive\\NRCan work\\NRCan-Optimization-Results\\EIP-Technology-Forecasting\\CEUD-Data\\CEUD-translator-txt.csv"

gPathToCEUD = "CEUD-translator-txt.csv"


#gPathToERS  = "C:\\cygwin64\\home\\aferguso\\ERS_Database\\D_E_combined_2016-10-18-forR.csv"
#gPathToERS  = "C:\\Ruby4HTAP\\ERS_archetype_analysis\\ERS_combined_forR.csv"

#gPathToERS   = "C:\\Users\\aferguso\\Google Drive\\NRCan work\\NRCan-Optimization-Results\\EIP-Technology-Forecasting\\ERS-data\\D_E_combined_2016-10-18-forR.csv"

gPathToERS   = "D_E_combined_2016-10-18-forR.csv"

# General parameters

# Number of archetypes to be defined: 
gTotalArchetypes = 500

# year for model
gStockYear = 2013

gnERSrows = 20000

#==============
# Start of script. 
sayHello()

#==============
# CEUD: Parse and pre-process data. 
# Parse CEUD data from csv file. 
stream_out(c(" - About to parse CEUD data (",gPathToCEUD, ")..."))

debug_out(c("reading CEUD data from ",gPathToCEUD,"\n\n"))

mydata <- read.csv (file=gPathToCEUD, header=TRUE, sep = ",", stringsAsFactors=FALSE)
stream_out (c("  done (",nrow(mydata), " rows) \n\n"))


if (debug){
  debug_out (c("raw data contains ",nrow(mydata), " rows.\n"))
  debug_out (c("I found the following columns in mydata : \n"))
  debug_vector( colnames(mydata) )
  debug_out(c("\n"))
}



# Currently,  CEUD-translator-txt.csv contains a couple of duplicate 1990 rows - these are
# flagged by 'Filter_extra_1990 = true

CEUDraw <- subset( mydata, FilterExtra1990 == FALSE, stringsAsFactors=FALSE )
mydata <- c()
# compute number of homes (Number_static is in '1000s')

# Translate some ambigous names. 
CEUDraw$Form[ CEUDraw$Form == "Ap" ] <- "AP"
CEUDraw$Form[ CEUDraw$Form == "Ap+MH" ] <- "AP"
CEUDraw$Equipment[ CEUDraw$Equipment == "Dual (oil/electric)" ] <- "Dual: (oil/electric)"


GroupEquipmentByFuelType = FALSE 

if ( GroupEquipmentByFuelType ){

  CEUDraw$Equipment[ grepl ("^Gas", CEUDraw$Equipment) ] <- "Gas"

  CEUDraw$Equipment[ grepl ("^Oil", CEUDraw$Equipment) ] <- "Oil"

}

CEUD <- subset (CEUDraw, select = c(Province, Form, Stories, Equipment, Vintage, Year, Metric, Number_static))
CEUDraw <- c()
CEUD$NumHomes = CEUD$Number_static * 1000


if(debug){

  debug_out (c("After duplicate 1990 rows were removed, I found ", nrow(CEUD), " rows.\n"))
}



# List of all 'topologies's, which define the aggregations in CEUD tables

CEUDTopologies =  unique( as.vector(CEUD$Metric))


if (debug) {
  debug_out(c("\n","Topology list in CEUD data. - \n"))
  debug_vector( CEUDTopologies )
  
}



# ==============
# Create separate topologies 
# Get subsets that contain housing stock by aggregations of interest. 
CEUDProvFormVintageYr   <- subset( CEUD, Year == gStockYear & Metric == "Province|Form|Vintage|Year") 
CEUDProvFormSHEquipYr   <- subset( CEUD, Year == gStockYear & Metric == "Province|Form|Equipment|Year" & ! grepl("^WH",Equipment) ) 
CEUDProvFormDHWEquipYr  <- subset( CEUD, Year == gStockYear & Metric == "Province|Form|Equipment|Year" & grepl("^WH",Equipment) ) 
CEUDProvAirConYr        <- subset( CEUD, Year == gStockYear & Metric == "Province|Equipment|Year" & grepl("^AC",Equipment) ) 
CEUDProvFormYr          <- subset( CEUD, Year == gStockYear & Metric == "Province|Form|Year") 


# AC system count only enumerates how many homes have ac - not how many don't. We need to append rows 
# for equipment = No AC, computed as [total homes] - [ homes with ac ]
for ( prov in  unique( CEUDProvFormYr$Province ) ) {
  
  HomesNOAC =  sum(CEUDProvFormYr$NumHomes[CEUDProvFormYr$Province==prov]) - 
  
               sum(CEUDProvAirConYr$NumHomes[CEUDProvAirConYr$Province==prov & grepl("^AC",CEUDProvAirConYr$Equipment)])

               
             
               
  appendme = data.frame( Province=c(prov), 
                         Form=c("*"), 
                         Stories=c("*"), 
                         Equipment=c("no AC"), 
                         Vintage=c("*"), 
                         Year=c(gStockYear), 
                         Metric = c("Province|Equipment|Year"),
                         Number_static = c(HomesNOAC/1000),
                         NumHomes= c(HomesNOAC)
                       )
                          
   CEUDProvAirConYr =  rbind(  CEUDProvAirConYr , appendme)                 

} 

### ==============  Consider if the following is really necessary. 
### Perform substitutions on 'Equipment' toplogies - map to simpler definitions for now.  
##if (debug){
##  debug_out ("Equipment keyword replacement (pre)\n")
##  cat( unique( as.vector(CEUDProvFormSHEquipYr$Equipment) ) )
##  debug_out ("/Equipment keyword replacement (pre)\n")
##}
##CEUDProvFormSHEquipYr$Equipment <- as.character(CEUDProvFormSHEquipYr$Equipment)
##
##CEUDProvFormSHEquipYr$Equipment[ CEUDProvFormSHEquipYr$Equipment == "Gas-Medium" ] <- "Gas"
##CEUDProvFormSHEquipYr$Equipment[ CEUDProvFormSHEquipYr$Equipment == "Gas-High"   ] <- "Gas"
##CEUDProvFormSHEquipYr$Equipment[ CEUDProvFormSHEquipYr$Equipment == "Gas-Normal" ] <- "Gas"
##
##CEUDProvFormSHEquipYr$Equipment[ CEUDProvFormSHEquipYr$Equipment == "Oil-Medium" ] <- "Oil"
##CEUDProvFormSHEquipYr$Equipment[ CEUDProvFormSHEquipYr$Equipment == "Oil-High"   ] <- "Oil"
##CEUDProvFormSHEquipYr$Equipment[ CEUDProvFormSHEquipYr$Equipment == "Oil-Normal" ] <- "Oil"
##
##CEUDProvFormSHEquipYr$Equipment <- as.factor(CEUDProvFormSHEquipYr$Equipment)
##
##if ( debug ){
##  debug_out ("Equipment keyword replacement (post)\n")
##  cat (paste(c(unique( as.character(CEUDProvFormSHEquipYr$Equipment))), sep="\n" ))
##  debug_out ("Equipment keyword replacement (post)\n")
##}



CEUDTotalHomes=sum(CEUDProvFormYr$NumHomes) 


# How many homes will each archetype represent? 
gNumHomesEachArchRepresents = CEUDTotalHomes / gTotalArchetypes

debug_out (c( "CEUD Data for", gStockYear, ":\n"))
debug_out (c( "  - Total homes      :",CEUDTotalHomes,"\n"))
debug_out (c( "  - If I generate", gTotalArchetypes, "archetypes, each archetype will represent ", gNumHomesEachArchRepresents," homes\n\n"))

# Pre-processing on CEUD data to pull 'Archetype descriptors' 

#=============ERS 
stream_out (c("\n - About to parse the ERS data at", gPathToERS,"..."))
debug_out  (c("reading ERS data from ", gPathToERS,"\n\n"))
#=ERS data gets parsed here. 

myERSdata <- read.csv (file=gPathToERS, nrows=gnERSrows, header=TRUE, sep = ",") 

#myERSdata <- read.csv (file=gPathToERS, header=TRUE, sep = ",")


# show the columns that we pulled - 
debug_out("List of columns in ERS database:\n")
debug_vector(sort(colnames(myERSdata)))

# randomize the ERS data 
set.seed(42);
myRandomERSdata <- myERSdata[sample(nrow(myERSdata)), ]
myERSdata <- myRandomERSdata

stream_out (c("The ERS database is now randomized"))
# set myERSdata to randomized data frame


stream_out (c(" - ... done.(",nrow(myERSdata)," rows)\n\n"))

debug_out (c( "ERS Data:\n"))
debug_out (c( "  - Total rows      :",nrow(myERSdata),"\n"))
debug_out (c( "  - Total cols      :",ncol(myERSdata),"\n"))
debug_out (c( "\n\n"))


# ============= Flag bad data 
# 
# myERSdata$dataOK <- TRUE
# myERSdata$dataOK[ myERSdata$FURSSEFF.E<0  ] <- FALSE 
#

#unique(as.character(myERSdata$SHEU.vintage.E))



#====================================
# Create new records in ERS data that map to CEUD

# Vintage

stream_out("\n\n - Mapping vintage to CEUD definitions... ")
debug_out(c("\n\n =============== Vintage analysis ====================\n\n"))

debug_out (c(" Vintages used in CEUD:\n"))
debug_vector(c(unique(as.character(CEUD$Vintage))))
debug_out (c("\n\n"))

# Set all vintage tags in ERS as error; as we identify ones that are valid, flip them 
# to our common keywords. 
myERSdata$CEUDVintage <- "error"
myERSdata$CEUDVintage[ myERSdata$YEARBUILT.E < 1946 ] <- "Before 1946"
myERSdata$CEUDVintage[ myERSdata$YEARBUILT.E > 1945 & myERSdata$YEARBUILT.E < 1961 ] <- "1946-1960"
myERSdata$CEUDVintage[ myERSdata$YEARBUILT.E > 1960 & myERSdata$YEARBUILT.E < 1978 ] <- "1961-1977"
myERSdata$CEUDVintage[ myERSdata$YEARBUILT.E > 1977 & myERSdata$YEARBUILT.E < 1984 ] <- "1978-1983"
myERSdata$CEUDVintage[ myERSdata$YEARBUILT.E > 1983 & myERSdata$YEARBUILT.E < 1996 ] <- "1984-1995"
myERSdata$CEUDVintage[ myERSdata$YEARBUILT.E > 1995 & myERSdata$YEARBUILT.E < 2001 ] <- "1996-2000"
myERSdata$CEUDVintage[ myERSdata$YEARBUILT.E > 2000 & myERSdata$YEARBUILT.E < 2006 ] <- "2001-2005"
myERSdata$CEUDVintage[ myERSdata$YEARBUILT.E > 2005 & myERSdata$YEARBUILT.E < 2011 ] <- "2006-2010"
myERSdata$CEUDVintage[ myERSdata$YEARBUILT.E > 2010 & myERSdata$YEARBUILT.E < 2014 ] <- "2011-2013"
myERSdata$CEUDVintage[ myERSdata$YEARBUILT.E > 2014 & myERSdata$YEARBUILT.E < 1977 ] <- "After 2014"

myERSdata$CEUDVintage <- as.factor(myERSdata$CEUDVintage)

debug_out (c("Vintages set in ERS for valid rows:\n"))
debug_vector(c(unique(as.character(myERSdata$CEUDVintage))))
debug_out (c("\n\n"))

debug_out ("Vintage Codes that weren't set properly:\n")
debug_vector(c(unique(as.character(myERSdata$YEARBUILT.E[myERSdata$CEUDVintage == "error"]))))
debug_out (c("(",nrow(myERSdata[myERSdata$CEUDVintage == "error",])," rows in total)\n"))

stream_out(c("done.\n"))


# Province 

stream_out(c(" - Mapping province to CEUD definitions..."))


debug_out(c("\n\n =============== Province analysis ====================\n\n"))

debug_out (c(" Provinces used in CEUD:\n"))
debug_vector(c(unique(as.character(CEUD$Province))))
debug_out (c("\n\n"))

debug_out (c(" Provinces used in ERS:\n"))
debug_vector(c(unique(as.character(myERSdata$PROVINCE.D))))
debug_out (c("\n\n"))


myERSdata$CEUDProvince <- "error"
myERSdata$CEUDProvince[ myERSdata$PROVINCE.D == "QC" ] <- "QC"
myERSdata$CEUDProvince[ myERSdata$PROVINCE.D == "BC" ] <- "BC"
myERSdata$CEUDProvince[ myERSdata$PROVINCE.D == "ON" ] <- "ON"
myERSdata$CEUDProvince[ myERSdata$PROVINCE.D == "SK" ] <- "SK"
myERSdata$CEUDProvince[ myERSdata$PROVINCE.D == "MB" ] <- "MB"
myERSdata$CEUDProvince[ myERSdata$PROVINCE.D == "NF" ] <- "NF"
myERSdata$CEUDProvince[ myERSdata$PROVINCE.D == "AB" ] <- "AB"
myERSdata$CEUDProvince[ myERSdata$PROVINCE.D == "NS" ] <- "NS"
myERSdata$CEUDProvince[ myERSdata$PROVINCE.D == "NB" ] <- "NB"
myERSdata$CEUDProvince[ myERSdata$PROVINCE.D == "PE" ] <- "PEI"
myERSdata$CEUDProvince[ myERSdata$PROVINCE.D == "NT" ] <- "TR"
myERSdata$CEUDProvince[ myERSdata$PROVINCE.D == "YK" ] <- "TR"
myERSdata$CEUDProvince[ myERSdata$PROVINCE.D == "NU" ] <- "TR" 
 

debug_out (c("Provinces set in ERS for valid rows:\n"))
debug_vector(c(unique(as.character(myERSdata$CEUDProvince[myERSdata$CEUDProvince != "error"]))))
debug_out (c("\n\n"))

debug_out ("Province Codes that weren't set properly:\n")
debug_vector(c(unique(as.character(myERSdata$PROVINCE.D[myERSdata$CEUDProvince == "error"]))))
debug_out (c("(",nrow(myERSdata[myERSdata$CEUDProvince == "error",])," rows in total)\n"))

stream_out(c("done.\n"))



#========Type of house 

stream_out(" - Mapping house type to CEUD definitions...")

debug_out(c("\n\n =============== House type analysis ====================\n\n"))

debug_out (c("Housing Forms used in CEUD:\n"))
debug_vector(c(unique(as.character(CEUD$Form))))
debug_out (c("\n\n"))

debug_out (c("Housing types used in ERS:\n"))
debug_vector(c(unique(as.character(myERSdata$TYPEOFHOUSE.D))))
debug_out (c("\n\n"))


  myERSdata$CEUDForm <- "error"
  myERSdata$CEUDForm[ myERSdata$TYPEOFHOUSE.D == "Single^detached" ] <- "SD"
  myERSdata$CEUDForm[ myERSdata$TYPEOFHOUSE.D == "Mobile^home" ] <- "MH"
  myERSdata$CEUDForm[ myERSdata$TYPEOFHOUSE.D == "Double/Semi-detached" |  
                      myERSdata$TYPEOFHOUSE.D == "Attached^Duplex" |  
                      myERSdata$TYPEOFHOUSE.D == "Duplex^(non-MURB)"  |  
                      myERSdata$TYPEOFHOUSE.D == "Attached^Triplex"   |
                      myERSdata$TYPEOFHOUSE.D == "Row^house_^end^unit" |
                      myERSdata$TYPEOFHOUSE.D == "Detached^Duplex"  |
                      myERSdata$TYPEOFHOUSE.D == "Detached^Triplex"  |
                      myERSdata$TYPEOFHOUSE.D == "Triplex^(non-MURB)"  | 
                      myERSdata$TYPEOFHOUSE.D == "Row^house_^middle^unit"  ] <- "SA"
  myERSdata$CEUDForm[ myERSdata$TYPEOFHOUSE.D == "Apartment" |  
                      myERSdata$TYPEOFHOUSE.D == "Apartment^Row" ] <- "AP"
 
 
  # Create variant for DHW analysis because CEUD groups AP+MH"
  myERSdata$CEUDFormDHW <- myERSdata$CEUDForm
  
  myERSdata$CEUDFormDHW[ myERSdata$CEUDForm=="MH" ] <- "AP"
  myERSdata$CEUDFormDHW[ myERSdata$CEUDForm=="AP" ] <- "AP"
 
  
debug_out (c("House types set in ERS for valid rows:\n"))
debug_vector(c(unique(as.character(myERSdata$CEUDForm[myERSdata$CEUDForm != "error"]))))
debug_out (c("\n\n"))

debug_out ("House Type Codes that weren't set properly:\n")
debug_vector(c(unique(as.character(myERSdata$TYPEOFHOUSE.D[myERSdata$CEUDForm == "error"]))))
debug_out (c("(",nrow(myERSdata[myERSdata$CEUDForm == "error",])," rows in total)\n"))






stream_out(c("done.\n"))
  
  
  
  

# Heating fuel  

stream_out(" - Mapping heating fuel  to CEUD definitions...")


debug_out(c("\n\n =============== Heating fuel/equipment analysis ====================\n\n"))
debug_out (" Equipment types used in CEUD:\n")
debug_vector(c(unique(as.character(CEUD$Equipment))))

  debug_out (" Furnace fuel used in ERS:\n")
  

  
  debug_vector(c(unique(as.character(myERSdata$FURNACEFUEL.D))))
 
  
  debug_out ("\n")
  debug_out (" Furnace efficiencies used in ERS:\n")
  debug_vector(c(unique(as.character(myERSdata$FURSSEFF.D))))
  debug_out (" Heat pump types used in ERS:\n")
  debug_vector(c(unique(as.character(myERSdata$HPSOURCE.D[ as.numeric(myERSdata$COP.D) > 1.1 ] ))))
  debug_out (" Heat pump COPS types used in ERS:\n")
  debug_vector(c(unique(as.character(myERSdata$COP.D))))
  

  

  # 1: Classify dual fuel systems
 # Recode
  myERSdata$CEUDSHCode <- "Code0"
  myERSdata$SHFuel1    <- "Fuel1"
  myERSdata$SHFuel2    <- "Fuel2"
  myERSdata$SHFuel3    <- "Fuel3"
  
  #myERSdata$FURNACEFUEL.D   <- as.character(myERSdata$FURNACEFUEL.D)
  #myERSdata$SUPPHTGFUEL1.D  <- as.character(myERSdata$SUPPHTGFUEL1.D)
  #myERSdata$SUPPHTGFUEL2.D  <- as.character(myERSdata$SUPPHTGFUEL2.D )
  
  myERSdata$SHFuel1 <- as.character(myERSdata$FURNACEFUEL.D)
  myERSdata$SHFuel2 <- as.character(myERSdata$SUPPHTGFUEL1.D)
  myERSdata$SHFuel3 <- as.character(myERSdata$SUPPHTGFUEL2.D)
  
  
  # Set NA to none. 
  myERSdata$SHFuel2[ myERSdata$SUPPHTGFUEL1.D == "N/A" ] <- "none"
  myERSdata$SHFuel3[ myERSdata$SUPPHTGFUEL2.D == "N/A" ] <- "none"
  
  # Rename wood variants 
  myERSdata$SHFuel1[ myERSdata$FURNACEFUEL.D == "Mixed^wood" |
                     myERSdata$FURNACEFUEL.D == "Hardwood" |  
                     myERSdata$FURNACEFUEL.D == "Softwood" |
                     myERSdata$FURNACEFUEL.D == "Wood^Pellets"   ] <- "Wood"
    
  myERSdata$SHFuel2[ myERSdata$SUPPHTGFUEL1.D  == "Mixed^wood" |
                     myERSdata$SUPPHTGFUEL1.D  == "Hardwood" |  
                     myERSdata$SUPPHTGFUEL1.D  == "Softwood" |
                     myERSdata$SUPPHTGFUEL1.D  == "Wood^Pellets"   ] <- "Wood"
  
  myERSdata$SHFuel3[ myERSdata$SUPPHTGFUEL2.D  == "Mixed^wood" |
                     myERSdata$SUPPHTGFUEL2.D  == "Hardwood" |  
                     myERSdata$SUPPHTGFUEL2.D  == "Softwood" |
                     myERSdata$SUPPHTGFUEL2.D  == "Wood^Pellets"   ] <- "Wood"  

                     
                     
  if (debug){ 
  pre_combinations0 = paste( as.character(myERSdata$CEUDSHCode) , " + "  )
  pre_combinations1 = paste( as.character(myERSdata$SHFuel1) , " + "  )
  pre_combinations2 = paste( as.character(myERSdata$SHFuel2) , " + "  )
  pre_combinations3 = paste( as.character(myERSdata$SHFuel3) , " + "  )
  }       
                     
                                          
 
 # if fuel3 is a duplicate, eliminate it. 

   myERSdata$SHFuel3[     myERSdata$SHFuel3 == myERSdata$SHFuel2 ] <- "none"
   myERSdata$SHFuel3[     myERSdata$SHFuel3 == myERSdata$SHFuel1 ] <- "none"                    

 # if fuel2 is a duplicate, eliminate it. 

   myERSdata$SHFuel2[     myERSdata$SHFuel2 == myERSdata$SHFuel1 ] <- "none"

 
                     
  # if fuel2 is none, use fuel 3.If fuel1 is none, use fuel 2.
  myERSdata$SHFuel2[     myERSdata$SHFuel2 == "none" ] <-  myERSdata$SHFuel3[  myERSdata$SHFuel2 == "none" ]
  myERSdata$SHFuel1[     myERSdata$SHFuel1 == "none" ] <-  myERSdata$SHFuel2[  myERSdata$SHFuel1 == "none" ]

   # We may have made more duplicates. lets eliminate them again.  

   myERSdata$SHFuel3[     myERSdata$SHFuel3 == myERSdata$SHFuel2 ] <- "none"
   myERSdata$SHFuel3[     myERSdata$SHFuel3 == myERSdata$SHFuel1 ] <- "none"                    

 # if fuel2 is a duplicate, eliminate it. 

   myERSdata$SHFuel2[     myERSdata$SHFuel2 == myERSdata$SHFuel1 ] <- "none"
  
  
  

   
#
#  myERSdata$SHFuel3[     myERSdata$SHFuel1 == myERSdata$SHFuel3 ] <- "none"
#                         
#
#  myERSdata$SHFuel3[     myERSdata$SHFuel2 == myERSdata$SHFuel3 ] <- "none"
#
#
#                         
# 
#  # When fuel2 & 3 are none, use fuel 1 only   
  myERSdata$CEUDSHCode[  myERSdata$SHFuel2 == "none" & 
                         myERSdata$SHFuel3 == "none" ]   <- "Code1"
                                                                                

 
  # First priority = code 1/2. Set fuel flags based on elec/gas
  myERSdata$SHHasElec[ myERSdata$SHFuel1 == "Electricity" | myERSdata$SHFuel2 == "Electricity" ] = TRUE
  myERSdata$SHHasGas[ myERSdata$SHFuel1 == "Natural^Gas" | myERSdata$SHFuel2 == "Natural^Gas" ] = TRUE
  myERSdata$SHHasOil[ myERSdata$SHFuel1 == "Oil" | myERSdata$SHFuel2 == "Oil" ] = TRUE
  myERSdata$SHHasWood[ myERSdata$SHFuel1 == "Wood" | myERSdata$SHFuel2 == "Wood" ] = TRUE  
  
  
  myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & myERSdata$SHHasElec &  myERSdata$SHHasGas  ] = "Dual: gas/electric"
  myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & myERSdata$SHHasElec &  myERSdata$SHHasOil  ] = "Dual: (oil/electric)"  
  myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & myERSdata$SHHasElec &  myERSdata$SHHasWood ] = "Dual: wood/electric"  
  myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & myERSdata$SHHasWood &  myERSdata$SHHasOil  ] = "Dual: wood/oil"    
  
  # expand to code 3, and recleass remaining 
  myERSdata$SHHasElec[ myERSdata$SHFuel3 == "Electricity" ] = TRUE
  myERSdata$SHHasGas[ myERSdata$SHFuel3 == "Natural^Gas"  ] = TRUE
  myERSdata$SHHasOil[ myERSdata$SHFuel3 == "Oil"          ] = TRUE
  myERSdata$SHHasWood[ myERSdata$SHFuel3 == "Wood"        ] = TRUE  
  
  myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & myERSdata$SHHasElec &  myERSdata$SHHasGas  ] = "Dual: gas/electric"
  myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & myERSdata$SHHasElec &  myERSdata$SHHasOil  ] = "Dual: (oil/electric)"  
  myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & myERSdata$SHHasElec &  myERSdata$SHHasWood ] = "Dual: wood/electric"  
  myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & myERSdata$SHHasWood &  myERSdata$SHHasOil  ] = "Dual: wood/oil"    
    
  
#
#
#
#                                                                                
#  myERSdata$FURNACEFUEL.D   <- as.factor(myERSdata$FURNACEFUEL.D)
#  myERSdata$SUPPHTGFUEL1.D  <- as.factor(myERSdata$SUPPHTGFUEL1.D)
#  myERSdata$SUPPHTGFUEL2.D  <- as.factor(myERSdata$SUPPHTGFUEL2.D ) 
#    
#  
  
  
 #if (debug){ 
 #
 #    ers_combinations = sort( paste( as.character(myERSdata$CEUDSHCode), " <- ",
 #                              as.character(myERSdata$SHFuel1),  " + ", 
 #                              as.character(myERSdata$SHFuel2) , " + ", 
 #                              as.character(myERSdata$SHFuel3) ) )
 #    
 #    as.data.frame(table( ers_combinations))
 #    
 #    
 #    ers_combinations0 = paste( as.character(myERSdata$CEUDSHCode) , " + "  )
 #    
 #    
 #    ers_combinations1 = paste( as.character(myERSdata$SHFuel1) , " + "  )
 #
 #    
 #    ers_combinations2 = paste( as.character(myERSdata$SHFuel2) , " + "  )
 #
 #    
 #    ers_combinations3 = paste( as.character(myERSdata$SHFuel3) , " + "  )
 #
 #    
 #    as.data.frame(table( pre_combinations0))
 #    
 #    as.data.frame(table( ers_combinations0))
 #    
 #    
 #    as.data.frame(table( pre_combinations1))    
 #      as.data.frame(table( ers_combinations1))  
 #    
 #    as.data.frame(table( pre_combinations2))
 #      as.data.frame(table( ers_combinations2))
 #    
 #    
 #    as.data.frame(table( pre_combinations3))  
 #      as.data.frame(table( ers_combinations3))
 #  }
    
    

	
	
  myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code1" ] <- "Code0"

  

  # Set all gas furnaces to 'medium', and then recode ones for which valid efficiency data exists. 
  myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & 
                        myERSdata$FURNACEFUEL.D == "Natural^Gas" & 
						is.numeric(myERSdata$FURSSEFF.D) ] <- "Gas-Medium"
  myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & 
                        myERSdata$FURNACEFUEL.D == "Natural^Gas" & 
                        as.numeric(myERSdata$FURSSEFF.D) > 89 ] <- "Gas-High"
  myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & 
                        myERSdata$FURNACEFUEL.D == "Natural^Gas" & as.numeric(myERSdata$FURSSEFF.D) > 77 
                                                                        & as.numeric(myERSdata$FURSSEFF.D) < 90 ] <- "Gas-Medium"
  myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & 
                        myERSdata$FURNACEFUEL.D == "Natural^Gas" & as.numeric(myERSdata$FURSSEFF.D) < 78 ] <- "Gas-Normal"
  
  # Set all oil furnaces to 'medium', and then recode ones for which valid efficiency data exists. 
  myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & 
                        myERSdata$FURNACEFUEL.D == "Oil" & 
						is.numeric(myERSdata$FURSSEFF.D) ] <- "Oil-Medium"
  myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & 
                        myERSdata$FURNACEFUEL.D == "Oil" & 
                        as.numeric(myERSdata$FURSSEFF.D) > 85 ] <- "Oil-High"
  myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & 
                        myERSdata$FURNACEFUEL.D == "Oil" & 
                        as.numeric(myERSdata$FURSSEFF.D) > 77 &
                        as.numeric(myERSdata$FURSSEFF.D) < 85 ] <- "Oil-Medium"
  myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & 
                        myERSdata$FURNACEFUEL.D == "Oil" & 
                        as.numeric(myERSdata$FURSSEFF.D) < 78 ] <- "Oil-Normal"  
  
   # Set all electric to 'electric', and then recode heat pumps as needed 
   myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & 
                        myERSdata$FURNACEFUEL.D == "Electricity" ] <- "Electric"
   myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & 
                        myERSdata$CEUDSHCode == "Code0" & 
                        myERSdata$FURNACEFUEL.D == "Electricity" & 
                               ( myERSdata$HPSOURCE.D == "Water" | 
                                 myERSdata$HPSOURCE.D == "Air" | 
                                 myERSdata$HPSOURCE.D == "Ground" ) & 
                        is.numeric(myERSdata$COP.D)	 ] <- "Heat-pump"
                                
   myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & 
                         myERSdata$FURNACEFUEL.D == "Propane" ] <- "Other"
                                
                                
    myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" & 
                        myERSdata$FURNACEFUEL.D == "Mixed^wood" |
                                 myERSdata$FURNACEFUEL.D == "Hardwood" |  
                                 myERSdata$FURNACEFUEL.D == "Softwood" |
                                 myERSdata$FURNACEFUEL.D == "Wood^Pellets"   ] <- "Wood"
    
    myERSdata$CEUDSHCode[ myERSdata$CEUDSHCode == "Code0" ] <- "error"
	

	if ( GroupEquipmentByFuelType ){

      myERSdata$CEUDSHCode[ grepl ("^Gas", myERSdata$CEUDSHCode) ] <- "Gas"

      myERSdata$CEUDSHCode[ grepl ("^Oil", myERSdata$CEUDSHCode) ] <- "Oil"

    }

	
	
	
    #  # The following code is useful for inspecting classifications     
    #  ers_combinations = sort( paste( as.character(myERSdata$CEUDSHCode), " <- ",
    #                                  as.character(myERSdata$SHFuel1),  " + ", 
    #                                  as.character(myERSdata$SHFuel2) , " + ", 
    #                                  as.character(myERSdata$SHFuel3) ) )
    #    
    #  as.data.frame(table( ers_combinations))
    #    
    #    
    #  ers_combinations0 = paste( as.character(myERSdata$CEUDSHCode) , " + "  )
    #    
    #    
    #  ers_combinations1 = paste( as.character(myERSdata$SHFuel1) , " + "  )
    #  
    #    
    #  ers_combinations2 = paste( as.character(myERSdata$SHFuel2) , " + "  )
    #  
    #    
    #  ers_combinations3 = paste( as.character(myERSdata$SHFuel3) , " + "  )
    #  
    #    
    #  as.data.frame(table( pre_combinations0))
    #  as.data.frame(table( ers_combinations0))
    #  as.data.frame(table( pre_combinations1))    
    #  as.data.frame(table( ers_combinations1))  
    #    
    #  as.data.frame(table( pre_combinations2))
    #  as.data.frame(table( ers_combinations2))
    #    
    #    
    #  as.data.frame(table( pre_combinations3))  
    #  as.data.frame(table( ers_combinations3))
    
  
  debug_out (" Heating Equipment set in ERS according to CEUD definitions :\n")
  debug_vector(c(unique(as.character(myERSdata$CEUDSHCode[myERSdata$CEUDSHCode != "error"]))))
  
  if (nrow(myERSdata[myERSdata$CEUDSHCode== "error",]) > 0 ) {
    debug_out (c("I found that ",nrow(myERSdata[myERSdata$CEUDSHCode == "error",])," rows with the following equip codes rows couldn't be coded:\n"))
    debug_vector(c(unique(as.character(myERSdata$FURNACEFUEL.D[myERSdata$CEUDSHCode == "error"]))))
  }
  
      
  stream_out("done.\n")
    
# Cooling 



debug_out(c("\n\n =============== AC analysis ====================\n\n"))

stream_out(" - Mapping AC to CEUD definitions...")




  debug_out (" AC definitions used in ERS:\n")
  debug_vector(c(unique(as.character(myERSdata$AIRCONDTYPE.D))))
  debug_out ("\n")



 # Recode
   
  myERSdata$CEUDAirCon <- "error"
  myERSdata$CEUDAirCon [ myERSdata$AIRCONDTYPE.D == "N/A"  |
                         myERSdata$AIRCONDTYPE.D == "Not^installed"    ] <- "no AC"
                         
  myERSdata$CEUDAirCon [ myERSdata$AIRCONDTYPE.D == "Conventional^A/C" |
                         myERSdata$AIRCONDTYPE.D == "Conventional^A/C:^with^vent.^cooling"  |
                         myERSdata$AIRCONDTYPE.D == "A/C^with^economizer"  ] <- "AC-Central"                         

  myERSdata$CEUDAirCon [ myERSdata$AIRCONDTYPE.D == "Window^A/C" |
                         myERSdata$AIRCONDTYPE.D == "Window^A/C^w/economizer"    |
                         myERSdata$AIRCONDTYPE.D == "Window^A/C^w/^economizer"   |
                         myERSdata$AIRCONDTYPE.D == "Window^A/C^w/vent^cooling"    ] <- "AC-Room"    
                         
                         

  debug_out (" \n"); 
  debug_out (" AC Equipment set in ERS according to CEUD definitions :\n")
  debug_vector(c(unique(as.character(myERSdata$CEUDAirCon[myERSdata$CEUDAirCon != "error"]))))
  
  if (nrow(myERSdata[myERSdata$CEUDAirCon== "error",]) > 0 ) {
    debug_out (c("I found that ",nrow(as.character(myERSdata[myERSdata$CEUDAirCon == "error",]))," rows with the following AC codes rows couldn't be coded:\n"))
    debug_vector(c(unique(as.character(myERSdata$AIRCONDTYPE.D[myERSdata$CEUDAirCon == "error"]))))
  }
  
  stream_out("done.\n")
 
    
# Water Heating  


debug_out(c("\n\n =============== Water Heating analysis ====================\n\n"))

stream_out(" - Mapping DHW to CEUD definitions...")

  debug_out (" WH definitions used in ERS:\n")
  debug_vector(c(unique(as.character(myERSdata$PDHWFUEL.D))))
  debug_out ("\n")



 # Recode
   
  myERSdata$CEUDdhw <- "error"
  myERSdata$CEUDdhw[ myERSdata$PDHWFUEL.D == "Natural^Gas"     ] <- "WH-Gas"
  myERSdata$CEUDdhw[ myERSdata$PDHWFUEL.D == "Electricity"     ] <- "WH-Elec"                    
  myERSdata$CEUDdhw[ myERSdata$PDHWFUEL.D == "Oil"     ] <- "WH-Oil"  
  
  myERSdata$CEUDdhw[ myERSdata$PDHWFUEL.D == "Hardwood"       |                   
                     myERSdata$PDHWFUEL.D == "Softwood"       |                   
                     myERSdata$PDHWFUEL.D == "Mixed^wood"     |
                     myERSdata$PDHWFUEL.D == "Wood^Pellets"    ] <- "WH-Wood"     
                         
  myERSdata$CEUDdhw[ myERSdata$PDHWFUEL.D == "Propane"     ] <- "WH-Other"  
  
  
  
  myERSdata$CEUDdhw[ myERSdata$PDHWFUEL.D == "Solar" & myERSdata$SDHWFUEL.D == "Natural^Gas"     ] <- "WH-Gas"
  myERSdata$CEUDdhw[ myERSdata$PDHWFUEL.D == "Solar" &myERSdata$SDHWFUEL.D == "Electricity"     ] <- "WH-Elec"                    
  myERSdata$CEUDdhw[ myERSdata$PDHWFUEL.D == "Solar" &myERSdata$SDHWFUEL.D == "Oil"     ] <- "WH-Oil"  
  
  myERSdata$CEUDdhw[ myERSdata$SDHWFUEL.D == "Solar" & 
                       ( myERSdata$SDHWFUEL.D == "Hardwood"      |                   
                        myERSdata$SDHWFUEL.D == "Softwood"       |                   
                        myERSdata$SDHWFUEL.D == "Mixed^wood"     |
                        myERSdata$SDHWFUEL.D == "Wood^Pellets"  )  ] <- "WH-Wood"     
                         
  myERSdata$CEUDdhw[ myERSdata$PDHWFUEL.D == "Solar" &  myERSdata$PDHWFUEL.D == "Propane"     ] <- "WH-Other"  




  debug_out (" \n"); 
  debug_out (" DHW Equipment set in ERS according to CEUD definitions :\n")
  debug_vector(c(unique(as.character(myERSdata$CEUDdhw[myERSdata$CEUDdhw != "error"]))))
  
  if (nrow(myERSdata[myERSdata$CEUDdhw== "error",]) > 0 ) {
    debug_out (c("I found that ",nrow(myERSdata[myERSdata$CEUDdhw == "error",])," rows with the following DHW codes rows couldn't be coded:\n"))
    debug_vector(c(unique(as.character(myERSdata$PDHWFUEL.D[myERSdata$CEUDdhw == "error"]))))
  }
 
  
  stream_out("done.\n")
    
  
# Set master flag that indicates if mapping was successful or not 
myERSdata$CEUDerror <- FALSE
myERSdata$CEUDerror[ myERSdata$CEUDdhw == "error"  |
                     myERSdata$CEUDProvince == "error"  |
                     myERSdata$CEUDVintage == "error"  |
                     myERSdata$CEUDForm == "error"  |
                     myERSdata$CEUDSHCode == "error"  |
                     myERSdata$CEUDAirCon == "error"   ] <- TRUE
                   
				   
				   
# Create Keys to map records to CEUD topologies: 

#myERSdata$CEUDTopProvFormVintageYr = paste()				   
				                      
myERSdata$CEUDTopProvFormVintage <-  paste( myERSdata$CEUDProvince, myERSdata$CEUDForm,    myERSdata$CEUDVintage, sep="|")								
myERSdata$CEUDTopProvFormSH      <-  paste( myERSdata$CEUDProvince, myERSdata$CEUDForm,    myERSdata$CEUDSHCode,  sep="|")								
myERSdata$CEUDTopProvFormDHW     <-  paste( myERSdata$CEUDProvince, myERSdata$CEUDFormDHW, myERSdata$CEUDdhw,  sep="|")								
myERSdata$CEUDTopProvAC          <-  paste( myERSdata$CEUDProvince, myERSdata$CEUDAirCon, sep="|" )

            
                   
stream_out (c("\n - I processed ",nrow(myERSdata)," rows. ", nrow(myERSdata[myERSdata$CEUDerror,]), " contained errors \n\n"))
    
	
#=============================== PICK the archetypes we need 
# Archetypes now mapped to CEUD tags. Next steps: 

stream_out(c(" - Selecting ",gTotalArchetypes," to represent Canadian Housing Stock\n"))


debug_out(c("\n\n =============== Try picking archetypes. ====================\n\n"))



myERSdata$ArchInclude <- FALSE 

gCount <- 0
NumOfArchPerProv <- NULL
NumOfArchPerProvFormVintage <- NULL
NumOfArchPerProvFormSH <- NULL
NumOfArchPerProvFormDHW <- NULL
NumOfArchPerProvAC <-NULL
ArchProvince <- NULL



debug_out(c("# of archetypes needed for each province. \n"))

arch_run_total <- 0


#unique(as.character(CEUDProvFormVintageYr$Key))


#--Initialize Counters 
CountProvFormVintage <- NULL
CountProvFormSH      <- NULL 
CountProvAC          <- NULL
CountProvFormDHW     <- NULL


# BY PROVICE - Can be deleted
#--for each Province in the CEUD Database, calculate the size of each archetype bucket; NumOfArch
#for (ArchProvince in unique(CEUDProvFormYr$Province)){
#
#
#  #debug_out(c(ArchProvince))
#  
#  #debug_out(c("-",sum(CEUDProvFormYr$NumHomes[CEUDProvFormYr$Province == ArchProvince]),"\n")) 
#  
#  # Set size of buckets for each topology, and round them to nearest value
#  NumOfArchPerProv[ArchProvince] <- ( (sum(CEUDProvFormVintageYr$NumHomes[CEUDProvFormVintageYr$Province == ArchProvince]))  / CEUDTotalHomes * gTotalArchetypes)
#  NumOfArchPerProv[ArchProvince] <- round(NumOfArchPerProv[ArchProvince])
#  
#  debug_out(c(" Prov-:", ArchProvince, " #: ", NumOfArchPerProv[ArchProvince],"\n"))
#  
#  
#  arch_run_total = arch_run_total + NumOfArchPerProv[ArchProvince] 
#    
#}  



# BY Province / Form / Vintage 




CEUDProvFormVintageYr$Key = paste(CEUDProvFormVintageYr$Province,CEUDProvFormVintageYr$Form, CEUDProvFormVintageYr$Vintage, sep="|" )

 
#--for each Province|Form|Vintage in the CEUD Database, calculate the size of each archetype bucket; NumOfArch
for (KeyVal in unique(CEUDProvFormVintageYr$Key)){
  NumbInCanada <- sum(CEUDProvFormVintageYr$NumHomes[CEUDProvFormVintageYr$Key == KeyVal])
  NumOfArch   <- NumbInCanada / CEUDTotalHomes * gTotalArchetypes
  if ( NumOfArch < 1 && NumOfArch > 0 ) {
    NumOfArch = 1
  }  
  NumOfArchPerProvFormVintage[KeyVal] <- round(NumOfArch)
   
  debug_out(c(" ProvFormVintage:", KeyVal, " #: ", NumOfArchPerProvFormVintage[KeyVal]," of ", sum(CEUDProvFormVintageYr$NumHomes),"\n"))
  
  CountProvFormVintage[KeyVal] <- 0
  
     
}  

debug_out(c("---------------------------------------\n"))


# BY Province / Form / SH EQUIPMENT

CEUDProvFormSHEquipYr$Key = paste(CEUDProvFormSHEquipYr$Province,CEUDProvFormSHEquipYr$Form, CEUDProvFormSHEquipYr$Equipment, sep="|" )

#--for each Province|Form|Equipment in the CEUD Database, calculate the size of each archetype bucket; NumOfArch
for (KeyVal in unique(CEUDProvFormSHEquipYr$Key)){
    
  NumbInCanada <- sum(CEUDProvFormSHEquipYr$NumHomes[CEUDProvFormSHEquipYr$Key == KeyVal])
  NumOfArch   <- NumbInCanada / CEUDTotalHomes * gTotalArchetypes
  if ( NumOfArch < 1 && NumOfArch > 0 ) {
    NumOfArch = 1
  }  
  NumOfArchPerProvFormSH[KeyVal] <- round(NumOfArch) 
  
  CountProvFormSH[KeyVal] <- 0 
  
  debug_out(c(" ProvFormSH:>", KeyVal, "< ##: ", NumOfArchPerProvFormSH[KeyVal]," of ", sum(CEUDProvFormSHEquipYr$NumHomes), "\n"))
    
}  

 
 CEUDProvFormDHWEquipYr$Key = paste(CEUDProvFormDHWEquipYr$Province,CEUDProvFormDHWEquipYr$Form, CEUDProvFormDHWEquipYr$Equipment, sep="|" )

#--for each Province|Form|WH in the CEUD Database, calculate the size of each archetype bucket; NumOfArch
for (KeyVal in unique(CEUDProvFormDHWEquipYr$Key)){
  
  NumbInCanada <- sum(CEUDProvFormDHWEquipYr$NumHomes[CEUDProvFormDHWEquipYr$Key == KeyVal])
  NumOfArch   <- NumbInCanada / CEUDTotalHomes * gTotalArchetypes
  
  if ( NumOfArch < 1 && NumOfArch > 0 ) {
    NumOfArch = 1
  }
 
  NumOfArchPerProvFormDHW[KeyVal] <- round(NumOfArch) 
 
  CountProvFormDHW[KeyVal] <- 0 
  
  debug_out(c(" ProvFormDHW:>", KeyVal, "< ##: ", NumOfArchPerProvFormDHW[KeyVal]," of ", sum(CEUDProvFormDHWEquipYr$NumHomes), "\n"))
    
}  

CEUDProvAirConYr$Key = paste ( CEUDProvAirConYr$Province, CEUDProvAirConYr$Equipment, sep="|" ) 

#--for each Province|Form|AC in the CEUD Database, calculate the size of each archetype bucket; NumOfArch
for (KeyVal in unique(CEUDProvAirConYr$Key)){
      
  NumbInCanada <- sum(CEUDProvAirConYr$NumHomes[CEUDProvAirConYr$Key == KeyVal])
  NumOfArch   <- NumbInCanada / CEUDTotalHomes * gTotalArchetypes
  
  if ( NumOfArch < 1 && NumOfArch > 0 ) {
    NumOfArch = 1
  }

  NumOfArchPerProvAC[KeyVal] <- round(NumOfArch)
    
  CountProvAC[KeyVal] <- 0 
  
  debug_out(c(" ProvAC:>", KeyVal, "< ##: ", NumOfArchPerProvAC[KeyVal]," of ", sum(CEUDProvAirConYr$NumHomes), "\n"))
    
}  

 



 



rowcount <- 0
batchcount <- 0
FoundCount <- 0

#--for each HOUSEID in my ERS data , add a TRUE or FALSE if that ID will be added to the Archetype
for (ID in unique( myERSdata$HOUSE_ID.D[ ! myERSdata$CEUDerror ] )){
  
  rowcount <- rowcount + 1
  batchcount <- batchcount + 1 
  
 
  # Get topology codes fro this record 
  ProvFormVintage  <- myERSdata$CEUDTopProvFormVintage[myERSdata$HOUSE_ID.D == ID ]
  ProvFormSHEquip  <- myERSdata$CEUDTopProvFormSH[myERSdata$HOUSE_ID.D == ID ]
  ProvFormDHWEquip <- myERSdata$CEUDTopProvFormDHW[myERSdata$HOUSE_ID.D == ID ]
  ProvAC           <- myERSdata$CEUDTopProvAC[myERSdata$HOUSE_ID.D == ID ]
  CEUDError        <- myERSdata$CEUDError[myERSdata$HOUSE_ID.D == ID]
  
  
  #stream_out (c(" --->", ProvFormVintage,"&&",ProvFormSHEquip,"&&",ProvFormDHWEquip,"&&",ProvAC, "<\n"))
    
  # Check to see if there is room in the 
  
  if ( CountProvFormVintage[ProvFormVintage]  < NumOfArchPerProvFormVintage[ProvFormVintage] &&  
       CountProvFormSH[ProvFormSHEquip]       < NumOfArchPerProvFormSH[ProvFormSHEquip]  && 
       CountProvFormDHW[ProvFormDHWEquip]     < NumOfArchPerProvFormDHW[ProvFormDHWEquip] && 
       CountProvAC[ProvAC]                    < NumOfArchPerProvAC[ProvAC]                 ){
       
       
     # There is room: Mark this record for inclusion 
	 myERSdata$ArchInclude[myERSdata$HOUSE_ID.D == ID] <- TRUE 
      
          
     # Increment counters 
	 CountProvFormVintage[ProvFormVintage] <- CountProvFormVintage[ProvFormVintage] + 1
     CountProvFormSH[ProvFormSHEquip]      <- CountProvFormSH[ProvFormSHEquip] + 1 
     CountProvFormDHW[ProvFormDHWEquip]    <- CountProvFormDHW[ProvFormDHWEquip] + 1 
     CountProvAC[ProvAC]                   <- CountProvAC[ProvAC] + 1
     
	 FoundCount <- FoundCount + 1
 	 
   }
   
   if ( batchcount >= 5000 ) {
   
     stream_out (c("  - scanned ",rowcount, " ERS records, selected ", FoundCount," archetypes.\n"))
     batchcount <- 0
    
   }
  
}

stream_out (c("  - scanned ",rowcount, " ERS records in total, selected ", FoundCount," archetypes.\n"))
     

myERSdata$CEUDWeight <-NULL     


WeightsDone <- FALSE
WeightLoopCount <- 0 
LastLoop <- FALSE 
# Default all weights to zero. 
myERSdata$CEUDInitialWeights <- 0 


# For homes in the archetype set, set initial weights to estimate (based on target archetypes) 
 

# Copy to final weights 

myERSdata$CEUDCountCol <- 0 
myERSdata$CEUDCountCol[myERSdata$ArchInclude] <- 1 

NetOver <- 0 
NetUnder <- 0 

relaxFactor <- 0.9

adjMax <- 0.2

Loops = 20

gNumHomesEachArchRepresents = CEUDTotalHomes / FoundCount 
myERSdata$CEUDInitialWeights[ ! myERSdata$CEUDerror & myERSdata$ArchInclude ]<- gNumHomesEachArchRepresents
myERSdata$CEUDFinalWeights  <- myERSdata$CEUDInitialWeights

stream_out (c(" - Computing weights (starting at ", round(gNumHomesEachArchRepresents)," homes per archetype) \n"))


while( ! WeightsDone ) {

  myERSdata$CEUDInitialWeights<- myERSdata$CEUDFinalWeights
  # Start with least important: AC 
  for ( KeyVal in unique(CEUDProvAirConYr$Key) ) {
  
    CEUDHomes = sum(CEUDProvAirConYr$NumHomes[  CEUDProvAirConYr$Key == KeyVal ])
    CEUDArchetypes = NumOfArchPerProvAC[KeyVal]
    
    
    ERSHomes <- sum( myERSdata$CEUDFinalWeights[ myERSdata$CEUDTopProvAC == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude] )  
    
    ERSArchetypes <- sum( myERSdata$CEUDCountCol[ myERSdata$CEUDTopProvAC == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude ] ) 
      
    if ( !is.null(ERSArchetypes) ){ ERSArchetypes <- 0 }
      
    if ( LastLoop ) {
      # Don't recompute weights, just evaluate over/under representation 
      
      if ( ERSHomes > CEUDHomes ) { NetOver   = NetOver + ERSHomes - CEUDHomes }
      if ( ERSHomes < CEUDHomes  ) { NetUnder = NetUnder + CEUDHomes - ERSHomes }
   
   
    }else if ( ERSHomes > 0 && ! LastLoop )   {         
      adjWeight = ( CEUDHomes / ERSHomes - 1 ) * relaxFactor + 1
      if (adjWeight > 1 + adjMax ) { adjWeight = 1 + adjMax }
      if (adjWeight < 1 - adjMax ) { adjWeight = 1 - adjMax }
      
      myERSdata$CEUDFinalWeights[ myERSdata$CEUDTopProvAC == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude ] <-
         myERSdata$CEUDFinalWeights[ myERSdata$CEUDTopProvAC == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude ] * adjWeight    
        
      debug_out (c(" (Loop ", WeightLoopCount,") AC:", KeyVal, " : CEUD = ", CEUDHomes, " , ERS = ", ERSHomes, 
                   " [count: ", ERSArchetypes,  " / ", CEUDArchetypes," w=* ", adjWeight," ]\n"))
    
        
    }else {
      adjWeight = 0 
      
    
      debug_out (c(" (Loop ", WeightLoopCount,") AC:", KeyVal, " : CEUD = ", CEUDHomes, " , ERS = NONE FOUND! ]", ERSHomes, 
                     " [count: --- / ", CEUDArchetypes," w = nil ]\n"))
    }  
  }
  
  #stream_out (c(" (Loop ", WeightLoopCount,") ON|AC-central: CEUD = ", sum(CEUDProvAirConYr$NumHomes[  CEUDProvAirConYr$Key == "ON|AC-Central" ]),
  #                                                         " ERSi= ", sum(myERSdata$CEUDInitialWeights[ myERSdata$CEUDTopProvAC == "ON|AC-Central" & ! myERSdata$CEUDerror & myERSdata$ArchInclude ]),
  #                                                         " ERSf= ", sum(myERSdata$CEUDFinalWeights[ myERSdata$CEUDTopProvAC == "ON|AC-Central" & ! myERSdata$CEUDerror & myERSdata$ArchInclude ]),
  #                                                         "\n"))
                                                          


  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Now try 3rd most important thing: DHW
  
  for ( KeyVal in unique(CEUDProvFormDHWEquipYr$Key) ) {
  
    CEUDHomes = sum(CEUDProvFormDHWEquipYr$NumHomes[  CEUDProvFormDHWEquipYr$Key == KeyVal ])
    CEUDArchetypes = NumOfArchPerProvFormDHW[KeyVal]
    
    
    
    
    ERSHomes <- sum( myERSdata$CEUDFinalWeights[ myERSdata$CEUDTopProvFormDHW == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude] )  
    
    ERSArchetypes <- sum( myERSdata$CEUDCountCol[ myERSdata$CEUDTopProvFormDHW == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude ] ) 
          
    
          
    if ( LastLoop ) {
      # Don't recompute weights, just evaluate over/under representation 
      
      if ( ERSHomes > CEUDHomes ) { NetOver = NetOver +  ERSHomes - CEUDHomes }
      if ( ERSHomes < CEUDHomes  ) { NetUnder = NetUnder + CEUDHomes - ERSHomes }
   
   
    }else if ( ERSHomes > 0 )   {         
      adjWeight = ( CEUDHomes / ERSHomes - 1 ) * relaxFactor + 1 
      if (adjWeight > 1 + adjMax ) { adjWeight = 1 + adjMax }
      if (adjWeight < 1 - adjMax ) { adjWeight = 1 - adjMax }
      
      preweight = ERSHomes / ERSArchetypes #<- ave(myERSdata$CEUDFinalWeights[ myERSdata$CEUDTopProvFormDHW == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude ])      
      
      
      myERSdata$CEUDFinalWeights[ myERSdata$CEUDTopProvFormDHW == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude ] <-
         myERSdata$CEUDFinalWeights[ myERSdata$CEUDTopProvFormDHW == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude ] * adjWeight    
        
        
      postweight <- sum(myERSdata$CEUDFinalWeights[ myERSdata$CEUDTopProvFormDHW == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude ])  / ERSArchetypes
      
      
      
      
      if ( KeyVal == "ON|SD|WH-Elec"  ) {
      
      debug_out (c(" (Loop ", WeightLoopCount,") DHW:", KeyVal, " : CEUD = ", CEUDHomes, " , ERS = ", ERSHomes, 
                   " [count: ", ERSArchetypes,  "/", CEUDArchetypes,"w=*", adjWeight,":",round(preweight), " -> ",round(postweight), "]\n"))
 
    }else {
      adjWeight = 0 
      
    
      debug_out (c(" (Loop ", WeightLoopCount,") DHW:", KeyVal, " : CEUD = ", CEUDHomes, " , ERS = NONE FOUND! ]", ERSHomes, 
                     " [count: --- /", CEUDArchetypes,"w = nil ]\n"))
    }    
  }

  #stream_out (c("         ON|SD|WH-Gas: CEUD = ", sum(CEUDProvAirConYr$NumHomes[  CEUDProvFormDHWEquip$Key == "ON|SD|WH-Gas" ]),
  #                                                        " ERSi= ", sum(myERSdata$CEUDInitialWeights[ myERSdata$CEUDTopProvAC == "ON|AC-Central" &  myERSdata$CEUDTopProvFormDHW == "ON|SD|WH-Gas" & myERSdata$CEUDTopProvFormVintage=="ON|SD|1978-1983" & myERSdata$CEUDTopProvFormSH=="ON|SD|Gas-High" & ! myERSdata$CEUDerror & myERSdata$ArchInclude ]),
  #                                                        " ERSf= ", sum(myERSdata$CEUDFinalWeights[ myERSdata$CEUDTopProvAC == "ON|AC-Central" &  myERSdata$CEUDTopProvFormDHW == "ON|SD|WH-Gas" &   myERSdata$CEUDTopProvFormVintage=="ON|SD|1978-1983" & myERSdata$CEUDTopProvFormSH=="ON|SD|Gas-High" & ! myERSdata$CEUDerror & myERSdata$ArchInclude ]),
  #                                                        "\n"))  
  
  
  
  # Second most important thing: Vintage 
  for ( KeyVal in unique(CEUDProvFormVintageYr$Key) ) {
  
    CEUDHomes = sum(CEUDProvFormVintageYr$NumHomes[  CEUDProvFormVintageYr$Key == KeyVal ])
    CEUDArchetypes = NumOfArchPerProvFormVintage[KeyVal]
    
    
    
    
    ERSHomes      <- sum( myERSdata$CEUDFinalWeights[ myERSdata$CEUDTopProvFormVintage == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude] )  
    
    ERSArchetypes <- sum( myERSdata$CEUDCountCol[ myERSdata$CEUDTopProvFormVintage == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude ] ) 
          
    
          
    if ( LastLoop ) {
      # Don't recompute weights, just evaluate over/under representation 
      
      if ( ERSHomes > CEUDHomes ) { NetOver = NetOver +  ERSHomes - CEUDHomes }
      if ( ERSHomes < CEUDHomes  ) { NetUnder = NetUnder + CEUDHomes - ERSHomes }
   
   
    }else if ( ERSHomes > 0 )   {         
      adjWeight = ( CEUDHomes / ERSHomes - 1 ) * relaxFactor + 1 
      
      if (adjWeight > 1 + adjMax ) { adjWeight = 1 + adjMax }
      if (adjWeight < 1 - adjMax ) { adjWeight = 1 - adjMax }      
      
      
      preweight <- ERSHomes / ERSArchetypes
      
      
      myERSdata$CEUDFinalWeights[ myERSdata$CEUDTopProvFormVintage == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude ] <-
         myERSdata$CEUDFinalWeights[ myERSdata$CEUDTopProvFormVintage == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude ] * adjWeight    
        
        
      postweight <- sum(myERSdata$CEUDFinalWeights[ myERSdata$CEUDTopProvFormVintage == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude ]) / ERSArchetypes
      
      
      debug_out (c(" (Loop ", WeightLoopCount,") Vin:", KeyVal, " : CEUD = ", CEUDHomes, " , ERS = ", ERSHomes, 
                   " [count: ", ERSArchetypes,  "/", CEUDArchetypes,"w=*", adjWeight," ]\n"))
    
        
    }else {
      adjWeight = 0 
      
    
      debug_out (c(" (Loop ", WeightLoopCount,") DHW:", KeyVal, " : CEUD = ", CEUDHomes, " , ERS = NONE FOUND! ]", ERSHomes, 
                     " [count: --- /", CEUDArchetypes,"w = nil ]\n"))
    }  
  } 
  
   # Most important thing gets final say: Equipment (&therefore, fuel type)
  for ( KeyVal in unique(CEUDProvFormSHEquipYr$Key) ) {
  
    CEUDHomes = sum(CEUDProvFormSHEquipYr$NumHomes[  CEUDProvFormSHEquipYr$Key == KeyVal ])
    CEUDArchetypes = NumOfArchPerProvFormSH[KeyVal]
       
    ERSHomes <- sum( myERSdata$CEUDFinalWeights[ myERSdata$CEUDTopProvFormSH == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude] )  
    
    ERSArchetypes <- sum( myERSdata$CEUDCountCol[ myERSdata$CEUDTopProvFormSH == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude ] ) 
          
    
          
    if ( LastLoop ) {
      # Don't recompute weights, just evaluate over/under representation 
      
      if ( ERSHomes > CEUDHomes ) { NetOver = NetOver +  ERSHomes - CEUDHomes }
      if ( ERSHomes < CEUDHomes  ) { NetUnder = NetUnder + CEUDHomes - ERSHomes }
   
   
    }else if ( ERSHomes > 0 )   {         
      adjWeight = ( CEUDHomes / ERSHomes - 1 ) * relaxFactor + 1 
      
      if (adjWeight > 1 + adjMax ) { adjWeight = 1 + adjMax }
      if (adjWeight < 1 - adjMax ) { adjWeight = 1 - adjMax }      
      
      
      preweight <- ERSHomes / ERSArchetypes
      
      
      myERSdata$CEUDFinalWeights[ myERSdata$CEUDTopProvFormSH == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude ] <-
         myERSdata$CEUDFinalWeights[ myERSdata$CEUDTopProvFormSH == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude ] * adjWeight    
        
        
      postweight <- sum(myERSdata$CEUDFinalWeights[ myERSdata$CEUDTopProvFormSH == KeyVal & ! myERSdata$CEUDerror & myERSdata$ArchInclude ]) / ERSArchetypes
      
     
      
      debug_out (c(" (Loop ", WeightLoopCount,") SH:", KeyVal, " : CEUD = ", CEUDHomes, " , ERS = ", ERSHomes, 
                   " [count: ", ERSArchetypes,  "/", CEUDArchetypes,"w=*", adjWeight," ]\n"))
    
        
    }else {
      adjWeight = 0 
      
    
      debug_out (c(" (Loop ", WeightLoopCount,") DHW:", KeyVal, " : CEUD = ", CEUDHomes, " , ERS = NONE FOUND! ]", ERSHomes, 
                     " [count: --- /", CEUDArchetypes,"w = nil ]\n"))
    }  
  } 
  
  
  
  WeightLoopCount <- WeightLoopCount + 1
   
  if ( WeightLoopCount > Loops ){
  
    WeightsDone = TRUE 
    
  }else if (WeightLoopCount > Loops -1  ) {
  
    LastLoop = TRUE
    
  }
  
   

}


stream_out ( c("Weights set: Net Over: ", NetOver/1E06, "m , Net under: ", NetUnder/1E06, "m\n"))

#for (ID in unique( myERSdata$HOUSE_ID.D[ ! myERSdata$CEUDerror && myERSdata$ArchInclude ] )){     
#
#  ProvFormVintage  <- myERSdata$CEUDTopProvFormVintage[myERSdata$HOUSE_ID.D == ID ]
#  ProvFormSHEquip  <- myERSdata$CEUDTopProvFormSH[myERSdata$HOUSE_ID.D == ID ]
#  ProvFormDHWEquip <- myERSdata$CEUDTopProvFormDHW[myERSdata$HOUSE_ID.D == ID ]
#  ProvAC           <- myERSdata$CEUDTopProvAC[myERSdata$HOUSE_ID.D == ID ]
#} 
          

mySubData <- myERSdata[myERSdata$ArchInclude,]


# Here's what we actually found 
arch_run_total <- 0 
for (ArchProvince in unique(CEUDProvFormYr$Province)){
  
  
  stream_out(c(" Found for Prov:", ArchProvince, " #: ", nrow(mySubData[mySubData$CEUDProvince==ArchProvince,])," / ", NumOfArchPerProv[ArchProvince], " \n"))
  
  arch_run_total <- arch_run_total + nrow(mySubData[mySubData$CEUDProvince==ArchProvince,])
    
}  

stream_out(c("Total archetypes:", arch_run_total,"\n"))


stream_out(c("Net Homes represented by archetypes:", round(sum(myERSdata$CEUDFinalWeights[myERSdata$ArchInclude] )),"\n"))


stream_out (" - writing out ERS dbs (myERSdata_out.txt)...")
write.csv(myERSdata, file = "myERSdata_out.txt")	  
stream_out (" done.\n")


write(c("Results:",relaxFactor, adjMax, gnERSrows, arch_run_total, NetOver/1.0E06, NetUnder/1.0E06 ), file = "res-config.txt",
      ncolumns = 7,
      append = TRUE, sep = " ")




#write.csv(myERSdata[myERSdata$ArchInclude], file="myERSdata_ArchInclude.txt")

#ArchProvince <- (myERSdata$CEUDProvince[myERSdata$HOUSE_ID.D == HOUSE_ID.D]) 

#}

#write.table(gCount[ArchProvince], file = "CEUDCounter.txt")

               
                   
HouseIDsFOrModel = c()


#CEUD_for_count <- CEUD[CEUD$year] 


close(DebugConn)
stream_out ("\n\n")