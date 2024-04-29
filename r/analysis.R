#### Julia Haywood - Biodiversify tool - 02-08-2022

library(sf)
library(dplyr)
library(tidyr)
library(data.table)
library(geojsonsf)
library(jsonlite)

##################
# For production version -> set to FALSE! (DO NOT PUSH "TRUE" to the repo!)
# Attention: It also deactivates writing to the database
isLocalDev = TRUE # Set to TRUE if you are debuggging locally, 
doWriteCSV = TRUE
doWriteToDB = TRUE # set TRUE by default! By debugging: set to FALSE
##################

if(isLocalDev == TRUE){
  ####### LOCAL DEVELOPMENT ########
  print("######### DEBUGGING ##########")
  # Set working directory
  library(rstudioapi)
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  wd_path <- getwd()
  getwd()
  # Arguments - local version
  batch_id <- "b8b6deee-57e1-4855-8ef0-53893f64a5a5"
  ###### LOCAL DEVELOPMENT END #######
} else {
  ####### FME DEVELOPMENT ########
  ## Get arguments from command line
  #1: working dir
  #2: batch_id
  args <- commandArgs(trailingOnly=TRUE)
  if (length(args)==0) {
    stop("At least one argument must be supplied", call.=FALSE)
  }
  ## Set working directory
  setwd(args[1])
  wd_path <- args[1]
  batch_id <- args[2]
  ####### FME DEVELOPMENT ########
}

# If writing is needed
if(doWriteCSV == TRUE){
  outputpath.designated_sites <- 'Designated sites.csv'
  outputpath.baseline_habitats_linear <- 'Baseline linear habitats.csv'
  outputpath.baseline_habitats_area <- 'Baseline area habitats.csv'
  outputpath.biodiv_units_linear <- 'Biodiversity Units - Linear.csv'
  outputpath.biodiv_units_area <- 'Biodiversity Units - Area.csv'
  outputpath.non_biodiv_habitats <- 'Non-Biodiversity Metric habitats.csv'
  outputpath.trading_notes <- 'Trading notes.csv'
  outputpath.summary_table <- 'Summary table.csv'
}
# Get DB connection script
source("postgres_connection.R")

# Helpers
extendDfWithAllSites <- function(controlDF, dfToExtend, by.controlCol, by.dfToExtendCol){
  # controlDf: df, eg. Options
  # dfToExtend: df, incomplete df
  # by: columns (control and ) that we use as base if comparsion
  missing <- setdiff(controlDF[[by.controlCol]], dfToExtend[[by.dfToExtendCol]])
  dfToExtend[nrow(dfToExtend) + seq_along(missing), by.dfToExtendCol] <- missing
  dfToExtend[is.na(dfToExtend)] <- "-"
  return(dfToExtend)
}

checkIntertidal <- function (Options, Tide){
  # Check if the sites are intertidal
  # Returns a dataframe where each site is gets a boolean value
  df1 <- setNames(data.frame(matrix(ncol = 2, nrow = length(Options$id))), c("Id", "isIntertidal"))
  df1$Id <- Options$id
  df1$`Site Label` <- Options$aoi_label
  df1$isIntertidal <- FALSE
  # if Intertidal not empty, return true for specific sites
  if (dim(Tide)[1] > 0) {
    df1$isIntertidal[df1$Id == Tide$id] <- TRUE
  }
  return(df1)
}

getOverlapWithIndividualDesignatedSites <- function(Options, Des) {
  # Identifies overlapping designated sites individually
  # Returns a df
  if (dim(Des)[1] == 0) {
    df1 <- Options[, c("id")]                          # if no intersection with designated sites, create an empty df
    st_geometry(df1) <- NULL 
    sites <- c("Local Nature Reserves","National Nature Reserve","RAMSAR",
               "Site of special scientific interest","Special Areas of Conservation",
               "Special Protection Areas","RSPB Reserves","RSPB Important Bird Areas",
               "Area of Outstanding Natural Beauty","National Parks","Biosphere reserves")
    missing <- data.frame(sites)
    df1 <- crossing(df1$id, missing$sites)                                  # all combinations
    df1 <- base::merge(Options[,1:2], df1, by.x = c('id'),by.y = c('df1$id'),             
                       all.x = TRUE, all.y = TRUE)                                        # Add id
    names(df1)[names(df1) == "missing$sites"] <- "sites"   # Rename
    df1 <- subset(df1, select=c(id,sites,aoi_label))                                            # Simplify table
    names(df1)[names(df1) == "sites"] <- "Designated Sites"   # Rename
    names(df1)[names(df1) == "aoi_label"] <- "Site Label"   # Rename
    names(df1)[names(df1) == "id"] <- "Id"   # Rename
    st_geometry(df1) <- NULL                                                        # Remove geometries
    df1$'Site overlap with designated sites (%)' <- 0
    col_order <- c("Id","Site Label","Site overlap with designated sites (%)","Designated Sites")            # Reorder df
    df1 <- df1[, col_order]                                                         # Reorder df
    return(df1)
  } else {
    df1 <- st_intersection(Options, Des)                                            # Extract where they intersect
    df1$Overlap_area <- as.numeric(st_area(df1$geometry)/10000)                   # Calculate area in hectare
    st_geometry(df1) <- NULL                                                        # Remove geometries
    df1 <- df1 %>%
      group_by(id,aoi_label, O_area, sites) %>%
      summarise(Overlap_area = sum(Overlap_area))                                   # Group overlap by Option and Des
    df1$Percentage <- (df1$Overlap_area / df1$O_area)*100                           # Calculate proportion overlap
    df1 <- subset(df1, select=c(aoi_label,sites,Percentage))                              # Simplify table
    # Add non-overlapping designated sites
    sites <- c("Local Nature Reserves","National Nature Reserve","RAMSAR",
               "Site of special scientific interest","Special Areas of Conservation",
               "Special Protection Areas","RSPB Reserves","RSPB Important Bird Areas",
               "Area of Outstanding Natural Beauty","National Parks","Biosphere reserves")
    missing <- data.frame(sites)
    df2 <- crossing(Options$aoi_label, missing$sites)                             # all combinations
    names(df2)[names(df2) == "Options$aoi_label"] <- "aoi_label"   # Rename
    names(df2)[names(df2) == "missing$sites"] <- "sites"   # Rename
    df3 <- base::merge(df2,df1, by.x = c('aoi_label','sites'),by.y = c('aoi_label','sites'),             
                       all.x = TRUE, all.y = TRUE)                                        # Ensure score for all Options ans des sites
    df3 <- base::merge(Options[,1:2], df3, by.x = c('aoi_label'),by.y = c('aoi_label'),             
                       all.x = TRUE, all.y = TRUE)                                        # Add id
    st_geometry(df3) <- NULL                                                        # Remove geometries
    # Create output
    df3[c("Percentage")][is.na(df3[c("Percentage")])] <- 0
    df3[,'Percentage']=round(df3[,'Percentage'],1)                                  # Round up dp
    df3 <- df3[order(as.numeric(df3$aoi_label)),]                    # Order Option by numeric
    col_order <- c("id", "aoi_label", "Percentage","sites")            # Reorder df
    df3 <- df3[, col_order]                                                         # Reorder df
    names(df3)[names(df3) == "Percentage"] <- "Site overlap with designated sites (%)"   # Rename
    names(df3)[names(df3) == "aoi_label"] <- "Site Label"   # Rename
    names(df3)[names(df3) == "id"] <- "Id"   # Rename
    names(df3)[names(df3) == "sites"] <- "Designated Sites"   # Rename
    return(df3)
  }
}

analyseNonBiodivMetrichabitats <- function(Options, Peat, AW, Non_BM) {
  ## Possible optimisations: combine peat and AW into one file. Pre-transform into correct projection
  # Calculate area of Option overlapping Non_BM habitats
  # Returns a df
  col_order <- c("Id", "Habitat", "Area of habitat in site (ha)","Site Label")            # Reorder df
  df1 <- st_intersection(Options, Peat)                                           # Intersect
  df2 <- st_intersection(Options, AW)                                             # Intersect
  if (dim(df1)[1] != 0 | dim(df2)[1] != 0) {
    df3 <- rbind(df1, df2)                                                          # Combine
    df3$HAB_amount <- as.numeric(st_area(df3$geometry)/10000)                       # Calculate area of habitat in hectare
    st_geometry(df3) <- NULL                                                        # Remove geometries
    df4 <- df3 %>%
      group_by(id, cf_id) %>%
      summarise(HAB_amount = sum(HAB_amount))                                       # Group amount by Option and cf_id
    # Add non-overlapping non-bm habitats
    cf_id <- c("DPS", "SPP", "SPS", "AW")
    missing <- data.frame(cf_id)
    df5 <- crossing(Options$id, missing$cf_id)                                  # all combinations
    names(df5)[names(df5) == "Options$id"] <- "id"   # Rename
    names(df5)[names(df5) == "missing$cf_id"] <- "cf_id"   # Rename
    df5 <- base::merge(df5, df4, by.x = c('id','cf_id'),by.y = c('id','cf_id'),             
                       all.x = TRUE, all.y = TRUE)                                        # Ensure score for all Options
    df5[c("HAB_amount")][is.na(df5[c("HAB_amount")])] <- 0
    # Create output
    df5 <- base::merge(Options[,1:2], df5, by.x = c('id'),by.y = c('id'),             
                       all.x = TRUE, all.y = TRUE)                                        # Add id
    st_geometry(df5) <- NULL                                                        # Remove geometries
    df5[,'HAB_amount']=round(df5[,'HAB_amount'],2)     
    df6 <- base::merge(df5, Non_BM, by.x = c('cf_id'), by.y = c('cf_id'),             
                       all.x = TRUE, all.y = FALSE)                                       # Assign full name
    names(df6)[names(df6) == "habitat"] <- "Habitat"   # Rename# Round up dp
    df6 <- subset(df6, select=c(id,Habitat,HAB_amount,aoi_label))                                            # Simplify table
    names(df6)[names(df6) == "id"] <- "Id"   # Rename
    names(df6)[names(df6) == "HAB_amount"] <- "Area of habitat in site (ha)"
    names(df6)[names(df6) == "aoi_label"] <- "Site Label"   # Rename
    df6 <- df6[, col_order]                                                         # Reorder df
    df6 <- df6[order(as.numeric(df6$Id)),]                    # Order Option by numeric
    return(df6)
  } else {
    df3 <- Options[, c("id")]                          # if no intersection with designated sites, create an empty df
    st_geometry(df3) <- NULL 
    cf_id <- c("DPS", "SPP", "SPS", "AW")
    missing <- data.frame(cf_id)
    df3 <- crossing(df3$id, missing$cf_id)                                  # all combinations
    names(df3)[names(df3) == "missing$cf_id"] <- "cf_id"   # Rename
    df3 <- base::merge(Options[,1:2], df3, by.x = c('id'),by.y = c('df3$id'),             
                       all.x = TRUE, all.y = TRUE)                                        # Add id
    df3 <- base::merge(df3, Non_BM, by.x = c('cf_id'), by.y = c('cf_id'),             
                       all.x = TRUE, all.y = FALSE)                                       # Assign full name
    df3 <- subset(df3, select=c(id,habitat,aoi_label))                                            # Simplify table
    names(df3)[names(df3) == "aoi_label"] <- "Site Label"   # Rename
    names(df3)[names(df3) == "id"] <- "Id"   # Rename
    names(df3)[names(df3) == "habitat"] <- "Habitat"   # Rename
    st_geometry(df3) <- NULL                                                        # Remove geometries
    df3$'Area of habitat in site (ha)' <- 0
    col_order <- c("Id", "Habitat", "Area of habitat in site (ha)","Site Label")            # Reorder df
    df3 <- df3[, col_order]
    return(df3)
  }
}

analyseBiodivMetricshabitatsLinearFast <- function(Options, habitatsLinear, BM_Values, BM_Habs) {
  # Biodiversity metric habitats - Biodiversity Units - Linear habitats
  # Returns a df
  if (dim(habitatsLinear)[1] == 0) {
    cf_id <- c("HEDGE", "RIV", "PRIV", "CAN")
    df1 <- data.frame(cf_id)
    df1 <- crossing(Options$id, df1$cf_id)                             # all combinations
    names(df1)[names(df1) == "Options$id"] <- "Id"   # Rename
    names(df1)[names(df1) == "df1$cf_id"] <- "cf_id"   # Rename
    df1$Length_km <- 0                      
    df1$distinctiveness <- "-"                      
    df1$condition <- "-"                      
    df1$trading_notes <- "-"                      
    df1$checks <- "-"                      
    df1$uk_bap <- "-"                      
    df1$BU_Linear <- "0"     
    df1 <- base::merge(df1, BM_Habs, by.x = c('cf_id'), by.y = c('cf_id'),             
                       all.x = TRUE, all.y = FALSE)                                       # Assign full name
    df1 <- base::merge(df1,Options[,1:2], by.x = c('Id'),by.y = c('id'),             
                       all.x = TRUE, all.y = TRUE)                                        # Add id
    names(df1)[names(df1) == "habitat"] <- "Habitat"
    df1 <- subset(df1, select=c(Id,Habitat,Length_km,distinctiveness,condition,BU_Linear,trading_notes,checks,uk_bap,aoi_label))                            # Simplify table
    col_order <- c("Id", "Habitat", "Length_km","distinctiveness","condition","BU_Linear","trading_notes","checks","uk_bap","aoi_label")                # Reorder df
    df1 <- df1[, col_order]                                                         # Reorder df
    df1 <- df1[order(as.numeric(df1$Id)),]                    # Order Option by numeric
    return(df1)
  } else {
    df1 <- st_intersection(habitatsLinear, Options)                                      # Extract where they intersect
    df1$Length_km <- as.numeric(st_length(df1)/1000)               # Calculate length in km
    st_geometry(df1) <- NULL                                                  # Remove geometries
    df1 <- df1 %>% group_by(id, cf_id) %>%
      summarise(Length_km = sum(Length_km))                              # Group length by Option and habitat (cf_id) 
    # Assign Biodiversity Metric attributes
    df2 <- base::merge(df1, BM_Values, by.x = c('cf_id'),
           by.y = c('cf_id'),             
           all.x = TRUE, all.y = FALSE)                                       # Assign additional Biodiversity Metric information
    # Calculate Biodiversity units
    df2$D_Score <- as.numeric(ifelse(endsWith(df2$distinctiveness, "V.High"),8,
           ifelse(endsWith(df2$distinctiveness, "High"), 6,
           ifelse(endsWith(df2$distinctiveness, "Medium"), 4,
           ifelse(endsWith(df2$distinctiveness, "Low"), 2,
           ifelse(endsWith(df2$distinctiveness, "V.Low"), 0,
           "NA"))))))                                                       # Assing Scores
    df2$C_Score <- as.numeric(ifelse(endsWith(df2$condition, "Moderate"),2,
                                     ifelse(endsWith(df2$condition, "Poor"), 1,"NA")))                # Assign condition, good=3, moderate=2 
    df2$BU_Linear <- df2$Length_km * df2$D_Score * df2$C_Score                      # Caluclate Biodiversity Units
    df2 <- subset(df2, select=-c(D_Score,C_Score))                            # Simplify table
    df2[,'BU_Linear'] <- round(df2[,'BU_Linear'],2)                                    # Round up dp
    # Add non-overlapping linear habitats
    cf_id <- c("HEDGE", "RIV", "PRIV", "CAN")
    missing <- data.frame(cf_id)
    missing <- crossing(Options$id, missing$cf_id)                             # all combinations
    names(missing)[names(missing) == "Options$id"] <- "Id"   # Rename
    names(missing)[names(missing) == "missing$cf_id"] <- "cf_id"   # Rename
    df2 <- base::merge(missing, df2, by.x = c('Id','cf_id'), by.y = c('id','cf_id'),             
                       all.x = TRUE, all.y = TRUE)                                        # Ensure score for all Options
    df2[c("BU_Linear","Length_km")][is.na(df2[c("BU_Linear", "Length_km")])] <- 0
    df2[c("distinctiveness","condition","trading_notes","checks","uk_bap")][is.na(df2[c("distinctiveness","condition","trading_notes","checks","uk_bap")])] <- "-"
    # Create output
    df2 <- base::merge(df2, BM_Habs, by.x = c('cf_id'), by.y = c('cf_id'),             
                       all.x = TRUE, all.y = FALSE)                                       # Assign full name
    df2 <- base::merge(df2,Options[,1:2], by.x = c('Id'),by.y = c('id'),             
                       all.x = TRUE, all.y = TRUE)                                        # Add id
    names(df2)[names(df2) == "habitat"] <- "Habitat"
    df2 <- subset(df2, select=c(Id,Habitat,Length_km,distinctiveness,condition,BU_Linear,trading_notes,checks,uk_bap,aoi_label))                            # Simplify table
    col_order <- c("Id", "Habitat", "Length_km","distinctiveness","condition","BU_Linear","trading_notes","checks","uk_bap","aoi_label")                # Reorder df
    df2 <- df2[, col_order]                                                         # Reorder df
    df2 <- df2[order(as.numeric(df2$Id)),]                    # Order Option by numeric
    df2[,'Length_km']=round(df2[,'Length_km'],3)                                    # Round up dp
    return(df2)
  }
}

calculateBiodivLinearUnits <- function(df2, Options) {
  # Calculate Biodiversity Units
  # Returns a df
  df4 <- subset(df2, select = c(Id,BU_Linear,aoi_label))                 # Simplify table
  if (any(df4$BU_Linear != 0)) {
    df4 <- df4 %>% group_by(Id, aoi_label) %>% summarise(BU_Linear = sum(BU_Linear))      # Group BU by Option
    df4$Rank_BU_Linear <- dense_rank(df4$BU_Linear)                                 # Rank without skipping numbers
    df4 <- df4[order(as.numeric(df4$Id)),]                    # Order Option by numeric
    names(df4)[names(df4) == "Rank_BU_Linear"] <- "Rank for Linear Biodiversity Units"
    names(df4)[names(df4) == "BU_Linear"] <- "Linear Biodiversity Units"
    names(df4)[names(df4) == "aoi_label"] <- "Site Label"   # Rename
    col_order <- c("Id", "Linear Biodiversity Units", "Rank for Linear Biodiversity Units","Site Label")                # Reorder df
    df4 <- df4[, col_order]                                                         # Reorder df
  } else {
    df4 <- df4[!duplicated(df4), ]                                      # Remove duplicates
    df4$Rank_BU_Linear <- dense_rank(df4$BU_Linear) 
    names(df4)[names(df4) == "Rank_BU_Linear"] <- "Rank for Linear Biodiversity Units"
    names(df4)[names(df4) == "BU_Linear"] <- "Linear Biodiversity Units"
    names(df4)[names(df4) == "aoi_label"] <- "Site Label"   # Rename
    col_order <- c("Id", "Linear Biodiversity Units", "Rank for Linear Biodiversity Units","Site Label")                # Reorder df
    df4 <- df4[, col_order]                                                         # Reorder df
  }
  return(df4)
}

calculateBMhabitatOverlapFast <- function(Options, habitats) {
  # Calculate amount of BM habitats in Options
  # Returns a df
  df1 <- st_intersection(habitats, Options)                                      # Extract where they intersect
  df1$HAB_amount_MAX <- as.numeric(st_area(df1)/10000)           # Calculate area in hectare
  return(df1)
}

analyseBiodivMetricshabitatsPolygons <- function(habitatsPolygon, Options, BM_Values, BM_Habs) {
  # Biodiversity Metrics: Polygon habitats
  # Returns a df
  col_order <- c("id", "habitat","HAB_amount_MAX","distinctiveness","condition","BU","trading_notes","checks", "uk_bap") # Reorder df
  if (dim(habitatsPolygon)[1] == 0) {
    df5 <- setNames(data.frame(matrix(ncol = length(col_order), nrow = length(Options$id))), col_order)
    df5$id <- Options$id
    df5$aoi_label <- Options$aoi_label
    df5[is.na(df5)] <- "-"
  } else {
    df1 <- calculateBMhabitatOverlapFast(habitats = habitatsPolygon, Options)
    df2 <- df1
    st_geometry(df2) <- NULL                                                        # Remove geometries
    df2 <- df2 %>% group_by(id, cf_id) %>%
      summarise(HAB_amount_MAX = sum(HAB_amount_MAX))                               # Group area by Option and habitat (cf_id) 
    
    # Assign Biodiversity Metric attributes
    df5 <- base::merge(df2, BM_Values, by.x = c('cf_id'),
                       by.y = c('cf_id'),             
                       all.x = TRUE, all.y = FALSE)                                     # Assign additional Biodiversity Metric information
    
    # Calculate max BUs for each habitat in each site
    df5$C_Score <- as.numeric(ifelse(endsWith(df5$condition, "Moderate"),2,
                                     ifelse(endsWith(df5$condition, "Poor"), 1,"NA")))                # Assign condition, good=3, moderate=2 
    df5$D_Score <- as.numeric(ifelse(endsWith(df5$distinctiveness, "V.High"),8,
                                     ifelse(endsWith(df5$distinctiveness, "High"), 6,
                                            ifelse(endsWith(df5$distinctiveness, "Medium"), 4,
                                                   ifelse(endsWith(df5$distinctiveness, "Low"), 2,
                                                          ifelse(endsWith(df5$distinctiveness, "V.Low"), 0,
                                                                 "NA"))))))                                                       # Assign Distinctiveness Score and set as numeric
    df5$BU <- df5$HAB_amount_MAX * df5$C_Score * df5$D_Score                        # Calculate max Biodiversity Unit
    df5[,'BU']=round(df5[,'BU'],2)                                                  # Round up dp
    df5 <- subset(df5, select=-c(C_Score, D_Score))                                 # Simplify table
    
    # Calculate max BUs for each habitat in each site
    df5$C_Score <- as.numeric(ifelse(endsWith(df5$condition, "Moderate"),2,
        ifelse(endsWith(df5$condition, "Poor"), 1,"NA")))                # Assign condition, good=3, moderate=2 
    df5$D_Score <- as.numeric(ifelse(endsWith(df5$distinctiveness, "V.High"),8,
        ifelse(endsWith(df5$distinctiveness, "High"), 6,
        ifelse(endsWith(df5$distinctiveness, "Medium"), 4,
        ifelse(endsWith(df5$distinctiveness, "Low"), 2,
        ifelse(endsWith(df5$distinctiveness, "V.Low"), 0,
        "NA"))))))                                                       # Assign Distinctiveness Score and set as numeric
    df5$BU <- df5$HAB_amount_MAX * df5$C_Score * df5$D_Score                        # Calculate max Biodiversity Unit
    df5[,'BU']=round(df5[,'BU'],2)                                                  # Round up dp
    df5 <- subset(df5, select=-c(C_Score, D_Score))                                 # Simplify table
    
    # Create output
    df5 <- base::merge(df5, BM_Habs, by.x = c('cf_id'), by.y = c('cf_id'),             
                       all.x = TRUE, all.y = FALSE)                                       # Assign full name
    df5 <- subset(df5, select = -c(cf_id))           # Simplify table
    df5 <- df5[, col_order]                                                         # Reorder df
    df5 <- df5[order(as.numeric(df5$id)),]                    # Order Option by numeric
    df5[,'HAB_amount_MAX'] = round(df5[,'HAB_amount_MAX'],3)                          # Round up dp
    df5 <- extendDfWithAllSites(controlDF = Options, dfToExtend = df5, by.controlCol = "id", by.dfToExtendCol = "id")
    df5 <- base::merge(df5,Options[,1:2], by.x = c('id'),by.y = c('id'),             
                       all.x = TRUE, all.y = TRUE)                                        # Add id
    df5 <- subset(df5, select = -c(geometry))           
  }
  names(df5)[names(df5) == "HAB_amount_MAX"] <- "Maximum area of habitat in site (ha)"
  names(df5)[names(df5) == "distinctiveness"] <- "Distinctiveness"
  names(df5)[names(df5) == "trading_notes"] <- "Trading notes"
  names(df5)[names(df5) == "habitat"] <- "Habitat"
  names(df5)[names(df5) == "BU"] <- "Maximum Biodiversity Unit"
  names(df5)[names(df5) == "checks"] <- "Checks"
  names(df5)[names(df5) == "uk_bap"] <- "UK BAP"
  names(df5)[names(df5) == "id"] <- "Id"
  names(df5)[names(df5) == "condition"] <- "Condition"
  names(df5)[names(df5) == "aoi_label"] <- "Site Label"   # Rename
  return(df5)
}

getBiodivUnitsPolygons <- function(habitatsPolygon, Options, BM_Values) {
  # Calculate Biodiversity Units
  # Returns a df
  if (dim(habitatsPolygon)[1] == 0) {
    col_order <- c("id", "aoi_label", "BU", "Rank_BU") # Reorder df
    df8 <- setNames(data.frame(matrix(ncol = length(col_order), nrow = length(Options$id))), col_order)
    df8$id <- Options$id
    df8$aoi_label <- Options$aoi_label
    df8[c("BU")][is.na(df8[c("BU")])] <- 0
  } else {
    df1 <- calculateBMhabitatOverlapFast(habitats = habitatsPolygon, Options = Options)
    df7 <- subset(df1, select = -c(HAB_amount_MAX))                                    # Simplify table
    df7 <- df7[order(df7$id),]                                                 # Order
    df8 <- data.frame(matrix(vector(), 0, 2, 
           dimnames = list(c(), c("id","BU"))))                    # Create empty dataframe  
    
    for (i in 1:nrow(Options)) {                                                          
      tryCatch({                                                                    # Continue if iteration breaks                                                 
        print(i)                                                                    # Print iteration
        temp1 <- subset(df7,df7$id == Options$id[i])  # Subset data
        grid <- st_make_grid(temp1, square = T, cellsize = c(100, 100)) %>% 
          st_sf()                                                             # Create 100m x 100m grid
        grid$fid <- 1:nrow(grid)                                                       # Assign grid cell number
        temp2 <- st_intersection(temp1,grid)                                        # Intersect 
        temp2 <- temp2[order(temp2$fid),]                                            # Order
        temp3 <- data.frame(matrix(vector(), 0,2, 
                                   dimnames = list(c(), c("id","BU"))))             # Create empty dataframe  
        for (j in 1:nrow(grid)) {
          tryCatch({                                                                # Continue if iteration breaks                                                 
            print(j)                                                                # Print iteration
            temp4 <- subset(temp2,temp2$fid == grid$fid[j])                     # Subset data
            stopifnot(nrow(temp4) != 0)                                                 # Stop if no intersection
            int_temp <- st_intersection(temp4)                                                    # Intersect
            int_temp$int_area <- as.numeric(st_area(int_temp)/10000)                     # Calculate area in hectare
            int_temp$int_area[int_temp$int_area == 0] <- NA                                         # Replace 0 with NA
            int_temp <- int_temp[complete.cases(int_temp$int_area),]                                # Remove NAs
            st_geometry(int_temp) <- NULL                                                         # Remove geometries
            int_temp2 <- int_temp                                                                 # Rename
            int_temp2$ids <- sapply(int_temp2$origins, function(x) paste0(as.character(temp4$cf_id)[x], collapse = ",")) # Assign habitat ID (cf_id) that it overlaps with
            int_temp2 <- int_temp2 %>% mutate(origins = sapply(origins, toString))                # Convert origin list to character
            int_temp2$int_number <- 1:nrow(int_temp2)                                               # Assign an intersection number
            int_temp2 <- subset(int_temp2, select = -c(cf_id,n.overlaps,origins))                   # Simplify table
            int_temp3 <- separate_rows(int_temp2, ids, convert = TRUE, sep = "\\,")               # Separate rows with commas in
            int_temp4 <- base::merge(int_temp3, BM_Values, by.x = c('ids'),
                                     by.y = c('cf_id'),             
                                     all.x = TRUE, all.y = FALSE)                                             # Assign Biodiversity Metric attributes
            int_temp4$C_Score <- as.numeric(ifelse(endsWith(int_temp4$condition, "Moderate"),2,
                                                   ifelse(endsWith(int_temp4$condition, "Poor"), 1,"NA")))              # Assign condition, good=3, moderate=2 
            int_temp4$D_Score <- as.numeric(ifelse(endsWith(int_temp4$distinctiveness, "V. High"),8,
                                                   ifelse(endsWith(int_temp4$distinctiveness, "High"), 6,
                                                          ifelse(endsWith(int_temp4$distinctiveness, "Medium"), 4,
                                                                 ifelse(endsWith(int_temp4$distinctiveness, "Low"), 2,
                                                                        ifelse(endsWith(int_temp4$distinctiveness, "V. Low"), 0,
                                                                               "NA"))))))                                                               # Assign Distinctiveness Score and set as numeric
            int_temp5 <- subset(int_temp4, select = -c(user_email, batch_id, aoi_label, O_area, habitat,id,fid,ids,distinctiveness,
                                                       condition,trading_notes, checks, uk_bap))                                        # Simplify table
            int_temp5$BU <- int_temp5$int_area * int_temp5$C_Score * int_temp5$D_Score        # Calculate High Biodiversity Unit
            int_temp6 <- subset(int_temp5, select = -c(int_area,C_Score,D_Score))              # Simplify table
            int_temp8 <- int_temp6 %>% group_by(int_number) %>% summarise(BU = max(BU))     # Max value for each iteration
            BU <- sum(int_temp8$BU)                                                       # Sum Max BU                                                  # sum Min BU
            int_temp10 <- data.frame(id  = Options$id[i],BU = BU)                                                      # Create df
            temp3 <- rbind(temp3,int_temp10)                                                      # Bind to premade df
            stop("Worked")                                                              # State if iteration worked
          }
          , error = function(e){cat("ERROR :",conditionMessage(e), "\n")})              # State what iteration doesn't work
        }
        temp5 <- temp3 %>% group_by(id) %>%
          summarise(BU = sum(BU))                   # Group area by Option
        df8 <- rbind(df8,temp5)
        
        stop("Worked")                                                              # State if iteration worked
      }
      , error = function(e){cat("ERROR :",conditionMessage(e), "\n")})                # State what iteration doesn't work
    }
    # Create output
    df8 <- base::merge(Options, df8, by.x = c('id'),by.y = c('id'),             
                       all.x = TRUE, all.y = FALSE)                                       # Ensure score for all Options
    st_geometry(df8) <- NULL                                                        # Remove geometries
    df8[is.na(df8)] <- 0                                                            # Replace NAs with 0 
    df8[,'BU'] = round(df8[,'BU'],2)                                        # Round up dp
  }
  df8 <- extendDfWithAllSites(controlDF = Options, dfToExtend = df8, by.controlCol = "id", by.dfToExtendCol = "id")
  df8$Rank_BU <- dense_rank(df8$BU)                                    # Rank without skipping numbers
  df8 <- df8[order(as.numeric(df8$id)),]                    # Order Option by numeric
  names(df8)[names(df8) == "BU"] <- "Area Biodiversity Units"         # Rename
  names(df8)[names(df8) == "Rank_BU"] <- "Rank for Area Biodiversity Units"
  names(df8)[names(df8) == "id"] <- "Id"
  names(df8)[names(df8) == "aoi_label"] <- "Site Label"
  df8 <- subset(df8, select=c(Id, `Site Label`, `Area Biodiversity Units`,`Rank for Area Biodiversity Units`))                                        # Simplify table
  return(df8)
}

getTradingNotes <- function(Options, biodivMetricsLinear, biodivMetricshabitatsPolygons){
  # Creates trading notes
  # Returns a df
  Trading_L <- subset(biodivMetricsLinear , `Length of habitat in site (km)` > 0)
  Trading_L <- subset(Trading_L, select=c(Id,`Trading notes`,`Site Label`))                     # Simplify table
  Trading_P <- subset(biodivMetricshabitatsPolygons , `Maximum area of habitat in site (ha)` > 0)
  Trading_P <- subset(Trading_P, select=c(Id,`Trading notes`,`Site Label`))                     # Simplify table
  Trading <- rbind(Trading_L,Trading_P)
  if (dim(Trading)[1] == 0) {
    col_order <- c("Id", "Same distinctiveness or better habitat required", 
                   "Same broad habitat or a higher distinctiveness habitat required", 
                   "Same habitat required", "Bespoke compensation likely to be required", "Site Label")
    df1 <- setNames(data.frame(matrix(ncol = length(col_order), nrow = length(Options$id))), col_order)
    df1$Id <- Options$id
    df1$`Site Label` <- Options$aoi_label
    df1[is.na(df1)] <- "-"
    return(df1)
  } else {
    Trading <- separate_rows(Trading, Id, `Trading notes`,
                             sep = "; ", convert = TRUE)                                           # Separate rows
    Trading <- Trading[!duplicated(Trading), ]                                      # Remove duplicates
    Trading$Count <- "Advised"                                                              # Assign count
    Trading_wide <- spread(Trading, `Trading notes`, Count)                           # df long to wide
    Trading_wide[is.na(Trading_wide)] <- "-"                                          # Replace NAs with - 
    Actions <- c("Id","Same distinctiveness or better habitat required", 
                 "Same broad habitat or a higher distinctiveness habitat required", 
                 "Same habitat required", "Bespoke compensation likely to be required", "Site Label")
    Missing <- setdiff(Actions, names(Trading_wide))                                # Find names of missing columns
    Trading_wide[Missing] <- "-"                                                      # Add them, filled with '0's
    Trading_wide <- Trading_wide[Actions]                                           # Put columns in desired order
    Trading_wide <- Trading_wide[order(Trading_wide$Id),]                                            # Order Site_ID by numeric
    Trading_wide <- extendDfWithAllSites(controlDF = Options, dfToExtend = Trading_wide, by.controlCol = "id", by.dfToExtendCol = "Id")
    Trading_wide <- base::merge(Trading_wide,Options[,1:2], by.x = c('Id'),by.y = c('id'),             
                                all.x = TRUE, all.y = TRUE)                                        # Add id
    Trading_wide <- subset(Trading_wide, select = -c(geometry,`Site Label`))           
    names(Trading_wide)[names(Trading_wide) == "aoi_label"] <- "Site Label"
    return(Trading_wide)
  }
}

getSummaryTable <-function(Options, biodivUnits.polygons, biodivUnitsLinear, overlapWithDesignatedSites.individual, nonBiodivMetrichabitats, biodivMetricsLinear, biodivMetricshabitatsPolygons){
  # Creates tsummary table
  # Returns a df
  Area_BU <- subset(biodivUnits.polygons, select=-c(`Rank for Area Biodiversity Units`))         # Simplify table
  
  Linear_BU <- subset(biodivUnitsLinear, select=-c(`Rank for Linear Biodiversity Units`))   # Simplify table
  
  overlapWithDesignatedSites.individual$Designated <- ifelse(overlapWithDesignatedSites.individual$`Site overlap with designated sites (%)` > 0 ,"Present","-")
  overlapWithDesignatedSites.individual <- subset(overlapWithDesignatedSites.individual, select=-c(`Designated Sites`, `Site overlap with designated sites (%)`))       # Simplify table
  Designated <- overlapWithDesignatedSites.individual %>% 
    group_by(Id,`Site Label`) %>% 
    filter(nchar(Designated)==max(nchar(Designated)))
  Designated <- Designated[!duplicated(Designated), ]                                         # Remove duplicates
  
  nonBiodivMetrichabitats$Non_BM <- ifelse(nonBiodivMetrichabitats$`Area of habitat in site (ha)` > 0 ,"Present","-")
  Non_BM <- subset(nonBiodivMetrichabitats, select=-c(Habitat, `Area of habitat in site (ha)`))      # Simplify table
  Non_BM <- Non_BM %>% 
    group_by(Id,`Site Label`) %>% 
    filter(nchar(Non_BM)==max(nchar(Non_BM)))
  Non_BM <- Non_BM[!duplicated(Non_BM), ]                                         # Remove duplicates
  
  Baseline_Habs_L <- subset(biodivMetricsLinear , `Length of habitat in site (km)` > 0)
  Baseline_Habs_L <- subset(Baseline_Habs_L, select=c(Id, `UK BAP`))               # Simplify table
  Baseline_Habs_P <- subset(biodivMetricshabitatsPolygons , `Maximum area of habitat in site (ha)` > 0)
  Baseline_Habs_P <- subset(Baseline_Habs_P, select=c(Id, `UK BAP`))               # Simplify table
  Baseline_Habs <- rbind(Baseline_Habs_L,Baseline_Habs_P)
  Baseline_Habs <- Baseline_Habs[!grepl("-", Baseline_Habs$`UK BAP`),]             # Remove rows with ' -'
  if (dim(Baseline_Habs)[1] > 0) {
    Baseline_Habs$`UK BAP` <- "Present"
  }
  UK.BAP <- Baseline_Habs[!duplicated(Baseline_Habs), ]                                      # Remove duplicates
  
  # Combine data
  
  df_list <- list(Area_BU, Linear_BU, Designated, Non_BM, UK.BAP)      
  Summary <- Reduce(function(x, y) base::merge(x, y, all=TRUE), df_list)  
  
  # Create table
  Summary[is.na(Summary)] <- "-"
  
  Summary <- Summary[order(Summary$Id),]                    # Order Option by numeric
  names(Summary)[names(Summary) == "Area Biodiversity Units"] <- "Biodiversity units - Area habitats"   # Rename
  names(Summary)[names(Summary) == "Linear Biodiversity Units"] <- "Biodiversity units - Linear habitats"   # Rename
  names(Summary)[names(Summary) == "Designated"] <- "Designated sites"                    # Rename
  names(Summary)[names(Summary) == "Non_BM"] <- "Non-Biodiversity Metric habitats"                    # Rename
  names(Summary)[names(Summary) == "UK BAP"] <- "UK BAP habitats"                    # Rename
  return(Summary)
}

escapeApostrophesForSQL <- function(s) {
  return(gsub("'", "''", s, fixed = TRUE))
}

main <- function(batch_id) {
  #------------------
  # Connect to DB

  dbConnection <- connectDB(wd_path)
  if (exists("dbConnection") && !inherits(dbConnection, "try-error")) {
    
    start_time <- Sys.time()
    #------------------
    # Get layers from DB
    print("Loading: sites")
    # Site
    qOptions <- sprintf("SELECT id, aoi_label, user_email, user_id, batch_id, ST_Transform(aoi_polygon_4326, 27700) as geometry FROM public.processing_queue
    WHERE batch_id = '%s' ORDER BY upload_time DESC", batch_id)
    Options <- st_read(dbConnection, query=qOptions)
    
    # If the query does not return back anything
    if (dim(Options)[1]== 0)
      stop(paste("Could not find any site with this batch id: ", batch_id))
    
    # #######################
    # ## Using refdata Options
    # qOptions <- "SELECT site_id, ST_Transform(geometry, 27700) as geometry FROM refdata.options"
    # Options <- st_read(dbConnection, query=qOptions)
    # Options$id <- seq.int(nrow(Options))
    # names(Options)[names(Options) == "site_id"] <- "aoi_label"
    # Options$user_email <- 1
    # Options$batch_id <- 1
    # #Options <- head(Options,2) ## default is 6
    # site_bbox <- st_as_text(st_union(Options))
    # ########################
    Options$aoi_label <- escapeApostrophesForSQL(Options$aoi_label)

    Options$O_area <- as.numeric(st_area(Options)/10000)                   # Calculate area in hectare
    # Base layers queried by AOI
    site_crs <- st_crs(Options)
    
    # Create envelopes around individual sites and collect them as a multipolygon
    qSiteBBOX <- sprintf("SELECT ST_AsText(ST_Union(ST_Transform(aoi_polygon_4326, 27700))) as geometry FROM public.processing_queue WHERE batch_id = '%s'", batch_id)
    site_bbox <- dbGetQuery(dbConnection, qSiteBBOX)
    
    ## habitats - Linear
    print("Loading: Habitats - linear")
    qhabitatsLinear <- sprintf("SELECT habitat, cf_id, geom 
    FROM (SELECT habitat, cf_id, ST_Intersection(ST_GeomFromText('%s', 27700), geometry) as geom 
    FROM refdata.biodiv_habitats_linear) as df
    WHERE ST_isEmpty(df.geom) = false;", site_bbox)
    habitatsLinear <- st_read(dbConnection, query=qhabitatsLinear)
    
    ## habitats - Polygon
    print("Loading: Habitats - polygon")
    qhabitatsPolygon <- sprintf("SELECT habitat, cf_id, geom 
    FROM (SELECT habitat, cf_id, ST_Intersection(ST_GeomFromText('%s', 27700), geometry) as geom 
    FROM refdata.biodiv_habitats_polygon) as df
    WHERE ST_isEmpty(df.geom) = false;", site_bbox)
    habitatsPolygon <- st_read(dbConnection, query=qhabitatsPolygon)

## Designated sites
    print("Loading: Designated sites")
    qDes <- sprintf("SELECT sites, geom
    FROM (SELECT sites, ST_Intersection(ST_GeomFromText('%s', 27700), ST_Buffer(geometry,0)) as geom 
    FROM refdata.designated_sites) as df
    WHERE ST_isEmpty(df.geom) = false;", site_bbox)
    Des <- st_read(dbConnection, query=qDes)
    
    ### Non-Biodiversity metric habitats
    print("Loading: Non-Biodiversity metric habitats")
    qPeat <- sprintf("SELECT cf_id, geom 
    FROM (SELECT cf_id, ST_Intersection(ST_GeomFromText('%s', 27700), ST_Buffer(geometry,0)) as geom 
    FROM refdata.non_biodiv_habitats_peats) as df
    WHERE ST_isEmpty(df.geom) = false;", site_bbox)
    Peat <- st_read(dbConnection, query=qPeat)
    qAW <- sprintf("SELECT cf_id, geom 
    FROM (SELECT cf_id, ST_Intersection(ST_GeomFromText('%s', 27700), ST_Buffer(geometry,0)) as geom 
    FROM refdata.non_biodiv_habitats_aw) as df
    WHERE ST_isEmpty(df.geom) = false;", site_bbox)
    AW <- st_read(dbConnection, query=qAW)
    Peat <-st_sf(Peat, crs=st_crs(Options))
    AW <-st_sf(AW, crs=st_crs(Options))
    
    print("Loading: tables")
    qNon_BM <- "SELECT cf_id, habitat FROM refdata.non_biodiv_overview"
    Non_BM <- dbGetQuery(dbConnection, qNon_BM)
    
    # biodiversity metrics
    qBM_Values <- "SELECT cf_id, distinctiveness, condition, trading_notes, checks, uk_bap FROM refdata.biodiv_scores"
    BM_Values <- dbGetQuery(dbConnection, qBM_Values)
    qBM_Habs <- "SELECT cf_id, habitat FROM refdata.biodiv_overview"
    BM_Habs <- dbGetQuery(dbConnection, qBM_Habs)
    
    #------------------
    # Processing
    
    ## Output: isInterTidal: bool
    
    # designated sites
    print("Compute: Overlap with designated sites - individual")
    overlapWithDesignatedSites.individual <- getOverlapWithIndividualDesignatedSites(Options, Des)
    gc()
    
    # non biodiversity metrics habitats
    print("Compute: Analyse non biodiversity metric habitats")
    nonBiodivMetrichabitats <- analyseNonBiodivMetrichabitats(Options, Peat, AW, Non_BM)
    gc()
    ## Output: nonBiodivMetrichabitats: df to JSON
    
    # biodiv metrics habitats - linear
    print("Compute: Analyse biodiv metrics habitats - linear")
    biodivMetricsLinear <- analyseBiodivMetricshabitatsLinearFast(Options, habitatsLinear, BM_Values, BM_Habs)
    biodivUnitsLinear <- calculateBiodivLinearUnits(biodivMetricsLinear, Options)
    
    # Prettifying
    # TODO: rename it within the function
    names(biodivMetricsLinear)[names(biodivMetricsLinear) == "Length_km"] <- "Length of habitat in site (km)"
    names(biodivMetricsLinear)[names(biodivMetricsLinear) == "distinctiveness"] <- "Distinctiveness"
    names(biodivMetricsLinear)[names(biodivMetricsLinear) == "condition"] <- "Condition"
    names(biodivMetricsLinear)[names(biodivMetricsLinear) == "checks"] <- "Checks"
    names(biodivMetricsLinear)[names(biodivMetricsLinear) == "trading_notes"] <- "Trading notes"
    names(biodivMetricsLinear)[names(biodivMetricsLinear) == "uk_bap"] <- "UK BAP"
    names(biodivMetricsLinear)[names(biodivMetricsLinear) == "BU_Linear"] <- "Linear Biodiversity units"
    names(biodivMetricsLinear)[names(biodivMetricsLinear) == "aoi_label"] <- "Site Label"   # Rename
    ## Output: biodivMetricsLinear: df -> JSON
    ## Output: biodivUnitsLinear: df -> JSON
    
    gc()
    # Biodiv metrics habitats - polygons
    print("Compute: Analyse biodiv metrics habitats - polygons")
    biodivMetricshabitatsPolygons <- analyseBiodivMetricshabitatsPolygons(habitatsPolygon, Options, BM_Values, BM_Habs)
    ## Output: biodivMetricshabitatsPolygons: df -> JSON
    
    # Biodiversity units
    biodivUnits.polygons <- getBiodivUnitsPolygons(habitatsPolygon, Options, BM_Values)
    
    ## Trading notes
    print("Creating: Trading notes")
    Trading_wide <- getTradingNotes(Options, biodivMetricsLinear, biodivMetricshabitatsPolygons)
    ## Output: Trading_wide: df -> JSON
    
    # Create summary table
    summaryTable <- getSummaryTable(Options, biodivUnits.polygons, biodivUnitsLinear, overlapWithDesignatedSites.individual, nonBiodivMetrichabitats, biodivMetricsLinear, biodivMetricshabitatsPolygons)
    #------------------
    # Write to csv
    if(doWriteCSV == TRUE){
      write.csv(biodivUnits.polygons, outputpath.biodiv_units_area, row.names = FALSE)
      write.csv(Trading_wide, outputpath.trading_notes,row.names=FALSE)
      write.csv(biodivMetricshabitatsPolygons, outputpath.baseline_habitats_area, row.names = FALSE)
      write.csv(biodivMetricsLinear, outputpath.baseline_habitats_linear,row.names = FALSE)
      write.csv(biodivUnitsLinear, outputpath.biodiv_units_linear,row.names = FALSE)
      write.csv(nonBiodivMetrichabitats, outputpath.non_biodiv_habitats,row.names = FALSE)
      write.csv(overlapWithDesignatedSites.individual, outputpath.designated_sites, row.names = FALSE)
      write.csv(summaryTable, outputpath.summary_table, row.names = FALSE)
    }
    
    # Write to DB
    for(site in unique(Options$id)){
      ## Flag Intertidal areas
      print("Loading: Tides")
      site_geom  <- st_as_text(Options$geometry[Options$id==site])
      qTide <- sprintf("SELECT * FROM (SELECT ST_Intersects(ST_GeomFromText('%s', 27700), geometry) as res 
                     FROM refdata.tide) as res WHERE res = true;", site_geom)
      Tide <- dbGetQuery(dbConnection, qTide)
      print("Checking: Intertidal areas")
      intertidal_df <- checkIntertidal(Options,Tide)                              # check Intertidal has 0 fields
      isIntertidal <- intertidal_df$isIntertidal[intertidal_df$Id==site]
      # Create JSON-s to insert in DB
      # TO DO: investigate: biodivMetricshabitatsPolygonsJSON inserts the min area as 0 into the DB, however the values are fine here
      biodivMetricshabitatsPolygonsJSON <- toJSON(subset(biodivMetricshabitatsPolygons, Id == site))
      biodivMetricsLinearJSON <- toJSON(subset(biodivMetricsLinear, Id == site))
      biodivUnits.polygonsJSON <- toJSON(subset(biodivUnits.polygons, Id == site))
      biodivUnitsLinearJSON <- toJSON(subset(biodivUnitsLinear, Id == site))
      overlapWithDesignatedSitesJSON <- toJSON(subset(overlapWithDesignatedSites.individual, Id == site))
      nonBiodivMetrichabitatsJSON <- toJSON(subset(nonBiodivMetrichabitats, Id == site))
      batch_id <- Options$batch_id[Options$id == site]
      email <- Options$user_email[Options$id == site]
      user_id <- Options$user_id[Options$id == site]
      rank_bu_linear <- biodivUnitsLinear$`Rank for Linear Biodiversity Units`[biodivUnitsLinear$Id == site]
      rank_bu_high <- biodivUnits.polygons$`Rank for Area Biodiversity Units`[biodivUnits.polygons$Id == site]
      tradingJSON <- toJSON(subset(Trading_wide, Id == site))
      summaryJSON <- toJSON(subset(summaryTable, Id == site))
      insert_time <- Sys.time()
      
      tryCatch({
        qUpsert <- sprintf("INSERT INTO processing_results 
          (site_id, user_email, user_id, rank_bu_linear, rank_bu_high, baseline_habitats_area, baseline_habitats_linear,
          biodiversity_units_area, biodiversity_units_linear, designated_sites_combined, intertidal, non_biodiv_habitats, 
          batch_id, trading_notes, processing_time, summary_table)
          VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s') 
          ON CONFLICT (site_id)
          DO UPDATE SET 
              user_email = EXCLUDED.user_email, 
              user_id = EXCLUDED.user_id, 
              rank_bu_linear = EXCLUDED.rank_bu_linear, 
              rank_bu_high = EXCLUDED.rank_bu_high, 
              baseline_habitats_area = EXCLUDED.baseline_habitats_area, 
              baseline_habitats_linear = EXCLUDED.baseline_habitats_linear,
              biodiversity_units_area = EXCLUDED.biodiversity_units_area, 
              biodiversity_units_linear = EXCLUDED.biodiversity_units_linear, 
              designated_sites_combined = EXCLUDED.designated_sites_combined, 
              intertidal = EXCLUDED.intertidal, 
              non_biodiv_habitats = EXCLUDED.non_biodiv_habitats, 
              batch_id = EXCLUDED.batch_id, 
              trading_notes = EXCLUDED.trading_notes, 
              processing_time = EXCLUDED.processing_time, 
              summary_table = EXCLUDED.summary_table;",
                                 # Insert values
                                 site, # site_id
                                 email, # user_email
                                 user_id, # user_id
                                 rank_bu_linear, # rank_bu_linear
                                 rank_bu_high, # rank_bu_high
                                 biodivMetricshabitatsPolygonsJSON, # baseline_habitats_area
                                 biodivMetricsLinearJSON, # baseline_habitats_linear
                                 biodivUnits.polygonsJSON, # biodiversity_units_area
                                 biodivUnitsLinearJSON, # biodiversity_units_linear
                                 overlapWithDesignatedSitesJSON, # designated_sites_combined
                                 isIntertidal, # intertidal
                                 nonBiodivMetrichabitatsJSON, # non_biodiv_habitats
                                 batch_id, # batch_id
                                 tradingJSON, # trading_notes
                                 insert_time, # processing_time
                                 summaryJSON # summary_table
                           # No need to repeat the variables for the UPDATE part, EXCLUDED.<column> is used instead
        )
        if(doWriteToDB == TRUE){
          dbSendQuery(dbConnection, qUpsert)
        }
      }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})     
      #------------------
    }
    end_time <- Sys.time()
    proc <- end_time - start_time
    print(proc)
    dbDisconnect(dbConnection)
    print("FINISHED")
  }
}

#------------------

## Run the analysis
main(batch_id)

