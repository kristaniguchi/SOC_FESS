#CEFF Manuscript: Apply rating curves to the discharge data from LSPC model
#for "restored" XS at Aliso@STP, loop through read in hydraulic rating curve data, use approxfun to interpolate hyd variables based on flow
#also processes hydraulics for the low-flow bias corrected data and saves that in the hydraulics outputs folder (instead of unprocessed data)
#need different script to translate Aliso climate scenarios

#load libraries
library("tidyverse")
#install.packages("readr")
library("readr")
library("dplyr")
#install.packages("filesstrings")
library("filesstrings")

###UPDATE THIS: directory and files for LSPC model
#Aliso recalibration - current
flow.dir0 <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201105_Aliso_Recalibration_Update/Model_Output_WY1993-2019/"
#Aliso recalibration - ref
flow.dir1 <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201105_Aliso_Reference_Condition/WY94-Present/"

#combine all flow directories and loop through them to translate flow to hydraulics for each senario
flow.dir <- c(flow.dir0, flow.dir1)

#or only one directory set here
#flow.dir <- flow.dir2 #aliso water conservation

#lookup table with X_Sect_ID, Reach.ID (used for wildermuth), LSPC.ID
#model reach values (slope, manning's n)
reach.metrics <- read.csv("C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/data/hydraulics/Full_Model_Reaches_av_geom_metrics.csv") %>% 
  select("Reach.ID", "LSPC.ID", "Slope", "Mannings.n", "Downstream.ID", "Downstream.LSPC.ID")
reach.metrics2 <- reach.metrics[2:length(reach.metrics$Reach.ID),]
#read in lookup table with subbasin and X_SECT_ID, filter, and merge with reach metrics by Reach.ID and merge with manning's n at outlet
lookup <- read.csv("C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/data/hydraulics/nearest_XS_pourpoints_final.csv") %>% 
  rename(Reach.ID = Subbasin) %>% 
  select(X_SECT_ID, Reach.ID) %>% 
  merge(reach.metrics2, by = "Reach.ID") 
#filter out reaches that don't have representative XS (X_SECT_ID is NA --> ie underground channel)
lookup <- lookup[!is.na(lookup$X_SECT_ID),]

#determine which sites have reach parameters, when slope = 0, needs slope calculated
param.sites <- lookup[as.numeric(lookup$Slope) > 0,]


#loop through each directory and calculate hydraulics

#empty vector to write xs with no geom data and rating curves generated
no.xs.geom <- NA

for(j in 1:length(flow.dir)){
  #for(j in 10:length(flow.dir)){
  #list files in directory
  list.files <- list.files(flow.dir[j], full.names = TRUE, pattern ="\\.out$", ignore.case = TRUE)
  subbasins <- "201020" #only run for Aliso@STP
  
  #directory and files for rating tables
  rating.dir <- "C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Manuscripts/CEFF_casestudy/data/hydraulics/rating_curve_data_restoredXS/"
  list.files.rating <- list.files(rating.dir, full.names = TRUE, pattern ="\\.csv$")
  
  ###UPDATE THIS: output directory for the hydraulics output
  #current recalibration update from 201105
  #output.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics/hydraulic_output_current_LSCP_recalibration_update/"
  #current recalibration update with low flow bias
  
  #find directory with root name
  flow.dir.root <- sapply(strsplit(flow.dir[j], "/"), '[', 7)
  output.dir <- paste0("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Manuscripts/CEFF_casestudy/data/hydraulics/", flow.dir.root, "/")
  #if bias corrected save it within the root folder
  ind.bias <- grep("bias", flow.dir[j])
  if(length(ind.bias)>0){
    output.dir <- paste0(output.dir, "low.flow.bias/")
  }
  
  #create directory for outputs
  dir.create(output.dir)
  
  #remove sites without reach params from list files
  #find index of param sites in subbasin list
  ind.paramsites <- grep(subbasins, list.files)
  #subset list.files with only sites with reach params
  list.files <- list.files[ind.paramsites]
  
  
  #loop through files
  
  for(i in 1:length(list.files)){
    #for(i in 9:length(list.files)){
    
    #read in discharge data
    #check to see if bias corrected data, if so skip=0, else skip=23
    ind.bias <- grep("bias", list.files[i])
    if(length(ind.bias)>0){
      skip = 1
    }else{
      skip = 23
    }
    #read in discharge data
    q.data <- read.table(list.files[i], skip=skip)
    names(q.data) <- c("gage", "year", "month", "day", "hour", "min", "precip", "depth", "av.depth", "hyd.radius","flow.cfs")
    #format date
    #update to date.time since hourly data
    #add leading zero to hour
    NHOUR <- sprintf("%02d",q.data$hour)
    date <- paste0(q.data$month, "/", q.data$day, "/", q.data$year, " ", NHOUR, ":00")
    #date <- paste(q.data$month, q.data$day, q.data$year, sep="/")
    q.data$date <- date
    unique.dates <- unique(date)
    ################
    
    #output data in cfs, need to convert to cms
    q.data$q.cms <- q.data$flow.cfs/35.3147
    
    #find Reach.ID
    Reach.ID <- lookup$Reach.ID[lookup$LSPC.ID == subbasins[i]]
    
    #read in rating curve data
    ind.curve <- grep(Reach.ID, list.files.rating)
    #if no rating curve developed, save xs as error and move on to next XS
    if(length(ind.curve) > 0){
      rating.data <- read.csv(list.files.rating[ind.curve], header=TRUE)
      
      #loop through each column in rating table and find associated hydraulic values
      col.indices.rating <- 2:length(names(rating.data))
      
      #create empty output matrix, first column is row, rest are same as rating data
      output.data <- data.frame(matrix(NA, nrow=length(q.data$date), ncol=length(names(rating.data))+1))
      names(output.data) <- c("date", names(rating.data))
      
      #save date and q.cms based on discharge timeseries
      output.data$date <- q.data$date
      output.data$q.cms <- q.data$q.cms
      #max(output.data$q.cms)
      
      for(column in col.indices.rating){
        #subset to first column q.cms and cols and remove NA values (if any)
        rating.sub <- rating.data[,c(1, column)]
        #save orig column name
        rating.name <- names(rating.sub)[2]
        #rename col to generic name
        names(rating.sub)[2] <- "variable"
        #remove potential -inf values and na
        rating.sub$variable[is.infinite(rating.sub$variable)] <- NA
        #remove NA
        rating.sub <- na.omit(rating.sub)
        
        #Use approxfun to linear interpolate the hyd variables based on Q
        rating.fun <- approxfun(rating.sub$q.cms, rating.sub$variable, rule=1:1)
        variable.pred <-rating.fun(output.data$q.cms)
        #max(variable.pred, na.rm = TRUE)
        
        #NA values are when discharge is greater than rating table discharge, replace NA with max variable that can be predicted
        if(length(which(is.na(variable.pred))) > 0 ){
          max.var.rating <- max(rating.sub$variable)
          variable.pred[which(is.na(variable.pred))] <- max.var.rating
        }
        
        #write this into new spreadsheet
        #new output column is column + 1 (since we added date into output)
        output.data[,column+1] <- variable.pred
      }
      
      #write output csv into output directory
      file.name.output <- paste0(output.dir, Reach.ID, "_hydraulic_outputs_restoredxs.csv")
      write_csv(output.data,  path=file.name.output)
    }else{
      #else if there's no xs geom do not run script, write xs name with missing geom/rating curve
      no.xs.geom <- c(no.xs.geom, Reach.ID)
    }
  }
  
}



