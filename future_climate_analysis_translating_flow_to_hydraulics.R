#Translating flow to hydraulics for future climate scenarios Aliso Creek

#Apply rating curves to the discharge data from LSPC model
#for every subbasin, loop through read in hydraulic rating curve data, use approxfun to interpolate hyd variables based on flow
#also processes hydraulics for the low-flow bias corrected data and saves that in the hydraulics outputs folder (instead of unprocessed data)

#load libraries
library("tidyverse")
#install.packages("readr")
library("readr")
library("dplyr")
#install.packages("filesstrings")
library("filesstrings")

###UPDATE THIS: directory and files for LSPC model
# #Aliso recalibration - future climate scenarios for two time periods: Historical_WY75-05 and RCP85_WY30-60 and 4 GCMs
flow.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201111_Aliso_Climate_Scenario/"
#list the subdirectories for the 2 time periods
timeperiod.dirs <- list.files(flow.dir, full.names=TRUE)
#find all future flow directories for gcms
#set empty flow directory to fill in loop
gcm.dirs <- NA

for(z in 1:length(timeperiod.dirs)){
  gcm.dirs.z <- list.files(timeperiod.dirs[z], full.names=TRUE)
  #save into gcm.dirs
  gcm.dirs <- c(gcm.dirs, gcm.dirs.z)
}
#omit first na value
gcm.dirs <- gcm.dirs[2:length(gcm.dirs)]

#make list of low flow bias directories by pasting "low.flow.bias.corrected" to end of gcm directories (all dirs have low flow bias folders)
low.flow.dirs <- paste0(gcm.dirs, "/low.flow.bias.corrected/")

#combine all flow directories and loop through them to translate flow to hydraulics for each senario
flow.dir <- c(gcm.dirs, low.flow.dirs)

###########################################
#set up hydraulic output directories for each flow.dir
#parent output directory where all scenarios and gcms will be saved
output.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics/201111_Aliso_Climate_Scenario/"
#create directory
dir.create(output.dir)

#find directory time period for each flow.dir
flow.dir.root.timeperiod <- sapply(strsplit(flow.dir, "/"), '[', 8)
#create list of output directories for time period and gcms
output.dir.timeperiod <- paste0(output.dir, flow.dir.root.timeperiod, "/")
#create directories for all timeperiods
dir.create(unique(output.dir.timeperiod)[1]) #first time period
dir.create(unique(output.dir.timeperiod)[2]) #second time period

#find directory gcm for each flow.dir
flow.dir.root.gcm <- sapply(strsplit(flow.dir, "/"), '[', 9)
#create list of output directories for time period and gcms
output.dir.timeperiod.gcm <- paste0(output.dir, flow.dir.root.timeperiod, "/", flow.dir.root.gcm, "/")
#find directories for all unique timeperiods and gcms
unique.dir.timeperiod.gcm <- unique(output.dir.timeperiod.gcm)
#create directories for all unique timeperiods and gcms
for(y in 1:length(unique.dir.timeperiod.gcm)){
  dir.create(unique.dir.timeperiod.gcm[y])
}

#Note: will create bias corrected output directories for only those that are bias corrected (within loop below)

#lookup table with X_Sect_ID, Reach.ID (used for wildermuth), LSPC.ID
#model reach values (slope, manning's n)
reach.metrics <- read.csv("C:/Users/KristineT/Documents/Git/SOC_FESS/data/hydraulics/Full_Model_Reaches_av_geom_metrics.csv") %>% 
  select("Reach.ID", "LSPC.ID", "Slope", "Mannings.n", "Downstream.ID", "Downstream.LSPC.ID")
reach.metrics2 <- reach.metrics[2:length(reach.metrics$Reach.ID),]
#read in lookup table with subbasin and X_SECT_ID, filter, and merge with reach metrics by Reach.ID and merge with manning's n at outlet
lookup <- read.csv("C:/Users/KristineT/Documents/Git/SOC_FESS/data/hydraulics/nearest_XS_pourpoints_final.csv") %>% 
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
  subbasins <- list.files(flow.dir[j], pattern ="\\.out$", ignore.case = TRUE) %>% 
    strsplit(split="\\.")
  subbasins <- sapply(subbasins, `[`, 1)
  
  #remove 201080_old from list
  ind.old <- grep("old", subbasins) 
  #exclude old if old is still in list
  if(length(ind.old) > 0){
    subbasins <- subbasins[-ind.old]
  }

  #optional: to run only for missing slope sites, filter list.files and subbasins to only include those that didn't have slope
  #missing.slope <- read.csv("L:/San Juan WQIP_KTQ/Data/SpatialData/Hydraulics/missing.slope.XSECTID.csv")
  #only include LSPC sites
  #missing.slope <- missing.slope[missing.slope$Source == "LSPC",]
  #find index of missing sites that need to be run
  #ind.list.files <- which(subbasins %in% missing.slope$LSPC.ID)
  #subset to missing sites
  #list.files <- list.files[ind.list.files]
  #subbasins <- subbasins[ind.list.files]
  
  #directory and files for rating tables
  rating.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics/rating_curve_data/"
  list.files.rating <- list.files(rating.dir, full.names = TRUE, pattern ="\\.csv$")
  

  #if bias corrected save it as its own low.flow.bias directory
  ind.bias <- grep("bias", flow.dir[j])
  if(length(ind.bias)>0){
    #output should be saved in low flow bias folder
    output.dir.timeperiod.gcm[j] <- paste0(output.dir.timeperiod.gcm[j], "low.flow.bias/")
    #create output directory for low.flow.bias
    dir.create(output.dir.timeperiod.gcm[j])
  }
  
  
  #remove sites without reach params from list files
  #find index of param sites in subbasin list
  ind.paramsites <- which(subbasins %in% param.sites$LSPC.ID)
  #further subset subbasin list to those with reach parameters
  subbasins <- subbasins[ind.paramsites]
  #subset list.files with only sites with reach params
  list.files <- list.files[ind.paramsites]
  
  
  #loop through files
  
  for(i in 1:length(list.files)){

    #read in discharge data
    #check to see if bias corrected data, if so skip=1, else skip=24
    ind.bias <- grep("bias", list.files[i])
    if(length(ind.bias)>0){
      skip = 1
    }else{
      skip = 24
    }

    #read in discharge data
    q.data <- read.table(list.files[i], skip=skip)
    names(q.data) <- c("gage", "year", "month", "day", "hour", "min", "precip", "surf.outflow", "av.depth", "hyd.radius", "av.velocity", "flow.cfs")
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
      file.name.output <- paste0(output.dir.timeperiod.gcm[j], Reach.ID, "_hydraulic_outputs.csv")
      write_csv(output.data,  path=file.name.output)
    }else{
      #else if there's no xs geom do not run script, write xs name with missing geom/rating curve
      no.xs.geom <- c(no.xs.geom, Reach.ID)
    }
  }
  
}


###############################################################################################################
#Save all low flow bias corrected hydraulics into the main scenario folder and replace the non corrected files

#hydraulic directories of time period and gcms
hyd.files.all <- output.dir.timeperiod.gcm
#only the root directories for time period and gcms (will loop within and save bias corrected in each)
hyd.files <- hyd.files.all[1:8]

#loop through all directories and take bias corrected and replace old files, save not corrected files in new directory for backup

for(f in 1:length(hyd.files)){
  #list files in directory
  dir.files <- list.files(hyd.files[f])
  dir.files.long <- list.files(hyd.files[f], full.name = TRUE)
  #bias directory files
  bias.dir <- list.files(hyd.files[f], full.name = TRUE, pattern="low.flow.bias")
  
  #list files in bias.dir to get files names and move over files in dir.files to a new directory of original files
  bias.files <- list.files(bias.dir)
  bias.files.long <- list.files(bias.dir, full.name = TRUE, pattern=".csv")
  #find files indices that match in hyd directory
  old.files <- dir.files.long[dir.files %in% bias.files] 
  #create new directoy to move old files into
  old.dir.create <- paste0(bias.dir, "/original.not.corrected/")
  dir.create(old.dir.create)
  #move old files not corrected into created dir original.not.corrected
  file.move(old.files, old.dir.create)
  #copy bias corrected to main dir
  file.copy(bias.files.long, hyd.files[f])
  
}



