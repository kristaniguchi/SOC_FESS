#Future Climate Scenarios Aliso Creek: Low Flow Bias Corrected Script
####SOC: Post-processing low-flow bias 
#if positive bias in low flow, either aplly multiplier (to reduce value) or threshold for actual flow (all flows below threshold should be 0)

###########################################################################
#UPDATE: directories where flow to be updated is located
# #Aliso recalibration - future climate scenarios for two time periods: Historical_WY75-05 and RCP85_WY30-60 and 4 GCMs
flow.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201111_Aliso_Climate_Scenario/"
#list the subdirectories for the 2 time periods
timeperiod.dirs <- list.files(flow.dir, full.names=TRUE)
###########################################################################

#Read in data:
##read in lookuptable to convert subbasin codes for model output subbasin names
subbasin_lookup <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/Old_Runs/191220_Interim_Calibration/site_name_lookupletternumbers.csv")

#read in low flow bias table
bias <- read.csv("C:/Users/KristineT/SCCWRP/SOC WQIP - Flow Ecology Study - General/lowflow_bias_SOC_LSPC.csv")
#subset to positive bias only
bias.pos <- bias[bias$Bias.lowflow == "Positive",]

#convert basin orig name to outputfile name (model subbasin name)
new.subbasinname <- bias.pos$Subbasin

for(z in 1:length(subbasin_lookup$Letter)){
  new.subbasinname <- gsub(subbasin_lookup$Letter[z], subbasin_lookup$Number[z], new.subbasinname)
}

#find and replace - in new.subbasinname with nothing, make consistent with file name
new.subbasinname <- gsub("-", "", new.subbasinname)
bias.pos$new.subbasinname <- new.subbasinname

###########################################################################

#loop through the flow.dir for each time period i and gcm j and subbasin k, read in data and post-process low-flow bias

for(i in 1:length(timeperiod.dirs)){
  #list the gcm directories in timeperiod i directory to loop through
  gcm.dirs <- list.files(timeperiod.dirs[i], full.names=TRUE)
  
  #loop through each gcm j
  for(j in 1:length(gcm.dirs)){
    #files in gcm directori j
    fnames <- list.files(gcm.dirs[j], full.names = TRUE)
    fnames.short <- list.files(gcm.dirs, pattern = ".out")
    fnames.short <- gsub(".out", "", fnames.short)
    
    #create new directory with bias corrected flow to be saved
    out.dir <- paste0(gcm.dirs[j], "/low.flow.bias.corrected/")
    dir.create(out.dir)
    
    #subset new.subbasinname list for positive bias to only subbasins with modeled flow - list of subbasin that need to be corrected for low flow bias
    new.subbasinname.j <- new.subbasinname[new.subbasinname %in% fnames.short]
    
    ##############################
    #loop through subbasins with positive bias and correct, save new file
    
    for(k in 1:length(new.subbasinname.j)){
      
      #get subbasin data i
      subbasin.model <- new.subbasinname.j[k]
      
      #bias table subset for subbasin k
      bias.sub <- bias.pos[bias.pos$new.subbasinname == new.subbasinname.j[k],]
      ################################################
      ####LSPC current data
      #when reading in data, skip 24 rows 
      skip = 24

      #load in daily model for timeperiod i, gcm j, subbasin k
      curr <- read.table(paste0(fnames[k]), skip=skip)
      names(curr) <- c("gage", "year", "month", "day", "hour", "min", "precip", "surf.outflow", "av.depth", "hyd.radius", "av.velocity", "flow.cfs")
      #format date
      #add leading zero to hour
      MONTH <- sprintf("%02d",curr$month)
      DAY <- sprintf("%02d",curr$day)
      date <- paste(MONTH, DAY, curr$year, sep="/")
      #format q to be numeric
      curr$flow.cfs <- as.numeric(as.character(curr$flow.cfs))
      ################
      
      #update low flow bias correction
      #list dry months: june - sept
      dry.months <- sprintf("%02d", c(06:09))
      #if no actual flow threshold (is low flow multiplier), then multiply flow in dry months by multiplier
      if(is.na(bias.sub$actual.flow.threshold)){
        curr$flow.cfs[MONTH %in% dry.months] <- curr$flow.cfs[MONTH %in% dry.months] * bias.sub$lowflow.multiplier
      }else{
        #find index of dry months
        ind.dry <- which(MONTH %in% dry.months)
        #find which < threshold
        ind.threshold <- which(curr$flow.cfs[MONTH %in% dry.months] < bias.sub$actual.flow.threshold)
        #substitute the flow values in dry months that are less than flow threshold with zero
        #test to see if the max is below threshold
        #max(curr$flow.cfs[ind.dry[ind.threshold]])
        #set these values to zero
        curr$flow.cfs[ind.dry[ind.threshold]] <- 0
      }
      
      #write the new hourly data 
      filename <- paste0(out.dir, subbasin.model, ".out")
      write.table(curr, row.names = FALSE, file = filename)
      
    }
    
  }
}




