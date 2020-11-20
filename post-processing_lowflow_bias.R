####SOC: Post-processing low-flow bias 
#if positive bias in low flow, either applly multiplier (to reduce value) or threshold for actual flow (all flows below threshold should be 0)

#UPDATE: directories where flow to be updated is located
flow.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201105_Aliso_Recalibration_Update/Model_Output_WY1993-2019/"
#files in flow dir
fnames <- list.files(flow.dir, full.names = TRUE)
fnames.short <- list.files(flow.dir, pattern = ".out")
fnames.short <- gsub(".out", "", fnames.short)

#new directory with bias corrected flow to be saved
out.dir <- paste0(flow.dir, "low.flow.bias.corrected/")
dir.create(out.dir)

#read in low flow bias table
bias <- read.csv("C:/Users/KristineT/SCCWRP/SOC WQIP - Flow Ecology Study - General/lowflow_bias_SOC_LSPC.csv")

#subset to positive bias only
bias.pos <- bias[bias$Bias.lowflow == "Positive",]

##lookuptable to convert subbasin codes for model output subbasin names
subbasin_lookup <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/191220_Interim_Calibration/site_name_lookupletternumbers.csv")

##############################
#convert basin orig name to outputfile name (model subbasin name)
new.subbasinname <- bias.pos$Subbasin

for(z in 1:length(subbasin_lookup$Letter)){
  new.subbasinname <- gsub(subbasin_lookup$Letter[z], subbasin_lookup$Number[z], new.subbasinname)
}

#find and replace - in new.subbasinname with nothing, make consistent with file name
new.subbasinname <- gsub("-", "", new.subbasinname)
bias.pos$new.subbasinname <- new.subbasinname

#subset to only subbasins with modeled flow
new.subbasinname <- new.subbasinname[new.subbasinname %in% fnames.short]

##############################
#loop through subbasins with positive bias and correct, save new file

for(i in 1:length(new.subbasinname)){
  
  #get subbasin data i
  subbasin.model <- new.subbasinname[i]
  
  #bias sub
  bias.sub <- bias.pos[bias.pos$new.subbasinname == new.subbasinname[i],]
  ################################################
  ####LSPC current data
  #when reading in data, skip 23 rows
  skip = 23
  
  #load in hourly model current predictions
  curr <- read.table(paste0(fnames[i]), skip=skip)
  names(curr) <- c("gage", "year", "month", "day", "hour", "min", "precip", "depth", "av.depth", "av.vel","flow.cfs")
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
  #if no actual flow threshold (is low flow mult), then multiply flow in dry months by multiplier
  if(is.na(bias.sub$actual.flow.threshold)){
    curr$flow.cfs[MONTH %in% dry.months] <- curr$flow.cfs[MONTH %in% dry.months] * bias.sub$lowflow.multiplier
  }else{
    #find index of dry months
    ind.dry <- which(MONTH %in% dry.months)
    #find which < threshold
    ind.threshold <- which(curr$flow.cfs[MONTH %in% dry.months] < bias.sub$actual.flow.threshold)
    #substitute the flow values in dry months that are less then flow threshold with zero
    #test to see if the max is below threshold
    #max(curr$flow.cfs[ind.dry[ind.threshold]])
    #set these values to zero
    curr$flow.cfs[ind.dry[ind.threshold]] <- 0
  }
  
  #write the new hourly data 
  filename <- paste0(out.dir, subbasin.model, ".out")
  write.table(curr, row.names = FALSE, file = filename)
  
}
  