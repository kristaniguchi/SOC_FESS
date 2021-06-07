####SOC: Post-processing low-flow bias 
#if positive bias in low flow, either aplly multiplier (to reduce value) or threshold for actual flow (all flows below threshold should be 0)
  #need different script to translate Aliso climate scenarios

#UPDATE: directories where flow to be updated is located
# #Aliso recalibration - current
# flow.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201105_Aliso_Recalibration_Update/Model_Output_WY1993-2019/"
# #Aliso recalibration - ref
# flow.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201105_Aliso_Reference_Condition/WY94-Present/"
# #Aliso recalibration - water conservation
 flow.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201111_Aliso_Water_Conservation_Scenario/"
# 
# #Oso and small tribs - existing (make sure to exclude the Oso subbasins since updated calibration is in the San Juan LSPC directory) 
# flow.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201118_Oso,_Small_Creeks_Existing_Conditions/Model_Output_WY94-19/"
# #Oso and small tribs - ref 
# flow.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201118_Oso,_Small_Creeks_Reference_Condition/WY94-19/"
# #Oso and small tribs - water conservation 
# flow.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201119_Oso,_Small_Creeks_Water_Conservation_Scenario/"
# 
# #San Juan LSPC - existing
# flow.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/210422_San_Juan_Existing_Conditions/"
# #San Juan LSPC - ref
# flow.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/210422_San_Juan_Reference_Condition/"
# #San Juan LSPC - water conservation
# flow.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/210422_San_Juan_Water_Conservation/"


#files in flow dir
fnames <- list.files(flow.dir, full.names = TRUE)
fnames.short <- list.files(flow.dir, pattern = ".out")
fnames.short <- gsub(".out", "", fnames.short)
#if Oso output directory, exclude Oso subbasins since recalibration is included in San Juan directory
outdir.subbasin.name <- grep("Oso", flow.dir)
oso.subbasins <- c("403010", "403011", "403020", "403030", "404010", "405010", "405020")
if(length(outdir.subbasin.name) > 0){
  fnames.short <- fnames.short[! (fnames.short %in% oso.subbasins)]
}

#new directory with bias corrected flow to be saved
out.dir <- paste0(flow.dir, "low.flow.bias.corrected/")
dir.create(out.dir)

#read in low flow bias table
bias <- read.csv("C:/Users/KristineT/SCCWRP/SOC WQIP - Flow Ecology Study - General/lowflow_bias_SOC_LSPC.csv")

#subset to positive bias only
bias.pos <- bias[bias$Bias.lowflow == "Positive",]

##lookuptable to convert subbasin codes for model output subbasin names
subbasin_lookup <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/Old_Runs/191220_Interim_Calibration/site_name_lookupletternumbers.csv")

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
  #when reading in data, skip 23 rows except for Aliso water conservation scenario outputs skip 24
  outdir.subbasin.name <- grep("Aliso_Water_Conservation", flow.dir)
  if(length(outdir.subbasin.name > 0)){
    skip = 24
  }else{
    skip = 23
  }

  
  #load in hourly model current predictions
  curr <- read.table(paste0(fnames[i]), skip=skip)
  names(curr) <- c("gage", "year", "month", "day", "hour", "min", "precip", "depth", "av.depth", "hyd.radius","flow.cfs")
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
  