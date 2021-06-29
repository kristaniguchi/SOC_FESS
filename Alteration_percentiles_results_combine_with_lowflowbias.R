#Compile the percentiles and annual results from all directories
  #Do it for all current, ref, water conservation and all lowflow bias corrected data
#create empty data frames for results and percentiles making sure right columns to distinguish
  #loop through all directories and pull out percentiles and results, save into make sure has right scenario label (ref, current), subbasinmodel
  #include low flow bias corrected data (in replace of uncorrected) and Oso outputs from the San Juan folder (subbasins ^40, do not use these outputs in Oso directory only San Juan)
#do the same for current conditions, ref, water conservation, [future scenarios already have percentiles and results outputted as its own]

################################################################################################################

####UPDATE: directories for current LSCP, reference LSPC
######
#All current
curr.dir1 <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201105_Aliso_Recalibration_Update/Model_Output_WY1993-2019/daily/FFMs/"
curr.dir2 <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201118_Oso,_Small_Creeks_Existing_Conditions/Model_Output_WY94-19/daily/FFMs/"
curr.dir3 <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/210422_San_Juan_Existing_Conditions/daily/FFMs/"
#all current low flow bias
curr.dir1.lowflowbias <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201105_Aliso_Recalibration_Update/Model_Output_WY1993-2019/low.flow.bias.corrected/daily/FFMs/"
curr.dir2.lowflowbias <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201118_Oso,_Small_Creeks_Existing_Conditions/Model_Output_WY94-19/low.flow.bias.corrected/daily/FFMs/"
curr.dir3.lowflowbias <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/210422_San_Juan_Existing_Conditions/low.flow.bias.corrected/daily/FFMs/"
#combine to make directory list of all current (low flow and normal)
current.dir.all <- c(curr.dir1, curr.dir2, curr.dir3, curr.dir1.lowflowbias, curr.dir2.lowflowbias, curr.dir3.lowflowbias)

#current alteration directory where percentiles and annual ffm results should be saved
current.dir.alteration <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Aliso_Oso_SmCk_SanJuan_all_lowflowbias/"

#also get a list of low flow bias files for each subbasin (will not use those subbasin in common dir, and will take them from low flow bias dir)
low.flow.subbasins <- list.files(c(curr.dir1.lowflowbias, curr.dir2.lowflowbias, curr.dir3.lowflowbias))

######
#Reference directories - all
ref.dir1 <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201105_Aliso_Reference_Condition/WY94-Present/daily/FFMs/"
ref.dir2 <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201118_Oso,_Small_Creeks_Reference_Condition/WY94-19/daily/FFMs/"
ref.dir3 <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/210422_San_Juan_Reference_Condition/daily/FFMs/"
#reference low flow bias corrected
ref.dir1.lowflowbias <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201105_Aliso_Reference_Condition/WY94-Present/low.flow.bias.corrected/daily/FFMs/"
ref.dir2.lowflowbias <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201118_Oso,_Small_Creeks_Reference_Condition/WY94-19/low.flow.bias.corrected/daily/FFMs/"
ref.dir3.lowflowbias <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/210422_San_Juan_Reference_Condition/low.flow.bias.corrected/daily/FFMs/"
#combine to make directory list of all ref (low flow and normal)
ref.dir.all <- c(ref.dir1, ref.dir2, ref.dir3, ref.dir1.lowflowbias, ref.dir2.lowflowbias, ref.dir3.lowflowbias)

#reference alteration directory where percentiles and annual ffm results should be saved
reference.dir.alteration <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Reference_Aliso_SmCk_SanJuan_all_lowflowbias/"

######
#Water conservation directories - all
watercon.dir1 <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201111_Aliso_Water_Conservation_Scenario/daily/FFMs/"
watercon.dir2 <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201119_Oso,_Small_Creeks_Water_Conservation_Scenario/daily/FFMs/"
watercon.dir3 <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/210422_San_Juan_Water_Conservation/daily/FFMs/"
#Water conservation low flow bias corrected
watercon.dir1.lowflowbias <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201111_Aliso_Water_Conservation_Scenario/low.flow.bias.corrected/daily/FFMs/"
watercon.dir2.lowflowbias <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201119_Oso,_Small_Creeks_Water_Conservation_Scenario/low.flow.bias.corrected/daily/FFMs/"
watercon.dir3.lowflowbias <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/210422_San_Juan_Water_Conservation/low.flow.bias.corrected/daily/FFMs/"
#combine to make directory list of all watercon (low flow and normal)
watercon.dir.all <- c(watercon.dir1, watercon.dir2, watercon.dir3, watercon.dir1.lowflowbias, watercon.dir2.lowflowbias, watercon.dir3.lowflowbias)

#watercon alteration directory where percentiles and annual ffm results should be saved
watercon.dir.alteration <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/WaterConservation_Aliso_SmCk_SanJuan_all_lowflowbias/"

######
#Future climate aliso - already saved into one df but need to combine with low flow bias
climate.dir1 <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Future_Climate_Aliso/"
#future climate low flow bias
climate.dir1.lowflowbias <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Future_Climate_Aliso_lowflowbias/"
#combine to make directory list of all climate (low flow and normal)
climate.dir.all <- c(climate.dir1, climate.dir1.lowflowbias)

################################################################################################################

#Loop through each directory, subbasin folder and combine percentiles, excluding the low flow bias

#Set scenario directory and output directory (alteration dir) to loop through
# Current condition all
#scenario.dir.all <- current.dir.all #current condition all
#output.dir <- current.dir.alteration

# Reference condition all
#scenario.dir.all <- ref.dir.all #reference condition all
#output.dir <- reference.dir.alteration

# Water Conservation all
scenario.dir.all <- watercon.dir.all #water conservation all
output.dir <- watercon.dir.alteration


#create empty data frames to be saved for percentiles and annual results
#empty df for percentiles under various scenarios and subbasins
percentiles.df.overall <- data.frame(matrix(data=NA, nrow=1, ncol=10))
names(percentiles.df.overall) <- c("p10", "p25", "p50", "p75", "p90", "metric", "comid", "result_type", "source2", "subbasin.model")

#empty df for annual FFM results under various gcms and time periods - can use this to calculate change in percentiles from historical to mid-century
results.df.overall <- data.frame(matrix(data=NA, nrow=1, ncol=30))
names(results.df.overall) <- c("Year", "DS_Dur_WS", "DS_Tim", "DS_Mag_50", "DS_Mag_90", "FA_Dur", "FA_Mag", 
         "FA_Tim", "SP_ROC", "SP_Dur", "SP_Mag", "SP_Tim", "Wet_BFL_Dur", "Wet_BFL_Mag_10", 
        "Wet_BFL_Mag_50", "Wet_Tim", "Peak_Tim_10", "Peak_Tim_2", "Peak_Tim_5", "Peak_Dur_10", "Peak_Dur_2", 
        "Peak_Dur_5", "Peak_10", "Peak_2", "Peak_5", "Peak_Fre_10", "Peak_Fre_2", "Peak_Fre_5", 
        "type", "subbasin.model")

for(i in 1:length(scenario.dir.all)){
  #find low flow bias directories index
  ind.low.flow.dir <- grep("low.flow.bias.corrected", scenario.dir.all)
  #if i is the low flow bias corrected directory,
  if(i %in% ind.low.flow.dir){
    #use all files in those directories since they will be excluded below
    #list files in directory i - subbasins (folder names)
    subbasins.dir.i <- list.files(scenario.dir.all[i])
    #list long paths for directory i
    subbasins.dir.i.long <- list.files(scenario.dir.all[i], full.names = TRUE)
  }else{
    #else not low flow bias, go through all subbasins except the ones that have low flow bias corrected
    #then exclude subbasins in low flow bias
    #list all subbasins in directory i - (folder names)
    subbasins.dir.all <- list.files(scenario.dir.all[i])
    #only take subbasins that are not corrected for low flow bias
    subbasins.dir.i <- setdiff(subbasins.dir.all, low.flow.subbasins)
    #find the index of the folders that we want to use
    ind.subbasins.use <- which(subbasins.dir.all %in% subbasins.dir.i)
    #list long paths for directy i for the index of the folders we want to use
    subbasins.dir.i.long <- list.files(scenario.dir.all[i], full.names = TRUE)[ind.subbasins.use]
    
    #if Oso and Small Creeks scenarios, exclude oso outputs since recalibration is in San Juan folders
    #first determine if scenario folder is Oso
    ind.oso <- grep("Oso,_Small_Creeks_", scenario.dir.all[i])
    #if Oso and small creeks directory, then 
    if(length(ind.oso) > 0){
      #then subset to exclude Oso subbasins starting with 40
      #find index of oso files
      ind.oso.subbasins <- grep("^40", subbasins.dir.i)
      #remove those from subbasins.dir.i and subbasins.dir.i.long
      subbasins.dir.i <- subbasins.dir.i[-ind.oso.subbasins]
      subbasins.dir.i.long <- subbasins.dir.i.long[-ind.oso.subbasins]
    }
    
  }
  
  #loop through each subdirecty subbasin folder to get the annual results and percentiles to save in common dataframe
  for(j in 1:length(subbasins.dir.i.long)){
    ##########################
    #Percentiles
    
    #path for file percentiles.all.csv (percentiles for that subbasin)
    percentile.file <- list.files(subbasins.dir.i.long[j], pattern="percentiles.all.csv", full.names = TRUE)

    #read in percentiles
    percentiles.j <- read.csv(percentile.file)
    
    #add in scenario label
    #determine which scenario the file is based on percentile csv name
    current.scenario <- grep("curr.percentiles.all.csv", percentile.file) #to determine if current scenario
    water.con.scenario <- grep("water.con.percentiles.all.csv", percentile.file) #to determine if water conservation scenario
    
    #if scenario is current make scenario label "LSPC Current", else keep it as is
    percentiles.j$source2 <- ifelse(length(current.scenario) > 0, "LSPC\nCurrent", unique(percentiles.j$source2))
    #if scenario is water conservation, make scenario label "LSPC Water Conservation", else keep it as is
    percentiles.j$source2 <- ifelse(length(water.con.scenario) > 0, "LSPC\nWater Con.", unique(percentiles.j$source2))
    #ref keep it as is since it already has LSPC Ref as the source2
    
    #add in subbasin name
    percentiles.j$subbasin.model <- subbasins.dir.i[j]
    
    #save percentiles into output data frame
    percentiles.df.overall <- data.frame(rbind(percentiles.df.overall, percentiles.j))
    
    ##########################
    #Annual Results FFM
    
    #path for file results.all.csv (results for that subbasin)
    results.file <- list.files(subbasins.dir.i.long[j], pattern="results.ffm.all.csv", full.names = TRUE)
    
    #read in results
    results.j <- read.csv(results.file)
    
    #scenario labels were already outputted as type
    
    #add in subbasin name
    results.j$subbasin.model <- subbasins.dir.i[j]
    
    #save results into output data frame
    results.df.overall <- data.frame(rbind(results.df.overall, results.j))
  }
}


#remove first NA rows in output data frames
##omit first row of nA values for 
percentiles.df.overall <- percentiles.df.overall[2:length(percentiles.df.overall$p10),]
results.df.overall <- results.df.overall[2:length(results.df.overall$Year),]

#write csv
percentiles.fname <- paste0(output.dir, "percentiles.FFM.Aliso.Oso.SmallCks.SanJuan.lowflowbias.csv")
write.csv(percentiles.df.overall, file=percentiles.fname, row.names=FALSE)
annual.results.fname <- paste0(output.dir, "annual.results.FFM.Aliso.Oso.SmallCks.SanJuan.lowflowbias.csv")
write.csv(results.df.overall, file=annual.results.fname, row.names=FALSE)

