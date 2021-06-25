#Future Climate scenarios - Aliso Creek
  #script to process future climate scenarios (daily, 2030-2060) in comparison to historical (daily, 1989-2005)
  #this code will loop through each flow output file from LSPC, match model code with COMID, and evaluate alteration
  #alteration results will be saved as csvs
#use separate code to run all the low flow bias corrected data since it's saved in different format


#load library
library("devtools")
library("ffcAPIClient")
library("ggplot2")
library("scales")
library("purrr")
library("plyr")
library("tidyverse")
library("filesstrings")

#my token for FFC API Client
mytoken <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJLcmlzIiwibGFzdE5hbWUiOiJUYW5pZ3VjaGkgUXVhbiIsImVtYWlsIjoia3Jpc3RpbmV0cUBzY2N3cnAub3JnIiwicm9sZSI6IlVTRVIiLCJpYXQiOjE1NzM4NjgwODN9.UJhTioLNNJOxvY_PYb_GIbcMRI_qewjkfYx-usC_7ZA"
#set token
set_token(mytoken)

#directory of LSPC future and historical model output results
hist.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201111_Aliso_Climate_Scenario/Historical_WY75-05/"
future.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201111_Aliso_Climate_Scenario/RCP85_WY30-60/"
#within each directory, need to go into subdirs for each GCM
hist.subdir <- list.files(hist.dir, full.names = TRUE)
future.subdir <- list.files(future.dir, full.names = TRUE)
#list of gcm names
gcms <- list.files(future.dir)
#FFM output dir for future climate scenario analysis
output.dir <- "D:/SOC_FlowEcologyStudy/FutureClimateScenarios_06232021"
#create output directory
dir.create(output.dir)
#create new directory to save ffm outputs
dir.ffm <- paste0(output.dir,"/FFMs")
dir.create(dir.ffm)

#read in information on subbasin and COMID
basin_comid_lookup <- read.csv("L:/San Juan WQIP_KTQ/Data/SpatialData/v13_pourpoints_NHD_comids.csv")

#lookuptable to convert subbasin codes for model output subbasin names
subbasin_lookup <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/Old_Runs/191220_Interim_Calibration/site_name_lookupletternumbers.csv")

##############################
#convert basin orig name to outputfile name (model subbasin name)
new.subbasinname <- basin_comid_lookup$Subbasin

for(z in 1:length(subbasin_lookup$Letter)){
  new.subbasinname <- gsub(subbasin_lookup$Letter[z], subbasin_lookup$Number[z], new.subbasinname)
}

#find and replace - in new.subbasinname with nothing, make consistent with file name
new.subbasinname <- gsub("-", "", new.subbasinname)
basin_comid_lookup$new.subbasinname <- new.subbasinname

##############################
######loop to run through each subbasin, format flow for FFC and run in FFC
#Functional flow metric names and labels for plots
filename <- ("L:/CA  E-flows framework_ES/Misc/Functional Flows metrics/functional_flow_metric_modeling/all_metric_def_list_FFMs_v2.csv")
ffm.labels <- read.csv(filename)
ffm.labels$metric <- ffm.labels$flow_metric

#list of file names to loop through
ex.dir <- paste0(hist.dir, "/CanESM2/")
fnames <- list.files(ex.dir, pattern = "\\.out$")
#exclude landuse and stream .out files
fnames <- fnames[fnames!= "landuse.out"]
fnames <- fnames[fnames!= "stream.out"]
#exclude old file
fnames <- fnames[fnames!= "201080_old.out"]
#fnames.ref <- list.files(hist.dir, pattern = "\\.out$", full.names=TRUE)

#empty df for alteration determination and direction
alteration.df.overall <- data.frame(matrix(data=NA, nrow=1, ncol=10))
names(alteration.df.overall) <- c("COMID", "subbasin.model", "subbasin", "ffm", "alteration.status", "alteration.direction", "alteration.status.statewide", "alteration.direction.statewide","comid.notes", "gcm")

#empty df for percentiles under various gcms and time periods - can use this to calculate change in percentiles from historical to mid-century
percentiles.df.overall <- data.frame(matrix(data=NA, nrow=1, ncol=12))
names(percentiles.df.overall) <- c("p10", "p25", "p50", "p75", "p90", "metric", "comid", "result_type", "source2", "gcm", "timeperiod", "subbasin.model")

#empty vector of missing COMID from stream_class_data
missing.comid.streamclass <- NA
missing.comid.streamclass.subbasinmodel <- NA


#update to loop through diff subdirs for each gcm 
for(k in 1:length(future.subdir)){
  #set working directory to hist.subdir[k]
  setwd(future.subdir[k])
  #gcm k
  gcm.k <- gcms[k]
  
  ###########################################################################
  #First save the low flow bias corrected data to replace the original files, save original as not corrected
  #list files in directory
  dir.files <- list.files(future.subdir[k])
  dir.files.long <- list.files(future.subdir[k], full.name = TRUE)

  ##UPDATE: replace 201080 with 201079 if 201079 file still exists, rename 201080 original to old file
  if(length(grep("201079", dir.files)) > 0 ){
    #old filenames
    old.name <- paste0(future.subdir[k], "/201080.out")
    new.name <- paste0(future.subdir[k], "/201080_old.out")
    #rename 201080 to 201080_old for curr and ref
    file.rename(old.name, new.name)
    #rename 201079 to 201080 curr and ref
    file.rename(paste0(future.subdir[k], "/201079.out"), paste0(future.subdir[k], "/201080.out"))
  }
  
  ###########################################################################
  
  
  #loop through each subbasin output file for gcm.k
  for (i in 1:length(fnames)){
    
    #get subbasin data i
    subbasin.model <- gsub(".out","", fnames[i])
    sub <- basin_comid_lookup[basin_comid_lookup$new.subbasinname == subbasin.model,] 
    gage.name <- sub$Gage
    subbasin <- as.character(sub$Subbasin)
    COMID <- sub$COMID_forcalc
    #find if COMID is missing from stream_class_data
    ind.comid <- grep(COMID, stream_class_data$COMID)
    #if length of ind.comid == 0 then add new row with COMID and "RGW"
    if(length(ind.comid) == 0){
      #save missing COMID and associated subbasin
      missing.comid.streamclass <- c(missing.comid.streamclass, COMID)
      missing.comid.streamclass.subbasinmodel <- c(missing.comid.streamclass.subbasinmodel, subbasin.model)
      #change comid to one that works with RGW
      COMID <- 20350507 #a comid that works that is also RGW
    }
    
    #number of rows to skip in output files
    skip=24
    
    ################################################
    ####LSPC future data
    
    #file name to read in
    
    #load in daily model prediction
    future <- read.table(fnames[i], skip=skip)
    names(future) <- c("gage", "year", "month", "day", "hour", "min", "precip", "depth", "hyd.radius", "av.vel","flow.cfs")
    #format date
    date <- paste(future$month, future$day, future$year, sep="/")
    #format date mm/dd/yyyy
    date2 <- as.POSIXct(date, format = "%m/%d/%Y")
    future$date <- format(date2, "%m/%d/%Y")
    #format q to be numeric
    future$flow.cfs <- as.numeric(as.character(future$flow.cfs))
    ################
    
    #create new data frame with date and daily flow to go into FFC
    data.future <- data.frame(cbind(date, future$flow.cfs))
    names(data.future) <- c("date", "flow")
    ################
    
    #calc FFMs and alteration for future data
    #create new directory to save ffm outputs
    dir.new <- paste0(output.dir,"/FFMs/",subbasin.model)
    dir.create(dir.new)

    #new FFC api set up
    results.future <- FFCProcessor$new()  # make a new object we can use to run the commands
    #setup
    results.future$set_up(timeseries=data.future,
                        token=mytoken,
                        comid = COMID)
    #set to "RGW" stream class for all --> every COMID is in this class in SOC
    results.future$stream_class <- "RGW"
    
    #then run
    results.future$run()
    
    #reference percentiles
    ref.percentiles <- results.future$predicted_percentiles
    ref.percentiles$source2 <- rep("Statewide\nReference", length(ref.percentiles$p10))
    ref.percentiles.wyt <- results.future$predicted_wyt_percentiles
    #predicted results, LSPC future
    future.alteration.all <- results.future$alteration
    future.percentiles.all <- results.future$ffc_percentiles
    future.label <- paste0("LSPC\n", gcm.k, "\nMid-Century")
    future.percentiles.all$source2 <- rep(future.label, length(future.percentiles.all$p10))
    future.results.ffm.all <- results.future$ffc_results
    future.results.ffm.all$type <- "future"
    future.drh.data <- results.future$drh_data.future
    #write outputs to dir
    write.csv(ref.percentiles, file=paste0(dir.new,"/ref.percentiles.statewide.csv"), row.names=FALSE)
    write.csv(future.alteration.all, file=paste0(dir.new,"/future.alteration.statewide.all.", gcm.k,".csv"), row.names=FALSE)
    write.csv(future.percentiles.all, file=paste0(dir.new,"/future.percentiles.all.", gcm.k,".csv"), row.names=FALSE)
    write.csv(future.results.ffm.all, file=paste0(dir.new,"/future.results.ffm.all.", gcm.k,".csv"), row.names=FALSE)
    write.csv(future.drh.data, file=paste0(dir.new,"/future.drh.data.", gcm.k,".csv"), row.names=FALSE)
    
    #save future percentiles in overall df
    #add column for "gcm", "timeperiod", "subbasin.model"
    future.percentiles.all$gcm <- gcm.k
    future.percentiles.all$timeperiod <- "future"
    future.percentiles.all$subbasin.model <- subbasin.model
    #save in overall df
    percentiles.df.overall <- data.frame(rbind(percentiles.df.overall, future.percentiles.all))
    
    ################################################
    ####LSPC historical data in comparison to future climate run######
    
    #load in historical LSPC model for same subbasin.model
    #read in historical data
    historical <- read.table(paste0(hist.subdir[k],"/", fnames[i]), skip=skip)
    names(historical) <- c("gage", "year", "month", "day", "hour", "min", "precip", "depth", "hyd.radius", "av.vel","flow.cfs")
    #format date
    date3 <- paste(historical$month, historical$day, historical$year, sep="/")
    #format date mm/dd/yyyy
    date4 <- as.POSIXct(date3, format = "%m/%d/%Y")
    historical$date <- format(date4, "%m/%d/%Y")
    #format q to be numeric
    historical$flow.cfs <- as.numeric(as.character(historical$flow.cfs))

    #create new data frame with date and mean daily flow to go into FFC
    data.historical <- data.frame(cbind(historical$date, historical$flow.cfs))
    names(data.historical) <- c("date", "flow")
    
    ################
    #calc FFMs and alteration for historical data
    #save output in same directory 

    #new FFC api set up
    results.historical <- FFCProcessor$new()  # make a new object we can use to run the commands
    #setup
    results.historical$set_up(timeseries=data.historical,
                          token=mytoken,
                          comid = COMID)
    #then run
    results.historical$run()
    
    #save output df
    historical.alteration.all <- results.historical$alteration #using statewide percentiles
    historical.percentiles.all <- results.historical$ffc_percentiles
    historical.label <- paste0("LSCP\n", gcm.k, "\nHistorical")
    historical.percentiles.all$source2 <- historical.label
    historical.results.ffm.all <- results.historical$ffc_results
    historical.drh.data <- results.historical$drh_data
    
    #write outputs to dir
    write.csv(historical.alteration.all, file=paste0(dir.new,"/historical.alteration.statewide.all.csv"), row.names=FALSE)
    write.csv(historical.percentiles.all, file=paste0(dir.new,"/historical.percentiles.all.", gcm.k,".csv"), row.names=FALSE)
    write.csv(historical.results.ffm.all, file=paste0(dir.new,"/historical.results.ffm.all.", gcm.k, ".csv"), row.names=FALSE)
    write.csv(historical.drh.data, file=paste0(dir.new,"/historical.drh.data.",  gcm.k, ".csv"), row.names=FALSE)
    
    #save historical percentiles in overall df
    #add column for "gcm", "timeperiod", "subbasin.model"
    historical.percentiles.all$gcm <- gcm.k
    historical.percentiles.all$timeperiod <- "historical"
    historical.percentiles.all$subbasin.model <- subbasin.model
    #save in overall df
    percentiles.df.overall <- data.frame(rbind(percentiles.df.overall, historical.percentiles.all))
    
    ############################
    ###Alteration Determination between future and historical LSPC
    
    #Loop to determine alteration status and direction of alteration for every ffm
    ffm <- as.character(ffm.labels$metric)
    alteration.status <- NA
    alteration.direction <- NA
    alteration.status.statewide <- NA
    alteration.direction.statewide <- NA
    
    for(l in 1:length(ffm)){
      #need to fix this area! if statewide his NA could change 247 chunk if you address this in the front end
      #statewide historical model alteration status and direction for ffm i 
      #if no data then what?
      if(length(future.alteration.all$status[future.alteration.all$metric == ffm[l]]) < 1){
        alteration.status.statewide[l] <- NA
        alteration.direction.statewide[l] <- NA
      }else{
        alteration.status.statewide[l] <- future.alteration.all$status[future.alteration.all$metric == ffm[l]]
        alteration.direction.statewide[l] <- future.alteration.all$alteration_type[future.alteration.all$metric == ffm[l]]
      }
      
      #model results and median for ffm i
      ind.ffm.l <- grep(ffm[l], names(future.results.ffm.all))
      model.future.ffms.i <- as.numeric(as.character(future.results.ffm.all[,ind.ffm.l]))
      model.future.i.med <- future.percentiles.all$p50[future.percentiles.all$metric==ffm[l]]
      #find 10-90th historical percentiles
      model.historical.i.90 <-  historical.percentiles.all$p90[historical.percentiles.all$metric == ffm[l]]
      model.historical.i.10 <-  historical.percentiles.all$p10[historical.percentiles.all$metric == ffm[l]]
      
      #if NA values for future or historical ffms, then all are NA
      if(is.na(model.future.i.med) |  is.na(model.historical.i.90)){
        alteration.status[l] <- NA
        alteration.direction[l] <- NA
      }else{
        
        #if median falls outside of 10-90, likely altered
        if(model.future.i.med > model.historical.i.90 | model.future.i.med < model.historical.i.10){
          alteration.status[l] <- "likely_altered"
          #if it is altered determine direction of alteration high or low
          if(model.future.i.med > model.historical.i.90){
            alteration.direction[l] <- "high"
          }else{
            alteration.direction[l] <- "low"
          }
        }else{
          #if median falls within 10-90th and >50% falls within the range, then indeterminate or <50% falls within range (likely unaltered)
          #since not altered, no alteration direction
          alteration.direction[l] <- "none_found"
          #determine how many values fall in the range
          count.in.range <- length(which(model.future.ffms.i <= model.historical.i.90 & model.future.ffms.i >= model.historical.i.10))
          percent.inrange <- count.in.range/length(na.omit(model.future.ffms.i))
          
          if(percent.inrange > 0.5){
            alteration.status[l] <- "likely_unaltered"
            
          }else{
            alteration.status[l] <- "indeterminate"
          }
        }
      }
    }
    
    #save into alteration data frame
    comid.notes <- as.character(sub$Notes)
    alteration.comparison.df <- data.frame(cbind(COMID, subbasin.model, subbasin, ffm, alteration.status, alteration.direction, alteration.status.statewide, alteration.direction.statewide,comid.notes))
    #add gcm column
    alteration.comparison.df$gcm <- gcm.k
    write.csv(alteration.comparison.df, file=paste0(dir.new,"/",subbasin.model, "_alteration_comparison_future_historical_statewide", gcm.k,".csv"),row.names=FALSE)
    
    #save alteration in overall df
    alteration.df.overall <- data.frame(rbind(alteration.df.overall, alteration.comparison.df))
    
    ############################
    ###Boxplot comparisons of entire POR gcm future, historical, statewide ref
    #create combined boxplots for each component
    unique.components <- as.character(unique(ffm.labels$title_component))
    for(m in 1:length(unique.components)){
      #subset percentiles based on component m
      ind.comp.m <- grep(unique.components[m], ffm.labels$title_component)
      ffm.names.m <- ffm.labels[ind.comp.m,]
      sub.future.comp <- future.percentiles.all[as.character(future.percentiles.all$metric) %in%  as.character(ffm.names.m$flow_metric),]
      sub.historical.lspc.comp <- historical.percentiles.all [as.character(historical.percentiles.all $metric) %in%  as.character(ffm.names.m$flow_metric),]
      sub.ref.statewide.comp <- ref.percentiles[as.character(ref.percentiles$metric) %in%  as.character(ffm.names.m$flow_metric),]
      #add in gcm, timeperiod, and subbasin.model
      sub.ref.statewide.comp$gcm <- "NA"
      sub.ref.statewide.comp$timeperiod <- "statewide ref"
      sub.ref.statewide.comp$subbasin.model <- subbasin.model
      
      
      
      #Boxplots for components
      title <- as.character(ffm.names.m$title_component[1]) #component
      subtitle.bp <- paste0("Subbasin ", subbasin)
      characteristic <- sort(as.character(ffm.names.m$flow_characteristic)) 
      metrics.title <- sort(as.character(ffm.names.m$title_ffm)) #boxplot title
      
      #combine all percentiles dataframes and merge metric label names
      mergeCols <- names(future.percentiles.all)
      percentiles.cbind.all <- full_join(sub.future.comp, sub.historical.lspc.comp, by=mergeCols) %>% 
        full_join(sub.ref.statewide.comp, by=mergeCols) %>% 
        merge(ffm.labels, by="metric")
      #save as factor and set levels for source2
      percentiles.cbind.all$source2 <- factor(percentiles.cbind.all$source2, levels = c(future.label,historical.label,"Statewide\nReference"))
      
      #subset percentiles for component only
      percentiles.cbind.all.sub.m <- percentiles.cbind.all[percentiles.cbind.all$metric %in% as.character(ffm.names.m$flow_metric),] #%>%
      
      #if peak flow plots, create boxplots in order of increasing magnitude (not based on alphabetical order)
      if(ffm.labels$flow_component[m] == "Peak Flow"){
        #fill color based on entire POR LSPC (add colored points), Gage, historical
        fill<- percentiles.cbind.all.sub.m$source2
        #reorder peak metric plots
        if(unique.components[m] == "Peak Flow Magnitude"){
          percentiles.cbind.all.sub.m$title_ffm <- factor(as.character(percentiles.cbind.all.sub.m$title_ffm), levels = c(" Magnitude (2-year flood, cfs)", " Magnitude (5-year flood, cfs)", " Magnitude (10-year flood, cfs)"))
        }
        if(unique.components[m] == "Peak Flow Duration"){
          percentiles.cbind.all.sub.m$title_ffm <- factor(as.character(percentiles.cbind.all.sub.m$title_ffm), levels = c(" Duration (2-year flood, days)", " Duration (5-year flood, days)", " Duration (10-year flood, days)"))
        }
        if(unique.components[m] == "Peak Flow Frequency"){
          percentiles.cbind.all.sub.m$title_ffm <- factor(as.character(percentiles.cbind.all.sub.m$title_ffm), levels = c(" Frequency (2-year flood)", " Frequency (5-year flood)", " Frequency (10-year flood)"))
        }
        #All years plots
        P<- ggplot(percentiles.cbind.all.sub.m, aes(x=source2, ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90, fill=source2)) +
          geom_boxplot(stat = "identity") +  facet_wrap(~title_ffm, scales="free") +
          scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a")) +
          labs(title=title,x ="", y = "", subtitle = subtitle.bp) 
        #print(P)
        #save as jpg
        component.2 <- gsub(" ", "_", title)
        plot.fname <- paste0(dir.new,"/",component.2, "_boxplots.jpeg")
        ggsave(plot.fname)
        
      }else{
        #fill color based on entire POR LSPC (add colored points), Gage, historical
        fill<- percentiles.cbind.all.sub.m$source2
        #All years plots
        P<- ggplot(percentiles.cbind.all.sub.m, aes(x=source2, ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90, fill=source2)) +
          geom_boxplot(stat = "identity") +  facet_wrap(~title_ffm, scales="free") +
          scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a")) +
          labs(title=title,x ="", y = "", subtitle = subtitle.bp) 
        #print(P)
        #save as jpg
        component.2 <- gsub(" ", "_", title)
        plot.fname <- paste0(dir.new,"/",component.2, "_boxplots_", gcm.k,".jpeg")
        ggsave(plot.fname)
        
      }
    }
  }
}


##omit first row of nA values for 
alteration.df.overall <- alteration.df.overall[2:length(alteration.df.overall$p10),]
percentiles.df.overall <- percentiles.df.overall[2:length(percentiles.df.overall$p10),]

#write csv
alteration.fname <- paste0(dir.ffm, "/alteration.FFM.future.climate.all.scenarios.csv")
write.csv(alteration.df.overall, file=alteration.fname)
percentiles.fname <- paste0(dir.ffm, "/percentiles.FFM.future.climate.all.scenarios.csv")
write.csv(percentiles.df.overall, file=percentiles.fname)
