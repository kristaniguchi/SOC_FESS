#Alteration assessment for all stream reaches from LSPC model - updated recalibration LSPC
  #this code will loop through each flow output file from LSPC, match model code with COMID, and evaluate alteration
  #alteration results will be saved as csvs
#Note: 201080 is updated to 201079 - didn't include English trib, Delete 201080 trib file and replace with 201079
#If COMID is missing from stream type in FFC, hardwired the COMID to a RGW since all should be ran with those parameters, previously used FFC defaults


#load library
library("devtools")
library("ffcAPIClient")
library("ggplot2")
library("scales")
library("purrr")
library("plyr")
library("tidyverse")

#to uninstall package and reinstall (if updates to R package were made)
#install.packages("devtools")
#library("devtools")
#remove.packages("ffcAPIClient") #uninstall then restart R session
#restart R session
#library("devtools")
#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
#install.packages("ffcAPIClient")
#library("ffcAPIClient")


#my token for FFC API Client
mytoken <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJLcmlzIiwibGFzdE5hbWUiOiJUYW5pZ3VjaGkgUXVhbiIsImVtYWlsIjoia3Jpc3RpbmV0cUBzY2N3cnAub3JnIiwicm9sZSI6IlVTRVIiLCJpYXQiOjE1NzM4NjgwODN9.UJhTioLNNJOxvY_PYb_GIbcMRI_qewjkfYx-usC_7ZA"
#set token
set_token(mytoken)

####UPDATE: directories for current LSCP, reference LSPC
#Aliso
curr.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201105_Aliso_Recalibration_Update/Model_Output_WY1993-2019/"
ref.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201105_Aliso_Reference_Condition/WY94-Present/"
#aliso recalibration directory name
alt.dir.name <- "Aliso_RecalibrationUpdate/"

#Oso and other coastal watersheds
#curr.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201118_Oso,_Small_Creeks_Existing_Conditions/Model_Output_WY94-19/"
#ref.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201118_Oso,_Small_Creeks_Reference_Condition/WY94-19/"
#oso recalibration directory name
#alt.dir.name <- "Oso_SmallCreeks/"

#San Juan - LSPC
#curr.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/210422_San_Juan_Existing_Conditions/"
#ref.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/210422_San_Juan_Reference_Condition/"
#San Juan directory name
#alt.dir.name <- "SanJuan_LSPC/"


#alteration directory
alteration.dir <- paste0("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/", alt.dir.name)
#create directory if does not exist
dir.create(alteration.dir)


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

#write.csv(basin_comid_lookup, file="L:/San Juan WQIP_KTQ/Data/SpatialData/SOC_FESS_Subbasin_NHD.COMID_Lookup_Table.csv")


##############################
######loop to run through each subbasin, calc mean daily flow, save daily flow
#Functional flow metric names and labels for plots
filename <- ("L:/CA  E-flows framework_ES/Misc/Functional Flows metrics/functional_flow_metric_modeling/all_metric_def_list_FFMs_v2.csv")
ffm.labels <- read.csv(filename)
ffm.labels$metric <- ffm.labels$flow_metric

#list of file names to loop through
fnames <- list.files(curr.dir, pattern = "\\.out$")

#UPDATE: replace 201080 with 201079 if 2301079 file still exists, rename 201080 original to old file
if(length(grep("201079", fnames)) > 0 ){
  #old filenames
  old.name.curr <- paste0(curr.dir, "201080.out")
  old.name.ref <- paste0(ref.dir, "201080.out")
  #rename 201080 to 201080_old for curr and ref
  file.rename(old.name.curr, paste0(curr.dir, "201080_old.out"))
  file.rename(old.name.ref, paste0(ref.dir, "201080_old.out"))
  #rename 201079 to 201080 curr and ref
  file.rename(paste0(curr.dir, "201079.out"), paste0(curr.dir, "201080.out"))
  file.rename(paste0(ref.dir, "201079.out"), paste0(ref.dir, "201080.out"))
}

#update fname list to loop through, do not loop through old 201080
fnames <- list.files(curr.dir, pattern = "\\.out$")
ind.old <- grep("old", fnames) 
#exclude old if old is still in list
if(length(ind.old) > 0){
  fnames <- fnames[-ind.old]
}

#empty df for alteration determination and direction
alteration.df.overall <- data.frame(matrix(data=NA, nrow=1, ncol=9))
names(alteration.df.overall) <- c("COMID", "subbasin.model", "subbasin", "ffm", "alteration.status", "alteration.direction", "alteration.status.statewide", "alteration.direction.statewide","comid.notes")

#empty vector of missing COMID from stream_class_data
missing.comid.streamclass <- NA
missing.comid.streamclass.subbasinmodel <- NA


#empty vector if FFC error:
ffc.errors <- NA

#for (i in 1:3){
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
  
  
  ################################################
  ####LSPC current data
  #when reading in data, skip 23 rows
  skip = 23
  
  #load in hourly model current predictions
  curr <- read.table(paste0(curr.dir,fnames[i]), skip=skip)
  names(curr) <- c("gage", "year", "month", "day", "hour", "min", "precip", "depth", "av.depth", "av.vel","flow.cfs")
  #format date
  #add leading zero to hour
  MONTH <- sprintf("%02d",curr$month)
  DAY <- sprintf("%02d",curr$day)
  date <- paste(MONTH, DAY, curr$year, sep="/")
  curr$date <- date
  unique.dates <- unique(date)
  #format q to be numeric
  curr$flow.cfs <- as.numeric(as.character(curr$flow.cfs))
  ################
  
  #calc mean daily flow for current predicted data
  mean.daily.curr <- curr %>% 
    group_by(date) %>% 
    summarize(flow = mean(flow.cfs, ra.rm = TRUE)) %>% 
    ungroup()
  
  #create new data frame with date and mean daily flow to go into FFC
  data.curr <- data.frame(cbind(mean.daily.curr$date, mean.daily.curr$flow))
  names(data.curr) <- c("date", "flow")
  data.curr$flow <- as.numeric(data.curr$flow )
  #write daily output file
  fname <- paste0(curr.dir,"daily/", subbasin.model,"_curr_daily.txt")
  if(i == 1){
    dir.create(paste0(curr.dir,"daily/"))
    dir.create(paste0(curr.dir,"daily/FFMs/"))
    dir.create(paste0(ref.dir,"daily/"))
  }
  write.table(data.curr, fname, row.names = FALSE, sep = ",")
  ################
  
  #calc FFMs and alteration for current data
  #create new directory to save ffm outputs
  dir.new <- paste0(curr.dir,"daily/FFMs/",subbasin.model)
  dir.create(dir.new)
  
  #Try catch errors in evaluate alteration, skip iteration
  tryCatch({
    #Run data.currframe through FFC online with my own gage data.curr or model data.curr
    #new FFC api set up
    results.curr <- FFCProcessor$new()  # make a new object we can use to run the commands
    #allow ffc to run with min of 1 years
    results.curr$fail_years_data <- 1
    #setup
    results.curr$set_up(timeseries=data.curr,
                        token=mytoken,
                        comid = COMID)
    #set to "RGW" stream class for all --> every COMID is in this class in SOC
    results.curr$stream_class <- "RGW"
    
    #then run
    results.curr$run()
    
    #results.curr <- ffcAPIClient::evaluate_alteration(timeseries_df = data.curr, comid = COMID, token = mytoken)
    #reference percentiles
    ref.percentiles <- results.curr$predicted_percentiles
    ref.percentiles$source2 <- rep("Statewide\nReference", length(ref.percentiles$p10))
    ref.percentiles.wyt <- results.curr$predicted_wyt_percentiles
    #predicted results, LSPC current
    curr.alteration.all <- results.curr$alteration
    curr.percentiles.all <- results.curr$ffc_percentiles
    curr.percentiles.all$source2 <- rep("LSPC\nCurrent", length(curr.percentiles.all$p10))
    curr.results.ffm.all <- results.curr$ffc_results
    curr.results.ffm.all$type <- "curr"
    curr.doh.data <- results.curr$doh_data
    #write outputs to dir
    write.csv(ref.percentiles, file=paste0(dir.new,"/ref.percentiles.statewide.csv"), row.names=FALSE)
    write.csv(curr.alteration.all, file=paste0(dir.new,"/curr.alteration.statewide.all.csv"), row.names=FALSE)
    write.csv(curr.percentiles.all, file=paste0(dir.new,"/curr.percentiles.all.csv"), row.names=FALSE)
    write.csv(curr.results.ffm.all, file=paste0(dir.new,"/curr.results.ffm.all.csv"), row.names=FALSE)
    write.csv(curr.doh.data, file=paste0(dir.new,"/curr.doh.data.csv"), row.names=FALSE)
    
    
    ################################################
    ####LSPC reference data######
    
    #load in reference LSPC model for same subbasin.model
    #read in ref data
    ref <- read.table(paste0(ref.dir,fnames[i]), skip=skip)
    names(ref) <- c("gage", "year", "month", "day", "hour", "min", "precip", "depth", "av.depth", "av.vel","flow.cfs")
    #format date
    #add leading zero to hour
    MONTH2 <- sprintf("%02d",ref$month)
    DAY2 <- sprintf("%02d",ref$day)
    date2 <- paste(MONTH2, DAY2, ref$year, sep="/")
    ref$date <- date2
    unique.dates2 <- unique(date2)
    #format q to be numeric
    ref$flow.cfs <- as.numeric(as.character(ref$flow.cfs))
    ################
    
    #calc mean daily flow for reference lspc
    #calc mean daily flow for current predicted data
    mean.daily.ref <- ref %>% 
      group_by(date) %>% 
      summarize(flow = mean(flow.cfs, na.rm = TRUE)) %>% 
      ungroup()
    
    #if NaN, replace with NA
    flow.ref<- as.numeric(sub("NaN", "NA", mean.daily.ref$flow))
    #create new data frame with date and mean daily flow to go into FFC
    data.ref <- data.frame(cbind(mean.daily.ref$date, flow.ref))
    names(data.ref) <- c("date", "flow")
    data.ref$flow <- as.numeric(data.ref$flow)
    
    #write daily output file
    fname2 <- paste0(ref.dir,"daily/", subbasin.model,"_ref_daily.txt")
    dir.create(paste0(ref.dir,"daily/", subbasin.model,"/"))
    write.table(data.ref, fname2, row.names = FALSE, sep = ",")
    ################
    #calc FFMs and alteration for referved data
    #create new directory to save ffm outputs
    dir.new2 <- paste0(ref.dir,"daily/FFMs/",subbasin.model)
    dir.create(paste0(ref.dir,"daily/FFMs/"))
    dir.create(dir.new2)
    
    #Run dataframe through FFC online with my own gage data or model data
    #results.ref <- ffcAPIClient::evaluate_alteration(timeseries_df = data.ref, comid = COMID, token = mytoken)
    #new FFC api set up
    results.ref <- FFCProcessor$new()  # make a new object we can use to run the commands
    #allow ffc to run with min of 1 years
    results.ref$fail_years_data <- 1
    #setup
    results.ref$set_up(timeseries=data.ref,
                        token=mytoken,
                        comid = COMID)
    #set to "RGW" stream class for all --> every COMID is in this class in SOC
    results.ref$stream_class <- "RGW"
    #then run
    results.ref$run()
    
    #save output df
    ref.alteration.all <- results.ref$alteration #using statewide percentiles
    ref.percentiles.all <- results.ref$ffc_percentiles
    ref.percentiles.all$source2 <- rep("LSCP\nReference", length(ref.percentiles.all$p10))
    ref.results.ffm.all <- results.ref$ffc_results
    ref.results.ffm.all$type <- "ref"
    ref.drh.data <- results.ref$doh_data
    
    #write outputs to dir
    write.csv(ref.alteration.all, file=paste0(dir.new2,"/ref.alteration.statewide.all.csv"), row.names=FALSE)
    write.csv(ref.percentiles.all, file=paste0(dir.new2,"/ref.percentiles.all.csv"), row.names=FALSE)
    write.csv(ref.results.ffm.all, file=paste0(dir.new2,"/ref.results.ffm.all.csv"), row.names=FALSE)
    write.csv(ref.drh.data, file=paste0(dir.new2,"/ref.drh.data.csv"), row.names=FALSE)
    
    ############################
    ###Alteration Determination between current and ref LSPC
    
    #Loop to determine alteration status and direction of alteration for every ffm
    ffm <- as.character(ffm.labels$metric)
    alteration.status <- NA
    alteration.direction <- NA
    alteration.status.statewide <- NA
    alteration.direction.statewide <- NA
    
    for(l in 1:length(ffm)){
      #need to fix this area! if statewide his NA could change 247 chunk if you address this in the front end
      #statewide ref model alteration status and direction for ffm i 
      #if no data then what?
      if(length(curr.alteration.all$status[curr.alteration.all$metric == ffm[l]]) < 1){
        alteration.status.statewide[l] <- NA
        alteration.direction.statewide[l] <- NA
      }else{
        alteration.status.statewide[l] <- curr.alteration.all$status[curr.alteration.all$metric == ffm[l]]
        alteration.direction.statewide[l] <- curr.alteration.all$alteration_type[curr.alteration.all$metric == ffm[l]]
      }
      
      #model results and median for ffm i
      ind.ffm.l <- grep(ffm[l], names(curr.results.ffm.all))
      model.curr.ffms.i <- as.numeric(as.character(curr.results.ffm.all[,ind.ffm.l]))
      model.curr.i.med <- curr.percentiles.all$p50[curr.percentiles.all$metric==ffm[l]]
      #find 10-90th ref percentiles
      model.ref.i.90 <-  ref.percentiles.all$p90[ref.percentiles.all$metric == ffm[l]]
      model.ref.i.10 <-  ref.percentiles.all$p10[ref.percentiles.all$metric == ffm[l]]
      
      #if NA values for current or reference ffms, then all are NA
      if(is.na(model.curr.i.med) |  is.na(model.ref.i.90)){
        alteration.status[l] <- NA
        alteration.direction[l] <- NA
      }else{
        
        #if median falls outside of 10-90, likely altered
        if(model.curr.i.med > model.ref.i.90 | model.curr.i.med < model.ref.i.10){
          alteration.status[l] <- "likely_altered"
          #if it is altered determine direction of alteration high or low
          if(model.curr.i.med > model.ref.i.90){
            alteration.direction[l] <- "high"
          }else{
            alteration.direction[l] <- "low"
          }
        }else{
          #if median falls within 10-90th and >50% falls within the range, then indeterminate or <50% falls within range (likely unaltered)
          #since not altered, no alteration direction
          alteration.direction[l] <- "none_found"
          #determine how many values fall in the range
          count.in.range <- length(which(model.curr.ffms.i <= model.ref.i.90 & model.curr.ffms.i >= model.ref.i.10))
          percent.inrange <- count.in.range/length(na.omit(model.curr.ffms.i))
          
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
    write.csv(alteration.comparison.df, file=paste0(dir.new,"/",subbasin.model, "_alteration_comparison_lspcref_statewide.csv"),row.names=FALSE)
    
    #save alteration in overall df
    alteration.df.overall <- data.frame(rbind(alteration.df.overall, alteration.comparison.df))
    
    ############################
    ###Boxplot comparisons of entire POR LSPC current, LSPC ref, statewide ref
    #create combined boxplots for each component
    unique.components <- as.character(unique(ffm.labels$title_component))
    for(m in 1:length(unique.components)){
      #subset percentiles based on component m
      ind.comp.m <- grep(unique.components[m], ffm.labels$title_component)
      ffm.names.m <- ffm.labels[ind.comp.m,]
      sub.curr.comp <- curr.percentiles.all[as.character(curr.percentiles.all$metric) %in%  as.character(ffm.names.m$flow_metric),]
      sub.ref.lspc.comp <- ref.percentiles.all [as.character(ref.percentiles.all $metric) %in%  as.character(ffm.names.m$flow_metric),]
      sub.ref.statewide.comp <- ref.percentiles[as.character(ref.percentiles$metric) %in%  as.character(ffm.names.m$flow_metric),]
      
      #Boxplots for components
      title <- as.character(ffm.names.m$title_component[1]) #component
      subtitle.bp <- paste0("Subbasin ", subbasin)
      characteristic <- sort(as.character(ffm.names.m$flow_characteristic)) 
      metrics.title <- sort(as.character(ffm.names.m$title_ffm)) #boxplot title
      
      #combine all percentiles dataframes and merge metric label names
      mergeCols <- names(curr.percentiles.all)
      #combine just LSPC ref and current
      percentiles.cbind.all <- full_join(sub.curr.comp, sub.ref.lspc.comp, by=mergeCols) %>% 
        merge(ffm.labels, by="metric")
      #save as factor and set levels for source2
      percentiles.cbind.all$source2 <- factor(percentiles.cbind.all$source2, levels = c("LSPC\nCurrent","LSCP\nReference"))
      
      #merge with alteration, make red if altered, 
      alteration.comparison.df$metric <- alteration.comparison.df$ffm
      percentiles.cbind.all <-  percentiles.cbind.all %>% 
        merge(alteration.comparison.df, by="metric")
      #replace alteration for reference
      percentiles.cbind.all$alteration.status[percentiles.cbind.all$source2 == "LSCP\nReference"] <- "NA"
      #save names of alteration status, save as factor
      percentiles.cbind.all$alteration.status <- factor(percentiles.cbind.all$alteration.status, levels= c("likely_altered", "likely_unaltered","indeterminate", "NA"))
      #attach colors for each status
      percentiles.cbind.all$colors <- NA
      percentiles.cbind.all$colors[percentiles.cbind.all$alteration.status == "likely_altered"] <- "#cb181d"
      percentiles.cbind.all$colors[percentiles.cbind.all$alteration.status == "likely_unaltered"] <- "#2171b5"
      percentiles.cbind.all$colors[percentiles.cbind.all$alteration.status == "indeterminate"] <- "#f7f7f7"
      percentiles.cbind.all$colors[percentiles.cbind.all$alteration.status == "NA"] <- "#bdbdbd"
      percentiles.cbind.all$colors <- factor(percentiles.cbind.all$colors, levels = c("#cb181d", "#2171b5", "#f7f7f7", "#bdbdbd"))
      
      #list of colors and alteration statuses, color current by alteration status
      colors <- c("#cb181d", "#2171b5", "#f7f7f7", "#bdbdbd")
      status <- c("Likely Altered", "Likely Unaltered", "Indeterminate", "NA")
      lookup <- data.frame(cbind(colors, status))
      
      #subset percentiles for component only
      percentiles.cbind.all.sub.m <- percentiles.cbind.all[percentiles.cbind.all$metric %in% as.character(ffm.names.m$flow_metric),] #%>%
      #subset colors and status
      lookup.sub <- lookup[lookup$colors %in% percentiles.cbind.all.sub.m$colors,]
      
      #if peak flow plots, create boxplots in order of increasing magnitude (not based on alphabetical order)
      if(ffm.labels$flow_component[m] == "Peak Flow"){
        #fill color based on entire POR LSPC (add colored points), Gage, Reference
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
        P<- ggplot(percentiles.cbind.all.sub.m, aes(x=source2, ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90, fill=alteration.status)) +
          geom_boxplot(stat = "identity") +  facet_wrap(~title_ffm, scales="free") +
          scale_fill_manual(name = "Alteration Status", labels = lookup.sub$status, values=lookup.sub$colors) + 
          labs(title=title,x ="", y = "", subtitle = subtitle.bp) 
        #print(P)
        #save as jpg
        component.2 <- gsub(" ", "_", title)
        plot.fname <- paste0(dir.new,"/",component.2, "_boxplots.jpeg")
        ggsave(plot.fname)
        
      }else{
        #fill color based on entire POR LSPC (add colored points), Gage, Reference
        fill<- percentiles.cbind.all.sub.m$source2
        #All years plots
        P<- ggplot(percentiles.cbind.all.sub.m, aes(x=source2, ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90, fill=colors)) +
          geom_boxplot(stat = "identity") +  facet_wrap(~title_ffm, scales="free") +
          scale_fill_manual(name = "Alteration Status", labels = lookup.sub$status, values=lookup.sub$colors) +
          labs(title=title,x ="", y = "", subtitle = subtitle.bp) 
        #print(P)
        #save as jpg
        component.2 <- gsub(" ", "_", title)
        plot.fname <- paste0(dir.new,"/",component.2, "_boxplots.jpeg")
        ggsave(plot.fname)
        
      }
    }
  }, error = function(e) {
    print(paste0(i, " FFC Error"))
    ffc.errors <- c(ffc.errors, subbasin.model)
    #}, warning = function(w) {
    #print(warnings()) #could delete this if not necessary
  })
}


##############################################################################################
#Only rerun this if you do not want to run entire script above but have already done so previously
#loop through all dirs and combine alteration df with overall df (since started at i 19 in loop above)
alteration.df.overall <- data.frame(matrix(data=NA, nrow=1, ncol=9))
names(alteration.df.overall) <- c("COMID", "subbasin.model", "subbasin", "ffm", "alteration.status", "alteration.direction", "alteration.status.statewide", "alteration.direction.statewide","comid.notes")

dir.ffm <- paste0(curr.dir, "daily/FFMs/" )
list <- list.files(dir.ffm, full.names = TRUE)

for(y in 1:length(list)){
  path.open.list <- list.files(list[y], full.names = TRUE)
  ind.file <- grep("_alteration_comparison_lspcref_statewide.csv$", path.open.list)
  if(length(ind.file) > 0){
    file <- read.csv(path.open.list[ind.file])
    #append to overall df
    alteration.df.overall <- data.frame(rbind(alteration.df.overall, file))
  }
}

#backup.alteration.df.overall <- alteration.df.overall
#alteration.df.overall <- backup.alteration.df.overall
#############################################################################################################################################
#component alteration summary
#if one metric is altered, whole component altered

#alteration DF
unique.sites <- unique(alteration.df.overall$subbasin.model)
#if first site is NA, remove first row
if(is.na(unique.sites[1])){
  alteration.df.overall <- alteration.df.overall[2:length(alteration.df.overall$COMID),]
}
#join the alteration df with the ffm table
ffm.labels$ffm <- as.character(ffm.labels$metric)
alteration.df.overall$ffm <- as.character(alteration.df.overall$ffm)
unique.ffm <- unique(ffm.labels$ffm)

alteration.df.overall.join <- full_join(alteration.df.overall, ffm.labels, by="ffm")
#add column for component.alteration
alteration.df.overall.join$component_alteration <- alteration.df.overall.join$alteration.status
alteration.df.overall.join$component_alteration <- gsub("likely_unaltered", NA, alteration.df.overall.join$component_alteration)
alteration.df.overall.join$component_alteration <- gsub("indeterminate", NA, alteration.df.overall.join$component_alteration)

#write overall alteration FFM
write.csv(alteration.df.overall.join, file=paste0(alteration.dir, "ffm_alteration.df.overall.join.csv"), row.names=FALSE)


#synthesis component alteration
ind.NA <- which(is.na(alteration.df.overall.join$component_alteration))
component.alteration.subset <- alteration.df.overall.join[-ind.NA,]
#write component alteration df
write.csv(component.alteration.subset, file=paste0(alteration.dir, "component.alteration.df.overall.join.csv"), row.names=FALSE)





