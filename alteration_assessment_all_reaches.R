#Alteration assessment for all stream reaches from LSPC model
  #this code will loop through each flow output file from LSPC, match model code with COMID, and evaluate alteration
  #alteration results will be saved as csvs

#load library
library("devtools")
library("ffcAPIClient")
library("ggplot2")
library("scales")
library("purrr")
library("plyr")
library("tidyverse")

#my token for FFC API Client
mytoken <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJLcmlzIiwibGFzdE5hbWUiOiJUYW5pZ3VjaGkgUXVhbiIsImVtYWlsIjoia3Jpc3RpbmV0cUBzY2N3cnAub3JnIiwicm9sZSI6IlVTRVIiLCJpYXQiOjE1NzM4NjgwODN9.UJhTioLNNJOxvY_PYb_GIbcMRI_qewjkfYx-usC_7ZA"

#directories for current LSCP, reference LSPC
curr.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200411_Updated_Calibration/WY94-Present/"
ref.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200422_Predevelopment_Reference_Condition/WY94-Present/"

#read in information on subbasin and COMID
basin_comid_lookup <- read.csv("L:/San Juan WQIP_KTQ/Data/SpatialData/v13_pourpoints_NHD_comids.csv")
#lookuptable to convert subbasin codes for model output subbasin names
subbasin_lookup <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/191220_Interim_Calibration/site_name_lookupletternumbers.csv")

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
#####CREATE new files where two subbasins are added together to represent downstream COMID 
#for additive subbasins, create new basin name and add those files together for current and ref
additive <- basin_comid_lookup$new.subbasinname[basin_comid_lookup$Additive != ""]
#create new subbasin name
new.name <- paste0(additive[1],"+", additive[2])
#create new row for additive subbasins
new.row <- basin_comid_lookup[basin_comid_lookup$new.subbasinname == additive[1],]
new.row$new.subbasinname <- new.name
#omit the additive subbasins from table
#add in the new row
basin_comid_lookup <- rbind(basin_comid_lookup, new.row)

#add in two basin files for combined 205010+201060
#read in current conditions
fname1 <- paste0(curr.dir, "205010.out")
fname2 <- paste0(curr.dir, "201060.out")
#read in 2 table to be added together
dat1 <- read.table(fname1, skip=23)
dat2 <- read.table(fname2, skip=23) 
#add two together (column 11 is q but add all)
new.dat <- dat1
for(y in 8:11){
  new.dat[,y] <- dat1[,y] + dat2[,y]
}
#change reach name for new.dat
new.dat$V1 <- rep("205010+201060",length(new.dat$V1))
#write combined file, rember to not skipe rows when reading this file in
write.table(new.dat, paste0(curr.dir, "205010+201060.out"), row.names= FALSE)

#reference models:
#read in current conditions
fname3 <- paste0(ref.dir, "205010.out")
fname4 <- paste0(ref.dir, "201060.out")
#read in 2 table to be added together
dat3 <- read.table(fname3, skip=23)
dat4 <- read.table(fname4, skip=23) 
#add two together (column 11 is q but add all)
new.dat2 <- dat3
for(j in 8:11){
  new.dat2[,j ] <- dat3[,j ] + dat4[,j ]
}
#change reach name for new.dat
new.dat2$V1 <- rep("205010+201060",length(new.dat2$V1))
#write combined file, rember to not skipe rows when reading this file in
write.table(new.dat2, paste0(ref.dir, "205010+201060.out"), row.names= FALSE)




##############################
######loop to run through each subbasin, calc mean daily flow, save daily flow
#Functional flow metric names and labels for plots
filename <- ("L:/CA  E-flows framework_ES/Misc/Functional Flows metrics/functional_flow_metric_modeling/all_metric_def_list_FFMs_v2.csv")
ffm.labels <- read.csv(filename)
ffm.labels$metric <- ffm.labels$flow_metric

#list of file names to loop through
fnames <- list.files(curr.dir, pattern = "\\.out$")
#exclude
fnames <- fnames[fnames!= "exclude_201040+204010.out"]
fnames.ref <- list.files(ref.dir, pattern = "\\.out$", full.names=TRUE)

#empty df for alteration determination and direction
alteration.df.overall <- data.frame(matrix(data=NA, nrow=1, ncol=9))
names(alteration.df.overall) <- c("COMID", "subbasin.model", "subbasin", "ffm", "alteration.status", "alteration.direction", "alteration.status.statewide", "alteration.direction.statewide","comid.notes")

#for (i in 1:3){
for (i in 8:length(fnames)){
    
  #get subbasin data i
  subbasin.model <- gsub(".out","", fnames[i])
  sub <- basin_comid_lookup[basin_comid_lookup$new.subbasinname == subbasin.model,] 
  gage.name <- sub$Gage
  subbasin <- as.character(sub$Subbasin)
  COMID <- sub$COMID_forcalc
  
  #when reading in data, skip 23 rows, else skip 0
  if(fnames[i]=="205010+201060.out"){
    skip=0
  }else{skip = 23}
  
  ################################################
  ####LSPC current data
  
  #load in hourly model curriction
  curr <- read.table(paste0(curr.dir,fnames[i]), skip=skip)
  names(curr) <- c("gage", "year", "month", "day", "hour", "min", "precip", "depth", "hyd.radius", "av.vel","flow.cfs")
  #format date
  date <- paste(curr$month, curr$day, curr$year, sep="/")
  curr$date <- date
  unique.dates <- unique(date)
  ################
  
  #calc mean daily flow for curricted data
  #empty mean daily flow vector
  flow.curr <- NA
  
  for (j in 1:length(unique.dates)){
    sub.day <- curr[curr$date == unique.dates[j],]
    flow.curr[j] <- mean(sub.day$flow.cfs)
  }
  #create new data frame with date and mean daily flow to go into FFC
  data.curr <- data.frame(cbind(unique.dates, flow.curr))
  names(data.curr) <- c("date", "flow")
  #write daily output file
  fname <- paste0(curr.dir,"daily/", subbasin.model,"_curr_daily.txt")
  write.table(data.curr, fname, row.names = FALSE, sep = ",")
  ################
  
  #calc FFMs and alteration for current data
  #create new directory to save ffm outputs
  dir.new <- paste0(curr.dir,"daily/FFMs/",subbasin.model)
  dir.create(dir.new)
  #Run data.currframe through FFC online with my own gage data.curr or model data.curr
  results.curr <- ffcAPIClient::evaluate_alteration(timeseries_df = data.curr, comid = COMID, token = mytoken)
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
  curr.drh.data <- results.curr$drh_data.curr
  #write outputs to dir
  write.csv(ref.percentiles, file=paste0(dir.new,"/ref.percentiles.statewide.csv"), row.names=FALSE)
  write.csv(curr.alteration.all, file=paste0(dir.new,"/curr.alteration.statewide.all.csv"), row.names=FALSE)
  write.csv(curr.percentiles.all, file=paste0(dir.new,"/curr.percentiles.all.csv"), row.names=FALSE)
  write.csv(curr.results.ffm.all, file=paste0(dir.new,"/curr.results.ffm.all.csv"), row.names=FALSE)
  write.csv(curr.drh.data, file=paste0(dir.new,"/curr.drh.data.csv"), row.names=FALSE)
  
  
  ################################################
  ####LSPC reference data######
  
  #load in reference LSPC model for same subbasin.model
  ind.ref <- grep(paste0("/",fnames[i]), fnames.ref)
  #read in ref data
  ref <- read.table(fnames.ref[ind.ref], skip = skip)
  names(ref) <- c("gage", "year", "month", "day", "hour", "min", "precip", "depth", "hyd.radius", "av.vel","flow.cfs")
  #format date
  date2 <- paste(ref$month, ref$day, ref$year, sep="/")
  ref$date <- date2
  unique.dates2 <- unique(date2)
  ################
  
  #calc mean daily flow for reference lspc
  #empty mean daily flow vector
  flow.ref <- NA
  
  for (k in 1:length(unique.dates2)){
    sub.day2 <- ref[ref$date == unique.dates2[k],]
    flow.ref[k] <- mean(sub.day2$flow.cfs, na.rm=TRUE)
  }
  #if NaN, replace with NA
  flow.ref<- as.numeric(sub("NaN", "NA", flow.ref))
  #create new data frame with date and mean daily flow to go into FFC
  data.ref <- data.frame(cbind(unique.dates2, flow.ref))
  names(data.ref) <- c("date", "flow")
  
  #write daily output file
  fname2 <- paste0(ref.dir,"daily/", subbasin.model,"_ref_daily.txt")
  write.table(data.ref, fname2, row.names = FALSE, sep = ",")
  ################
  #calc FFMs and alteration for referved data
  #create new directory to save ffm outputs
  dir.new2 <- paste0(ref.dir,"daily/FFMs/",subbasin.model)
  dir.create(dir.new2)
  
  #Run dataframe through FFC online with my own gage data or model data
  results.ref <- ffcAPIClient::evaluate_alteration(timeseries_df = data.ref, comid = COMID, token = mytoken)
  #save output df
  ref.alteration.all <- results.ref$alteration #using statewide percentiles
  ref.percentiles.all <- results.ref$ffc_percentiles
  ref.percentiles.all$source2 <- rep("LSCP\nReference", length(ref.percentiles.all$p10))
  ref.results.ffm.all <- results.ref$ffc_results
  ref.results.ffm.all$type <- "ref"
  ref.drh.data <- results.ref$drh_data
  
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
    percentiles.cbind.all <- full_join(sub.curr.comp, sub.ref.lspc.comp, by=mergeCols) %>% 
      full_join(sub.ref.statewide.comp, by=mergeCols) %>% 
      merge(ffm.labels, by="metric")
    #save as factor and set levels for source2
    percentiles.cbind.all$source2 <- factor(percentiles.cbind.all$source2, levels = c("LSPC\nCurrent","LSCP\nReference","Statewide\nReference"))
    
    #subset percentiles for component only
    percentiles.cbind.all.sub.m <- percentiles.cbind.all[percentiles.cbind.all$metric %in% as.character(ffm.names.m$flow_metric),] #%>%
    
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
      P<- ggplot(percentiles.cbind.all.sub.m, aes(x=source2, ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90, fill=source2)) +
        geom_boxplot(stat = "identity") +  facet_wrap(~title_ffm, scales="free") +
        scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a")) +
        labs(title=title,x ="", y = "", subtitle = subtitle.bp) 
      print(P)
      #save as jpg
      component.2 <- gsub(" ", "_", title)
      plot.fname <- paste0(dir.new,"/",component.2, "_boxplots.jpeg")
      ggsave(plot.fname)

    }else{
      #fill color based on entire POR LSPC (add colored points), Gage, Reference
      fill<- percentiles.cbind.all.sub.m$source2
      #All years plots
      P<- ggplot(percentiles.cbind.all.sub.m, aes(x=source2, ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90, fill=source2)) +
        geom_boxplot(stat = "identity") +  facet_wrap(~title_ffm, scales="free") +
        scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a")) +
        labs(title=title,x ="", y = "", subtitle = subtitle.bp) 
      print(P)
      #save as jpg
      component.2 <- gsub(" ", "_", title)
      plot.fname <- paste0(dir.new,"/",component.2, "_boxplots.jpeg")
      ggsave(plot.fname)
      
    }
  }
}






