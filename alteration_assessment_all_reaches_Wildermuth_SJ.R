#Alteraion Assessment - San Juan Creek Wildermuth Model outputs
#this code will loop through each flow output file from GSFlow, match model code with COMID, and evaluate alteration based on statewide ref model
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
#old token:
  #mytoken <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJLcmlzIiwibGFzdE5hbWUiOiJUYW5pZ3VjaGkgUXVhbiIsImVtYWlsIjoia3Jpc3RpbmV0cUBzY2N3cnAub3JnIiwicm9sZSI6IlVTRVIiLCJpYXQiOjE1NzM4NjgwODN9.UJhTioLNNJOxvY_PYb_GIbcMRI_qewjkfYx-usC_7ZA"
mytoken <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJLcmlzIiwibGFzdE5hbWUiOiJUYW5pZ3VjaGkgUXVhbiIsImVtYWlsIjoia3Jpc3RpbmV0cUBzY2N3cnAub3JnIiwicm9sZSI6IlVTRVIiLCJpYXQiOjE2MDM5OTIyNjZ9.-HNCqqu-P_g0l7LLsD2PeUWgMtakzNZMynzVt-7QUcg"

#directories for current Wildermuth (reference based on statewide model)
curr.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200929_Wildermuth_Model_Results/Wildermuth_Model_Results_for_SCCWRP/"

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

#find and replace - in new.subbasinname with nothing, for wildermuth outputs, using actual subbasin name not new one
new.subbasinname <- gsub("-", "", new.subbasinname)
basin_comid_lookup$new.subbasinname <- new.subbasinname




##############################
######loop to run through each subbasin, calc mean daily flow, save daily flow
#Functional flow metric names and labels for plots
filename <- ("L:/CA  E-flows framework_ES/Misc/Functional Flows metrics/functional_flow_metric_modeling/all_metric_def_list_FFMs_v2.csv")
ffm.labels <- read.csv(filename)
ffm.labels$metric <- ffm.labels$flow_metric

#list of file names to loop through
fnames <- list.files(curr.dir, pattern = "\\.csv$")

#empty df for alteration determination and direction
alteration.df.overall <- data.frame(matrix(data=NA, nrow=1, ncol=7))
names(alteration.df.overall) <- c("COMID", "subbasin.model", "subbasin", "ffm", "alteration.status", "alteration.direction", "comid.notes")
#names(alteration.df.overall) <- c("COMID", "subbasin.model", "subbasin", "ffm", "alteration.status", "alteration.direction", "alteration.status.statewide", "alteration.direction.statewide","comid.notes")

ffc.errors <- NA

#test i=3 site that works

for (i in 1:length(fnames)){
  
  #get subbasin data i
  subbasin.model <- gsub(".csv","", fnames[i])
  sub <- basin_comid_lookup[basin_comid_lookup$Subbasin == subbasin.model,] 
  #gage.name <- sub$Gage
  subbasin <- as.character(sub$Subbasin)
  subbasin.new <- basin_comid_lookup$new.subbasinname[i]
  COMID <- sub$COMID_forcalc
  #COMID <- 20348363

  ################################################
  ####LSPC current data
  
  #load in hourly model curriction
  data.curr <- read.csv(paste0(curr.dir,fnames[i]))
  names(data.curr) <- c("date", "flow")
  #format date mm/dd/yyyy
  date <- as.POSIXct(data.curr$date, format = "%Y-%m-%d ")
  data.curr$date <- format(date, "%m/%d/%Y")
  ################
  
  #calc FFMs and alteration for current data
  #create new directory to save ffm outputs
  dir.new <- paste0(curr.dir,"FFMs/",subbasin.model)
  dir.create(dir.new)
  
  #Try catch errors in evaluate alteration, skip iteration
  tryCatch({
    #Run data.currframe through FFC online with my own gage data.curr or model data.curr
    results.curr <- ffcAPIClient::evaluate_alteration(timeseries_df = data.curr, comid = COMID, token = mytoken, plot_output_folder = dir.new)
    #reference percentiles
    ref.percentiles$source2 <- rep("Statewide\nReference", length(ref.percentiles$p10))
    ref.percentiles.wyt <- results.curr$predicted_wyt_percentiles
    #predicted results, LSPC current
    curr.alteration.all <- results.curr$alteration
    curr.percentiles.all <- results.curr$ffc_percentiles
    curr.percentiles.all$source2 <- rep("GSFlow\nCurrent", length(curr.percentiles.all$p10))
    curr.results.ffm.all <- results.curr$ffc_results
    curr.results.ffm.all$type <- "curr"
    curr.drh.data <- results.curr$drh_data.curr
    #write outputs to dir
    write.csv(ref.percentiles, file=paste0(dir.new,"/ref.percentiles.statewide.csv"), row.names=FALSE)
    write.csv(curr.alteration.all, file=paste0(dir.new,"/curr.alteration.statewide.all.csv"), row.names=FALSE)
    write.csv(curr.percentiles.all, file=paste0(dir.new,"/curr.percentiles.all.csv"), row.names=FALSE)
    write.csv(curr.results.ffm.all, file=paste0(dir.new,"/curr.results.ffm.all.csv"), row.names=FALSE)
    write.csv(curr.drh.data, file=paste0(dir.new,"/curr.drh.data.csv"), row.names=FALSE)
    
    #save in overall alteration df
    comid2 <- curr.alteration.all$comid
    subbasin.model2 <- rep(subbasin.model, length(curr.alteration.all$comid))
    subbasin2 <- rep(subbasin.new, length(curr.alteration.all$comid))
    ffm2 <- curr.alteration.all$metric
    alteration.status2 <- curr.alteration.all$status
    alteration.direction2 <- curr.alteration.all$alteration_type
    comid.notes2 <- rep("", length(curr.alteration.all$comid))
    #save in output df
    outdf <- data.frame(cbind(comid2, subbasin.model2, subbasin2, ffm2, alteration.status2, alteration.direction2, comid.notes2))
    names(outdf) <- c("COMID", "subbasin.model", "subbasin", "ffm", "alteration.status", "alteration.direction", "comid.notes")
    #append to overall df
    alteration.df.overall <- rbind(alteration.df.overall, outdf)
    
    }, error = function(e) {
      print(paste0(i, " FFC Error"))
      ffc.errors[i] <- subbasin.model
    #}, warning = function(w) {
      #print(warnings()) #could delete this if not necessary
    })

}


unique(alteration.df.overall$subbasin.model)
ind.ds <- grep("DS", alteration.df.overall$ffm)
alteration.df.overall[ind.ds,]

#Alteration based on flow component:
#if one metric in component is altered, component is considered altered


#alteration directory
alteration.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/"
unique.sites <- unique(alteration.df.overall$subbasin.model)
#join the alteration df with the ffm table
ffm.labels$ffm <- as.character(ffm.labels$metric)
alteration.df.overall$ffm <- as.character(alteration.df.overall$ffm)
unique.ffm <- unique(ffm.labels$ffm)
#join with ffm labels
alteration.df.overall.join <- full_join(alteration.df.overall, ffm.labels, by="ffm")
#write table
fname.alteration <- paste0(alteration.dir,"ffm_alteration.df.overall.join.SanJuan.csv")
write.csv(alteration.df.overall.join, file=fname.alteration, row.names=FALSE)

#remove NA first row
alteration.df.overall.join <- alteration.df.overall.join[2:length(alteration.df.overall.join$COMID),]
#add column for component.alteration
alteration.df.overall.join$component_alteration <- alteration.df.overall.join$alteration.status
alteration.df.overall.join$component_alteration <- gsub("likely_unaltered", NA, alteration.df.overall.join$component_alteration)
alteration.df.overall.join$component_alteration <- gsub("indeterminate", NA, alteration.df.overall.join$component_alteration)

#write table joined with component alteration
fname.alteration2 <- paste0(alteration.dir,"component.alteration.df.overall.join.SanJuan.csv")
write.csv(alteration.df.overall.join, file=fname.alteration2, row.names=FALSE)


#synthesis component alteration
ind.NA <- which(is.na(alteration.df.overall.join$component_alteration))
component.alteration.subset <- alteration.df.overall.join[-ind.NA,]

#subset to only alteration per ffm



#write.csv(alteration.df.overall.join, file="L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/component.alteration.df.overall.join.csv", row.names=FALSE)



