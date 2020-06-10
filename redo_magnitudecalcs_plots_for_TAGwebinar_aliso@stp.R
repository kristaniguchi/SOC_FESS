#Calculate magnitude FFMs based on observed time window for that season for TAG webinar 6/6/2020


#read in ffm results for observed gage aliso @stp to get time window
obs.ffm <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200326_Gauge_Data/daily/FFMs/201020/obs.ffm.csv")
pred.ffm <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200411_Updated_Calibration/WY94-Present/daily/FFMs/201020/curr.results.ffm.all.csv")

#read in flow data for lspc aliso at stp
pred.flow <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200411_Updated_Calibration/WY94-Present/daily/FFMs/201020/annual_flow_matrix.csv", check.names = FALSE)


#Gage info, saved in previous calibration folder
gage <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200312_Updated_Calibration/WY15-19_Calibration_Assessment/WY15-19_Statistical_Summary_v22M_KTQ.csv", skip=5, header=TRUE)
gage.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200326_Gauge_Data/"
#only use first 3 rows of gage info
gage <- gage[1:3,]
#all gage data
gage.files <- list.files(gage.dir, full.names=TRUE)



#loop to calc DS_Mag_50, DS_Mag_90, Wet_BFL_Mag_10, Wet_BFL_Mag_50 using the start timing and duration for dry and wet season
overlap_years <- 2003:2018
DS_Mag_50 <- NA
DS_Mag_90 <- NA
Wet_BFL_Mag_10 <- NA
Wet_BFL_Mag_50 <- NA

for(i in 1:length(overlap_years)){
  
  #subset to year i column
  ind.col <- grep(overlap_years[i], names(pred.flow))
  sub <- pred.flow[,ind.col]
  
  #subset obs ffm to appropriate wy
  ind.row <- grep(overlap_years[i], obs.ffm$Year)
  obs.sub.ffm <- obs.ffm[ind.row,]
  
  #if dry season timing NA, skip
  if(is.na(obs.sub.ffm$DS_Tim)){
    DS_Mag_50[i] <- NA
    DS_Mag_90[i] <- NA
    
  }else{
    #dry season magnitude
    #subset to dry season timing/duration based on gage
    #find the dry season start and use duration to calc end time
    ds.start <- obs.sub.ffm$DS_Tim
    ds.end <- length(sub)
    dry <- sub[ds.start:ds.end]
    #calc 50th and 90th percentile flow
    DS_percentiles <- quantile(dry, c(.50, .90), na.rm=TRUE) 
    DS_Mag_50[i] <- DS_percentiles[1]
    DS_Mag_90[i] <- DS_percentiles[2]
    
    #dry season magnitude
    #subset to dry season timing/duration based on gage
    #find the dry season start and use duration to calc end time
    ds.start <- obs.sub.ffm$DS_Tim
    ds.end <- length(sub)
    dry <- sub[ds.start:ds.end]
    #calc 50th and 90th percentile flow
    DS_percentiles <- quantile(dry, c(.50, .90), na.rm=TRUE) 
    DS_Mag_50[i] <- DS_percentiles[1]
    DS_Mag_90[i] <- DS_percentiles[2]
  }
  
  #if wet season timing NA, skip
  if(is.na(obs.sub.ffm$Wet_Tim)){
    Wet_BFL_Mag_10[i] <- NA
    Wet_BFL_Mag_50[i] <- NA
  
  }else{
    #wet season magnitude
    #subset to wet season timing/duration based on gage
    #find the wet season start and use duration to calc end time
    Wet_BFL.start <- obs.sub.ffm$Wet_Tim
    Wet_BFL.end <- Wet_BFL.start + obs.sub.ffm$Wet_BFL_Dur
    wet <- sub[Wet_BFL.start:Wet_BFL.end]
    #calc 10th and 50th percentile flow
    Wet_BFL_percentiles <- quantile(wet, c(.10, .50), na.rm=TRUE) 
    Wet_BFL_Mag_10[i] <- Wet_BFL_percentiles[1]
    Wet_BFL_Mag_50[i] <- Wet_BFL_percentiles[2]
  }
}

new.mag.metrics <- data.frame(cbind(overlap_years, DS_Mag_50, DS_Mag_90, Wet_BFL_Mag_10, Wet_BFL_Mag_50))
new.mag.metrics$Year <- new.mag.metrics$overlap_years





#############
#Create plots of updated flow metrics

#Functional flow metric names and labels for plots
filename <- ("L:/CA  E-flows framework_ES/Misc/Functional Flows metrics/functional_flow_metric_modeling/all_metric_def_list_FFMs_v2.csv")
ffm.labels <- read.csv(filename)
ffm.labels$metric <- ffm.labels$flow_metric


#add WYT to annual results
#read in WYT for COMID, for now will use WYT based on sulfur creek
ind.wyt.file <- grep( paste0("COMID", COMID), gage.files)
wyt.comid <- read.csv(gage.files[ind.wyt.file])
#rename year col
names(wyt.comid)[names(wyt.comid)=="year"] <- "Year"

#read in WYT for sulfur creek rain gage: has current dates after 2014
ind.sulfur <- grep( "WYT_sulfurcreek_raingage_1975_2019", gage.files)
wyt.precip.sulfur <- read.csv(gage.files[ind.sulfur])
#rename year col
names(wyt.precip.sulfur)[names(wyt.precip.sulfur)=="WY"] <- "Year"

#add in WYT for sulfur creek to obs gage and pred LSPC results
obs.ffmwyt.comid <- data.frame(obs.ffm$Year)
#add WYT comid
obs.ffm <- merge(obs.ffm, wyt.precip.sulfur[,c("Year","WYT_Gage")], by="Year")
new.mag.metrics <- merge(new.mag.metrics, wyt.precip.sulfur[,c("Year","WYT_Gage")], by="Year")


################################################
#####create plots pred vs. obs FFMs#####

#subset ffm results to overlapping years
sub.obs <- obs.ffm[2:17,c("Year","DS_Mag_50","DS_Mag_90","Wet_BFL_Mag_10", "Wet_BFL_Mag_50", "WYT_Gage")]
sub.pred <- new.mag.metrics[1:16,c(1,3:7)]

ffm.names <- names(sub.pred)
#color points based on ><2015, >=2015
  pre.2015.lab <- "2003-2014"
  post.2014.lab <- "2015-2018"
  #label time period
  ind.2014 <- grep(2014, sub.obs$Year)
  label.years <- c(rep(pre.2015.lab, ind.2014), rep(post.2014.lab, (length(sub.obs$Year)-ind.2014)))


#loop to plot predicted vs obs for each ffm 
for(l in 2:5){
  #get flow metric and labels/title for plots
  ffm <- ffm.names[l]
  #find index of ffm in lookup table
  index.ffm <- grep(ffm, ffm.labels$flow_metric)

    #from lookup table find title and axes labels
    x.name <- paste0("Observed", ffm.labels$title_ffm[index.ffm])
    y.name <- paste0("Predicted", ffm.labels$title_ffm[index.ffm])
    title <- ffm.labels$title_name[index.ffm]

      #set xlim and ylim to be equal axes
      subset <- na.omit(cbind(sub.obs[,l],sub.pred[,l]))
      limits <- range(subset)
      #plot
      plot <- ggplot(data = data.frame(x = sub.obs[,l], y=sub.pred[,l], wyt = sub.pred$WYT_Gage, timeframe = label.years)) + 
        geom_point(mapping = aes(x = x, y = y, col=timeframe, shape = wyt, size=.5)) +
        labs(x = x.name, y= y.name, subtitle = "Aliso Creek @ STP", title = title) + 
        xlim(limits) + ylim(limits) +
        scale_size(guide=FALSE) + 
        scale_color_manual(values=c("#e66101", "#5e3c99")) + 
        theme(legend.title = element_blank(), legend.position = "bottom", legend.text= element_text(size=10)) +
        guides(colour=guide_legend(override.aes = list(size = 4))) + geom_abline()
      
    #print plots to screen
    print(plot)
}

