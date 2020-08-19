#Future Climate analysis - peak flow magnitude alteration evaluation
#calculate the % change in peak flow magnitude

#load library
library(tidyverse)

#directories

#future climate scenario ffm outputs directory to loop through
future.dir <- "W:/SOC_FlowEcologyStudy/FutureClimateScenarios/FFMs/"

#future subdirs
future.subdirs <- list.files(future.dir, full.names = TRUE)

##Functional flow metric names and labels for plots
filename <- ("L:/CA  E-flows framework_ES/Misc/Functional Flows metrics/functional_flow_metric_modeling/all_metric_def_list_FFMs_v2.csv")
ffm.labels <- read.csv(filename)
ffm.labels$metric <- ffm.labels$flow_metric
unique.components <- as.character(unique(ffm.labels$title_component))

#reach name
reach <- list.files(future.dir)
#gcms
gcm <- c("CanESM2", "CCSM4", "CNRM-CM5", "MIROC5")

############################################################
#loop through each reach subdirectory and create a summary table with baseline, future, and %change, gcm

#create empty data frame for baseline, future, %change
peakmag.all <- data.frame(matrix(NA,1,7))
names(peakmag.all) <- c("baseline", "future", "pct_change", "metric", "gcm", "comid", "reach")

for(i in 1:length(future.subdirs)){
  #get the future results and baseline results filenames (all consistent for every reach)
  files.all <- list.files(future.subdirs[i], full.names = TRUE)
  #find index of percentile results all
  ind.file <- grep("_percentiles_ALL_gcms_baseline_future.csv", files.all)
  #read in percentile file:
  percentiles.all <- read.csv(files.all[ind.file])
  
  #filter to Peak_10, Peak_2, Peak_5
  peaks <- filter(percentiles.all, metric == "Peak_10" | metric == "Peak_5" | metric == "Peak_2" )
  #get gcm for each
  peaks$gcm <- sapply(strsplit(peaks$Scenario2,"_"), `[`, 1)

  #loop to go through each gcm and peak metric, save into new row of output table
  for(j in 1:4){
    #subset to gcm j
    gcm.sub <- peaks[peaks$gcm == gcm[j],]
    
    #loop through each peak flow metrics
    peak.names <- unique(peaks$metric)
    for(k in 1:length(peak.names)){
      peak.sub <- gcm.sub[gcm.sub$metric == peak.names[k],]
      
      #find index of baseline and future scenario outputs
      ind.baseline <- grep("Baseline", peak.sub$Scenario_Name)
      ind.future <- grep("Future", peak.sub$Scenario_Name)
      
      #get values
      baseline <- peak.sub$p10[ind.baseline]
      future <- peak.sub$p10[ind.future]
      percent.change <- (future-baseline)/baseline * 100
      metric <- peak.names[k]
      gcm2 <- gcm[j]
      comid <- peak.sub$comid [1]
      reach2 <- reach[i]
      
      #save as new row
      new.row <- c(baseline, future, percent.change, metric, gcm2, comid, reach2)
      peakmag.all <- rbind(peakmag.all, new.row)
    }
    
  }
  
}


#remove first row
peakmag.all <- peakmag.all[2:length(peakmag.all$baseline),]
peakmag.all$pct_change <- as.numeric(peakmag.all$pct_change)

#create magnitude boxplots for each GCM

#fill color based on gcm
fill<- peakmag.all$gcm
#All years plots
P<- ggplot(peakmag.all, aes(x=gcm, y = pct_change, fill=gcm)) +
  geom_boxplot() +  facet_wrap(~metric) +
  #scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fdbe85", "#d94701", "#cbc9e2", "#6a51a3")) +
  labs(title="Change in Flood Magnitude",x ="", y = "Percent Change in Magnitude") + 
  geom_hline(yintercept=0, size = 1, linetype="dashed",)
print(P)
#save as jpg
component.2 <- gsub(" ", "_", title)
plot.fname <- paste0(reach[i],"_", component.2, "_boxplots_ALL.jpeg")
ggsave(plot.fname,   width = 14, height = 7)



#### Percent change in all metric percentiles #####
#create empty data frame for baseline, future, %change
percentiles.all <- data.frame(matrix(NA,1,8))
names(percentiles.all) <- c("baseline", "future", "pct_change", "percentile", "metric", "gcm", "comid", "reach")

for(i in 1:length(future.subdirs)){
  #get the future results and baseline results filenames (all consistent for every reach)
  files.all <- list.files(future.subdirs[i], full.names = TRUE)
  #find index of percentile results all
  ind.file <- grep("_percentiles_ALL_gcms_baseline_future.csv", files.all)
  #read in percentile file:
  percentiles.all.sub <- read.csv(files.all[ind.file])
  #get gcm for each
  percentiles.all.sub$gcm <- sapply(strsplit(percentiles.all.sub$Scenario2,"_"), `[`, 1)
  
  #loop to go through each gcm and metric, save into new row of output table
  for(j in 1:4){
    #subset to gcm j
    gcm.sub <- percentiles.all.sub[percentiles.all.sub$gcm == gcm[j],]
    
    #loop through each peak flow metrics
    unique.metrics <- unique(gcm.sub$metric)
    
    for(k in 1:length(unique.metrics)){
      metric.sub <- gcm.sub[gcm.sub$metric == unique.metrics[k],]
      
      #find index of baseline and future scenario outputs
      ind.baseline <- grep("Baseline", metric.sub$Scenario_Name)
      ind.future <- grep("Future", metric.sub$Scenario_Name)
      
      #get values that apply to all metrics
      metric <- unique.metrics[k]
      gcm2 <- gcm[j]
      comid <- metric.sub$comid [1]
      reach2 <- reach[i]
      
      #percentiles
      percentiles <- c("p10", "p25", "p50", "p75", "p90")
      
      #loop through p10-p90 percentiles
      for(l in 1:length(percentiles)){
        #find column index for percentile l
        col.ind.percentile <- grep(percentiles[l], names(metric.sub))
        #find metric values for baseline row, percentile column
        baseline.p <- metric.sub[ind.baseline, col.ind.percentile]
        future.p <- metric.sub[ind.future, col.ind.percentile]
        percent.change <- (future.p-baseline.p)/baseline.p * 100
        percntl.p <- percentiles[l]
        
        #save as new row
        new.row <- c(baseline.p, future.p, percent.change, percntl.p, metric, gcm2, comid, reach2)
        percentiles.all <- rbind(percentiles.all, new.row)
        
        
      }

      
    }
    
  }
  
}


#remove first row
percentiles.all <- percentiles.all[2:length(percentiles.all$baseline),]
percentiles.all$pct_change <- as.numeric(percentiles.all$pct_change)

write.csv(percentiles.all, file="W:/SOC_FlowEcologyStudy/FutureClimateScenarios/ffm_percentile_percent_change_future_baseline.csv", row.names = FALSE)


 

