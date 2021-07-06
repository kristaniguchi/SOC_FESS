#Future scenario evaluation - do we see a difference at the flow metric level?
  #1. Synthesis boxplots for every reach comparing baseline condition, each gcm
  #2. Heat maps of alteration for each gcm
  #3. Alteration maps (?)

############################################################
#load libraries

library(tidyverse)
library(readxl)
library(sf)
library(ggsn)
library(ggmap)
library(mapview)
library(spData)      
library(spDataLarge)
library(geosphere)
library(rgeos)

############################################################
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


############################################################
#1. Synthesis boxplots for every reach comparing baseline condition, each gcm and 

#loop through each reach and create boxplots of baseline and future gcm percentiles

#get the future results and baseline results filenames (all consistent for every reach)
files.all <- list.files(future.subdirs[1])
#find future results
#ind.future.results <- grep("future.results.ffm.all", files.all)
#future.results.fname <- files.all[ind.future.results]
#baseline results filename
#baseline.results.fname <- "historical.results.ffm.all.csv"

#find percentiles for future and baseline boxplot comparisons
ind.future.percentiles <- grep("future.percentiles.all.", files.all)
future.percentiles.fname <- files.all[ind.future.percentiles]
#percentiles for baseline boxplot comparison
ind.baseline.percentiles <- grep("historical.percentiles.all", files.all)
baseline.percentile.fname <- files.all[ind.baseline.percentiles]

#gcn names CNRM-CM5
gcm <- c("CanESM2", "CCSM4", "CNRM-CM5", "MIROC5", "Baseline")
#reach name
reach <- list.files(future.dir)


for (i in 1:length(future.subdirs)){
  #set working directory to reach i
  setwd(future.subdirs[i])
  
  #read in baseline and future percentiles for gcm 1 and append future percentiles to data frame
  percentiles <- read.csv(baseline.percentile.fname[1])
  #baseline name
  baseline.name <- paste0(gcm[1], "\n1975-2005")
  percentiles$Scenario <- rep(baseline.name, length(percentiles$p10))
  #read in future percentiles
  percentiles.future <- read.csv(future.percentiles.fname[1])
  future.name <- paste0(gcm[1], "\n2031-2060")
  percentiles.future$Scenario <- rep(future.name, length(percentiles.future$p10))
  #append to baseline dataframe
  percentiles <- data.frame(rbind(percentiles, percentiles.future))
  
  #read in future and baseline percentiles for remaining gcms and append to percentiles df starting at second (since did first above)
  for(j in 2:length(future.percentiles.fname)){
    
    #read in baseline data for gcm j
    baseline.j <- read.csv(baseline.percentile.fname[j])
    #scenario name
    name <- paste0(gcm[j], "\n1975-2005")
    baseline.j$Scenario <- rep(name, length(baseline.j$p10))
    
    #read in future data for gcm j
    future.j <- read.csv(future.percentiles.fname[j])
    #scenario name
    name <- paste0(gcm[j], "\n2031-2060")
    future.j$Scenario <- rep(name, length(percentiles.future$p10))
    
    #append future and baseline to percentiles df
    percentiles <- data.frame(rbind(percentiles, future.j, baseline.j))
  }
  
  #write the percentiles df all
  fname.all <- paste0(reach[i], "_percentiles_ALL_gcms_baseline_future.csv")
  #create new column replaceing \n with a _ since it doesn't get written in csv
  percentiles$Scenario2 <- gsub("\n", "_", percentiles$Scenario)
  percentiles$Scenario_Name <- gsub("2031-2060", "Future", percentiles$Scenario)
  percentiles$Scenario_Name <- gsub("1975-2005", "Baseline",  percentiles$Scenario_Name)
  write.csv(percentiles, file=fname.all, row.names = FALSE)
  
  
  #Boxplots of FFMs by GCM baseline and future for every flow metric
  for(m in 1:length(unique.components)){
    #subset percentiles based on component m
    ind.comp.m <- grep(unique.components[m], ffm.labels$title_component)
    ffm.names.m <- ffm.labels[ind.comp.m,]
    #subset percentiles to component m
    sub.obs.comp <- percentiles[as.character(percentiles$metric) %in%  as.character(ffm.names.m$flow_metric),]

    #Boxplots for components
    title <- as.character(ffm.names.m$title_component[1]) #component
    subtitle.bp <- paste0(reach[i])
    characteristic <- sort(as.character(ffm.names.m$flow_characteristic)) 
    metrics.title <- sort(as.character(ffm.names.m$title_ffm)) #boxplot title
    
    #merge metric label names with percentiles
    percentiles.cbind.all.sub.m <- merge(sub.obs.comp, ffm.labels, by="metric") 
    
    #if peak flow plots, create boxplots in order of increasing magnitude (not based on alphabetical order)
    if(ffm.labels$flow_component[m] == "Peak Flow"){
      #fill color based on entire POR LSPC (add colored points), Gage, Reference
      fill<- percentiles.cbind.all.sub.m$Scenario_Name
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
      P<- ggplot(percentiles.cbind.all.sub.m, aes(x=Scenario_Name, ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90, fill=Scenario_Name)) +
        geom_boxplot(stat = "identity") +  facet_wrap(~title_ffm, scales="free") +
        scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fdbe85", "#d94701", "#cbc9e2", "#6a51a3")) +
        labs(title=title,x ="", y = "", subtitle = subtitle.bp) 
      print(P)
      #save as jpg
      component.2 <- gsub(" ", "_", title)
      plot.fname <- paste0(reach[i],"_", component.2, "_boxplots_ALL.jpeg")
      ggsave(plot.fname,   width = 14, height = 7)
      
    }else{
      #fill color based on entire POR LSPC
      fill<- percentiles.cbind.all.sub.m$Scenario_Name
      #All years plots
      P<- ggplot(percentiles.cbind.all.sub.m, aes(x=Scenario_Name, ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90, fill=Scenario_Name)) +
        geom_boxplot(stat = "identity") +  facet_wrap(~title_ffm, scales="free") +
        scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fdbe85", "#d94701", "#cbc9e2", "#6a51a3")) +
        labs(title=title,x ="", y = "", subtitle = subtitle.bp) 
      print(P)
      #save as jpg
      component.2 <- gsub(" ", "_", title)
      plot.fname <- paste0(reach[i],"_", component.2, "_boxplots_ALL.jpeg")
      ggsave(plot.fname,   width = 14, height = 7)
      
    }
  }
  
}



############################################################
#2. Component alteration: heat maps of alteration for each gcm

#gcm names only
gcm.only <- c("CanESM2", "CCSM4", "CNRM-CM5", "MIROC5")
#reach name
reach <- list.files(future.dir)

###summary table of alteration for each metric
#loop through each reach and create an overall alteration table, add in gcm label

for (k in 1:length(future.subdirs)){
  #set working directory to reach i
  subdir.k <- future.subdirs[k]
  setwd(subdir.k)
  
  #find index of alteration comparison tables
  subdir.files <- list.files(subdir.k)
  ind.alt <- grep("alteration_comparison_future_historical_statewide", subdir.files)
  #list files of alteration comparison tables
  alt.tables <- subdir.files[ind.alt]
  
  #loop through and read in each alteration table, 
  for (n in 1:length(alt.tables)){
    #for the first iteration k and j, save this file and append on to this one, for all other iterations, just append
    if(k == 1 & n == 1){
      alteration.all <- read.csv(alt.tables[n])
      #add in gcm column
      alteration.all$gcm <- rep(gcm.only[n], length(alteration.all$COMID))
      #add in metric column to merge ffm labels
      alteration.all$metric <- alteration.all$ffm
      #merge the functional flow metric labels and component labels
      alteration.all <- merge(alteration.all, ffm.labels, by="metric")
    }else{
      #else for all subsequent models and reaches, append to the alteration.all df
      alt.table.n <- read.csv(alt.tables[n])
      #add in gcm column
      alt.table.n$gcm <- rep(gcm.only[n], length(alt.table.n$COMID))
      #add in metric column to merge ffm labels
      alt.table.n$metric <- alt.table.n$ffm
      #merge the functional flow metric labels and component labels
      alt.table.n <- merge(alt.table.n, ffm.labels, by="metric")
      #append (rbind) to alteration.all df
      alteration.all <- rbind(alteration.all, alt.table.n)
    }
  }
}

#change all peak mag alteration statuses to NA since we only have one value for baseline and one value for future to compare
alteration.all$alteration.status[alteration.all$title_component == "Peak Flow Magnitude"] <- NA
#change peak flow mag alteration direction to NA too
alteration.all$alteration.direction[alteration.all$title_component == "Peak Flow Magnitude"] <- NA

write.csv(alteration.all, file="W:/SOC_FlowEcologyStudy/FutureClimateScenarios/ffm.alteration.summary.Aliso.gcm.all.rcp8.5.csv", row.names = FALSE)


###summary table of component alteration
#loop to summarize component alteration
basins <- unique(alteration.all$subbasin.model)

#create empty df
comp_summary <- data.frame(matrix(nrow=1, ncol=9))
names(comp_summary) <- c("COMID", "subbasin.model", "subbasin", "flow_component", "component_alteration", "n_ffm_altered", "flow_metrics_altered", "flow_char_altered", "gcm")

for(o in 1:length(basins)){
  #subset to basin i
  sub <- alteration.all[alteration.all$subbasin.model == basins[o],]
  
  #loop through each gcm
  for(q in 1:length(gcm.only)){
    #subset to gcm q
    sub.gcm.q <- sub[sub$gcm == gcm.only[q],]
    
    #############
    #loop through each unique component
    unique.comp <- unique(sub.gcm.q$flow_component)
    #empty component df to be filled in and appended to summary df
    temp_df <- comp_summary[1,]
    
    for(p in 1:length(unique.comp)){
      comp <- sub.gcm.q[sub.gcm.q$flow_component == unique.comp[p],]
      #component j
      component <- unique.comp[p]
      
      #if one metric is likley altered, all altered
      ind.altered <- grep("likely_altered", comp$alteration.status)
      n_ffm_altered <- length(ind.altered)
      if(length(ind.altered) > 0 ){
        comp_alt <- "likely_altered"
        #find metrics that are altered and combine into one element
        flow_metrics_altered <- paste0(comp$flow_metric[ind.altered]) %>%
          str_c( collapse=", ")
        #find characteristic altered
        flow_char_altered <- paste0(comp$title_ffm[ind.altered]) %>%
          str_c( collapse=", ")
        #else, NA for all
      }else{
        comp_alt <- "NA"
        flow_metrics_altered <- "NA"
        flow_char_altered <- "NA"
      }
      
      #save row for flow component in temp df
      row <- c(comp$COMID[1], as.character(comp$subbasin.model[1]), as.character(comp$subbasin[1]), as.character(component), comp_alt, n_ffm_altered, flow_metrics_altered, flow_char_altered, comp$gcm[1])
      temp_df[p,] <- row
    }
    
    #save temporary df to summary
    comp_summary <- rbind(comp_summary, temp_df)
    
  }
}  


#remove first row of NA
comp_summary2 <- comp_summary[2:length(comp_summary$COMID),]

write.csv(comp_summary2, file = "W:/SOC_FlowEcologyStudy/FutureClimateScenarios/component.alteration.summary.Aliso.gcm.all.rcp8.5.csv", row.names = FALSE)

grep("likely_altered", comp_summary2$component_alteration)


################
#create bar plot of alteration statuses and number of subbasins considered likely altered


#summary table with number of subbasins that are in each alteration category for each ffm
ffm_summary <- data.frame(aggregate(alteration.all, by = alteration.all[c('ffm','alteration.status', 'flow_component', 'gcm')], length))


#histogram of alteration statuses - this is with all different scenarios included
#add in facet wrap to show the different elements
ffm_summary$alteration.status <- factor(ffm_summary$alteration.status, levels=c("likely_altered", "likely_unaltered", "indeterminant"))

g <- ggplot(ffm_summary) +
  geom_bar(color = "black", aes(x= ffm, y =  subbasin, fill = alteration.status), stat = "identity", position = position_fill(reverse = FALSE), width = 0.7) +
  ggtitle("Flow Metric Alteration Status") +
  guides(fill = guide_legend(reverse = FALSE)) +
  xlab("Functional Flow Metrics") + ylab("Proportion of Subbasins") +
  facet_wrap(~factor(flow_component, levels = c("Fall pulse flow", "Wet-season base flow", "Peak flow", "Spring recession flow", "Dry-season base flow")), scales="free_x", nrow=1) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") +
  scale_fill_manual(name = "Alteration Status", labels = c("Likely Altered", "Likely Unaltered", "Indeterminate"), values = c("#ca0020","#0571b0","gray100")) 

g

#create different plot based on heatmap

ggsave(g, filename="C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/flowmetricalteration_histogram.jpg", dpi=300, height=5, width=13)
ggsave(sub1, filename="C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/subbasin1_compalteration.jpg", dpi=300, height=8, width=8)


#histogram of alteration statuses - create individual bar plot for each GCM
#add in facet wrap to show the different elements
#summary table with number of subbasins that are in each alteration category for each ffm and gcm
ffm_summary2 <- data.frame(aggregate(alteration.all, by = alteration.all[c('ffm','alteration.status', 'alteration.direction', 'flow_component', 'gcm')], length))
#create new vector alteration status, direction
ffm_summary2$alt.dir <- paste0(ffm_summary2$alteration.status, ", ",ffm_summary2$alteration.direction)
#save as factor
ffm_summary2$alt.dir <- factor(ffm_summary2$alt.dir, levels=c("likely_altered, high", "likely_altered, low", "likely_unaltered, none_found", "indeterminate, none_found"))

#loop through each gcm and subset and create plots for each
for(r in 1:4){
  #subset alteration to gcm r
  sub.r <- ffm_summary2[ffm_summary2$gcm == gcm[r],]

  #create bar plot of each gcm
  g.r <- ggplot(sub.r) +
    geom_bar(color = "black", aes(x= ffm, y =  subbasin, fill = alt.dir), stat = "identity", position = position_fill(reverse = FALSE), width = 0.7) +
    ggtitle("Flow Metric Alteration Status") +
    labs(subtitle = gcm[r]) +
    guides(fill = guide_legend(reverse = FALSE)) +
    xlab("Functional Flow Metrics") + ylab("Proportion of Subbasins") +
    facet_wrap(~factor(flow_component, levels = c("Fall pulse flow", "Wet-season base flow", "Peak flow", "Spring recession flow", "Dry-season base flow")), scales="free_x", nrow=1) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") +
    scale_fill_manual(name = "Alteration Status", labels = c("Likely Altered, High", "Likely Altered, Low",  "Likely Unaltered", "Indeterminate"), values = c("#ca0020","#f4a582","#0571b0","gray100")) 
  
  g.r
  
  #file name for plot
  filename.r <- paste0("W:/SOC_FlowEcologyStudy/FutureClimateScenarios/flowalteration_histogram_", gcm[r], ".jpg")
  ggsave(g.r, filename=filename.r, dpi=300, height=5, width=13)
  
}


###create heat map of alteration by type



############################################################
#3. calc degree of alteration for those that were categorized


############################################################
