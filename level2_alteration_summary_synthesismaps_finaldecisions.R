#### Level 2 - alteration summary based on various thresholds

#install.packages("ggsn")
#install.packages("ggmap")
#install.packages("mapview")
#install.packages("geosphere")
#install.packages("rgeos")
#install.packages("ggspatial")
#to install spDataLarge
#devtools::install_github("robinlovelace/geocompr")
library(spData)
#install.packages("spDataLarge")
#install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
library(spDataLarge)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(sf)
library(ggsn)
library(ggmap)
library(ggspatial)
library(mapview)
library(spData)      
library(spDataLarge)
library(geosphere)
library(rgeos)


#load in data
#ASCI/CSCI
level2.dir <- "C:/Users/KristineT/SCCWRP/SOC WQIP - Flow Ecology Study - General/Tier2_analysis/"
#fname <- paste0(level2.dir, "09_metric_suitability_tally_condensed.csv") #update to directory
fname <- paste0(level2.dir, "09_metric_suitability_tally_condensed_aliso_oso_small_creeks.csv") #all lspc subbasins

#read in suitability data 
suit_data <- read.csv(fname)


#Do not process wildermuth data for annual report
#wildermuth data
#fname.wildermuth <- paste0(level2.dir, "09_metric_suitability_tally_Wildermuth.csv")
#read in wildermuth data 
#suit_data_wildermuth <- read.csv(fname.wildermuth) %>% 
  #rename(New_Name = site) #change site to New_Name to combine with other datasets
#create alteration vector: change suitable and Altered to altered and unaltered
#suit_data_wildermuth$alteration_75pct_time <- suit_data_wildermuth$Suitability
#suit_data_wildermuth$alteration_75pct_time[suit_data_wildermuth$Suitability == "Altered"] <- "Altered"
#suit_data_wildermuth$alteration_75pct_time[suit_data_wildermuth$Suitability == "Suitable"] <- "Unaltered"
#only use 50% prob
#suit_data_wildermuth <- suit_data_wildermuth[suit_data_wildermuth$Threshold == "Threshold50",]


#UPDATE
#set output directory
out.dir <- paste0(level2.dir, "Suitability_Maps/", "prob50_75time/")
#out.dir <- paste0(level3.dir, "Suitability_Maps/", "All_species_suit_class_wide_option2_strict_prob_shorter_time/")

#read in information on subbasin and New_Name
basin_comid_lookup <- read.csv("L:/San Juan WQIP_KTQ/Data/SpatialData/v13_pourpoints_NHD_comids.csv") 

#read in shapefiles subbasins and reaches
#subbasin polygon shapefile
basins <- st_read("data/subbasin_boundaries_forSCCWRP.shp", quiet = T)

#reach polylines
reaches <- st_read('data/reaches_forSCCWRP.shp', quiet = T)

#model source LSPC or Wildermuth
source <- read.csv("L:/San Juan WQIP_KTQ/Data/SpatialData/Model_Subbasins_Reaches/New_Subbasins_forSCCWRP_12062019/Subbasins_inmodel_source.csv")


############################################################
#post-process Aliso suitability data

#read in information on subbasin and COMID
basin_comid_lookup <- read.csv("L:/San Juan WQIP_KTQ/Data/SpatialData/v13_pourpoints_NHD_comids.csv")

#lookuptable to convert subbasin codes for model output subbasin names
subbasin_lookup <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/191220_Interim_Calibration/site_name_lookupletternumbers.csv")

#convert basin orig name to outputfile name (model subbasin name)
new.subbasinname <- basin_comid_lookup$Subbasin

for(z in 1:length(subbasin_lookup$Letter)){
  new.subbasinname <- gsub(subbasin_lookup$Letter[z], subbasin_lookup$Number[z], new.subbasinname)
}

#find and replace - in new.subbasinname with nothing, make consistent with file name
new.subbasinname <- gsub("-", "", new.subbasinname)
basin_comid_lookup$site <- as.numeric(new.subbasinname)

#join new subbasin name with 

suit_data2 <- suit_data %>% 
  inner_join(basin_comid_lookup, by = c('site')) %>% 
  select(c(names(suit_data), Subbasin)) %>% 
  rename(New_Name = Subbasin)



#####post process to get overall alteration

####################################
#DECISIONS: Use 50% probabilty threshold
suit_data2_50 <- suit_data2[suit_data2$Threshold == "Threshold50",]

####################################
#if >75% time Altered, altered
suit_data2_50$alteration_75pct_time[suit_data2_50$Altered > 75] <- "Altered"
suit_data2_50$alteration_75pct_time[suit_data2_50$Altered <= 75] <- "Unaltered"

#### if 1 metric is altered, then all altered
#aggregate site, Biol, Threshold, alteration_75pct_time
subset.75pct.time <- suit_data2_50 %>% 
  group_by(site, New_Name, Biol, Threshold, alteration_75pct_time) %>% 
  tally() %>% 
  ungroup()

#save as dataframe
subset.75pct.time <- data.frame(subset.75pct.time)



#2-3 metric altered, considered altered
#if number altered > 1 --> class it as overall altered
subset.75pct.time$overall.altered.2metric <- NA
subset.75pct.time$overall.altered.2metric[which(subset.75pct.time$alteration_75pct_time == "Altered" & subset.75pct.time$n > 1)] <- "Altered"
#if 3 metrics unaltered --> class it as unaltered
subset.75pct.time$overall.altered.2metric[which(subset.75pct.time$alteration_75pct_time == "Unaltered" & subset.75pct.time$n > 1)] <- "Unaltered"

#########################

###Do not process wildermuth for annual report ####
#format wildermuth data and tally
#subset.wildermuth <- suit_data_wildermuth %>% 
  #group_by(New_Name, Biol, Threshold, alteration_75pct_time) %>% 
  #tally() %>% 
  #ungroup()
#save as dataframe
#subset.wildermuth <- data.frame(subset.wildermuth)
#remove subbasins that are NA (Q99 rows)
#subset.wildermuth <- subset.wildermuth[-which(is.na(subset.wildermuth$New_Name)),]

#Wildermuth: 2 metric altered, considered altered (doesn't have Q99 so no 3rd metric)
#if number altered > 1 --> class it as overall altered
#subset.wildermuth$overall.altered.2metric <- NA
#subset.wildermuth$overall.altered.2metric[which(subset.wildermuth$alteration_75pct_time == "Altered" & subset.wildermuth$n  == 2)] <- "Altered"
#if 1 or 2 metrics unaltered --> class it as unaltered
#subset.wildermuth$overall.altered.2metric[which(subset.wildermuth$alteration_75pct_time == "Unaltered" & subset.wildermuth$n > 0)] <- "Unaltered"
#if NA stay NA
#combine with lspc data
#subset.75pct.time.all <- merge(subset.75pct.time, subset.wildermuth, all = TRUE)

#########################

#save csv
file.name <- paste0(level2.dir, "summary.50prob.75pct.time.altered.2metrics_aliso_oso_small_creeks.csv")
write.csv(subset.75pct.time, file=file.name, row.names = FALSE)


#summarize overall alteration: number of altered and unaltered subbasins, percent of total subbasins using these thresholds
site.length <- length(unique(subset.75pct.time$New_Name))

#2 metric
subset.75pct.time.summary2 <- subset.75pct.time %>% 
  group_by(Biol, Threshold, overall.altered.2metric) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(pct.2metric = 100*n/site.length) 

data.frame(subset.75pct.time.summary2)

#summary if using wildermuth:
#subset.75pct.time.summary2.all <- subset.75pct.time.all %>% 
 #group_by(Biol, Threshold, overall.altered.2metric) %>% 
  #tally() %>% 
  #ungroup() %>% 
  #na.omit()  

#data.frame(subset.75pct.time.summary2.all)

############################################################
#create synthesis of CSCI and ASCI alteration:
  #if both altered, high priority; if unaltered --> low priority

#if using wildermuth:
#remove NA values
#subset.75pct.time.all2 <- na.omit(subset.75pct.time.all)
#subset.75pct.time.summary2.all <- subset.75pct.time.all %>% 
  #group_by(New_Name, Threshold, overall.altered.2metric) %>% 
  #tally() %>% 
  #ungroup() %>% 
  #na.omit()  
#overall.summary <- data.frame(subset.75pct.time.summary2.all)

#remove NA values
subset.75pct.time.all2 <- na.omit(subset.75pct.time)
subset.75pct.time.summary2.all <- subset.75pct.time %>% 
  group_by(New_Name, Threshold, overall.altered.2metric) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit()  
overall.summary <- data.frame(subset.75pct.time.summary2.all)

#ASCI summary table
subset.75pct.time.all2.asci <- subset.75pct.time.all2[subset.75pct.time.all2$Biol == "ASCI",]

##
overall.summary$synthesis_alteration <- NA
#if 2 metrics altered, high priority
overall.summary$synthesis_alteration[which(overall.summary$overall.altered.2metric == "Altered" & overall.summary$n == 2)] <- "High Priority" 
#if one is unaltered, medium
overall.summary$synthesis_alteration[which(overall.summary$overall.altered.2metric == "Unaltered" & overall.summary$n == 1)] <- "Medium Priority" 
#if 2 metrics unaltered, high priority
overall.summary$synthesis_alteration[which(overall.summary$overall.altered.2metric == "Unaltered" & overall.summary$n == 2)] <- "Low Priority" 

#remove NA
synthesis.summary <- na.omit(overall.summary)

#summary
synthesis.summary.table <- synthesis.summary %>% 
  na.omit()  %>% 
  group_by(Threshold, synthesis_alteration) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(pct.2metric = 100*n/32) 
synthesis.summary.table <- data.frame(synthesis.summary.table)


############################################################
#create overall synthesis maps for 50% probabily, for 75% of time altered altered, 2 metrics


#colors for suitability categories
colors <- c("#ca0020", "#fdae61","#0571b0", "white")
priority <- c("High Priority",  "Medium Priority","Low Priority", NA)
categories <- c("High (Alteration: CSCI & ASCI)", "Medium (Alteration: CSCI or ASCI)","Low (Alteration: None)","Not evaluated")
lookup <- data.frame(cbind(colors, priority, categories))

#merge with basins
subset.join <- synthesis.summary %>% 
  full_join(basins, by = c('New_Name')) %>% 
  inner_join(source, by = c('New_Name'))

source2 <- synthesis.summary %>% 
  inner_join(basins, by = c('New_Name')) %>% 
  inner_join(source, by = c('New_Name'))


  #plot
  #Set up base map 
  study <- ggplot(basins) + 
    labs(title="Prioritization for Additional Analysis", subtitle = "Based on Biologically-Relevant Flow Alteration",x ="", y = "")  + 
    geom_sf(color = "lightgrey", fill="white") +
    annotation_scale() +
    annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
                           width = unit(.8, "cm")) +
    #labs(x ="", y = "")  + 
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_line(color = "white", size = 0.8)) 
  study
  
  #subset lookup categories and tables
  lookup.sub <- lookup[lookup$priority %in% unique(subset.join$synthesis_alteration),]
  
  #save as factor
  lookup.sub$priority <- factor(lookup.sub$priority, levels = unique(lookup.sub$priority))
  subset.join$synthesis_alteration <- factor(subset.join$synthesis_alteration, levels = unique(lookup.sub$priority))
  
  #synthesis map
  syn.plot <- study + geom_sf(data = subset.join, color= "lightgrey", aes(fill=synthesis_alteration, geometry = geometry)) +
    scale_fill_manual(name = "Priority based on Biologic Flow Alteration", labels = lookup.sub$categories, values=lookup.sub$colors) +
    geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 
  
  #add in model source
  syn.plot2 <- syn.plot + geom_sf(data = source2, size = 1, fill = NA, aes(color=Source, geometry = geometry)) +
    scale_color_manual(name = "Model Source", labels = c("LSPC", "GSFLOW"), values=c("black", "hotpink")) +
    geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 
  
  #print
  print(syn.plot)
  print(syn.plot2)
  

  #write plot
  out.filename <- paste0(out.dir, "Synthesis_Prioritization_map_Aliso_Oso_small_creeks.jpg")
  ggsave(syn.plot, file = out.filename, dpi=500, height=6, width=8)
  

















############################################################
#create suitability maps for 50% probabily, for 75% of time altered altered, 2 metrics

#names
#cols <- names(subset.75pct.time.all)
cols <- names(subset.75pct.time)
col.names <- "overall.altered.2metric"

#colors for suitability categories
colors <- c("#ca0020", "#0571b0", "white")
alteration <- c("Altered",  "Unaltered", NA)
categories <- c("Likely Altered", "Likely Unaltered", "Not evaluated")
lookup <- data.frame(cbind(colors, alteration, categories))


for(i in 7){
  #title
  col.name <- cols[i]
  metric.threshold <- gsub("overall.altered.", "", col.name)
  metric.threshold <- gsub("metric", " Metric Altered Threshold", metric.threshold)
  
  #subset 
  #subset <- subset.75pct.time[,c(1, 2, 3, i)]
  subset <- subset.75pct.time[,c(2, 3,4, i)]
  #subset <- subset.50pct.time[,c(2, 3,4, i)]
  #subset <- subset.25pct.time[,c(2, 3,4, i)]
  
  names(subset) <- c("New_Name", "Biol", "Probability_Threshold","Alteration - Biology")
  subset$New_Name <- as.character(subset$New_Name)
  #remove NA rows
  subset <- na.omit(subset)
  
  #loop through CSCI and ASCI thresholds
  indices <- c("ASCI", "CSCI")
  
  for(z in indices){
    #subset either csci or asci
    subset.index <- subset[subset$Biol == z,]
    
    #loop through the 3 thresholds
    unique.prob.thresholds <- na.omit(unique(subset.index$Probability_Threshold))
    for(q in unique.prob.thresholds){
      #subset based on prob threshold
      subset.index.threshold <- subset.index[subset.index$Probability_Threshold == q,]
      #prob value
      prob <- gsub("Threshold", "Probability Threshold at ", q)
      
      #merge with basins
      subset.join <- subset.index.threshold %>% 
        full_join(basins, by = c('New_Name'))
      
      #title
      #title <- paste0(z, ": ", prob, "%")
      #subtitle <- metric.threshold
      title <- paste0(z)
      subtitle <- "Biologically-Relevant Flow Alteration"
      
      #plot
      #Set up base map 
      study <- ggplot(basins) + 
        geom_sf(color = "lightgrey", fill="white") +
        labs(title=title, subtitle = subtitle, x ="", y = "")  + 
        annotation_scale() +
        annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
                               width = unit(.8, "cm")) +
        theme(panel.background = element_rect(fill = "white"),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.grid = element_line(color = "white", size = 0.8),
              plot.title = element_text(size=20)) 
      study
      
      #subset lookup categories and tables
      lookup.sub <- lookup[lookup$alteration %in% unique(subset.join$`Alteration - Bio`),]
      
      #save as factor
      lookup.sub$alteration <- factor(lookup.sub$alteration, levels = unique(lookup.sub$alteration))
      subset.join$`Alteration - Bio` <- factor(subset.join$`Alteration - Bio`, levels = unique(lookup.sub$alteration))
      
      
      #synthesis map
      syn.plot <- study + geom_sf(data = subset.join, color= "lightgrey", aes(fill=`Alteration - Bio`, geometry = geometry)) +
        scale_fill_manual(name = "Biologic Flow Alteration", labels = lookup.sub$categories, values=lookup.sub$colors) +
        geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 
      #print
      print(syn.plot)
      
      #write plot
      out.filename <- paste0(out.dir, z, "_alteration_map_Aliso_Oso_small_creeks.jpg")
      ggsave(syn.plot, file = out.filename, dpi=300, height=4, width=6)
      
    }
    
  }
  
}

#rename Alteration - Bio to likely unaltered likely altered categories
subset$`Alteration - Biology` <- gsub("Altered", "Likely Altered", subset$`Alteration - Biology`)
subset$`Alteration - Biology` <- gsub("Unaltered", "Likely Unaltered", subset$`Alteration - Biology`)

#save the subset summary table with indices, subbasin
#pivot wider to get hydro.alteration.CSCI and hydro.alteration.ASCI columns
subset2 <- subset %>% 
  pivot_wider(names_from = Biol, values_from = `Alteration - Biology`) %>% 
  rename(hydro.alteration.CSCI = CSCI) %>% 
  rename(hydro.alteration.ASCI = ASCI)

#combine with overall summary
#remove na rows from overall summary
overall.summary2 <- na.omit(overall.summary)
summary.csci.asci.synthesis <- subset2 %>% 
  inner_join(overall.summary2, by = c('New_Name')) %>% 
  select(c(names(subset2), "synthesis_alteration"))
#write csv summary for CSCI and ASCI
file.name.summary <- paste0(out.dir, "SOC_CSCI_ASCI_HydroAlt_Synthesis_Summary.csv")
write.csv(summary.csci.asci.synthesis, file = file.name.summary)




























############################################################

####Plot maps of suitability

#loop through each focal species, make maps for each

#species columns
cols <- names(suit_data)
species.cols <- 3:length(cols)

#colors for suitability categories
colors <- c("#0571b0", "#ca0020", "white")
suitability <- c("High",  "Low", NA)
categories <- c("Yes", "No", "Not evaluated")
lookup <- data.frame(cbind(colors, suitability, categories))


for(i in species.cols){
  #title
  col.name <- cols[i]
  title <- gsub("_", ": ", col.name)
  title <- gsub("\\.", "\\/", title)
  
  #subset 
  subset <- suit_data[,c(2,i)]
  names(subset)[2] <- "suitability"
  
  #merge with basins
  subset.join <- subset %>% 
    full_join(basins, by = c('New_Name'))
  
  #change Partial to Low for suitability
  subset.join$suitability <- gsub("Partial", "Low", subset.join$suitability)
  
  
  #Set up base map 
  study <- ggplot(basins) + 
    geom_sf(color = "#969696", fill="white") +
    labs(title=title, x ="", y = "")  + 
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_line(color = "white", size = 0.8),
          plot.title = element_text(size=20)) 
  study
  
  #subset lookup categories and tables
  lookup.sub <- lookup[lookup$suitability %in% unique(subset.join$suitability),]
  
  #save as factor
  lookup.sub$suitability <- factor(lookup.sub$suitability, levels = unique(lookup.sub$suitability))
  subset.join$suitability <- factor(subset.join$suitability, levels = unique(subset.join$suitability))
  
  
  #synthesis map
  syn.plot <- study + geom_sf(data = subset.join, color= "#969696", aes(fill=suitability, geometry = geometry)) +
    scale_fill_manual(name = "Flow conditions suitable?", labels = lookup.sub$categories, values=lookup.sub$colors) +
    geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 
  #print
  print(syn.plot)
  
  #write plot
  out.filename <- paste0(out.dir, col.name, "_suitability_map.jpg")
  ggsave(syn.plot, file = out.filename, dpi=300, height=4, width=6)
  
}


