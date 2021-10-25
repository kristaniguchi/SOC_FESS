### Level 2 - relating bio relevant flow alteration with observed and predicted CSCI/ASCI 
      #Management recommendations

library(tidyverse)
library(dplyr)
library(spData)
#install.packages("spDataLarge")
#install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
library(spDataLarge)
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

######################################################################################################
# #Only run these chunk when you have not already developed the management recommendations
# #Otherwise, skip this section 

# #read csvs with observed and predicted CSCI/ASCI
# observed.data <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Data_Products/FY2020_2021/Flow Ecology Level 2/Management Recommendations/Observed Bioassessment Scores/SOC_CSCI_ASCI_HydroAlt_Synthesis_Summary_Observed.csv")
# predicted.data <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Data_Products/FY2020_2021/Flow Ecology Level 2/Management Recommendations/Predicted Bioassessment Scores/CSCI_ASCI_hydro_alteration_predicted2.csv")
# 
# #add in the missing subbasins from predicted.data
# #read in subbasin COMID lookup table
# comid.lookup <- read.csv("L:/San Juan WQIP_KTQ/Data/SpatialData/SOC_FESS_Subbasin_NHD.COMID_Lookup_Table.csv")
# #read in modeled subbasins
# modeled.subbasins <- read.csv("L:/San Juan WQIP_KTQ/Data/SpatialData/Agg_Boundaries_v14_modeledsites.csv")
# #filter comid lookup with modeled.subbasins
# comid.lookup2 <- comid.lookup[which(comid.lookup$Subbasin %in% modeled.subbasins$New_Name),]
# length(comid.lookup2$Subbasin)
# length(modeled.subbasins$New_Name)
# #find which subbasins are missing from predicted.data
# `%notin%` <- Negate(`%in%`)
# comid.missing <- comid.lookup2[which(comid.lookup2$Subbasin %notin% predicted.data$New_Name),]
# #find the ASCI and CSCI predicted for missing comids
# #read in predicted CSCI and ASCI
# pred.csci.asci <- read.csv("L:/San Juan WQIP_KTQ/Data/SpatialData/CSCI_ASCI_Scores_COMID_full_rf_results_Heili.csv")
# comid.missing.csci.asci <- comid.missing %>% 
#   left_join(pred.csci.asci, by=c("COMID_forcalc" = "COMID"))
# #add in dummy columns for bio condition 
# comid.missing.csci.asci$New_Name <- comid.missing.csci.asci$Subbasin
# 
# comid.missing.csci.asci$Biological.Condition.CSCI.Predicted <- NA
# comid.missing.csci.asci$Hydro.Alteration.CSCI <- NA
# comid.missing.csci.asci$Stream.Characterization.CSCI <- NA
# comid.missing.csci.asci$Biological.Condition.ASCI.Predicted <- NA
# comid.missing.csci.asci$Hydro.Alteration.ASCI <- NA
# comid.missing.csci.asci$Stream.Characterization.ASCI <- NA
# comid.missing.csci.asci$Synthesis.Alteration <- NA
# comid.missing.csci.asci$StationCode <- NA
# comid.missing.csci.asci$Latitude <- NA
# comid.missing.csci.asci$Longitude <- NA
# #rename columns to be same with predicted data
# names(comid.missing.csci.asci)[6:8] <- c("New_Name", "COMID_old", "COMID")
# names(comid.missing.csci.asci)[11:13] <- c("new_subbasinname", "ASCI.Predicted", "CSCI.Predicted")
# 
# #join comid.missing.csci.asci with predicted.data
# predicted.data.join1 <- predicted.data %>% 
#   bind_rows(comid.missing.csci.asci)
# 
# #########
# #read in bio-relevant flow alteration for CSCI and ASCI
# bio.flow.alt <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Tier2_analysis/Suitability_Maps/prob25CSCI.75ASCI_75time_current/SOC_CSCI_ASCI_HydroAlt_Synthesis_Summary.csv")
# 
# #add in bio-relevant flow alteration CSCI and ASCI into observed and predicted df
# observed.data.join <- observed.data %>% 
#   left_join(bio.flow.alt, by = c("Subbasin" =  "New_Name"))
# #add in bio relevant alteration into blank columns for CSCI and ASCI
# observed.data$Hydro.Alteration.CSCI <- observed.data.join$hydro.alteration.CSCI
# observed.data$Hydro.Alteration.ASCI <- observed.data.join$hydro.alteration.ASCI
# #write csv and do categories in excel
# write.csv(observed.data, file="C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Data_Products/FY2020_2021/Flow Ecology Level 2/Management Recommendations/Observed Bioassessment Scores/SOC_CSCI_ASCI_HydroAlt_Synthesis_Summary_Observed.csv", row.names=FALSE)
# 
# predicted.data.join <- predicted.data.join1 %>% 
#   left_join(bio.flow.alt, by = c("New_Name"))
# #add in bio relevant alteration into blank columns for CSCI and ASCI
# #predicted.data$Hydro.Alteration.CSCI <- predicted.data.join$hydro.alteration.CSCI
# #predicted.data$Hydro.Alteration.ASCI <- predicted.data.join$hydro.alteration.ASCI
# #write csv and do categories in excel
# write.csv(predicted.data.join, file="C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Data_Products/FY2020_2021/Flow Ecology Level 2/Management Recommendations/Predicted Bioassessment Scores/CSCI_ASCI_hydro_alteration_predicted2.csv", row.names=FALSE)

#Stream characterization CSCI and ASCI rules, done outside of R:
  #1. if bio possibly, likely, or very likely altered and hydro alt is likely altered, then Prioritized for Flow Management
  #2. if bio possibly, likely, or very likely altered and hydro alt is likely unaltered, then Prioritized for Separate Stressor Evaluations
  #3. if bio likely intact and hydro unaltered, then prioritized for protection
######################################################################################################



#read in updated obs, predicted csvs
observed.data <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Data_Products/FY2020_2021/Flow Ecology Level 2/Management Recommendations/Observed Bioassessment Scores/SOC_CSCI_ASCI_HydroAlt_Synthesis_Summary_Observed.csv")
#add in synthesis alteration (forgot above)
#observed.data$Synthesis.Alteration <- observed.data.join$synthesis_alteration
#write.csv(observed.data, file="C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Data_Products/FY2020_2021/Flow Ecology Level 2/Management Recommendations/Observed Bioassessment Scores/SOC_CSCI_ASCI_HydroAlt_Synthesis_Summary_Observed.csv", row.names=FALSE)

predicted.data <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Data_Products/FY2020_2021/Flow Ecology Level 2/Management Recommendations/Predicted Bioassessment Scores/CSCI_ASCI_hydro_alteration_predicted2.csv")
#add in synthesis alteration (forgot above)
#predicted.data$Synthesis.Alteration <- predicted.data.join$synthesis_alteration
#write.csv(predicted.data, file="C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Data_Products/FY2020_2021/Flow Ecology Level 2/Management Recommendations/Predicted Bioassessment Scores/CSCI_ASCI_hydro_alteration_predicted.csv", row.names=FALSE)

######################################################################################
#create management recommendation maps

#read in information on subbasin and New_Name
basin_comid_lookup <- read.csv("L:/San Juan WQIP_KTQ/Data/SpatialData/v13_pourpoints_NHD_comids.csv") 

#read in shapefiles subbasins and reaches
#subbasin polygon shapefile
basins <- st_read("data/Agg_Boundaries_v14.shp", quiet = T)

#reach polylines
reaches <- st_read('data/reaches_forSCCWRP.shp', quiet = T)

#colors for suitability categories
colors <- c("#ca0020", "#fdae61", "#ffffbf", "#0571b0", "white")
priority <- c("Prioritized for Flow Management",  "Prioritized for Separate Stressor Evaluation", "Prioritized for Monitoring", "Prioritized for Protection", NA)
lookup <- data.frame(cbind(colors, priority))



#CSCI Observed
out.dir <- "C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Data_Products/FY2020_2021/Flow Ecology Level 2/Management Recommendations/Observed Bioassessment Scores/"

#merge with basins
subset.join <- observed.data %>% 
  full_join(basins, by = c('New_Name')) #%>% 
#inner_join(source, by = c('New_Name'))

#plot observed
#Set up base map 
study <- ggplot(basins) + 
  labs(title="Recommendations using Observed CSCI Data",x ="", y = "")  + 
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
lookup.sub <- lookup[lookup$priority %in% unique(subset.join$Stream.Characterization.CSCI),]

#save as factor
lookup.sub$priority <- factor(lookup.sub$priority, levels = unique(lookup.sub$priority))
subset.join$Stream.Characterization.CSCI <- factor(subset.join$Stream.Characterization.CSCI, levels = unique(lookup.sub$priority))

#synthesis map
syn.plot <- study + geom_sf(data = subset.join, color= "lightgrey", aes(fill=Stream.Characterization.CSCI, geometry = geometry)) +
  scale_fill_manual(name = "Management Recommendations", labels = lookup.sub$priority, values=lookup.sub$colors) +
  geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 

#print
print(syn.plot)


#write plot
out.filename <- paste0(out.dir, "CSCI_Observed_Recommendations.jpg")
ggsave(syn.plot, file = out.filename, dpi=500, height=6, width=8)

######
#ASCI Observed
#plot observed
#Set up base map 
study <- ggplot(basins) + 
  labs(title="Recommendations using Observed ASCI Data",x ="", y = "")  + 
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
lookup.sub <- lookup[lookup$priority %in% unique(subset.join$Stream.Characterization.ASCI),]

#save as factor
lookup.sub$priority <- factor(lookup.sub$priority, levels = unique(lookup.sub$priority))
subset.join$Stream.Characterization.ASCI <- factor(subset.join$Stream.Characterization.ASCI, levels = unique(lookup.sub$priority))

#synthesis map
syn.plot <- study + geom_sf(data = subset.join, color= "lightgrey", aes(fill=Stream.Characterization.ASCI, geometry = geometry)) +
  scale_fill_manual(name = "Management Recommendations", labels = lookup.sub$priority, values=lookup.sub$colors) +
  geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 

#print
print(syn.plot)


#write plot
out.filename <- paste0(out.dir, "ASCI_Observed_Recommendations.jpg")
ggsave(syn.plot, file = out.filename, dpi=500, height=6, width=8)




#############maps using predicted data

#CSCI Predicted
out.dir <- "C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Data_Products/FY2020_2021/Flow Ecology Level 2/Management Recommendations/Predicted Bioassessment Scores/"

#merge with basins
subset.join <- predicted.data %>% 
  full_join(basins, by = c('New_Name')) #%>% 
#inner_join(source, by = c('New_Name'))

#plot predicted
#Set up base map 
study <- ggplot(basins) + 
  labs(title="Recommendations using Predicted CSCI Data",x ="", y = "")  + 
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
lookup.sub <- lookup[lookup$priority %in% unique(subset.join$Stream.Characterization.CSCI),]

#save as factor
lookup.sub$priority <- factor(lookup.sub$priority, levels = unique(lookup.sub$priority))
subset.join$Stream.Characterization.CSCI <- factor(subset.join$Stream.Characterization.CSCI, levels = unique(lookup.sub$priority))

#synthesis map
syn.plot <- study + geom_sf(data = subset.join, color= "lightgrey", aes(fill=Stream.Characterization.CSCI, geometry = geometry)) +
  scale_fill_manual(name = "Management Recommendations", labels = lookup.sub$priority, values=lookup.sub$colors) +
  geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 

#print
print(syn.plot)


#write plot
out.filename <- paste0(out.dir, "CSCI_predicted_Recommendations.jpg")
ggsave(syn.plot, file = out.filename, dpi=500, height=6, width=8)

######
#ASCI predicted
#plot predicted
#Set up base map 
study <- ggplot(basins) + 
  labs(title="Recommendations using Predicted ASCI Data",x ="", y = "")  + 
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
lookup.sub <- lookup[lookup$priority %in% unique(subset.join$Stream.Characterization.ASCI),]

#save as factor
lookup.sub$priority <- factor(lookup.sub$priority, levels = unique(lookup.sub$priority))
subset.join$Stream.Characterization.ASCI <- factor(subset.join$Stream.Characterization.ASCI, levels = unique(lookup.sub$priority))

#synthesis map
syn.plot <- study + geom_sf(data = subset.join, color= "lightgrey", aes(fill=Stream.Characterization.ASCI, geometry = geometry)) +
  scale_fill_manual(name = "Management Recommendations", labels = lookup.sub$priority, values=lookup.sub$colors) +
  geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 

#print
print(syn.plot)


#write plot
out.filename <- paste0(out.dir, "ASCI_predicted_Recommendations.jpg")
ggsave(syn.plot, file = out.filename, dpi=500, height=6, width=8)

#summary of subbasins in each category
length(subset.join$New_Name)
length(unique(subset.join$New_Name))

