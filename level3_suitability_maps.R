#### Level 3 - Focal Species suitability maps

#install.packages("ggsn")
#install.packages("ggmap")
#install.packages("mapview")
#install.packages("geosphere")
#install.packages("rgeos")
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
library(mapview)
library(spData)      
library(spDataLarge)
library(geosphere)
library(rgeos)


#load in data
#species suitability data
level3.dir <- "C:/Users/KristineT/SCCWRP/SOC WQIP - Flow Ecology Study - General/Tier3_analysis/"
#UPDATE BASED ON OPTION AND UPDATE OUTDIRECTORY BASED ON SAME OPTION
#option 1
#fname <- paste0(level3.dir, "All_species_suit_class_wide_option1_med_prob_longer_time.csv") #update to directory
#option 2
fname <- paste0(level3.dir, "All_species_suit_class_wide_option2_strict_prob_shorter_time.csv") #update to directory

#read in suitability data 
suit_data <- read.csv(fname) %>% 
  rename(New_Name = Node)

#UPDATE BASED ON OPTION, match with file name option
#set output directory
#out.dir <- paste0(level3.dir, "Suitability_Maps/", "All_species_suit_class_wide_option1_med_prob_longer_time/")
out.dir <- paste0(level3.dir, "Suitability_Maps/", "All_species_suit_class_wide_option2_strict_prob_shorter_time/")

#read in shapefiles subbasins and reaches
#subbasin polygon shapefile
basins <- st_read("data/subbasin_boundaries_forSCCWRP.shp", quiet = T)

#reach polylines
reaches <- st_read('data/reaches_forSCCWRP.shp', quiet = T)


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
  
  #save as factor, set levels same in lookup and subset
  lookup.sub$suitability <- factor(lookup.sub$suitability, levels = unique(lookup.sub$suitability))
  subset.join$suitability <- factor(subset.join$suitability, levels = unique(lookup.sub$suitability))
  
  
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


