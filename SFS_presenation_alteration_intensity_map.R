#SFS Presentation 2021: FF hydrologic alteration

#libraries
library("tidyverse")
library("ggplot2")
library("dplyr")
library(spData)
#install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
library(spDataLarge)

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


######################################################################
#Level 1 alteration data exploration

#alteration directory
#alt.dir.name <- "Oso_SmallCreeks"
alt.dir.name <- "Aliso_Oso_SmCk_SanJuan_all_lowflowbias"
alteration.dir <- paste0("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/", alt.dir.name)

#read in combined ffm alteration: Aliso, Oso, small creeks
alt.data <- read.csv(paste0(alteration.dir, "/ffm_alteration.df.overall.join.Aliso.Oso.SmallCreeks.SanJuanLSPC.lowflowbias.csv")) %>% 
  mutate(New_Name = as.character(subbasin))
ffm.sites <- unique(alt.data$subbasin.model)
#calculate alteration intensity
#calculate the intensity of alteration for each subbasin using all FFMs: total number of likely altered metrics out of the 24 using entire POR
intensity.df.POR.allmetrics <- alt.data %>% 
  group_by(subbasin, alteration.status, New_Name) %>% 
  tally() %>% 
  ungroup()  %>% 
  filter(alteration.status == "likely_altered")

#create New_Name column with subbasin id to match polygon layer
#subbasin polygon shapefile
basins <- st_read("data/Agg_Boundaries_v14.shp", quiet = T)
#basins <- st_read('L:/San Juan WQIP_KTQ/Data/SpatialData/Model_Subbasins_Reaches/New_Subbasins_forSCCWRP_12062019/New_Subbasins_forSCCWRP/subbasin_boundaries_forSCCWRP.shp', quiet = T)
basins2 <- basins %>% 
  inner_join(intensity.df.POR.allmetrics, by = c('New_Name'))
basins2

#reach polylines
reaches <- st_read('data/reaches_forSCCWRP.shp', quiet = T)
#reaches <- st_read('L:/San Juan WQIP_KTQ/Data/SpatialData/Model_Subbasins_Reaches/New_Subbasins_forSCCWRP_12062019/New_Subbasins_forSCCWRP/reaches_forSCCWRP.shp', quiet = T)


#join subbasin shapefile with component alteration
intensity <- intensity.df.POR.allmetrics %>% 
  inner_join(basins, by = c('New_Name'))
intensity


#base map 
study2 <- ggplot(basins) + 
  geom_sf(color = "#969696", fill="#d9d9d9") +
  #labs(title=unique(basins4.sub$title_name),x ="", y = "") + 
  #annotation_scale() +
  #annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
                         #width = unit(.8, "cm")) +
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_line(color = "white", size = 0.8))

#filled alteration plots
alt.plot <- study2 + geom_sf(data = basins2,  color= "#969696", aes(fill=n)) +
  #scale_fill_manual(name = "Alteration Status", labels = lookup.sub$alteration.status.new, values=lookup.sub$colors) +
  scale_fill_distiller(palette = "Spectral", name="Alteration Intensity", breaks = c(5, 7, 9, 11, 13, 15)) +
  theme(legend.key.size = unit(1.5, 'cm'), legend.text = element_text(size=12), legend.title = element_text(size=12)) +
  #change scale in legend from 0-16
  #scale_fill_distiller(palette = "Spectral", name="Alteration Intensity", limits=c(0,16)) +
  geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 

alt.plot

#write plot
#save as jpg
plot.fname <- paste0(alteration.dir, "/Alteration_intensity_Aliso_Oso_smallstreams_SanJuan_ALL_lowflowbias.map.jpg")
ggsave(alt.plot, file=plot.fname, dpi=300, height=8, width=12)

