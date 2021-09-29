#CEFF Manuscript: Find the flows associated with thresholds
  #Arroyo Chub: medium prob - .53m depth
  #Willow: wet/dry season baseflow lower limit - 10cm depth of water in channel, max channel capacity (do not inundate overbanks)
            #spring recession lower limit - flow to inundate overbank at least 2 cm depth (slice X normal, slice 4 restored), upper limit - ref 90th percentile

#### load packages ####
library("tidyverse")


#Restored channel geometry:
#read in hydraulic timeseries 
hyd.data.restored <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Manuscripts/CEFF_casestudy/data/hydraulics/201105_Aliso_Recalibration_Update/J01-020_hydraulic_outputs_restoredxs.csv")
#read in rating curve data (in case current does not include values in range)
rating.data.restored <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Manuscripts/CEFF_casestudy/data/hydraulics/rating_curve_data_restoredXS/J01-020_Aliso_1_42_ratingcurve.data.restoredxs.csv")

#read in channel geometry
geom.restored <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Manuscripts/CEFF_casestudy/data/aliso_geometry_revised.csv")
#read in split channel locations
split <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Manuscripts/CEFF_casestudy/data/lookup_reach_manningsn_channeltype_splits_092021_restoredxs.csv") %>% 
  filter(X_SECT_ID == "Aliso_1_42")
#split column indices
split.col.ind <- grep("split", names(split))
#add in channel split info
split.sub <- split[,split.col.ind] %>% 
  pivot_longer(names(split[,split.col.ind])) %>% 
  as.matrix(nrow = length(split.col.ind), ncol=2) %>% 
  data.frame()
#split stations listed into a vector
split.stations.all <- as.numeric(na.omit(split.sub$value))
split.num <- length(split.stations.all)
#find actual station value closes to split.stations.all
split.stations.actual <- NA

for(l in 1:split.num){
  #find index of station closest to split.stations.all
  ind.station <- which(abs(geom.restored$station_m - split.stations.all[l])==min(abs(geom.restored$station_m - split.stations.all[l]), na.rm = TRUE))
  split.stations.actual[l] <- geom.restored$station_m[ind.station] 
}
#find all of the breaks to subsection each off of (add the first and last stations to list), will use to subset channel later on
split.stations.to.subset <- c(0, split.stations.actual, geom.restored$station_m[length(geom.restored$station_m)])


#plot geometry with channel splits (slices)
xs.prof.restored <- ggplot(geom.restored, aes(x = station_m, y = ELEVATION_M)) +
  geom_line() + 
  #geom_point()+
  scale_x_continuous(breaks = round(seq(0, max(geom.restored$station_m), by = spacing),1), expand = c(0, 0)) +
  labs(title = "Lower Aliso", subtitle = "Restored Channel", x = "Station (m)", y = "Elevation (m)") +
  geom_vline(xintercept = split.stations.actual, linetype="longdash")

print(xs.prof.restored)

#Arroyo chub depth .53 m, interpolate using approx (this is actually channel capacity flow)
chub.min.cms.restored <- approx(hyd.data.restored$max.depth.m_slice3, hyd.data.restored$q.cms, 0.53)
chub.min.cfs.restored <- chub.min.cms.restored$y*35.314666212661

#Willow - depth 10 cm min, need to use rating curve data because depth never gets that low
willow.min.cms.restored <- approx(rating.data.restored$max.depth.m_slice3, rating.data.restored$q.cms, 0.1)
willow.min.cfs.restored <- willow.min.cms.restored$y*35.314666212661
#find the WSE that inundates the overbanks (8.851392 is WSE at channel capacity), slice 4 overbank
willow.overbank.cms.restored <- approx(hyd.data.restored$max.depth.m_slice4, hyd.data.restored$q.cms, 0.02)
willow.overbank.cfs.restored <- willow.overbank.cms.restored$y*35.314666212661




#Current channel geometry:
#read in hydraulic timeseries 
hyd.data <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Manuscripts/CEFF_casestudy/data/hydraulics/201105_Aliso_Recalibration_Update/J01-020_hydraulic_outputs_currentXS.csv")
#read in rating curve data (in case current does not include values in range)
rating.data <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Manuscripts/CEFF_casestudy/data/hydraulics/J01-020_Aliso_1_42_ratingcurve.data_orig.csv")

#read in channel geometry
geom <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/SOC WQIP - Flow Ecology Study - General/Manuscripts/CEFF_casestudy/data/aliso_geometry_original.csv")
#read in split channel locations
split <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics/XS_plots/XS_plots_updated_axis_labels/lookup_reach_manningsn_channeltype_splits_10192020.csv") %>% 
  filter(X_SECT_ID == "Aliso_1_42")
#split column indices
split.col.ind <- grep("split", names(split))
#add in channel split info
split.sub <- split[,split.col.ind] %>% 
  pivot_longer(names(split[,split.col.ind])) %>% 
  as.matrix(nrow = length(split.col.ind), ncol=2) %>% 
  data.frame()
#split stations listed into a vector
split.stations.all <- as.numeric(na.omit(split.sub$value))
split.num <- length(split.stations.all)
#find actual station value closes to split.stations.all
split.stations.actual <- NA

for(l in 1:split.num){
  #find index of station closest to split.stations.all
  ind.station <- which(abs(geom$station_m - split.stations.all[l])==min(abs(geom$station_m - split.stations.all[l]), na.rm = TRUE))
  split.stations.actual[l] <- geom$station_m[ind.station] 
}
#find all of the breaks to subsection each off of (add the first and last stations to list), will use to subset channel later on
split.stations.to.subset <- c(0, split.stations.actual, geom$station_m[length(geom$station_m)])


#plot geometry with channel splits (slices)
xs.prof.current <- ggplot(geom, aes(x = station_m, y = ELEVATION_M)) +
  geom_line() + 
  #geom_point()+
  scale_x_continuous(breaks = round(seq(0, max(geom$station_m), by = spacing),1), expand = c(0, 0)) +
  labs(title = "Lower Aliso", subtitle = "Current Channel", x = "Station (m)", y = "Elevation (m)") +
  geom_vline(xintercept = split.stations.actual, linetype="longdash")

print(xs.prof.current)

#xs geometry change plot
combined.plot <- ggplot(geom.restored, aes(x = station_m, y = ELEVATION_M)) +
  geom_line(color="red") + 
  #geom_point()+
  scale_x_continuous(breaks = round(seq(0, max(geom.restored$station_m), by = spacing),1), expand = c(0, 0)) +
  labs(title = "Lower Aliso", subtitle = "Restored Channel", x = "Station (m)", y = "Elevation (m)") +
  geom_line(geom, mapping = aes(x = station_m, y = ELEVATION_M)) +
  xlim(14, 21) + ylim(NA, 10)

print(combined.plot)


#Arroyo chub depth .53 m, interpolate using approx 
chub.min.cms <- approx(hyd.data$max.depth.m_slice3, hyd.data$q.cms, 0.53)
chub.min.cfs <- chub.min.cms$y*35.314666212661


#Willow - depth 10 cm min, need to use rating curve data because depth never gets that low
willow.min.cms <- approx(rating.data$max.depth.m_slice3, rating.data$q.cms, 0.1)
willow.min.cfs <- willow.min.cms$y*35.314666212661
#find the WSE that inundates the overbanks (8.741664 is WSE at channel capacity), slice 4 overbank
willow.overbank.cms <- approx(hyd.data$max.depth.m_slice4, hyd.data$q.cms, 0.02)
willow.overbank.cfs <- willow.overbank.cms$y*35.314666212661 #too low because of that high flow channel, need to find WSE
#thalweg elevation
thalweg.current <- min(geom$ELEVATION_M)
#WSE we want to find 8.741664 m at capacity, 2 cm above this one
thalweg.slice4.overbank <- 8.668512
depth.to.aim.for.slice4 <- 8.741664+.02 - thalweg.slice4.overbank
#find the WSE that inundates the overbanks slice 4, 2 cm above capacity 8.741664 (9 cm max depth in overbank)
willow.overbank.cms <- approx(hyd.data$max.depth.m_slice4, hyd.data$q.cms, depth.to.aim.for.slice4)
willow.overbank.cfs <- willow.overbank.cms$y*35.314666212661 #too low because of that high flow channel, need to find WSE

#current channel capacity flow (contained in channel) - max wet/dry season baseflow for willow
depth.capacity <-  8.741664 - thalweg.current
capacity.current.cms <- approx(hyd.data$max.depth.m_slice3, hyd.data$q.cms, depth.capacity)
capacity.current.cfs <- capacity.current.cms$y*35.314666212661 #too low because of that high flow channel, need to find WSE





       