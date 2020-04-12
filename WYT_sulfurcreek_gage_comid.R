#WYT determinitation based on long-term precip data

#load library
library("devtools")
library("ffcAPIClient")
library("ggplot2")
library("scales")
library("purrr")
library("tidyverse")

#my token for FFC API Client
mytoken <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJLcmlzIiwibGFzdE5hbWUiOiJUYW5pZ3VjaGkgUXVhbiIsImVtYWlsIjoia3Jpc3RpbmV0cUBzY2N3cnAub3JnIiwicm9sZSI6IlVTRVIiLCJpYXQiOjE1NzM4NjgwODN9.UJhTioLNNJOxvY_PYb_GIbcMRI_qewjkfYx-usC_7ZA"

#read in precip data
gage.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200326_Gauge_Data/"
precip <- read.csv(paste0(gage.dir, "rainfall_sulfur_creek.csv"))

#sort precip, find terciles, determine WYT based on terciles
sort.precip <- precip[order(precip$WY.Total),]
sort.precip$tercile <- ntile(sort.precip$WY.Total, 3) 
sort.precip$WYT <- sort.precip$tercile
#rename tercile to dry, moderate, wet
sort.precip$WYT[sort.precip$WYT==1] <- "Dry"
sort.precip$WYT[sort.precip$WYT==2] <- "Moderate"
sort.precip$WYT[sort.precip$WYT==3] <- "Wet"
#sort based on date
sort.precip <- sort.precip[order(sort.precip$WY),]

#write csv
write.csv(sort.precip, file=paste0(gage.dir, "/WYT_sulfurcreek_raingage_1975_2019.csv"))

#######Compare to WYT designation based on COMID, from Ted's model
gage.comid <- 20348653

#read in WYT file by COMID
wyt.comid <- read.csv("L:/CA  E-flows framework_ES/Misc/socal_statewidemodel_evaluation/WYT_ALL_COMIDS.csv")
#subset to COMID
gage.wyt <- wyt.comid[wyt.comid$COMID == gage.comid,]
#subset to match dates
ind.start <- grep(1975, gage.wyt$year)
gage.wyt.sub <- gage.wyt[ind.start: length(gage.wyt$year),]
write.csv(gage.wyt, file=paste0(gage.dir, "/WYT_sulfurcreek_raingage_COMID1.csv"), row.names=FALSE)

#get WYT based on aliso gages
#Gage info
gage <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200312_Updated_Calibration/WY15-19_Calibration_Assessment/WY15-19_Statistical_Summary_v22M_KTQ.csv", skip=5, header=TRUE)
#only use first 3 rows of gage info
gage <- gage[1:3,]

for (i in 1:3){
  comid.wyt.dat <- wyt.comid[wyt.comid$COMID == gage$COMID[i],]
  write.csv(comid.wyt.dat, file= paste0(gage.dir, "/WYT_COMID", gage$COMID[i],"_subbasin", gage$LSPC.Subbasin[i],".csv"), row.names=FALSE)
  
}
