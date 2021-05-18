#Manuscript: Watershed Prioritization DSS - Level 1 and 2
#Explore level 1 and 2 alteration

#libraries
library("tidyverse")
library("ggplot2")
library("dplyr")


######################################################################
#Level 1 alteration data exploration

#alteration directory
alt.dir.name <- "Oso_SmallCreeks"
alteration.dir <- paste0("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/", alt.dir.name)

#read in combined ffm alteration: Aliso, Oso, small creeks
alt.data <- read.csv(paste0(alteration.dir, "/ffm_alteration.df.overall.join.Aliso.Oso.SmallCreeks.csv"))
ffm.sites <- unique(alt.data$subbasin.model)
#calculate alteration intensity
#calculate the intensity of alteration for each subbasin using all FFMs: total number of likely altered metrics out of the 24 using entire POR
intensity.df.POR.allmetrics <- alt.data %>% 
  group_by(subbasin.model, alteration.status) %>% 
  tally() %>% 
  ungroup()  %>% 
  filter(alteration.status == "likely_altered")



#read in DeltaH data - annually for POR
#calculate alteration status for Q99 metric
DeltaH_data1 <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Oso_SmallCreeks/DeltaH_Oso_Current/DeltaH_Oso_SmallCreeks_Q99.csv")
DeltaH_data2 <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Aliso_RecalibrationUpdate/DeltaH_Aliso_Current/DeltaH_Aliso.csv")
#bind rows together
DeltaH_data <- DeltaH_data1 %>% 
  bind_rows(DeltaH_data2)
#subset to Q99
Q99_deltaH <- DeltaH_data %>% 
  filter(flow_metric == "Q99")
#group by subbasin, 
Q99.percentiles <- Q99_deltaH %>% 
  group_by(site) %>% 
  summarise(ref.percentiles = quantile(reference_value, c(.1,.25,.5,.75,.9), na.rm=TRUE),
            current.percentiles = quantile(current_value, c(.1,.25,.5,.75,.9), na.rm=TRUE)) %>% 
  mutate(percentiles = c(.1,.25,.5,.75,.9))
#####assess alteration for Q99
#list unique subbasins #only keep sites that also have FFM calculated, omit sites 501010, 501011, 501020 - NA values for ref and no FFM calculated for those, 301011 no FFM values but Q99 calculated?
unique.subbasins <- unique(Q99.percentiles$site[Q99.percentiles$site %in% ffm.sites]) 


#empty df of alteration status
alteration.status <- data.frame(matrix(NA, length(unique.subbasins), 4))
names(alteration.status) <- c("subbasin.model", "ffm", "alteration.status", "alteration.direction")
#save ffm as Q99
alteration.status$ffm <- "Q99"

for(i in 1:length(unique.subbasins)) {
  #subset percentiles
  sub <- Q99.percentiles[Q99.percentiles$site == unique.subbasins[i],]
  #subset annual Q99 values to count number in range and outside p10-p90 range
  sub.Q99.values <- Q99_deltaH[Q99_deltaH$site ==  unique.subbasins[i],]
  #save site name into output df
  alteration.status$subbasin.model[i] <- as.numeric(unique.subbasins[i])
  
  #alteration determination
  #if median falls outside of 10-90, likely altered
  current.p50 <- sub$current.percentiles[sub$percentiles == "0.5"]
  ref.p10 <- sub$ref.percentiles[sub$percentiles == "0.1"]
  ref.p90 <- sub$ref.percentiles[sub$percentiles == "0.9"]
  
  if(current.p50 > ref.p90 | current.p50 < ref.p10){
    alteration.status$alteration.status[i] <- "likely_altered"
    #if it is altered determine direction of alteration high or low
    if(current.p50 > ref.p90){
      alteration.status$alteration.direction[i] <- "high"
    }else{
      alteration.status$alteration.direction[i] <- "low"
    }
  }else{
    #if median falls within 10-90th and >50% falls within the range, then likely unaltered or <50% falls within range (indeterminate)
    #since not altered, no alteration direction
    alteration.status$alteration.direction[i] <- "none_found"
    #determine how many values fall in the range
    count.in.range <- length(which(sub.Q99.values$current_value <= ref.p90 & sub.Q99.values$current_value >= ref.p10))
    percent.inrange <- count.in.range/length(na.omit(sub.Q99.values$current_value))
    
    if(percent.inrange > 0.5){
      alteration.status$alteration.status[i] <- "likely_unaltered"
      
    }else{
      alteration.status$alteration.status[i] <- "indeterminate"
    }
  }
}

#sum number of subbasins altered
alteration.status.Q99.count <- alteration.status %>% 
  group_by(alteration.status) %>% 
  tally() %>% 
  ungroup() 

#bind rows alt.data and Q99 alteration.status
alt.data2 <- alt.data %>% 
  bind_rows(alteration.status) 
#subset to only rows that have Q99 and FFM calculated
alt.data.sub <- alt.data2[alt.data2$subbasin.model %in% unique.subbasins,]

#calculate the intensity of alteration for each subbasin using all FFMs: total number of likely altered metrics out of the 25 using entire POR
intensity.df.POR.allmetrics <- alt.data.sub %>% 
  group_by(subbasin.model, alteration.status) %>% 
  tally() %>% 
  ungroup()  %>% 
  filter(alteration.status == "likely_altered")
#define overall alteration based on #DECISION: if >8 important metric is altered then index considered altered (if more than 1/3 metrics altered, considered altered)
intensity.df.POR.allmetrics$ffm.hyd.alt.level1 <- "likely_unaltered"
intensity.df.POR.allmetrics$ffm.hyd.alt.level1[intensity.df.POR.allmetrics$n >8] <- "likely_altered"


#calculate the intensity of alteration using only the 3 important metrics for CSCI and ASCI
#read in scaled bio thresholds from 0 to 1 for CSCI and ASCI to get important metrics
bio.thresholds <- read.csv("C:/Users/KristineT/SCCWRP/SOC WQIP - Flow Ecology Study - General/Tier2_analysis/08_all_delta_thresholds_scaled.csv")
unique.asci.metrics <- unique(bio.thresholds$metric[bio.thresholds$Biol == "ASCI"])
unique.csci.metrics <- unique(bio.thresholds$metric[bio.thresholds$Biol == "CSCI"])

#subset alt.data.sub to important metrics for ASCI and CSCI
alt.data.sub.asci.metrics <- alt.data.sub[alt.data.sub$ffm %in% unique.asci.metrics,]
alt.data.sub.csci.metrics <- alt.data.sub[alt.data.sub$ffm %in% unique.csci.metrics,]

#CSCI hyd alteration
#calculate the intensity of alteration using only the 3 important metrics for CSCI and ASCI
intensity.csci.metrics <- alt.data.sub.csci.metrics %>% 
  group_by(subbasin.model, alteration.status) %>% 
  tally() %>% 
  ungroup() 
#determine hyd alt for csci
#DECISION: if >1 important metric is altered then index considered altered (if more than 1/3 metrics altered, considered altered)
#change likely_unaltered 3 to likely_altered 0
tochange.unaltered3 <- which(intensity.csci.metrics$alteration.status == "likely_unaltered" & intensity.csci.metrics$n == 3)
#change to likely altered, n=0
intensity.csci.metrics$alteration.status[tochange.unaltered3] <- "likely_altered"
intensity.csci.metrics$n[tochange.unaltered3] <- 0
#remove indeterminate 1
ind.indet1 <-  which(intensity.csci.metrics$alteration.status == "indeterminate" & intensity.csci.metrics$n == 1)
#omit indeterminate 1
intensity.csci.metrics2 <- intensity.csci.metrics[-ind.indet1,]
#if indeterminate 2, metric alt considered indeterminate
intensity.csci.metrics2$csci.hyd.alt.level1 <- "" #create new column
intensity.csci.metrics2$csci.hyd.alt.level1[intensity.csci.metrics2$alteration.status == "indeterminate" & intensity.csci.metrics2$n == 2] <- "indeterminate"
#likely altered 0, then likely unaltered
intensity.csci.metrics2$csci.hyd.alt.level1[intensity.csci.metrics2$alteration.status == "likely_altered" & intensity.csci.metrics2$n == 0] <- "likely_unaltered"
#likely unaltered >=2, then likely unaltered
intensity.csci.metrics2$csci.hyd.alt.level1[intensity.csci.metrics2$alteration.status == "likely_unaltered" & intensity.csci.metrics2$n >=2] <- "likely_unaltered"
#likely altered >=2, then likely altered
intensity.csci.metrics2$csci.hyd.alt.level1[intensity.csci.metrics2$alteration.status == "likely_altered" & intensity.csci.metrics2$n >=2] <- "likely_altered"
#if likely altered 1 and likely unaltered 1 (means indeterminate 1), indeterminate
#subset to all with hyd alt
intensity.csci.metrics2.sub <- intensity.csci.metrics2[intensity.csci.metrics2$csci.hyd.alt.level1 != "",]
#to find missing from subset list, will be indeterminate
ind.missing <- setdiff(unique.subbasins, unique(intensity.csci.metrics2.sub$subbasin.model))  
#add missing into original intensity 
intensity.csci.metrics2$csci.hyd.alt.level1[intensity.csci.metrics2$subbasin.model == ind.missing] <- c("indeterminate", "")
#now subset to all with hyd alt
intensity.csci.metrics2.sub <- intensity.csci.metrics2[intensity.csci.metrics2$csci.hyd.alt.level1 != "",]

#asci hyd alteration
#calculate the intensity of alteration using only the 3 important metrics for asci and ASCI
intensity.asci.metrics <- alt.data.sub.asci.metrics %>% 
  group_by(subbasin.model, alteration.status) %>% 
  tally() %>% 
  ungroup() 
#determine hyd alt for asci
#DECISION: if >1 important metric is altered then index considered altered (if more than 1/3 metrics altered, considered altered)
#change likely_unaltered 3 to likely_altered 0
tochange.unaltered3 <- which(intensity.asci.metrics$alteration.status == "likely_unaltered" & intensity.asci.metrics$n == 3)
#change to likely altered, n=0
intensity.asci.metrics$alteration.status[tochange.unaltered3] <- "likely_altered"
intensity.asci.metrics$n[tochange.unaltered3] <- 0
#remove indeterminate 1
ind.indet1 <-  which(intensity.asci.metrics$alteration.status == "indeterminate" & intensity.asci.metrics$n == 1)
#omit indeterminate 1
intensity.asci.metrics2 <- intensity.asci.metrics[-ind.indet1,]
#if indeterminate 2, metric alt considered indeterminate
intensity.asci.metrics2$asci.hyd.alt.level1 <- "" #create new column
intensity.asci.metrics2$asci.hyd.alt.level1[intensity.asci.metrics2$alteration.status == "indeterminate" & intensity.asci.metrics2$n == 2] <- "indeterminate"
#likely altered 0, then likely unaltered
intensity.asci.metrics2$asci.hyd.alt.level1[intensity.asci.metrics2$alteration.status == "likely_altered" & intensity.asci.metrics2$n == 0] <- "likely_unaltered"
#likely unaltered >=2, then likely unaltered
intensity.asci.metrics2$asci.hyd.alt.level1[intensity.asci.metrics2$alteration.status == "likely_unaltered" & intensity.asci.metrics2$n >=2] <- "likely_unaltered"
#likely altered >=2, then likely altered
intensity.asci.metrics2$asci.hyd.alt.level1[intensity.asci.metrics2$alteration.status == "likely_altered" & intensity.asci.metrics2$n >=2] <- "likely_altered"
#if likely altered 1 and likely unaltered 1 (means indeterminate 1), indeterminate
#subset to all with hyd alt
intensity.asci.metrics2.sub <- intensity.asci.metrics2[intensity.asci.metrics2$asci.hyd.alt.level1 != "",]
#to find missing from subset list, will be indeterminate
ind.missing <- setdiff(unique.subbasins, unique(intensity.asci.metrics2.sub$subbasin.model))  
#add missing into original intensity 
intensity.asci.metrics2$asci.hyd.alt.level1[intensity.asci.metrics2$subbasin.model == ind.missing] <- c("indeterminate", "")
#now subset to all with hyd alt
intensity.asci.metrics2.sub <- intensity.asci.metrics2[intensity.asci.metrics2$asci.hyd.alt.level1 != "",]

#summarize number of altered subbasin depending on the number of metrics used
#all FFMs
altered.subbasins.allFFM.L1 <- intensity.df.POR.allmetrics %>% 
  group_by(ffm.hyd.alt.level1) %>% 
  tally() %>% 
  ungroup() 
#CSCI metrics
altered.subbasins.csci.L1 <- intensity.csci.metrics2.sub %>% 
  group_by(csci.hyd.alt.level1) %>% 
  tally() %>% 
  ungroup() 
#asci metrics
altered.subbasins.asci.L1 <- intensity.asci.metrics2.sub %>% 
  group_by(asci.hyd.alt.level1) %>% 
  tally() %>% 
  ungroup() 

#combine level 1 alterations to map across subbasins



######################################################################
#Level 2 alteration data exploration
# explore level 2 alteration with different time frames: past 25, 20, 15, 10 years




