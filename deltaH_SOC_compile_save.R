#Compile deltaH values for current conditions from multiple model runs (LSPC orig, San Juan LSPC with updated Oso, low-flow bias)

#load libraries
library("tidyverse")

#Read in DeltaH files from multiple directories
#Aliso Recalibration update
deltaH1 <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Aliso_RecalibrationUpdate/DeltaH_Aliso_Current/DeltaH_Aliso.csv")
#Oso and small creeks (will only use small creeks because San Juan LSPC has updated Oso)
deltaH2 <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Oso_SmallCreeks/DeltaH_Oso_Current/DeltaH_Oso_SmallCreeks_Q99.csv")
  #for Oso deltaH, exclude Oso subbasins since recalibration is included in San Juan deltaH
  oso.subbasins <- c("403010", "403011", "403020", "403030", "404010", "405010", "405020")
  deltaH2 <- deltaH2[! (deltaH2$site %in% oso.subbasins),]
#San Juan LSPC updated model
deltaH3 <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/SanJuan_LSPC/DeltaH_SanJuan_LSPC/DeltaH_SanJuan.csv")
#Low flow bias corrected reaches 
deltaH4 <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/low.flow.bias.all/DeltaH_lowflowbias.all/DeltaH_SanJuan_LowFlowBiasCorrected.csv")
#get unique deltaH4 sites, remove all non low flow bias corrected deltaH values from delta1 to delta3 and then rbind
unique.lowflow.sites <- unique(deltaH4$site)

#COMBINE all outputs but replace duplicates within each
#bind rows from Aliso, small creeks, San Juan
deltaH.all <- deltaH1 %>% 
  bind_rows(deltaH2) %>% 
  bind_rows(deltaH3)
#remove the low flow bias sites from the non-corrected data
deltaH.all <- deltaH.all[! (deltaH.all$site %in% unique.lowflow.sites),]

#combine with low flow bias corrected deltaH
deltaH.all <- deltaH.all %>% 
  bind_rows(deltaH4)

length(unique(deltaH.all$site))

write.csv(deltaH.all, file="L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/DeltaH_lowflowbias_all_Aliso_Oso_SmCk_SanJuan_LSPC_Current.csv", row.names=FALSE)
