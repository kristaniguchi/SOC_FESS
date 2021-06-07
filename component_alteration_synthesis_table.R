#Component alteration synthesis summary table
  #create summary table for component alteration based on metric alteration script
#combine with San Juan data

library("tidyverse")
library("ggplot2")
library("dplyr")



#read in alteration summary table
#from Aliso
data1 <- read.csv(file="L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Aliso_RecalibrationUpdate/ffm_alteration.df.overall.join.csv")
data1$subbasin.model <- as.character(data1$subbasin.model)
#from Oso and other watersheds
data2 <- read.csv(file="L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Oso_SmallCreeks/ffm_alteration.df.overall.join.csv")
data2$subbasin.model <- as.character(data2$subbasin.model)
#for data2 Oso output data, exclude Oso subbasins since recalibration is included in San Juan data3 outputs
oso.subbasins <- c("403010", "403011", "403020", "403030", "404010", "405010", "405020")
data2 <- data2[! (data2$subbasin.model %in% oso.subbasins),]

#from San Juan - LSPC
data3 <- read.csv(file="L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/SanJuan_LSPC/ffm_alteration.df.overall.join.csv")
data3$subbasin.model <- as.character(data3$subbasin.model)
data3$subbasin <- as.character(data3$subbasin)


#replace alteration data from low-flow bias
data4 <- read.csv(file="L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/low.flow.bias.all/ffm_alteration.df.overall.join.csv")
data4$subbasin.model <- as.character(data4$subbasin.model)
data4$subbasin <- as.character(data4$subbasin)
#get unique low flow bias sites, remove all non low flow bias corrected row values from delta1 to delta1 to 3, and then rbind
unique.lowflow.sites <- unique(data4$subbasin.model)



#alteration directory
#alt.dir.name <- "Oso_SmallCreeks"
#alt.dir.name <- "SanJuan_LSPC"
alt.dir.name <- "Aliso_Oso_SmCk_SanJuan_all_lowflowbias"
alteration.dir <- paste0("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/", alt.dir.name)

#merge into one df 
data <- full_join(data1, data2) %>% 
  full_join(data3)
#remove all low flow bias rows and add to overall data
data <- data[! (data$subbasin.model %in% unique.lowflow.sites),]
#combine with low flow bias corrected deltaH
data <- data %>% 
  bind_rows(data4)

#replace base flow with baseflow for all FFM and components
data <- data.frame(lapply(data, function(x){
  gsub("base flow", "baseflow", x, ignore.case = FALSE)
})) 

#replace Base Flow with Baseflow for all FFM and components
data <- data.frame(lapply(data, function(x){
  gsub("Base Flow", "Baseflow", x, ignore.case = FALSE)
})) 


#write combine ffm alteration
write.csv(data, file = paste0(alteration.dir, "/ffm_alteration.df.overall.join.Aliso.Oso.SmallCreeks.SanJuanLSPC.lowflowbias.csv"), row.names = FALSE)

##############################################################
#loop to summarize alteration component
basins <- unique(data$subbasin.model)

#create empty df
comp_summary <- data.frame(matrix(nrow=1, ncol=8))
names(comp_summary) <- c("COMID", "subbasin_model", "subbasin", "flow_component", "component_alteration", "n_ffm_altered", "flow_metrics_altered", "flow_char_altered")

for(i in 1:length(basins)){
  #subset to basin i
  sub <- data[data$subbasin.model == basins[i],]

  #############
  #loop through each unique component
  unique.comp <- unique(sub$flow_component)
  #empty component df to be filled in and appended to summary df
  temp_df <- comp_summary[1,]
  for(j in 1:length(unique.comp)){
    comp <- sub[sub$flow_component == unique.comp[j],]
    #component j
    component <- unique.comp[j]

    #if one metric is likley altered, all altered
    ind.altered <- grep("likely_altered", comp$component_alteration)
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
    row <- c(comp$COMID[1], as.character(comp$subbasin.model[1]), as.character(comp$subbasin[1]), as.character(component), comp_alt, n_ffm_altered, flow_metrics_altered, flow_char_altered)
    temp_df[j,] <- row
  }
  
  #save temporary df to summary
  comp_summary <- rbind(comp_summary, temp_df)
}

#remove first row of NA
comp_summary2 <- comp_summary[2:length(comp_summary$COMID),]

#write comp summary df
write.csv(comp_summary2, file = paste0(alteration.dir, "/summary_component_alteration.csv"), row.names = FALSE)




#######################
#create heatmap of alteration statuses and number of subbasins considered likely altered
#read in alteration summary table
data <- read.csv(file=paste0(alteration.dir, "/ffm_alteration.df.overall.join.Aliso.Oso.SmallCreeks.SanJuanLSPC.lowflowbias.csv"))
names(data)

#summary table with number of subbasins that are in each alteration category for each ffm
ffm_summary <- data.frame(aggregate(data, by = data[c('ffm','alteration.status', 'flow_component')], length))

#subset to likley altered only
#ffm_summary_altered <- ffm_summary[ffm_summary$alteration_status == "likely_altered",]
#df_heatmap <- ffm_summary_altered[,c(1, 5)]
#names(df_heatmap) <- c("Flow Metric", "Likely Altered Subbasins (n)")

#histogram of alteartion statuses
#add in facet wrap to show the different elements
ffm_summary$alteration.status <- factor(ffm_summary$alteration.status, levels=c("likely_altered", "likely_unaltered", "indeterminant"))

g <- ggplot(ffm_summary) +
  geom_bar(color = "black", aes(x= ffm, y =  subbasin, fill = alteration.status), stat = "identity", position = position_fill(reverse = FALSE), width = 0.7) +
  ggtitle("Flow Metric Alteration Status") +
  guides(fill = guide_legend(reverse = FALSE)) +
  xlab("Functional Flow Metrics") + ylab("Proportion of Subbasins") +
  facet_wrap(~factor(flow_component, levels = c("Fall pulse flow", "Wet-season baseflow", "Peak flow", "Spring recession flow", "Dry-season baseflow")), scales="free_x", nrow=1) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") +
  scale_fill_manual(name = "Alteration Status", labels = c("Likely Altered", "Likely Unaltered", "Indeterminate"), values = c("#ca0020","#0571b0","gray100")) 

g

#ggsave(g, filename="C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/flowmetricalteration_histogram.jpg", dpi=300, height=5, width=13)
#ggsave(sub1, filename="C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/subbasin1_compalteration.jpg", dpi=300, height=8, width=8)
