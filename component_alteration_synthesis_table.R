#Component alteration synthesis summary table
  #create summary table for component alteration based on metric alteration script

library("tidyverse")
library("ggplot2")



#read in alteration summary table
data <- read.csv(file="L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/ffm_alteration.df.overall.join.csv")

#loop to summarize alteration component
basins <- unique(data$subbasin_model)

#create empty df
comp_summary <- data.frame(matrix(nrow=1, ncol=8))
names(comp_summary) <- c("COMID", "subbasin_model", "subbasin", "flow_component", "component_alteration", "n_ffm_altered", "flow_metrics_altered", "flow_char_altered")

for(i in 1:length(basins)){
  #subset to basin i
  sub <- data[data$subbasin_model == basins[i],]

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
    row <- c(comp$COMID[1], as.character(comp$subbasin_model[1]), as.character(comp$subbasin[1]), as.character(component), comp_alt, n_ffm_altered, flow_metrics_altered, flow_char_altered)
    temp_df[j,] <- row
  }
  
  #save temporary df to summary
  comp_summary <- rbind(comp_summary, temp_df)
}

#remove first row of NA
comp_summary2 <- comp_summary[2:length(comp_summary$COMID),]


#write.csv(comp_summary2, file = "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/summary_component_alteration.csv", row.names = FALSE)




########
#create heatmap of alteration statuses and number of subbasins considered likely altered
#read in alteration summary table
data <- read.csv(file="L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/ffm_alteration.df.overall.join.csv")


#summary table with number of subbasins that are in each alteration category for each ffm
ffm_summary <- data.frame(aggregate(data, by = data[c('ffm','alteration_status', 'flow_component')], length))

#subset to likley altered only
#ffm_summary_altered <- ffm_summary[ffm_summary$alteration_status == "likely_altered",]
#df_heatmap <- ffm_summary_altered[,c(1, 5)]
#names(df_heatmap) <- c("Flow Metric", "Likely Altered Subbasins (n)")

#histogram of alteartion statuses
#add in facet wrap to show the different elements
ffm_summary$alteration_status <- factor(ffm_summary$alteration_status, levels=c("likely_altered", "likely_unaltered", "indeterminant"))

g <- ggplot(ffm_summary) +
  geom_bar(color = "black", aes(x= ffm, y =  subbasin, fill = alteration_status), stat = "identity", position = position_fill(reverse = FALSE), width = 0.7) +
  ggtitle("Flow Metric Alteration Status") +
  guides(fill = guide_legend(reverse = FALSE)) +
  xlab("Functional Flow Metrics") + ylab("Proportion of Subbasins") +
  facet_wrap(~factor(flow_component, levels = c("Fall pulse flow", "Wet-season base flow", "Peak flow", "Spring recession flow", "Dry-season base flow")), scales="free_x", nrow=1) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") +
  scale_fill_manual(name = "Alteration Status", labels = c("Likely Altered", "Likely Unaltered", "Indeterminate"), values = c("#ca0020","#0571b0","gray100")) 

g

ggsave(g, filename="C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/flowmetricalteration_histogram.jpg", dpi=300, height=5, width=13)
ggsave(sub1, filename="C:/Users/KristineT.SCCWRP2K/Documents/Git/SOC_FESS/subbasin1_compalteration.jpg", dpi=300, height=8, width=8)
