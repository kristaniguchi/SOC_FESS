#Component alteration synthesis summary table for FUTURE CLIMATE SCENARIOS - ALISO CREEK WATERSHED
#create summary table for component alteration based on metric alteration script
#combine low-flow bias corrected into dataframes
#also combine low flow bias corrected dataframes for percentiles

library("tidyverse")
library("ggplot2")
library("dplyr")

#compile all alteration comparison df for each time period, gcm, subbasin (...alteration_comparison_future_historical_statewide...csv)
#after rerunning ffc future climate for lowflow and non, need to compile the percentiles and flow metric results and 



#read in alteration summary table Future scenarios
#from Aliso
data1 <- read.csv(file="L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Future_Climate_Aliso/ffm_alteration.df.overall.join.csv")
data1$subbasin.model <- as.character(data1$subbasin.model)


#replace alteration data from low-flow bias (low flow doesn't have duplicate Osos in low flow bias)
#Aliso low flow
data2 <- read.csv(file="L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Future_Climate_Aliso_lowflowbias/ffm_alteration.df.overall.join.csv")
data2$subbasin.model <- as.character(data2$subbasin.model)
data2$subbasin <- as.character(data2$subbasin)
#omit NA rows
NA.ind <- is.na(data2$subbasin.model)
data2 <- data2[!NA.ind,]
#get unique low flow bias sites, remove all non low flow bias corrected row values from delta1 to delta1 to 3, and then rbind
unique.lowflow.sites <- unique(data2$subbasin.model)


#alteration directory
alt.dir.name <- "Future_Climate_Aliso_ALL_lowflowbias"
alteration.dir <- paste0("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/", alt.dir.name)


#remove all low flow bias rows from data1 
data.nolowflow <- data1[! (data1$subbasin.model %in% unique.lowflow.sites),]
#combine with low flow bias corrected deltaH
data <- data.nolowflow %>% 
  bind_rows(data2)

#replace base flow with baseflow for all FFM and components
data <- data.frame(lapply(data, function(x){
  gsub("base flow", "baseflow", x, ignore.case = FALSE)
})) 

#replace Base Flow with Baseflow for all FFM and components
data <- data.frame(lapply(data, function(x){
  gsub("Base Flow", "Baseflow", x, ignore.case = FALSE)
})) 


#write combine ffm alteration
write.csv(data, file = paste0(alteration.dir, "/ffm_alteration.df.overall.join.Aliso.ALL.lowflowbias_climate.csv"), row.names = FALSE)

##############################################################
#loop to summarize alteration component
basins <- unique(data$subbasin.model)

#create empty df
comp_summary <- data.frame(matrix(nrow=1, ncol=9))
names(comp_summary) <- c("COMID", "subbasin_model", "subbasin", "flow_component", "component_alteration", "n_ffm_altered", "flow_metrics_altered", "flow_char_altered", "gcm")

for(i in 1:length(basins)){
  #subset to basin i
  sub <- data[data$subbasin.model == basins[i],]
  
  #find all gcms
  gcms.all <- unique(sub$gcm)
  
  for(k in 1:length(gcms.all)){
    #subset to gcm.all k
    sub.gcm.k <- sub[sub$gcm == gcms.all[k],]
    
    #############
    #loop through each unique component
    unique.comp <- unique(sub.gcm.k$flow_component)
    
    #empty component df to be filled in and appended to summary df
    temp_df <- comp_summary[1,]
    
    for(j in 1:length(unique.comp)){
      comp <- sub.gcm.k[sub.gcm.k$flow_component == unique.comp[j],]
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
      row <- c(comp$COMID[1], as.character(comp$subbasin.model[1]), as.character(comp$subbasin[1]), as.character(component), comp_alt, n_ffm_altered, flow_metrics_altered, flow_char_altered, gcms.all[k])
      temp_df[j,] <- row
    }
    
    #save temporary df to summary
    comp_summary <- rbind(comp_summary, temp_df)
  }
  
  
}

#remove first row of NA
comp_summary2 <- comp_summary[2:length(comp_summary$COMID),]

#write comp summary df
write.csv(comp_summary2, file = paste0(alteration.dir, "/summary_component_alteration.csv"), row.names = FALSE)


###################################################################################
#Percentiles reg and low flow bias combine to common dataframe

#read in alteration summary table Future scenarios
#from Aliso
data1 <- read.csv(file="L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Future_Climate_Aliso/percentiles.FFM.future.climate.all.scenarios.csv")
data1$subbasin.model <- as.character(data1$subbasin.model)


#replace alteration data from low-flow bias (low flow doesn't have duplicate Osos in low flow bias)
#Aliso low flow
data2 <- read.csv(file="L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/Future_Climate_Aliso_lowflowbias/percentiles.FFM.future.climate.all.scenarios.csv")
data2$subbasin.model <- as.character(data2$subbasin.model)
#omit NA rows
NA.ind <- is.na(data2$subbasin.model)
data2 <- data2[!NA.ind,]
#get unique low flow bias sites, remove all non low flow bias corrected row values from delta1 to delta1 to 3, and then rbind
unique.lowflow.sites <- unique(data2$subbasin.model)

#remove all low flow bias rows from data1 
data.nolowflow <- data1[! (data1$subbasin.model %in% unique.lowflow.sites),]
#combine with low flow bias corrected deltaH
data <- data.nolowflow %>% 
  bind_rows(data2)

#replace base flow with baseflow for all FFM and components
data <- data.frame(lapply(data, function(x){
  gsub("base flow", "baseflow", x, ignore.case = FALSE)
})) 

#replace Base Flow with Baseflow for all FFM and components
data <- data.frame(lapply(data, function(x){
  gsub("Base Flow", "Baseflow", x, ignore.case = FALSE)
})) 


#write combine ffm alteration
write.csv(data, file = paste0(alteration.dir, "/percentiles.FFM.future.climate.all.scenarios.Aliso.ALL.lowflowbias.csv"), row.names = FALSE)


###Calculate the percent change in percentiles from historical to future
#for every subbasin, gcm, ffm calculate the %change in percentiles from historical to future

data.change <- data %>% 
  group_by(subbasin.model, gcm, metric) %>% 
  mutate_each()

pct.change <- function(x) { (x-lag(x))/lag(x) *100}

#pivot longer to have percentile column and value
data.change <- data %>% 
  gather(key=percentile, value=value, p10:p90) %>% 
  mutate(subbasin.gcm.metric.percentile = paste(subbasin.model, gcm, metric, percentile, sep=" "))

#unique subbasin.gcm.metric.percentile
unique.subbasin.gcm.metric.percentile <- unique(data.change$subbasin.gcm.metric.percentile)

#output data frame for deltaH values and also historical and midcentury
output.pctchange.percentiles <- data.frame(matrix(NA, 1, 13))
#set column names same as data.change and add in columns for historical and future values
names(output.pctchange.percentiles) <- c(names(data.change), "historical.value", "future.value")

for(m in 1:length(unique.subbasin.gcm.metric.percentile)){
  #subset data.change
  data.change.sub <- data.change[data.change$subbasin.gcm.metric.percentile == unique.subbasin.gcm.metric.percentile[m],]
  #calculate percent change from historical to future
  #future value
  future.value <- as.numeric(data.change.sub$value[data.change.sub$timeperiod == "future"])
  #historical value
  historical.value <- as.numeric(data.change.sub$value[data.change.sub$timeperiod == "historical"])
  pct.change <-  (future.value-historical.value)/historical.value
  
  #save future row as temp row and output it with new values
  temp.row <- data.change.sub[data.change.sub$timeperiod == "future",]
  #replace new values
  temp.row$value <- pct.change
  temp.row$future.value <- future.value
  temp.row$historical.value <- historical.value
  
  #save into overall df
  output.pctchange.percentiles <- rbind(output.pctchange.percentiles, temp.row)
  
}



#change value column to pct.change.value
names(output.pctchange.percentiles)[10] <- "pct.change.value"

#remove the first NA row
output.pctchange.percentiles <- output.pctchange.percentiles[2:length(output.pctchange.percentiles$metric),]

#filename
filenam.pctchange <- paste0(alteration.dir, "/pct.change.percentiles.FFM.future.climate.all.scenarios.Aliso.ALL.lowflowbias.csv")
write.csv(output.pctchange.percentiles, file=filenam.pctchange, row.names = FALSE)



#######################
#create heatmap of alteration statuses and number of subbasins considered likely altered
#read in alteration summary table
data <- read.csv(file=paste0(alteration.dir, "/ffm_alteration.df.overall.join.Aliso.ALL.lowflowbias_climate.csv"))
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
