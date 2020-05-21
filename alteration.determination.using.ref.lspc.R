#Flow alteration determination using own reference model (not statewide statistical)

#thresholds: if median outside 10th and 90th percentile range then likely altered

#read in all current annual metrics results to be compared to reference
gage.curr <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200326_Gauge_Data/daily/FFMs/201020/obs.results.ffm.all.csv")
#model lspc hourly timestep
#model.curr <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200411_Updated_Calibration/WY94-Present/daily/FFMs/201020/pred.results.ffm.all.csv")
#model lspc daily timestep
model.curr <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200507_DailyTotalQ_prelimtofuturegcm//pred.results.ffm.all.csv")


#read in reference annual metrics results to be compared to current to assess alteration (LSPC reference)
model.ref.percentiles <- read.csv("L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200422_Predevelopment_Reference_Condition/WY94-Present/daily/FFMs/201020/pred.ref.lspc.percentiles.all.csv")

#Loop to calculate medians for each flow metric in each file
cols <- unique(names(gage.curr))
ffm <- cols[2:(length(cols)-1)]
alteration.gage <- NA
alteration.model <- NA

for(i in 1:length(ffm)){
  #gage results for ffm i
  gage.ffms.i <- gage.curr[,ffm[i]]
  gage.i.med <- median(gage.ffms.i, na.rm = TRUE)
  #model results for ffm i
  model.curr.ffms.i <- model.curr[,ffm[i]]
  model.curr.i.med <- median(model.curr.ffms.i, na.rm = TRUE)
  #find 10-90th ref percentiles
  model.ref.i.90 <- model.ref.percentiles$p90[model.ref.percentiles$metric == ffm[i]]
  model.ref.i.10 <- model.ref.percentiles$p10[model.ref.percentiles$metric == ffm[i]]
  #if median outside of 10-90, likely altered
  if(gage.i.med > model.ref.i.90 | gage.i.med < model.ref.i.10){
    alteration.gage[i] <- "Likely Altered"
  }else{
    #if median falls within 10-90th and >50% falls within the range
    #determine how many values fall in the range
    count.in.range <- length(which(gage.ffms.i <= model.ref.i.90 & gage.ffms.i >= model.ref.i.10))
    percent.inrange <- count.in.range/length(na.omit(gage.ffms.i))
    
    if(gage.i.med < model.ref.i.90 & gage.i.med > model.ref.i.10 & percent.inrange > 0.5){
      alteration.gage[i] <- "Likely Unaltered"
    }else{
      alteration.gage[i] <- "Indeterminate"
    }
  }
  #Alteration assessment for model current
    if(model.curr.i.med > model.ref.i.90 | model.curr.i.med < model.ref.i.10){
      alteration.model[i] <- "Likely Altered"
    }else{
      #if median falls within 10-90th and >50% falls within the range
      #determine how many values fall in the range
      count.in.range2 <- length(which(model.curr.ffms.i <= model.ref.i.90 & model.curr.ffms.i >= model.ref.i.10))
      percent.inrange2 <- count.in.range2/length(na.omit(model.curr.ffms.i))

      if(model.curr.i.med < model.ref.i.90 & model.curr.i.med > model.ref.i.10 & percent.inrange2 > 0.5){
        alteration.model[i] <- "Likely Unaltered"
      }else{
        alteration.model[i] <- "Indeterminate"
      }
    }
}

alteration.comparison.df <- data.frame(cbind(ffm, alteration.gage, alteration.model))

#write.csv(alteration.comparison.df, "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200411_Updated_Calibration/WY94-Present/daily/FFMs/201020/alteration.lspcrefmodel.gage.lspccurrent.csv")
write.csv(alteration.comparison.df, "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200507_DailyTotalQ_prelimtofuturegcm/alteration.lspcrefmodel.gage.lspccurrentdaily.csv")
