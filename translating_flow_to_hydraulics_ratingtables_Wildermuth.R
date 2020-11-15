#Apply rating curves to the discharge data from Wildermuth model output (San Juan)
#for every subbasin, loop through read in hydraulic rating curve data, use approxfun to interpolate hyd variables based on flow
  #note: other script to translate LSPC model outputs

#load libraries
library("tidyverse")

####UPDATE THIS: directory and files for Wildermuth model
#current conditions
flow.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/200929_Wildermuth_Model_Results/Wildermuth_Model_Results_for_SCCWRP/"
#water conservation scenarios:
##aw20
#flow.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201002_Wildermuth_Water_Conservation_Scenario/aw20/Wildermuth_Model_Results_for_SCCWRP_Water_Conserv_aw20"
##aw20p5
#flow.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201002_Wildermuth_Water_Conservation_Scenario/aw20p5"
##aw20p10
#flow.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201002_Wildermuth_Water_Conservation_Scenario/aw20p10"
##aw20p20
#flow.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/201002_Wildermuth_Water_Conservation_Scenario/aw20p20"

#list files in directory
list.files <- list.files(flow.dir, full.names = TRUE, pattern ="\\.csv$")
#list subbasins
subbasins <- list.files(flow.dir, pattern ="\\.csv$") %>% 
  strsplit(split = "\\.") 
subbasins <- sapply(subbasins, `[`, 1)

#optional: to run only for missing slope sites, filter list.files and subbasins to only include those that didn't have slope
missing.slope <- read.csv("L:/San Juan WQIP_KTQ/Data/SpatialData/Hydraulics/missing.slope.XSECTID.csv")
#only include LSPC sites
missing.slope <- missing.slope[missing.slope$Source == "Wildermuth",]
#find index of missing sites that need to be run
ind.list.files <- which(subbasins %in% missing.slope$Reach.ID)
#subset to missing sites
list.files <- list.files[ind.list.files]
subbasins <- subbasins[ind.list.files]


#directory and files for rating tables
rating.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics/rating_curve_data/"
list.files.rating <- list.files(rating.dir, full.names = TRUE, pattern ="\\.csv$")

####UPDATE THIS: output directory for the hydraulics output
#current directory
output.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics/hydraulic_output_current_Wildermuth/"
#water conservation scenarios:
##aw20
#output.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics/hydraulic_output_water_conservation_Wildermuth/aw20/"
##aw20p5
#output.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics/hydraulic_output_water_conservation_Wildermuth/aw20p5/"
##aw20p10
#output.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics/hydraulic_output_water_conservation_Wildermuth/aw20p10/"
##aw20p20
#output.dir <- "L:/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics/hydraulic_output_water_conservation_Wildermuth/aw20p20/"


#lookup table with X_Sect_ID, Reach.ID (used for wildermuth), LSPC.ID
#model reach values (slope, manning's n)
reach.metrics <- read.csv("C:/Users/KristineT/Documents/Git/SOC_FESS/data/hydraulics/Full_Model_Reaches_av_geom_metrics.csv") %>% 
  select("Reach.ID", "LSPC.ID", "Slope", "Mannings.n", "Downstream.ID", "Downstream.LSPC.ID")
reach.metrics2 <- reach.metrics[2:length(reach.metrics$Reach.ID),]
#read in lookup table with subbasin and X_SECT_ID, filter, and merge with reach metrics by Reach.ID and merge with manning's n at outlet
lookup <- read.csv("C:/Users/KristineT/Documents/Git/SOC_FESS/data/hydraulics/nearest_XS_pourpoints_final.csv") %>% 
  rename(Reach.ID = Subbasin) %>% 
  select(X_SECT_ID, Reach.ID) %>% 
  merge(reach.metrics2, by = "Reach.ID") 

#determine which sites have reach parameters, when slope = 0, needs slope calculated
param.sites <- lookup[as.numeric(lookup$Slope) > 0,]

#remove sites without reach params from list files
#find index of param sites in subbasin list
ind.paramsites <- which(subbasins %in% param.sites$Reach.ID)
subbasins <- subbasins[ind.paramsites]
#subset list.files with only sites with reach params
list.files <- list.files[ind.paramsites]


#test site that worked
## i=3 site that works

for(i in 1:length(list.files)){
  #read in discharge data
  #####NEED UPDATE: add in if ends in .txt read.table, else read.csv
  q.data <- read.table(list.files[i], sep=",", header=TRUE)
  names(q.data) <- c("date", "flow")
  #format date mm/dd/yyyy
  date <- as.POSIXct(q.data$date, format = "%Y-%m-%d ")
  q.data$date <- format(date, "%m/%d/%Y")
  
  #output data in cfs, need to convert to cms
  q.data$q.cms <- q.data$flow/35.3147
  
  #find Reach.ID
  Reach.ID <- lookup$Reach.ID[lookup$Reach.ID == subbasins[i]]
  
  #read in rating curve data
  ind.curve <- grep(Reach.ID, list.files.rating)
  rating.data <- read.csv(list.files.rating[ind.curve], header=TRUE)
  
  #loop through each column in rating table and find associated hydraulic values
  col.indices.rating <- 2:length(names(rating.data))
  
  #create empty output matrix, first column is row, rest are same as rating data
  output.data <- data.frame(matrix(NA, nrow=length(q.data$date), ncol=length(names(rating.data))+1))
  names(output.data) <- c("date", names(rating.data))
  
  #save date and q.cms based on discharge timeseries
  output.data$date <- q.data$date
  output.data$q.cms <- q.data$q.cms
  #max(output.data$q.cms)
  
  for(column in col.indices.rating){
    #subset to first column q.cms and cols
    rating.sub <- rating.data[,c(1, column)]
    #save orig column name
    rating.name <- names(rating.sub)[2]
    #rename col to generic name
    names(rating.sub)[2] <- "variable"
    #remove potential -inf values and na
    rating.sub$variable[is.infinite(rating.sub$variable)] <- NA
    #remove NAs
    rating.sub <- na.omit(rating.sub)
    
    #Use approxfun to linear interpolate the hyd variables based on Q, NA values are outside of rating range
    rating.fun <- approxfun(rating.sub$q.cms, rating.sub$variable, rule=1:1)
    variable.pred <-rating.fun(output.data$q.cms)
    #max(variable.pred, na.rm = TRUE)
    
    #NA values are when discharge is greater than rating table discharge, replace NA with max variable that can be predicted
    if(length(which(is.na(variable.pred))) > 0 ){
      max.var.rating <- max(rating.sub$variable)
      variable.pred[which(is.na(variable.pred))] <- max.var.rating
    }
    
    #write this into new spreadsheet
    #new output column is column + 1 (since we added date into output)
    output.data[,column+1] <- variable.pred
  }
  
  #write output csv into output directory
  file.name.output <- paste0(output.dir,Reach.ID, "_hydraulic_outputs.csv")
  write.csv(output.data, row.names=FALSE, file=file.name.output)
}
