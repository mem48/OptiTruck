library(ggplot2)
library(dplyr)

# compare PTV and loop data
loop_data = readRDS("data/incidents/M18/webtris/traffic_counts_matched.Rds")
ptv_data =  readRDS("data/incidents/M18/clean/incidents.Rds")
ptv_incidents = readRDS("data/incidents/M18/clean/incidents_grouped.Rds")


matching_segments = unique(loop_data$segment_id)
matching_segments = matching_segments[order(matching_segments)]

#subset to those we can match on
ptv_incidents$segment_id = lapply(strsplit(ptv_incidents$segment_id," "), function(x){as.numeric(x)})
ptv_incidents = ptv_incidents[sapply(ptv_incidents$segment_id, function(x){any(x %in% matching_segments)}),]

ptv_data = ptv_data[ptv_data$segment_id %in% matching_segments,]

for(i in 1:nrow(ptv_incidents)){
  message(paste0("Doing ",i))
  segs = ptv_incidents$segment_id[[i]]
  segs = segs[segs %in% matching_segments]
  
  istart = ptv_incidents$start[i]
  iend = ptv_incidents$end[i]
  
  loop_sub = loop_data[loop_data$segment_id %in% segs,]
  ptv_sub = ptv_data[ptv_data$segment_id %in% segs,]
  
  loop_sub = loop_sub[,c("segment_id","DateTime_Ending","Avg mph")]
  ptv_sub = ptv_sub[,c("segment_id","date_time","incident_absolute_speed")]
  
  loop_sub$type = paste0(loop_sub$segment_id," - loop")
  ptv_sub$type = paste0(ptv_sub$segment_id," - ptv")
  
  loop_sub = loop_sub[loop_sub$DateTime_Ending >= istart, ]
  loop_sub = loop_sub[loop_sub$DateTime_Ending <= iend, ]
  
  ptv_sub = ptv_sub[ptv_sub$date_time >= istart,]
  ptv_sub = ptv_sub[ptv_sub$date_time <= iend,]
  
  names(loop_sub) = c("segment_id","DateTime","speed","type")
  names(ptv_sub) = c("segment_id","DateTime","speed","type")
  
  sub_all = rbind(loop_sub,ptv_sub )
  ggplot(sub_all) +
    geom_line(aes(x = DateTime, y = speed, color = type)) +
    ylim(c(0,90)) +
    ggtitle(paste0("Incident ",i," between ",istart," and ",iend)) +
    ggsave(paste0("plots/loop_vs_ptv/M18/incident_",i,".png"))
  
  
}

# 
# ptv_data_grouped = ptv_data %>%
#   group_by(segment_id,incident_type) %>%
#   
# 
# 
# 
# 
# grepl(ptv_data$incident_message[1],types)
# 
# unique(ptv_data$incident_message)

