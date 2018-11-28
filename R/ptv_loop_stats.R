# Group incidents for profile of PTV vs loop data
library(dplyr)
library(ggplot2)

incidents = readRDS("data/incidents/M62/clean/incidents.Rds")
loop_data = readRDS("data/incidents/M62/webtris/traffic_counts_matched.Rds")
incidents = incidents[incidents$segment_id %in% unique(loop_data$segment_id),]

incidents$group_id = group_indices(incidents, segment_id, incident_type)
incidents_group = lapply(unique(incidents$group_id),function(x){incidents[incidents$group_id == x,]})

source("R/cluster_times.R")

for(i in 1:length(incidents_group)){
  tmp = incidents_group[[i]]
  tmp$time_cluster = cluster_times(tmp$date_time, tolerance = 30, units = "mins")
  incidents_group[[i]] = tmp 
}

incidents_group = bind_rows(incidents_group)

incidents_group$group_final = group_indices(incidents_group, group_id, time_cluster)

incidents_group = incidents_group[,c("date_time","segment_id","incident_delay","incident_absolute_speed", "incident_type","group_final")]

incidents_group_summary = incidents_group %>%
  group_by(group_final) %>%
  summarise(start = min(date_time),
            end = max(date_time),
            segment_id = unique(segment_id),
            incident_type = unique(incident_type),
            n_records = n(),
            speed_mean_PTV = max(incident_absolute_speed, na.rm = T),
            speed_sd_PTV = sd(incident_absolute_speed, na.rm = T)
            )

loop_summary = list()

for(i in 1:nrow(incidents_group_summary)){
  message(paste0("Doing ",i))
  seg_id = incidents_group_summary$segment_id[i]
  istart = incidents_group_summary$start[i]
  iend = incidents_group_summary$end[i]
  tmp = loop_data[loop_data$segment_id == seg_id,]
  tmp = tmp[tmp$DateTime_Ending <= iend,]
  tmp = tmp[tmp$DateTime_Ending >= istart,]
  res = data.frame(id = i,
                   speed_mean_loop = mean(tmp$`Avg mph`, na.rm = T),
                   speed_sd_loop = sd(tmp$`Avg mph`, na.rm = T),
                   nvalues = length(tmp$`Avg mph`) )
  loop_summary[[i]] = res
}

loop_summary = bind_rows(loop_summary)

incidents_group_summary = left_join(incidents_group_summary, loop_summary, by = c("group_final" = "id"))


# Produce Summary Plots
for(j in unique(incidents_group_summary$incident_type)){
  tmp = incidents_group_summary[incidents_group_summary$incident_type == j,]
  #tmp2 = data.frame(Source = c(rep("PTV",nrow(tmp)),rep("Loop",nrow(tmp))), Data = c(tmp$speed_mean_PTV, tmp$speed_mean_loop))
  ggplot(tmp) +
     geom_histogram(aes(x = speed_mean_PTV,  fill = "blue"), binwidth = 5, alpha = 0.5) +
     geom_histogram(aes(x = speed_mean_loop, fill = "red" ), binwidth = 5, alpha = 0.5) +
     labs(fill = "Source") +
     scale_fill_discrete(labels = c("PTV", "Loop")) +
     xlab("Speed in mph") +
     ylab("Frequency") +
     ggtitle(paste0("Mean traffic speed during '",j,"'")) +
     ggsave(paste0("plots/loop_vs_ptv/",j,".png"))
    #+
    # geom_histogram(aes(x=Data, fill=Source))

}



# for(j in unique(incidents_group_summary$incident_type)){
#   tmp = incidents_group_summary[incidents_group_summary$incident_type == j,]
#   tmp2 = data.frame(Source = c(rep("PTV",nrow(tmp)),rep("Loop",nrow(tmp))), Data = c(tmp$speed_mean_PTV, tmp$speed_mean_loop))
#   ggplot(tmp2) +
#     geom_histogram(aes(x=Data, fill=Source), binwidth = 5, alpha = 0.5) +
#     xlab("Speed in mph") +
#     ylab("Frequency") +
#     ggtitle(paste0("Mean traffic speed during '",j,"'")) +
#     ggsave(paste0("plots/loop_vs_ptv/",j,".png"))
#   #+
#   # geom_histogram(aes(x=Data, fill=Source))
#   
# }
