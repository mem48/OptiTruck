# New grouping and stats
library(dplyr)

# Import data
folder = "data/incidents/M62/clean/"
incidents.active = readRDS(paste0(folder,"incidents.Rds"))
segments = st_read(paste0(folder,"segments.geojson"))


incidents.bysegment = lapply(unique(incidents.active$segment_id),function(x){incidents.active[incidents.active$segment_id == x,]})

source("R/cluster_times.R")

for(i in 1:length(incidents.bysegment)){
  tmp = incidents.bysegment[[i]]
  tmp$time_cluster = cluster_times(tmp$date_time, tolerance = 30, units = "mins")
  incidents.bysegment[[i]] = tmp 
}
rm(tmp, incidents.active)

incidents.bysegment = bind_rows(incidents.bysegment)

segments_lengths = as.data.frame(segments[,c("segment_id","length_measured")])
segments_lengths$geometry = NULL

incidents.unique = incidents.bysegment %>%
  group_by(segment_id, time_cluster) %>%
  summarise(start = min(date_time),
            end = max(date_time),
            n_records = n(),
            start_x = paste(unique(incident_start_x), collapse = " "),
            start_y = paste(unique(incident_start_y), collapse = " "),
            length_max = max(incident_length, na.rm = T),
            length_min = min(incident_length, na.rm = T),
            delay_max = ifelse(all(is.na(incident_delay)),0,max(incident_delay, na.rm = T)),
            delay_min = ifelse(all(is.na(incident_delay)),0,min(incident_delay, na.rm = T)),
            absolute_speed_max = max(incident_absolute_speed, na.rm = T),
            absolute_speed_min = min(incident_absolute_speed, na.rm = T),
            absolute_speed_Q25 = quantile(incident_absolute_speed, probs = 0.25, na.rm = T),
            absolute_speed_Q50 = quantile(incident_absolute_speed, probs = 0.50, na.rm = T),
            absolute_speed_Q75 = quantile(incident_absolute_speed, probs = 0.75, na.rm = T),
            #type = paste(unique(incident_type), collapse = " "),
            message = paste(unique(incident_message), collapse = " "))


incidents.unique$duration_hours = round(difftime(incidents.unique$end, incidents.unique$start, units = "hours"),1)
incidents.unique = left_join(incidents.unique,segments_lengths, by = "segment_id")

incidents.unique = incidents.unique[order(incidents.unique$start),]


write.csv(incidents.unique,paste0(folder,"incidents_stats.csv"), row.names = FALSE)
saveRDS(incidents.unique,paste0(folder,"incidents_stats.Rds"))

