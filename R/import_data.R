# Import data

library(dplyr)
library(lubridate)
library(sf)
library(tmap)
library(stringr)
library(RANN)
library(igraph)
tmap_mode("view")

folder_in = "data/incidents/Italy/raw/"
folder_out = "data/incidents/Italy/clean/"
files = list.files(folder_in, full.names = TRUE, pattern = "csv")
files = files[grepl("uk",files)]

raw = list()
for(i in seq(1,length(files))){
  message(paste0(Sys.time()," Doing File ",i))
  tmp = read.csv(files[i], header = T, stringsAsFactors = F)
  tmp = tmp[tmp$x != "x",]
  tmp$x = as.numeric(tmp$x)
  tmp$y = as.numeric(tmp$y)
  tmp$z = as.numeric(tmp$z)
  tmp$segment = as.integer(tmp$segment)
  tmp$length_m = as.integer(tmp$length_m)
  tmp$length_s = as.integer(tmp$length_s)
  tmp$calculate_speed_kmh = as.integer(tmp$calculate_speed_kmh)
  tmp$date_time = ymd_hms(gsub("T"," ",tmp$date_time))
  tmp$incident_start_x = as.integer(tmp$incident_start_x)
  tmp$incident_start_y = as.integer(tmp$incident_start_y)
  tmp$incident_length = as.integer(tmp$incident_length)
  tmp$incident_delay = as.integer(tmp$incident_delay)
  tmp$incident_absolute_speed = as.integer(tmp$incident_absolute_speed)
  raw[[i]] = tmp
}
rm(tmp)
raw = bind_rows(raw)

# Simplify Data
segments = raw[,c("segment","x","y","z","length_m","length_s","street_category","calculate_speed_kmh")]
segments = unique(segments)
segments$segment_id = 1:nrow(segments)
segments$duplicated = duplicated(segments$y) & duplicated(segments$x)

segments.sf = st_as_sf(segments, coords = c("x","y"))
st_crs(segments.sf) = 4326


#qtm(segments.sf)
st_write(segments.sf,paste0(folder_out,"segments.geojson"), delete_dsn = T)

incidents = raw[,c("date_time","x","y","segment","incident_start_x","incident_start_y","incident_length","incident_delay","incident_absolute_speed","incident_message")]
segments.join = segments[,c("segment","x","y","segment_id")]

incidents = left_join(incidents,segments.join, by = c("segment" = "segment","x" = "x","y" = "y"))
incidents = incidents[,c("date_time","segment_id","incident_start_x","incident_start_y","incident_length","incident_delay","incident_absolute_speed","incident_message")]

saveRDS(incidents,paste0(folder_out,"incidents_all.Rds"))

incidents.active = incidents[!is.na(incidents$incident_start_x) |
                       !is.na(incidents$incident_start_y) |
                       !is.na(incidents$incident_length) |
                       !is.na(incidents$incident_delay) |
                       !is.na(incidents$incident_absolute_speed) |
                       (!is.na(incidents$incident_message) & incidents$incident_message != "" ), ]

incidents.active = incidents.active[order(incidents.active$date_time),]

# simplify the message
message_cats = c("blocked","slow traffic","queuing traffic","roadworks","stationary traffic",
                 "shed load","broken down vehicle","road surface in poor condition","strong winds",
                 "accident","vehicle fire","traffic problem","people on roadway","vehicle on wrong carriageway",
                 "animals on roadway","right lane closed","smoke hazard","slow vehicle","heavy traffic, traffic building up")
incidents.active$incident_type = str_extract(incidents.active$incident_message, paste(message_cats, collapse="|"))
#unique(incidents.active$incident_message[is.na(incidents.active$incident_type)])
summary(as.factor(incidents.active$incident_type))

write.csv(incidents.active,paste0(folder_out,"incidents.csv"), row.names = FALSE)
saveRDS(incidents.active,paste0(folder_out,"incidents.Rds"))

incidents.bysegment = lapply(unique(incidents.active$segment_id),function(x){incidents.active[incidents.active$segment_id == x,]})

source("R/cluster_times.R")

for(i in 1:length(incidents.bysegment)){
  tmp = incidents.bysegment[[i]]
  tmp$time_cluster = cluster_times(tmp$date_time, tolerance = 30, units = "mins")
  incidents.bysegment[[i]] = tmp 
}

incidents.bysegment = bind_rows(incidents.bysegment)

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
                                #type = paste(unique(incident_type), collapse = " "),
                                message = paste(unique(incident_message), collapse = " "))


incidents.unique$duration_hours = round(difftime(incidents.unique$end, incidents.unique$start, units = "hours"),1)

# write.csv(incidents.unique,paste0(folder_out,"incidents_ungrouped.csv"), row.names = FALSE)
# saveRDS(incidents.unique,paste0(folder_out,"incidents_ungrouped.Rds"))



incidents.unique.grp = incidents.unique %>%
                        group_by(start, end) %>%
                        summarise(duration_hours = min(duration_hours),
                                  n_records = sum(n_records),
                                  segment_id = paste(unique(segment_id), collapse = " "),
                                  start_x = paste(unique(start_x), collapse = " "),
                                  start_y = paste(unique(start_y), collapse = " "),
                                  length_max = max(length_max),
                                  length_min = min(length_min),
                                  delay_max = max(delay_max),
                                  delay_min = min(delay_min),
                                  absolute_speed_max = max(absolute_speed_max),
                                  absolute_speed_min = min(absolute_speed_min),
                                  #type = paste(unique(type), collapse = " "),
                                  message = paste(unique(message), collapse = " "))

incidents.unique.grp = incidents.unique.grp[order(incidents.unique.grp$start),]
write.csv(incidents.unique.grp,paste0(folder_out,"incidents_grouped.csv"), row.names = FALSE)
saveRDS(incidents.unique.grp,paste0(folder_out,"incidents_grouped.Rds"))
