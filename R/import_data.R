# Import data

library(dplyr)
library(lubridate)
library(sf)
library(tmap)
tmap_mode("view")

folder = "data/incidents/"
files = list.files(folder, full.names = TRUE, pattern = "csv")

incidents = list()
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
  incidents[[i]] = tmp
}
rm(tmp)
incidents = bind_rows(incidents)

# Simplify Data
segments = incidents[,c("segment","x","y","z","length_m","length_s","street_category","calculate_speed_kmh")]
segments = unique(segments)
segments$segment_id = 1:nrow(segments)
segments$duplicated = duplicated(segments$y) & duplicated(segments$x)

segments.sf = st_as_sf(segments, coords = c("x","y"))
st_crs(segments.sf) = 4326
#qtm(segments.sf)
st_write(segments.sf,"data/incidents/segments.geojson")

incidents = incidents[,c("date_time","x","y","segment","incident_start_x","incident_start_y","incident_length","incident_delay","incident_absolute_speed","incident_message")]
segments.join = segments[,c("segment","x","y","segment_id")]

incidents = left_join(incidents,segments.join, by = c("segment" = "segment","x" = "x","y" = "y"))
incidents = incidents[,c("date_time","segment_id","incident_start_x","incident_start_y","incident_length","incident_delay","incident_absolute_speed","incident_message")]

incidents.active = incidents[!is.na(incidents$incident_start_x) |
                       !is.na(incidents$incident_start_y) |
                       !is.na(incidents$incident_length) |
                       !is.na(incidents$incident_delay) |
                       !is.na(incidents$incident_absolute_speed) |
                       (!is.na(incidents$incident_message) & incidents$incident_message != "" ), ]

incidents.active = incidents.active[order(incidents.active$date_time),]
write.csv(incidents.active,"data/incidents/incidents.csv", row.names = FALSE)

incidents.unique = incidents.active %>%
                    group_by(incident_start_x, incident_start_y) %>%
                      summarise(start = min(date_time),
                                end = max(date_time),
                                segment_id = paste(unique(segment_id), collapse = " "),
                                incident_length = paste(unique(incident_length), collapse = " "),
                                incident_delay = paste(unique(incident_delay), collapse = " "),
                                incident_absolute_speed = paste(unique(incident_absolute_speed), collapse = " "),
                                incident_message = paste(unique(incident_message), collapse = " "))


incidents.unique$duration = round(difftime(incidents.unique$end, incidents.unique$start, units = "days"),1)
