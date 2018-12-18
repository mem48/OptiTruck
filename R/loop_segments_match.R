# Match segments to loops
library(sf)
library(RANN)
library(stringi)
library(tmap)
tmap_mode("view")

loop = readRDS("data/incidents/M18/webtris/sites.Rds")
loop_data = readRDS("data/incidents/M18/webtris/traffic_counts.Rds")
loop_data = loop_data[,c("Site Name","DateTime_Ending","Time Interval","Avg mph","Total Volume")]
loop = loop[loop$Description %in% unique(loop_data$`Site Name`),]
dirs = c("Eastbound","Westbound","Northbound","Southbound","Clockwise","Anti-clockwise","Carriageway Connector")
loop$direction = unlist(lapply(loop$Name, function(x){if(is.na(x)){NA}else{dirs[stri_detect_fixed(x,dirs)]}}))
loop = loop[!is.na(loop$direction),]
loop = loop[loop$direction == "Northbound",]

segments = st_read("data/incidents/M18/clean/segments.geojson")
segments = st_transform(segments, 27700)

nn = nn2(data = st_coordinates(loop) , query = st_coordinates(segments), k = 1, searchtype = "radius", radius = 50)
nn_dist = nn$nn.dists
nn_idx = nn$nn.idx
nn_dist[nn_dist == max(nn_dist)] <- NA
nn_idx[nn_idx == 0] <- NA
hist(nn_dist)
summary(nn_dist)

segments$loop_id = loop$Description[nn_idx]

loop_data_sub = loop_data[loop_data$`Site Name` %in% unique(segments$loop_id),]


joins = segments[,c("segment_id","loop_id")]
st_geometry(joins) = NULL
joins = joins[!duplicated(joins$loop_id),]
joins = joins[!is.na(joins$loop_id),]
loop_data_sub = dplyr::left_join(loop_data_sub,joins, by = c("Site Name" = "loop_id"))

write.csv(loop_data_sub,"data/incidents/M18/webtris/traffic_counts_matched.csv")
saveRDS(loop_data_sub,"data/incidents/M18/webtris/traffic_counts_matched.Rds")
write_sf(segments,"data/incidents/M18/clean/segments_matched.geojson", delete_dsn = T)






qtm(segments) + qtm(loop, dots.col = "red")
