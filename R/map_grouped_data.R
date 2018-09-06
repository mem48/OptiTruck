library(sf)
library(tmap)
tmap_mode("view")

incidents_grouped = read.csv("data/incidents/M62/clean/incidents_grouped.csv", stringsAsFactors = F)
segments = st_read("data/incidents/M62/clean/segments.geojson")

mp = list()
for(i in 1:nrow(incidents_grouped)){
  ids = incidents_grouped$segment_id[i]
  ids = strsplit(ids," ")
  ids = unlist(ids)
  
  segs = segments[segments$segment_id %in% ids,]
  segs = st_union(segs)
  segs = segs[[1]]
  mp[[i]] = segs
}

mp = st_as_sfc(mp)

incidents_grouped$geometry = mp
incidents_grouped = st_sf(incidents_grouped)
st_crs(incidents_grouped) = st_crs(segments)
incidents_grouped$id = 1:nrow(incidents_grouped)

qtm(incidents_grouped[,], dots.col = "delay_max")

st_write(incidents_grouped,"data/incidents/M62/clean/incidents_grouped.geojson")
