
# Add lengths to segments
library(sf)


folder = "data/incidents/M18/clean/"
segments = st_read(paste0(folder,"segments.geojson"))



# Assume that they are end points
segments = st_transform(segments, 27700)
dists = st_distance(segments[1:(nrow(segments)-1),],segments[2:(nrow(segments)),], by_element = T )
dists = as.numeric(dists)
dists = c(dists,NA)

segments$length_measured = dists


segments$length_m_diff = segments$length_m - c(0,segments$length_m[1:(nrow(segments)-1)])
# fill in gaps
for(i in 1:nrow(segments)){
  if(is.na(segments$length_m_diff[i])){
    if(!is.na(segments$length_m[i])){
      segments$length_m_diff[i] = segments$length_m[i] - segments$length_m[i-2]
    }
  }
}

plot(segments$length_m_diff,segments$length_measured)

segments = st_transform(segments, 4326)
st_write(segments,paste0(folder,"segments.geojson"), delete_dsn = T)
