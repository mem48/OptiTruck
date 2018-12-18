# Loop Data Download

library(webTRISr)
library(sf)
library(dplyr)

sites = webtris_sites()
sites = st_as_sf(sites, coords = c("Longitude","Latitude"), crs = 4326)
sites = st_transform(sites, 27700)
segments = st_read("data/incidents/M18/clean/segments.geojson")
segments = st_transform(segments, 27700)
segments_buf = st_buffer(segments, 1000)
segments_buf = st_union(segments_buf)
sites_sub = sites[segments_buf,]
plot(sites_sub)

saveRDS(sites_sub,"data/incidents/M18/webtris/sites.Rds")

source("../WebTRIS/R/reports.R")

reports_list = list()
for(i in 1:nrow(sites_sub)){
  message(paste0("Downloading site ",sites_sub$Id[i]," number ",i," of ", nrow(sites_sub)))
  report = WebTRIS_reports(sites_sub$Id[i], period = "daily",
                           start_date = as.Date("2017-01-01"),
                           end_date = as.Date("2017-12-31"))
  reports_list[[i]] = report

  rm(report)
}
reports_list = reports_list[!is.na(reports_list)]
reports_list = bind_rows(reports_list)

saveRDS(reports_list,"data/incidents/M18/webtris/traffic_counts.Rds")

foo = reports_list[reports_list$`Site Name` == reports_list$`Site Name`[1],]
plot(foo$DateTime_Ending, foo$`Avg mph`, type = "l")

# DFT package does not work
# reports_alt = webtris_report(sites_sub$Id[1:100], report_type = "daily",
#                               start_date = as.Date("2017-01-01"),
#                               end_date = as.Date("2017-12-31"))
