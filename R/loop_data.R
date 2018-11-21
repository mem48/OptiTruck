library(oneminutetrafficdata)
library(sf)
library(tmap)
library(dplyr)
tmap_mode("view")

?DownloadCSV
?RoadData
?DataAggregator

files = list.files("data/webtris/", full.names = T)
res = list()
for(i in 1:length(files)){
  message(paste0("Doing ",i))
  tmp = read.csv(files[i], stringsAsFactors = F)
  tmp = tmp[,c("Site.Name","Report.Date","Time.Period.Ending","Time.Interval","Avg.mph")]
  res[[i]] = tmp
}
res = bind_rows(res)

points = data.frame(id = c("M62/1758A","M62/1727A","M62/1758K","M62/1803A","M62/1808A"), 
                    x = c(399043,396162,399037,403373,403786),
                    y = c(414844,414223,414868,416239,416490))

points = st_as_sf(points, coords = c("x","y"), crs = 27700)
points_buf = st_buffer(points, 100)

segments = st_read("data/incidents/M62/clean/segments.geojson")
segments = st_transform(segments, 27700)

segments_sub = segments[points_buf,]
qtm(segments_sub) +
  qtm(points, dots.col = "red")


ptv = readRDS("data/incidents/M62/clean/incidents.Rds")
ptv_all = readRDS("data/incidents/M62/clean/incidents_all.Rds")
summary(ptv$date_time)


# FOr segment_id 111

webtris = read.csv("data/webtris/DailyStandard_Report_1_3779_01_01_2017_01_01_2018.csv", stringsAsFactors = F)
webtris = webtris[,c("Site.Name","Report.Date","Time.Period.Ending","Time.Interval","Avg.mph")]
webtris$datetime = paste0(substr(webtris$Report.Date,1,10)," ",webtris$Time.Period.Ending)
webtris$datetime = lubridate::dmy_hms(webtris$datetime)
webtris = webtris[order(webtris$datetime),]

sub = ptv_all[ptv_all$segment_id == 111,]
sub = sub[,c("date_time","incident_absolute_speed")]
sub$incident_absolute_speed[is.na(sub$incident_absolute_speed)] = 80
sub = sub[order(sub$date_time),]
#sub = sub[sub$date_time > lubridate::ymd_hms("2017-07-05 12:00:00"),]
#sub = sub[sub$date_time < lubridate::ymd_hms("2017-07-05 14:00:00"),]
plot(sub$date_time, sub$incident_absolute_speed, type = "l", 
     ylim = c(0,85), ylab = "Speed", xlab = "Date", main = "Segment 111",
     sub = paste0("Mean = ",round(mean(sub$incident_absolute_speed),1), ",  Q1 = ",quantile(sub$incident_absolute_speed,0.25)
                  , ",  Q2 = ",quantile(sub$incident_absolute_speed,0.75), ",  Q3 = ",quantile(sub$incident_absolute_speed,0.75)))
lines(webtris$datetime ,webtris$Avg.mph, col = "red")


sub = ptv_all[ptv_all$segment_id == 322,]
sub = sub[,c("date_time","incident_absolute_speed")]

#sub$incident_absolute_speed[is.na(sub$incident_absolute_speed)] = 80
sub = sub[order(sub$date_time),]
plot(sub$date_time, sub$incident_absolute_speed, type = "l", ylim = c(0,85), ylab = "Speed", xlab = "Date")
