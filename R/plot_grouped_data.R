# Produces plots for each grouped event
library(sf)
library(lubridate)
library(ggplot2)


incidents_grouped = read.csv("data/incidents/Italy/clean/incidents_grouped.csv", stringsAsFactors = F)
segments = st_read("data/incidents/Italy/clean/segments.geojson")
incidents = read.csv("data/incidents/Italy/clean/incidents.csv", stringsAsFactors = F)
folder_out = "plots/Italy/"

incidents_grouped$segment_id = str_split(incidents_grouped$segment_id," ")
incidents_grouped$start = ymd_hms(incidents_grouped$start)
incidents_grouped$end = ymd_hms(incidents_grouped$end)
incidents$date_time = ymd_hms(incidents$date_time)


# loop over each incident
for(i in 1:nrow(incidents_grouped)){
  incidents_main = incidents_grouped[i,]
  incidents_tmp = incidents[incidents$segment_id %in% unlist(incidents_main$segment_id),]
  incidents_tmp = incidents_tmp[incidents_tmp$date_time <= incidents_main$end,]
  incidents_tmp = incidents_tmp[incidents_tmp$date_time >= incidents_main$start,]
  incidents_tmp$segment_id = as.character(incidents_tmp$segment_id)
  
  incidents_tmp$incident_delay[is.na(incidents_tmp$incident_delay)] = 0
  #scale = max(incidents_tmp$incident_delay/80)
  
  ggplot(incidents_tmp, aes(x = date_time)) +
    geom_line(aes(y = incident_absolute_speed, colour = "Speed"),size=0.25) + 
    geom_point(aes(y = incident_absolute_speed, colour = "Speed"), size = 0.5) +
    geom_line(aes(y = incident_delay/7, colour = "Delay"),size=0.25) + 
    geom_point(aes(y = incident_delay/7, colour = "Delay"),size = 0.5) +
    facet_wrap(~segment_id) + 
    scale_y_continuous(limits = c(0,80), sec.axis = sec_axis(~.*7, name = "Delay")) +
    scale_colour_manual(values = c("blue", "red")) +
    ylab("Speed in km/h") +
    xlab("Date & Time") +
    labs( title = paste0("Event ",i,": Speed, for each segment"),
          subtitle = paste0("From ",incidents_main$start," to ",incidents_main$end)) +
    ggsave(paste0(folder_out,"Event_",i,"_Speed_Delay.png"))
  
  message(paste0("Done ",i))
  
}

