# Make weekly profiles for each segment.
library(lubridate)
library(dplyr)
library(Hmisc)

# read in data
where = "M62"

incidents = readRDS(paste0("data/incidents/",where,"/clean/incidents_all.Rds"))
segment_ids = unique(incidents$segment_id)
segment_ids = segment_ids[order(segment_ids)]

# Remove NAs
incidents$incident_delay[is.na(incidents$incident_delay)] = 0
incidents$incident_absolute_speed[is.na(incidents$incident_absolute_speed)] = 80

# Rebase dates to a single week.
week2date = function(week){
  if(week == "Monday"){
    return("2018/08/20")
  }else if(week == "Tuesday"){
    return("2018/08/21")
  }else if(week == "Wednesday"){
    return("2018/08/22")
  }else if(week == "Thursday"){
    return("2018/08/23")
  }else if(week == "Friday"){
    return("2018/08/24")
  }else if(week == "Saturday"){
    return("2018/08/25")
  }else if(week == "Sunday"){
    return("2018/08/26")
  }else{
    message("Oh noes!")
    stop()
  }
}

week2date_alt = function(week){
  if(week == "Monday"){
    return("2018/08/20")
  }else if(week == "Tuesday"){
    return("2018/08/20")
  }else if(week == "Wednesday"){
    return("2018/08/20")
  }else if(week == "Thursday"){
    return("2018/08/20")
  }else if(week == "Friday"){
    return("2018/08/20")
  }else if(week == "Saturday"){
    return("2018/08/25")
  }else if(week == "Sunday"){
    return("2018/08/25")
  }else{
    message("Oh noes!")
    stop()
  }
}


incidents$weekday = weekdays(incidents$date_time)
incidents$weekday = sapply(incidents$weekday, week2date_alt)
incidents$time = strftime(incidents$date_time, format="%H:%M")
incidents$date_time2 = ymd_hm(paste0(incidents$weekday," ",incidents$time))
incidents$date_time2 = floor_date(incidents$date_time2, unit="1 hour")

counts_max = round(as.numeric(difftime(max(incidents$date_time),min(incidents$date_time), units="mins")),0) / 5


incidents_grouped = incidents %>%
  group_by(segment_id, date_time2) %>%
  summarise(speed_mean = mean(incident_absolute_speed, na.rm = T),
            speed_min = min(incident_absolute_speed, na.rm = T),
            speed_max = max(incident_absolute_speed, na.rm = T),
            speed_q1 = quantile(incident_absolute_speed, probs = 0.25, na.rm = T),
            speed_q2 = quantile(incident_absolute_speed, probs = 0.5 , na.rm = T),
            speed_q3 = quantile(incident_absolute_speed, probs = 0.75, na.rm = T),
          
            
            speed_q1_wtd = wtd.quantile(c(incident_absolute_speed,80), weights = c(rep(1,n()),( counts_max - n() )), probs = 0.25, na.rm = T),
            speed_q2_wtd = wtd.quantile(c(incident_absolute_speed,80), weights = c(rep(1,n()),( counts_max - n() )), probs = 0.5 , na.rm = T),
            speed_q3_wtd = wtd.quantile(c(incident_absolute_speed,80), weights = c(rep(1,n()),( counts_max - n() )), probs = 0.75, na.rm = T),
            
            n_records = n())

incidents_grouped = incidents_grouped[order(incidents_grouped$segment_id, incidents_grouped$date_time2),]


foo = incidents %>%
  group_by(segment_id) %>%
  summarise(count = n(),
            distrupt = length(incident_absolute_speed[incident_absolute_speed < 80]))

foo2 = incidents[incidents$segment_id == 295,]
