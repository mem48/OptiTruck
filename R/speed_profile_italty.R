#Speed Profiles Italty
library(dplyr)
library(ggplot2)
incident = readRDS("data/incidents/Italy/clean/incidents.Rds")

for(i in unique(incident$incident_type)){
  tmp = incident[incident$incident_type == i,]
  ggplot(tmp) +
    geom_histogram(aes(x = incident_absolute_speed ), binwidth = 5, alpha = 0.5, fill = "red") +
    xlim(c(0,70)) +
    xlab("Speed in kph") +
    ylab("Frequency") +
    ggtitle(paste0("Traffic speed during '",i,"'"))# +
    ggsave(paste0("plots/loop_vs_ptv/Italy/",i,".png"))
}


tmp = incident[incident$segment_id == 526,]
ggplot(tmp) +
  geom_point(aes(y = incident_absolute_speed, x = date_time ))# +
  # xlim(c(0,70)) +
  # xlab("Speed in kph") +
  # ylab("Frequency") +
  # ggtitle(paste0("Traffic speed during '",i,"'"))# +


summary.stats = incident %>%
  group_by(incident_type) %>%
  summarise( speed_mean = mean(incident_absolute_speed, na.rm = T),
             speed_sd = sd(incident_absolute_speed, na.rm = T),
             speed_min = min(incident_absolute_speed, na.rm = T),
             speed_max = max(incident_absolute_speed, na.rm = T),
             delay_mean = mean(incident_delay, na.rm = T),
             delay_sd = sd(incident_delay, na.rm = T),
             delay_min = min(incident_delay, na.rm = T),
             delay_max = max(incident_delay, na.rm = T)
             )


write.csv(summary.stats,"data/incidents/Italy/clean/stats.csv")
