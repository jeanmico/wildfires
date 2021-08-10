# remove purpleair sensors more than 4 standard deviations away from EPA
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

processed_pa_all = read.csv('~/wildfires/all_purpleair.csv')

#processed_pa <- processed_pa_all %>% filter(ymd(day_date) < ymd('2018-12-01') & ymd(day_date) > ymd('2018-10-31'))

processed_pm25 <- processed_pa_all %>% dplyr::select(id, lat, lon, day_date, mean_pm25_1, pm25_1_corr) %>%
  pivot_longer(cols = c('mean_pm25_1', 'pm25_1_corr'), names_to = 'measure_type', values_to = 'pm25')

epa17 = read.csv('~/wildfires/2017_CA_PM25_EPA.csv')
epa18 = read.csv('~/wildfires/2018_CA_PM25_EPA.csv')
epa19 = read.csv('~/wildfires/2019_CA_PM25_EPA.csv')

epa <- rbind(epa17, epa18)
epa <- rbind(epa, epa19)

epa <- epa %>% mutate(day_date = mdy(Date)) %>% filter(CBSA_CODE == 41860)

pa_corr <- processed_pm25 %>% filter(measure_type == 'pm25_1_corr')

epa_mean <- epa %>% group_by(day_date) %>%
  summarise(daymean = mean(Daily.Mean.PM2.5.Concentration),
            daysd = sd(Daily.Mean.PM2.5.Concentration),
            daymin = min(Daily.Mean.PM2.5.Concentration))

epa_mean <- epa_mean %>% mutate(dayminus4sd = daymean -4*daysd) %>% mutate(dayminus4sd = case_when(dayminus4sd < 0 ~ 0, TRUE ~ dayminus4sd))



epa_pacorr <- ggplot(data = epa, aes(x= ymd(day_date), y=Daily.Mean.PM2.5.Concentration, alpha = .5)) + 
  geom_point() + 
  scale_x_date() +
  #theme_minimal() +
  geom_point(data = pa_corr, aes(x=ymd(day_date), y=pm25, color = measure_type, alpha=.2)) + 
  geom_line(data= epa_mean, aes(x = day_date, y = daymean)) +
  geom_line(data = epa_mean, aes(x = day_date, y = dayminus4sd)) +
  geom_line(data = epa_mean, aes(x = day_date, y = daymin)) +
  theme_minimal()
epa_pacorr

ggsave(epa_pacorr, file = '~/wildfires/sdplot.png')

pa_corr <- pa_corr %>% mutate(day_date = ymd(day_date))
mean_pa <- merge(pa_corr, epa_mean, by = 'day_date')

# between Nov 8 and 21...which PA sensors are more than x standard deviations away from norm
pa_remove <- mean_pa %>% filter(pm25 < dayminus4sd)
write.csv(pa_remove, '/Volumes/Padlock/wildfires/pa_remove.csv', row.names = FALSE)

pa_remove15 <- pa_remove %>% filter(day_date == ymd('2018-11-15'))

lowpa <- pa_remove %>% filter(pm25 <20)
View(lowpa)
pa4788 <- pa_corr %>% filter(id == 4788)
low_sensor <- ggplot(pa4788, aes(x = day_date, y = pm25)) + geom_point() + scale_x_date()
low_sensor
