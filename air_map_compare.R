pa = read.csv('/Volumes/Padlock/wildfires/figure_data/nov16.csv', header = FALSE)
head(pa)
colnames(pa) = c("sensor_id", "day_date", "mean_pm25_atm", "mean_pm25_1", "mean_temp", "mean_humid", "pm25_atm_corr", "pm25_1_corr"  )

pa <- pa %>% mutate(sensor_id = str_split(sensor_id, '\\.', simplify = TRUE)[,1])

pdict <- read.csv('~/GitHub/wildfires/purpleair_sensors.csv')
sensors_purple <- merge(pa, pdict, by.x = 'sensor_id', by.y = 'id', all.y = FALSE)
sensors_purple <- sensors_purple %>% dplyr::select(sensor_id, day_date, mean_pm25_1, pm25_1_corr, lat, lon)
colnames(sensors_purple) = c('sensor_id', 'day_date', 'daily_mean_pm25', 'corrected', 'latitude', 'longitude')


coordinates(sensors_purple) <- ~longitude+latitude
crs(sensors_purple) <- CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

sensors_purple <- subset(sensors_purple, !is.na(daily_mean_pm25))
sensors_full <- sensors_purple
kv <<- as.formula(daily_mean_pm25 ~ latitude + longitude)
tmp_rast = kriging_full('2018-11-16', sensors_full, ca_county_map)

