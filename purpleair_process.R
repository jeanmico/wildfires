# this script is to read + process purpleair data
# data is housed on external drive due to size
library(dplyr)
fpath = '/Volumes/Padlock/purpleair'
fnames <- list.files(fpath, full.names = TRUE)
for(i in 1:length(fnames)){

  print(fnames[i])
  
  sensid = strsplit(basename(fnames[i]), '\\.')[[1]][1]
  outname = paste('/Volumes/Padlock/purpleair_2018/', as.character(sensid), '.csv', sep = '')
  if (!file.exists(outname)) {
  
    df <- read.csv(fnames[i])
    colnames(df) = c("entryid", "created_at", 'pm1_atm', 'pm25_atm', 'pm10_atm', 'uptime_min', 'ADC', 'temperature', 'humidity', 'pm25_1')

    df <- df %>% mutate(sensor_id = sensid, day_date = as.Date(created_at, format="%Y-%m-%d"))
    
    df <- df %>% group_by(day_date) %>%
      summarise(mean_pm25_atm = mean(pm25_atm),
                mean_temp = mean(temperature),
                mean_humid = mean(humidity),
                pm25_purple_corrected = .39*mean_pm25_atm + .0024*mean_temp -.050*mean_humid + 5.19)
    
    # get Oct 1 through dec 31 2018
    nov18df <- df %>% filter(day_date <= as.Date('2018-12-31') & day_date >= as.Date('2018-10-01'))
    
    write.csv(nov18df, file = outname, row.names = FALSE)
  }
}


sensor_loc = read.csv('~/GitHub/wildfires/bay_area_sensors.csv')

fpath = '/Volumes/Padlock/purpleair_2018'
fnames <- list.files(fpath, full.names = TRUE)
daydf = data.frame(matrix(ncol=5, nrow=0))
colnames(daydf) = c("day_date", "mean_pm25_atm", "mean_temp", "mean_humid", "id")
myids = vector('list', 2998)
for (i in 1:length(fnames)) {
  tmp = read.csv(fnames[i])
  tmp_id = strsplit(basename(fnames[i]), '\\.')[[1]][1]
  tmp <- tmp %>% mutate(id = tmp_id)
  myids[[i]] <- tmp_id
  daydf <- rbind(daydf, tmp)
}

# filter to one day
oneday = daydf %>% filter(as.Date(day_date) == as.Date('2018-11-15'))

day = merge(sensor_loc, oneday, by = 'id') 

write.csv(day, file='~/wildfires/2018_bayarea_pm25_purple.csv', row.names = FALSE)
