# this script is to read, correct, and combine purpleair data into one file
# data is housed on external drive due to size
library(dplyr)
fpath = '/Volumes/Padlock/purpleair'

# get the IDs that we want from previous summary process
pa_ids = list.files('/Volumes/Padlock/purpleair_2018')

fnames <- list.files(fpath, full.names = TRUE)

fnames <- fnames[basename(fnames) %in% pa_ids]

renames = c("entryid", "created_at", 'pm1_atm', 'pm25_atm', 'pm10_atm', 'uptime_min', 'ADC', 'temperature', 'humidity', 'pm25_1')
new_cols = c("sensor_id", "day_date", "mean_pm25_atm","mean_pm25_1","mean_temp","mean_humid","pm25_atm_corr","pm25_1_corr")

#sample_df = read_csv('/Volumes/Padlock/purpleair/10092.csv')

# create an empty dataframe to append files to
purpledf = data.frame(matrix(ncol=length(new_cols), nrow=0))
colnames(purpledf) = new_cols
for(i in 1:length(fnames)){
#for(i in 1:1){
  #print(fnames[i])
  
  sensid = strsplit(basename(fnames[i]), '\\.')[[1]][1]
  print(sensid)
  outname = paste('/Volumes/Padlock/purpleair_all_corr/', as.character(sensid), '.csv', sep = '')
  if (!file.exists(outname)) {
  
    df <- read.csv(fnames[i])
    colnames(df) <- renames

    df <- df %>% mutate(sensor_id = sensid, day_date = as.Date(created_at, format="%Y-%m-%d"))
    print(nrow(df))
    df <- df %>% dplyr::group_by(sensor_id, day_date) %>%
      dplyr::summarise(sensor_id = first(sensor_id),
                day_date = first(day_date),
                mean_pm25_atm = mean(pm25_atm),
                mean_pm25_1 = mean(pm25_1),
                mean_temp = mean(temperature),
                mean_humid = mean(humidity),
                pm25_atm_corr = .52*mean_pm25_atm  -.085*mean_humid + 5.71,
                pm25_1_corr = .52*mean_pm25_1  -.085*mean_humid + 5.71)
    print(nrow(df))

    # decide whether to write the file - are there at least 5 measurements?
    if(nrow(df) >= 5) {
      
      write.csv(df, file = outname, row.names = FALSE)
      purpledf = rbind(purpledf, df)
      
    }
  }
}

sensor_loc = read.csv('~/GitHub/wildfires/bay_area_sensors.csv')

# fpath = '/Volumes/Padlock/purpleair_all_corr'
# 
# summ_cols = c("sensor_id","day_date","mean_pm25_atm","mean_pm25_1","mean_temp","mean_humid","pm25_atm_corr","pm25_1_corr")
# 
# fnames <- list.files(fpath, full.names = TRUE)
# daydf = data.frame(matrix(ncol=length(summ_cols), nrow=0))
# 
# colnames(daydf) = summ_cols
# myids = vector('list', 2998)
# for (i in 1:length(fnames)) {
#   tmp = read.csv(fnames[i])
#   tmp_id = strsplit(basename(fnames[i]), '\\.')[[1]][1]
#   tmp <- tmp %>% mutate(id = tmp_id)
#   myids[[i]] <- tmp_id
#   daydf <- rbind(daydf, tmp)
# }

# # correction - fix
# daydf <- daydf %>% mutate(pm25_atm_corr = .524*mean_pm25_atm - 0.0852*mean_humid + 5.72,
#                           pm25_1_corr = .524*mean_pm25_1 - 0.0852*mean_humid + 5.72)

sensor_loc <- sensor_loc %>% dplyr::select(id, lat, lon, name, location_type)

daydf = merge(sensor_loc, purpledf, by.x = 'id', by.y = 'sensor_id') 

write.csv(daydf, file='~/wildfires/all_purpleair.csv', row.names = FALSE)

# filter to one day
oneday = daydf %>% filter(as.Date(day_date) == as.Date('2018-11-15'))



write.csv(day, file='~/wildfires/2018_bayarea_pm25_purple.csv', row.names = FALSE)
