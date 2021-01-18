
setwd("/Users/student/JelliffeWitte/wildfires")
fpath = "/Users/student/JelliffeWitte/wildfires/purpledata"

mydf = data.frame(matrix(ncol=11, nrow=0))
colnames(mydf) = c("entryid", "created_at", 'pm1_atm', 'pm25_atm', 'pm10_atm', 'uptime_min', 'ADC', 'temperature', 'humidity', 'pm25_1', 'sensor_id')


fnames <- list.files(fpath, full.names = TRUE)
for(i in 1:length(fnames)){
  
    
  print(fnames[i])
  df <- read_csv(fnames[i])
  colnames(df) = c("entryid", "created_at", 'pm1_atm', 'pm25_atm', 'pm10_atm', 'uptime_min', 'ADC', 'temperature', 'humidity', 'pm25_1')
  
  sensid = strsplit(basename(fnames[i]), '\\.')[[1]][1]
  
  df <- df %>% mutate(sensor_id = sensid)
  

  mydf = rbind(mydf, df)
}



sample_id = c(1742, 2574, 38457, 20709)

sampledf = mydf %>% filter(sensor_id %in% sample_id)
sampledf = read.csv("/Users/student/JelliffeWitte/wildfires/purpledata/1742.csv")
colnames(sampledf) = c("entryid", "created_at", 'pm1_atm', 'pm25_atm', 'pm10_atm', 'uptime_min', 'ADC', 'temperature', 'humidity', 'pm25_1')
lims <- as.POSIXct(strptime(c('2018-11-01', '2018-12-01'), format = '%Y-%m-%d'))

p <- ggplot(sampledf, aes(x=created_at, y=pm25_atm)) +
  geom_point(size=.1) +
  scale_x_datetime(limits = lims)

p
