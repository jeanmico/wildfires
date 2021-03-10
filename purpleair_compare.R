
testid = '1596'
testdate = '2018-11-15'
# comparing the PurpleAir I downloaded to the PurpleAir mazama science downloaded

pa_mine = read.csv('~/wildfires/2018_purpleair.csv')
pa_mine_day <- pa_mine %>% filter(!is.na(pm25_atm_corr))
pa_mine_day <- pa_mine_day %>% filter(as.Date(day_date) == testdate)

# Read in the mazama science file
pa_mz_qc = read.csv('~/wildfires/purple_qc_data_4.csv')

# ids from my dataset
pa_myids = unique(pa_mine_day$id)


mz_qc_test = pa_mz_qc %>% filter(grepl(testid, deviceDeploymentID, fixed=TRUE))
ddID = mz_qc_test$deviceDeploymentID


# read in the testid file from mazama

mzfile = paste('/Volumes/Padlock/purple_mz/', ddID, '_data.csv', sep ='')
mz_test = read.csv(mzfile)


# read in daily averages for our test day of Nov 15 2018
mz_daily = data.frame(matrix(ncol=20, nrow=0))

colnames(mz_daily) = c("datetime","pm25_A","pm25_B","temperature","humidity",
             "pressure","pm1_atm_A","pm25_atm_A","pm10_atm_A",
             "pm1_atm_B","pm25_atm_B","pm10_atm_B","uptime","rssi",
             "memory","adc0","bsec_iaq","datetime_A","datetime_B", "deviceDeploymentID")
mz_avgday = read.csv('')

daily_files = list.files(path = '/Volumes/Padlock/purple18_daily/', full.names = TRUE)

for (i in daily_files) {
  tmpdf = read.csv(i)
  tmpdf <- tmpdf %>% mutate(deviceDeploymentID = strsplit(basename(i), '\\.')[[1]][1])
  mz_daily = rbind(mz_daily, tmpdf)
}

mz_day = mz_daily %>% filter(as.Date(datetime) == as.Date(testdate))

# mz_day has 30 observations, I recorded 143 - investigate an id difference
mz_day <- mz_day %>% separate(deviceDeploymentID, into=c('id_first', 'id'), remove = FALSE)

mz_ids = unique(mz_day$id)

mine_only = setdiff(pa_myids, mz_ids)
mz_only = setdiff(mz_ids, pa_myids)

mine_only_ex = '1596'

mz_only_ex = '19065'

mine_ex = pa_mine %>% filter(id == '1596')
View(mine_ex)

mine_ex_nov = mine_ex %>% filter(as.Date(day_date) < as.Date('2018-12-01') & as.Date(day_date) > as.Date('2018-10-31'))

tplt = ggplot(mine_ex_nov, aes(x= day_date, y = mean_pm25_1)) +
  geom_point()
tplt


pamzqc <- pa_mz_qc %>% separate(deviceDeploymentID, c('id_first', 'id'), sep = '_')
View(pamzqc)

a = pamzqc %>% filter(id == testid)
View(a)


aplt <- ggplot(pa_mz_qc, aes(x = date_earliest)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))

aplt

ggsave(aplt, file = '~/wildfires/date_earliest_measurement.png')

library(AirSensor)
library(dplyr)
library(tidyr)

setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")

