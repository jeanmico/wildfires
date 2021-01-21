# read in the data from openaq and see what we have
library(dplyr)
library(tidyr)
library(ggplot2)
oaq_sf_raw = read.csv('~/wildfires/openAQ/openaq_sf_area.csv')
oaq_sf = oaq_sf_raw %>% filter(location == "San Francisco")

# make a plot of Nov 2018 for SF location only
oaq_sf_nov = oaq_sf %>% filter(location == "San Francisco")

oaq_nov = oaq_sf_raw %>% filter(utc == as.Date('2018-12-01') | utc <'2018-08-31')

# utc = Coordinated Universal Time
oaq_sf_raw <- oaq_sf_raw %>% mutate(datetime = as.POSIXct(local, format="%Y-%m-%dT%H:%M:%S"))

oaq_nov = oaq_sf_raw %>% filter(datetime < as.POSIXct('2018-12-01') & datetime > as.POSIXct('2018-10-31'))

plt = ggplot(oaq_nov, aes(y = value, x = datetime, colour = location)) +
  geom_line()
plt

# check the oakland locations
oak = oaq_nov %>% filter(location == "Oakland West")
# Oakland West is missing a few days towards the beginning of the month

# read and append all the openAQ files
fpath = '~/wildfires/openAQ'
fnames <- list.files(fpath, full.names = TRUE)
epa_hourly = data.frame(matrix(ncol=11, nrow=0))
colnames(epa_hourly) = c("location", "city", "country", "utc", "local", "parameter", "value", "unit", "latitude", "longitude", "attribution")

for (f in fnames) {
 tmp = read.csv(f)
 print('ok')
 epa_hourly = rbind(epa_hourly, tmp)
}

# get the hourly locations to construct a map
hourly_loc = epa_hourly %>% distinct(location, latitude, longitude)
# write the locations to a csv
write.csv(hourly_loc, file='~/wildfires/hourly_locations.csv', row.names = FALSE)

# INSTEAD OF USING OPENAQ DATA, USE THE HOURLY DATA FROM THE EPA SITE DIRECTLY!!
fulldata = read.csv('/Users/student/wildfires/hourly_EPA/hr_88101_CA_2018.csv', header=FALSE)
colnames(fulldata) = c("State_Code", "County_Code", "Site_Num", "Parameter_Code", "POC", 
                       "Latitude", "Longitude", "Datum", 
                       "Parameter_Name", "Date_Local", "Time_Local", "Date_GMT", "Time_GMT", 
                       "Sample_Measurement", "Units_of_Measure", "MDL", 
                       "Uncertainty", "Qualifier", "Method_Type", "Method_Code", "Method_Name", 
                       "State_Name", "County_Name", "Date_of_Last_Change")
