library(AirSensor)
library(dplyr)

setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")


all_pas <- pas_load()

north = 38.6
south = 36.6
east = -120.93
west = -123.0

ba_pas <- all_pas %>% filter(latitude <= north & latitude >= south & 
                               longitude <= east & longitude >= west)

ba_ids = ba_pas$deviceDeploymentID

ba_pas <- ba_pas %>% filter(deviceDeploymentID %in% ba_ids)


a <- pat_downloadParseRawData(
  id = '4195f1ffd51a417d_767',
  label = NULL,
  pas = ba_pas,
  startdate = '2018-10-31',
  enddate = '2018-12-01',
  timezone = "America/Los_Angeles",

  baseUrl = "https://api.thingspeak.com/channels/"
)

