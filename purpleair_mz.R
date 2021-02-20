library(AirSensor)
library(dplyr)
library(tidyr)

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

b <- pat_createPATimeseriesObject(a)

bplot <- b %>% pat_multiPlot(plottype = 'all')
bmat <- b %>% pat_scatterPlotMatrix()
bmat
bplot
# seems like Channel A is experiencing extreme noise, in this case can we just use channel b?
ba_unique_ids = unique(ba_ids)
ba_pats <- vector(length = length(ba_unique_ids))

bout <- b %>% pat_outliers(replace = TRUE, showPlot = TRUE)

# check for long strings of identical values
pm25a <- bout$data$pm25_A
pm25a <- pm25a[!is.na(pm25a)]
maxrle <- max(rle(pm25a)$lengths)

pm25b <- bout$data$pm25_B
pm25b <- pm25b[!is.na(pm25b)]

max_repeats = 20

if (maxrle > max_repeats) {
  mychannel <- "B"
}

# poor correlation between A and B
correlation_min <- .75

ab_cor <- cor(pm25a, pm25b)

if (ab_cor < correlation_min) {
  print('bad correlation')
}



# extreme noise in A
# https://mazamascience.github.io/AirSensor/articles/articles/Custom_QC_Algorithms.html#example-2-z-score-based-qc-1


for (i in 1:length(ba_unique_ids)) {
  print(i)
  tmp <- pat_downloadParseRawData(
    id = ba_unique_ids[i],
    label = NULL,
    pas = ba_pas,
    startdate = '2018-10-31',
    enddate = '2018-12-01',
    timezone = "America/Los_Angeles",
    baseUrl = "https://api.thingspeak.com/channels/"
  )
  cmb <- pat_createPATimeseriesObject(tmp)
  print(paste('length', as.character(length(cmb))))
  ba_pats[[i]] <- cmb
}
