library(AirSensor)
library(dplyr)
library(tidyr)

setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")

# wrapper function to manage quality control checks
qc_wrapper <- function(dd_ID, pat_tmp) {
  
  pm25a <- pat_tmp$data$pm25_A
  pm25a_narm <- pm25a[!is.na(pm25a)]
  
  pm25b <- pat_tmp$data$pm25_B
  pm25b_narm <- pm25b[!is.na(pm25b)]
  
  # check correlation between a and b channels
  if (sum(complete.cases(pm25a, pm25b)) > 0) {
    ab_corr <- cor(pm25a, pm25b, use = "complete.obs")
  } else {
    ab_corr = 0
  }
  print(ab_corr)
  
  # check channels for long strings of identical values
  if (length(pm25a_narm) > 0) {a_repeat <- max(rle(pm25a_narm)$lengths)} else {a_repeat = NaN}
  if (length(pm25b_narm) > 0) {b_repeat <- max(rle(pm25b_narm)$lengths)} else {b_repeat = NaN}
  
  
  #a_repeat <- max(rle(pm25a_narm)$lengths)
  #b_repeat <- max(rle(pm25b_narm)$lengths)
  
  # check channels for extreme noise
  
  # how changes made by default filtering
  
  if (sum(complete.cases(pm25a, pm25b)) > 0) {
    pat_tmp_out <- pat_tmp %>% pat_outliers(replace = TRUE, showPlot = FALSE)
    
    a_filter_diffs = pat_tmp$data$pm25_A == pat_tmp_out$data$pm25_A
    a_filter_diffs_tbl = table(a_filter_diffs)
    a_filtered_diff = a_filter_diffs_tbl['FALSE'][[1]]
    
    b_filter_diffs = pat_tmp$data$pm25_B == pat_tmp_out$data$pm25_B
    b_filter_diffs_tbl = table(b_filter_diffs)
    b_filtered_diff = b_filter_diffs_tbl['FALSE'][[1]]
  } else {
    a_filtered_diff = NaN
    b_filtered_diff = NaN
  }
  
  a_noisy = mean(diff(pm25a_narm))
  b_noisy = mean(diff(pm25b_narm))


  qc_tmpdf = data.frame('deviceDeploymentID' = dd_ID, 
                        'n_measurements' = nrow(pat_tmp$data), 
                        'ndays' = length(unique(as.Date(pat_tmp$data$datetime))),
                        'date_earliest'= as.Date(min(pat_tmp$data$datetime)),
                        'date_latest' = as.Date(max(pat_tmp$data$datetime)), 
                       'ab_correlation' = ab_corr, 
                       'a_repeats' = a_repeat, 
                       'b_repeats' = b_repeat, 
                       'a_noise' = a_noisy, 
                       'b_noise' = b_noisy,
                       'a_outlier_change' = a_filtered_diff, 
                       'b_outlier_change' = b_filtered_diff)
  return(qc_tmpdf)
  
}

all_pas <- pas_load()

north = 38.6
south = 36.6
east = -120.93
west = -123.0

ba_pas <- all_pas %>% filter(latitude <= north & latitude >= south & 
                               longitude <= east & longitude >= west)

ba_unique_ids = unique(ba_pas$deviceDeploymentID)
ba_pats <- vector(length = 2*length(ba_unique_ids)) # vector to hold data

qc_data <- data.frame(matrix(ncol=12, nrow=0))

colnames(qc_data) = c('deviceDeploymentID', 'n_measurements', 'ndays', 'date_earliest', 'date_latest', 
                      'ab_correlation', 'a_repeats', 'b_repeats', 'a_noise', 'b_noise', 'a_outlier_change', 'b_outlier_change')
good_ids = c()

#for (i in 1:length(ba_unique_ids)) {
for (i in 5592:length(ba_unique_ids)) {
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
  
  # if there are records, note the id and run the quality control functions
  if (nrow(cmb$data) > 0) {
    good_ids = c(good_ids, ba_unique_ids[i])
    qc_tmp = qc_wrapper(ba_unique_ids[i], cmb)
    qc_data = rbind(qc_data, qc_tmp)
    
    write.csv(cmb$meta, file = paste('/Volumes/Padlock/purple_mz/', ba_unique_ids[i], '_meta.csv', sep=''))
    write.csv(cmb$data, file = paste('/Volumes/Padlock/purple_mz/', ba_unique_ids[i], '_data.csv', sep=''))
  }
}
write.csv(qc_data, file = '~/wildfires/purple_qc_data_4.csv')
write.csv(good_ids, file = '~/wildfires/purple_good_ids.csv')
