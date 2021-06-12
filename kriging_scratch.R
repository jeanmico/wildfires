# this chunk should be turned into a function above and then deleted
pa_remove = read.csv('/Volumes/Padlock/wildfires/pa_remove.csv')

# define the 1st order polynomial equation
kv <- as.formula(daily_mean_pm25 ~ latitude + longitude)

days <- seq(as.Date('2018-11-01'), as.Date('2018-11-30'), by='days')

for (day in 1:15) {
  outrast = paste('/Volumes/Padlock/wildfires/rasts_krig/', Sys.Date(), sep = '')
  
  # filter to a single date
  sensors_tmpday <- subset(sensors_full, as.Date(date, '%m/%d/%Y') == as.Date(days[day]))
  
  # remove any sensors with bad readings
  tmp_remove <- pa_remove %>% filter(as.Date(day_date) == as.Date(days[day]))
  print(nrow(tmp_remove))
  sensors_tmpday <- subset(sensors_tmpday, !(site_id %in% tmp_remove$id))
  
  crs(sensors_tmpday) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  sensors_tmpday <- subset(sensors_tmpday, !is.na(daily_mean_pm25))
  
  # compute the sample variogram
  var_krig <- variogram(kv, sensors_tmpday)
  
  # make a plot to assess the fit
  var_fit <- fit.variogram(var_krig, fit.ranges = FALSE, fit.sills = FALSE, 
                           vgm(psill=14, model="Sph", range=590000, nugget=0))
  png(file = paste('/Volumes/Padlock/wildfires/rasts_krig_var/', Sys.Date(), days[day], '.png', sep = '_'))
  plot(var_krig, var_fit)
  dev.off()

  # define the trend model
  
  # perform the krige interpolation
  grd <- as.data.frame(spsample(sensors_full, 'regular', n=500000))
  
  names(grd) <- c("longitude", "latitude")
  coordinates(grd) <- c('longitude', 'latitude')
  gridded(grd) <- TRUE
  fullgrid(grd) <- TRUE
  
  # add projection to empty grid
  #proj4string(data_sp)
  crs(grd) <- CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  sensors_tmpday <- sensors_tmpday[-zerodist(sensors_tmpday)[,1],]
  #writeOGR(obj=sensors_tmpday, layer = 'sensors_tmpday_2', dsn='/Users/student/wildfires/', driver = "ESRI Shapefile")
  
  dat_krig <- krige(kv, sensors_tmpday, grd, model=var_fit)
  
  # convert kriged surface to raster object for clipping
  rkrig <- raster(dat_krig)
  rk_clip <- mask(rkrig, bound_box)
  rk_coast_clip <- mask(rk_clip, ca_county_map)
  
  writeRaster(rk_coast_clip, filename = paste(outrast, days[day], 'clip', '.grd', sep = ''))
  
  krig_map <- raster_map(dat_krig, sensors_full, bound_box,  TRUE, days[day], paste(Sys.Date(), days[day], sep = '_'))
  
}


for (day in 1:length(days)) {
  sensors_tmpday <- subset(sensors_full, as.Date(date, '%m/%d/%Y') == as.Date(days[day]))
  
  # remove any sensors with bad readings
  tmp_remove <- pa_remove %>% filter(as.Date(day_date) == as.Date(days[day]))
  print(nrow(tmp_remove))
  sensors_tmpday <- subset(sensors_tmpday, !(site_id %in% tmp_remove$id))
  
  write.csv(sensors_tmpday, file = paste('/Volumes/Padlock/wildfires/daily_sensor/', Sys.Date(), '_', days[day], '.csv', sep = ''))
}

