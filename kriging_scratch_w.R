kriging <- function(day_ord, water, sensors, ca_county, one_watershed) {
  var_krig <- variogram(kv, sensors)
  
  # make a plot to assess the fit
  var_fit <- fit.variogram(var_krig, fit.ranges = FALSE, fit.sills = FALSE, 
                           vgm(psill=14, model="Sph", range=590000, nugget=0))
  png(file = file_naming(paste(day_ord, water, 'var', sep = "_"), 'image'))
  plot(var_krig, var_fit)
  dev.off()
  print('var_done')
  
  
  # make empty grid
  grd <- as.data.frame(spsample(sensors_full, 'regular', n=500000))
  
  names(grd) <- c("longitude", "latitude")
  coordinates(grd) <- c('longitude', 'latitude')
  gridded(grd) <- TRUE
  fullgrid(grd) <- TRUE
  
  # add projection to empty grid
  crs(grd) <- CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  print('grid made')
  
  #sensors_tmpday1 <- sensors[-zerodist(sensors)[,1],]
  
  #write out sensors shapefile
  #writeOGR(obj=sensors, layer = file_naming(paste(day_ord, water, 'sensors', sep = '_' ), 'shp'), 
  #         dsn='/Volumes/', driver = "ESRI Shapefile", overwrite = TRUE)

  dat_krig <- krige(kv, sensors, grd, model=var_fit)
  
  # convert kriged surface to raster object for clipping
  rkrig <- raster(dat_krig)
  rk_coast_clip <- raster::mask(rkrig, ca_county)
  
  if (one_watershed) {
    rk_water_clip <- raster::mask(rk_coast_clip, subset(watershed_map, OBJECTID == water))
  } else {
    rk_water_clip <- raster::mask(rk_coast_clip, subset(watershed_map, !(OBJECTID %in% waters)))
  }
  krig_map <- raster_map(rk_water_clip, sensors_full, bound_box,  TRUE, days[day], paste(Sys.Date(), days[day], sep = '_'))  
  return(rk_water_clip)
  }

# this chunk should be turned into a function above and then deleted
pa_remove = read.csv('/Volumes/Padlock/wildfires/pa_remove.csv')

# define the 1st order polynomial equation
kv <<- as.formula(daily_mean_pm25 ~ latitude + longitude)

days <- seq(as.Date('2018-11-01'), as.Date('2018-11-30'), by='days')

waters <<- c("91", "88", "5", "90", "89", "100", "93", "81")

full_rast <- as.data.frame(spsample(sensors_full, 'regular', n=500000))
names(full_rast) <- c("longitude", "latitude")
coordinates(full_rast) <- c('longitude', 'latitude')
gridded(full_rast) <- TRUE
fullgrid(full_rast) <- TRUE
# add projection to empty grid
crs(full_rast) <- CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
for (day in 16:16) {
  outrast = paste('/Volumes/Padlock/wildfires/rasts_krig_water/', Sys.Date(), sep = '')
  
  # filter to a single date
  sensors_tmpday <- subset(sensors_full, as.Date(date, '%m/%d/%Y') == as.Date(days[day]))
  
  # remove any sensors with bad readings
  tmp_remove <- pa_remove %>% filter(as.Date(day_date) == as.Date(days[day]))
  print(nrow(tmp_remove))
  sensors_tmpday <- subset(sensors_tmpday, !(site_id %in% tmp_remove$id))
  
  crs(sensors_tmpday) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  sensors_tmpday <- subset(sensors_tmpday, !is.na(daily_mean_pm25))
  
  watersheds_test = unique(sensors_full$watershed_w)
  water_test = watersheds_test[watersheds_test != "66"]
  
  for (watershed in waters) {
    print(paste('water', water))
    sensors_tmpwater <- subset(sensors_tmpday, watershed_w == water)
    print(nrow(sensors_tmpwater))
    
    tmp_rast = kriging(day, watershed, sensors_tmpwater, ca_county_map, TRUE)
    print('returned')
    
    
  }
  
  sensors_other_watershed = subset(sensors_tmpday, !(watershed_w %in% waters))
  tmp_rast_other = kriging(day, 0, sensors_other_watershed, ca_county_map, FALSE)
  
  full_rast = raster::merge(tmp_rast_other, tmp_rast)
  
  # map the interpolated raster
  
  writeRaster(tmpfull, filename = paste(outrast, days[day],  '.tif', sep = ''), overwrite = TRUE)
  
  krig_map <- raster_map(full_rast, sensors_full, bound_box,  TRUE, days[day], paste(Sys.Date(), days[day], sep = '_'))
  
}


full_test <- extent(bound_box)
template <- raster(full_test)
projection(template) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
#writeRaster(template, file="MyBigNastyRasty.tif", format="GTiff")

tmp_aligned = projectRaster(from = tmp_rast, to = template)
tmp_other_aligned = projectRaster(from = tmp_rast_other, to = template)

tmpfull = raster::merge(tmp_aligned, tmp_other_aligned)

krig_map <- raster_map(tmp_rast, sensors_full, bound_box,  TRUE, days[day], paste(Sys.Date(), days[day], sep = '_'))

for (day in 1:length(days)) {
  sensors_tmpday <- subset(sensors_full, as.Date(date, '%m/%d/%Y') == as.Date(days[day]))
  
  # remove any sensors with bad readings
  tmp_remove <- pa_remove %>% filter(as.Date(day_date) == as.Date(days[day]))
  print(nrow(tmp_remove))
  sensors_tmpday <- subset(sensors_tmpday, !(site_id %in% tmp_remove$id))
  
  write.csv(sensors_tmpday, file = paste('/Volumes/Padlock/wildfires/daily_sensor/', Sys.Date(), '_', days[day], '.csv', sep = ''))
}

