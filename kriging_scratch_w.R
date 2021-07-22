# this chunk should be turned into a function above and then deleted
pa_remove = read.csv('/Volumes/Padlock/wildfires/pa_remove.csv')

# define the 1st order polynomial equation
kv <- as.formula(daily_mean_pm25 ~ latitude + longitude)

days <- seq(as.Date('2018-11-01'), as.Date('2018-11-30'), by='days')

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
  
  for (water in unique(sensors_full$watershed_w)) {
    print(paste('water', water))
    print(nrow(sensors_tmpday))
    sensors_tmpwater <- subset(sensors_tmpday, watershed_w == water)
    print(nrow(sensors_tmpwater))
    # compute the sample variogram
    var_krig <- variogram(kv, sensors_tmpwater)
    
    # make a plot to assess the fit
    var_fit <- fit.variogram(var_krig, fit.ranges = FALSE, fit.sills = FALSE, 
                             vgm(psill=14, model="Sph", range=590000, nugget=0))
    png(file = paste('/Volumes/Padlock/wildfires/rasts_krig_var_water/', Sys.Date(), days[day], '.png', sep = '_'))
    plot(var_krig, var_fit)
    dev.off()
    
    # define the trend model
    print('var_done')
    # perform the krige interpolation
    grd <- as.data.frame(spsample(sensors_full, 'regular', n=500000))
    
    names(grd) <- c("longitude", "latitude")
    coordinates(grd) <- c('longitude', 'latitude')
    gridded(grd) <- TRUE
    fullgrid(grd) <- TRUE
    
    # add projection to empty grid
    #proj4string(data_sp)
    crs(grd) <- CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    print('grid made')
    
    sensors_tmpday1 <- sensors_tmpwater[-zerodist(sensors_tmpwater)[,1],]
    #writeOGR(obj=sensors_tmpday, layer = 'sensors_tmpday_2', dsn='/Users/student/wildfires/', driver = "ESRI Shapefile")
    print(nrow(sensors_tmpwater))
    dat_krig <- krige(kv, sensors_tmpwater, grd, model=var_fit)
    
    # convert kriged surface to raster object for clipping
    rkrig <- raster(dat_krig)
    rk_coast_clip <- raster::mask(rkrig, ca_county_map)
    rk_water_clip <- raster::mask(rk_coast_clip, subset(watershed, OBJECTID == water))
    
    #writeRaster(rk_coast_clip, filename = paste(outrast, days[day], water '.tif', sep = ''), overwrite = TRUE)
    
    # map the interpolated raster
    rast_plt <- tm_shape(rk_water_clip) +
      tm_raster(n=12, palette = "YlOrRd", title='rast_title', style='fixed', breaks=c(0,25,50,75,100,125,150,175,200,225,250,300,400)) +
      tm_shape(sensors_full) + tm_dots(size=.01) +
      tm_legend(legend.outside=TRUE, legend.show=TRUE, legend.text.size=1.2)
    
    file_name = file_naming(water, 'image')
    tmap_save(rast_plt + tm_layout(outer.margins = c(0,0,0,0)), file=file_name)
    
    
    #krig_map <- raster_map(dat_krig, sensors_full, bound_box,  TRUE, days[day], paste(Sys.Date(), days[day], sep = '_'))
  }
  

  
}


for (day in 1:length(days)) {
  sensors_tmpday <- subset(sensors_full, as.Date(date, '%m/%d/%Y') == as.Date(days[day]))
  
  # remove any sensors with bad readings
  tmp_remove <- pa_remove %>% filter(as.Date(day_date) == as.Date(days[day]))
  print(nrow(tmp_remove))
  sensors_tmpday <- subset(sensors_tmpday, !(site_id %in% tmp_remove$id))
  
  write.csv(sensors_tmpday, file = paste('/Volumes/Padlock/wildfires/daily_sensor/', Sys.Date(), '_', days[day], '.csv', sep = ''))
}

