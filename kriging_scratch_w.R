library(gdalUtils)
library(rgdal)

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

  #write out sensors shapefile
  writeOGR(obj=sensors, layer = paste(Sys.Date(), day_ord, water, 'sensors', sep = '_'), 
         dsn = '/Volumes/Padlock/wildfires/output_geo/', driver = "ESRI Shapefile", overwrite = TRUE)

  dat_krig <- krige(kv, sensors, grd, model=var_fit)
  
  # convert kriged surface to raster object for clipping
  rkrig <- raster(dat_krig)
  rk_coast_clip <- raster::mask(rkrig, ca_county)
  
  if (one_watershed) {
    rk_water_clip <- raster::mask(rk_coast_clip, subset(watershed_map, OBJECTID == water))
  } else {
    rk_water_clip <- raster::mask(rk_coast_clip, subset(watershed_map, !(OBJECTID %in% waters)))
  }
  krig_map <- raster_map(rk_water_clip, sensors_full, bound_box,  TRUE, day_ord, paste(Sys.Date(), day_ord, sep = '_'))  
  writeRaster(rk_water_clip, filename = paste('/Volumes/Padlock/wildfires/rasts_krig_water/', Sys.Date(), day_ord, water,  '.tif', sep = '_'), overwrite = TRUE)
  return(rk_water_clip)
  }

# this chunk should be turned into a function above and then deleted
pa_remove = read.csv('/Volumes/Padlock/wildfires/pa_remove.csv')

# define the 1st order polynomial equation
kv <<- as.formula(daily_mean_pm25 ~ latitude + longitude)

days <- seq(as.Date('2018-11-01'), as.Date('2018-11-30'), by='days')

waters <<- c( "88", "5", "90", "89")
  
for (day in 1:30) {
  outrast = paste('/Volumes/Padlock/wildfires/rasts_krig_water/', Sys.Date(), sep = '')
  
  # filter to a single date
  sensors_tmpday <- subset(sensors_full, as.Date(date, '%m/%d/%Y') == as.Date(days[day]))
  
  # remove any sensors with bad readings
  tmp_remove <- pa_remove %>% filter(as.Date(day_date) == as.Date(days[day]))
  print(nrow(tmp_remove))
  sensors_tmpday <- subset(sensors_tmpday, !(site_id %in% tmp_remove$id))
  sensors_tmpday <- sensors_tmpday[-zerodist(sensors_tmpday)[,1],]
  
  crs(sensors_tmpday) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  sensors_tmpday <- subset(sensors_tmpday, !is.na(daily_mean_pm25))
  
  for (watershed in waters) {
    print(paste('day', day,'water', watershed))
    sensors_tmpwater <- subset(sensors_tmpday, watershed_w == watershed)
    print(nrow(sensors_tmpwater))
    
    tmp_rast = kriging(day, watershed, sensors_tmpwater, ca_county_map, TRUE)

  }
  
  sensors_other_watershed = subset(sensors_tmpday, !(watershed_w %in% waters))
  tmp_rast_other = kriging(day, 0, sensors_other_watershed, ca_county_map, FALSE)

}

## MERGE ALL WATERSHEDS
rasts_to_merge = list.files('/Volumes/Padlock/wildfires/rasts_krig_water', pattern = '.tif', full.names = TRUE)

mosaic_rasters(gdalfile=rasts_to_merge,dst_dataset="/Volumes/Padlock/wildfires/rasts_krig_water_1.tif",of="GTiff")
gdalinfo("/Volumes/Padlock/wildfires/rasts_krig_water.tif")

for (i in 1:1) {
  tmp_pattern = paste('_2021-07-24_', i, '_', sep = '')
  tmp_dst = paste("/Volumes/Padlock/wildfires/rasts_krig_water_full/rasts_krig_water_", i, '.tif', sep = '')
  
  rasts_to_merge = list.files('/Volumes/Padlock/wildfires/rasts_krig_water', pattern = tmp_pattern, full.names = TRUE)
  mosaic_rasters(gdalfile=rasts_to_merge, dst_dataset=tmp_dst, of = "GTiff")
}

# map the rasters for each day
rastdir = '/Volumes/Padlock/wildfires/rasts_krig_water_full'
rastfiles = list.files(rastdir, pattern = '.tif', full.names = TRUE)
for (i in 1:length(rastfiles)) {
  tmprast = raster(rastfiles[[i]])
  
  # extract the number from the file
  tmpname = strsplit(basename(rastfiles[[i]]), '_')[[1]][4]
  day = strsplit(tmpname, '\\.')[[1]][1]
  print(tmpname)
  print(day)
  
  rast_plt <- tm_shape(tmprast) +
    tm_raster(n=12, palette = "YlOrRd", title=day, style='fixed', breaks=c(0,25,50,75,100,125,150,175,200,225,250,300,400)) +
    tm_shape(sensors_full) + tm_dots(size=.01) +
    tm_legend(legend.outside=TRUE, legend.show=TRUE, legend.text.size=1.2)
  
  file_name = file_naming(paste('watershed_kriging_map_', day, sep = ''), 'image')
  tmap_save(rast_plt + tm_layout(outer.margins = c(0,0,0,0)), file=file_name)
  
  #raster_map(tmp_rast, sensors_full, bound_box,  TRUE, day, paste(day, 'watershed_kriging_map', sep = '_'))
}
