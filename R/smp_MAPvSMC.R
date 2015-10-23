# # 
#library(raster)
#library(rgdal)
# 
# # ndvi map
# ndvi_2015_06 <- "/run/user/1000/gvfs/smb-share:server=abz01fst.eurac.edu,share=users/Common/Interdisciplinary_projects/HiResAlp/01_data/Drone_Field_Work/NDVI_Matsch_2015_06_10/Data/20150610_it-mat_3rd_NDVI_index_ndvi.tif"
# # thermal map
# therm_2015_10 <- "/run/user/1000/gvfs/smb-share:server=abz01fst.eurac.edu,share=users/Common/Interdisciplinary_projects/HiResAlp/01_data/Drone_Field_Work/Campaign_2015_10_12/test_flir_transparent_mosaic_group1.tif"
# 
# #elevation
# dem <- "/run/user/1000/gvfs/smb-share:server=abz01fst.eurac.edu,share=users/Common/Interdisciplinary_projects/HiResAlp/01_data/Drone_Field_Work/Campaign_2015_06_10/Qgis/dem_2_5m/dem_2_5m"
# #slope map
# slp  <- "/run/user/1000/gvfs/smb-share:server=abz01fst.eurac.edu,share=users/Common/Interdisciplinary_projects/HiResAlp/01_data/Drone_Field_Work/Campaign_2015_06_10/Qgis/montacini_slope2_5m.tif"
# #aspect map
# asp  <- "/run/user/1000/gvfs/smb-share:server=abz01fst.eurac.edu,share=users/Common/Interdisciplinary_projects/HiResAlp/01_data/Drone_Field_Work/Campaign_2015_06_10/Qgis/montacini_aspect2_5m.tif"
# #topographic wetness index
# twi <- "/run/user/1000/gvfs/smb-share:server=abz01fst.eurac.edu,share=users/Common/Interdisciplinary_projects/HiResAlp/01_data/Drone_Field_Work/Campaign_2015_06_10/Qgis/montacini_wetnessindex_2_5m"
# #curvature
# curv <- "/run/user/1000/gvfs/smb-share:server=abz01fst.eurac.edu,share=users/Common/Interdisciplinary_projects/HiResAlp/01_data/Drone_Field_Work/Campaign_2015_06_10/Qgis/curvature/curv"
# prof <- "/run/user/1000/gvfs/smb-share:server=abz01fst.eurac.edu,share=users/Common/Interdisciplinary_projects/HiResAlp/01_data/Drone_Field_Work/Campaign_2015_06_10/Qgis/curvature/profile"
# plan <- "/run/user/1000/gvfs/smb-share:server=abz01fst.eurac.edu,share=users/Common/Interdisciplinary_projects/HiResAlp/01_data/Drone_Field_Work/Campaign_2015_06_10/Qgis/curvature/plan"

# 
# smc_file  <- "/run/user/1000/gvfs/smb-share:server=abz01fst.eurac.edu,share=users/Common/Interdisciplinary_projects/HiResAlp/01_data/Radarsat_Field_Work/WET_csv/HYD_2015_06_10.csv"
# smc_file  <- "/run/user/1000/gvfs/smb-share:server=abz01fst.eurac.edu,share=users/Common/Interdisciplinary_projects/HiResAlp/01_data/Radarsat_Field_Work/WET_csv/HYD_2015_08_21.csv"
# smc_file  <- "/run/user/1000/gvfs/smb-share:server=abz01fst.eurac.edu,share=users/Common/Interdisciplinary_projects/HiResAlp/01_data/Radarsat_Field_Work/WET_csv/HYD_2015_10_12.csv"
# 
# # smc data in %vol
# smc_data <- read.csv(smc_file)
# smc  <- smc_data[,6]
# smc_y_deg <- smc_data[,4]
# smc_x_deg <- smc_data[,5]


smp_MAPvSMC <- function(map, smc_data, smc_x_deg, smc_y_deg, plot_name, NAval = NULL,
                        crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
{
  data_map <- raster(map)
  
  if(!is.null(NAval))
    NAvalue(data_map) <- NAval
  
  coords_deg <- cbind(smc_x_deg, smc_y_deg)
  
  if (is.na(data_map@crs@projargs))
    data_map@crs@projargs <- crs
  
  coords_m   <- project(coords_deg, data_map@crs@projargs)
  
  df <- extract(data_map, coords_m)
  dfForScatter <- cbind(map=df, smc=smc_data)
# dfForScatter[c(43,87,98,44,49,56),] = NA
# dfForScatter[c(7,43,46,49),] = NA
  
  lm_map_smc <- lm(dfForScatter[,"smc"] ~ dfForScatter[,"map"])
  summ_lm     <- summary(lm_map_smc)
  R2 <- round(summ_lm$r.squared, 2)
  intercept <- round(summ_lm$coefficients[1,1],2)
  slope <- round(summ_lm$coefficients[2,1],2)
  
  plot(dfForScatter, xlab="MAP" , ylab="SMC [%vol]", main=plot_name)
  abline(lm_map_smc)
  legend("topleft", legend =  paste("y = x * ", slope, " + ", intercept, " , R2 = ", R2, sep=""))
}