################################Create Distance from the Coast tif from ArcMap Output###################################
setwd("~/UVA/MODIS Timeseries Project/Yamal Data")

library(readxl)

DistToCoast <- read_excel("DistToCoast_Updated/DistanceWithCoords.xlsx", 
                         na = "-9999")

library(sp)
library(raster)

###Create tables that only include the coordinates and only the MK test result
coords <- subset(DistToCoast, select = -c(Distance) )
result <- subset(DistToCoast, select = -c(X,Y) )

coords <- as.data.frame(coords)
result <- as.data.frame(result)

str(coords)
str(result) #need to make this numeric

#Create spatial points dataframe object to be used in the rasterize function
SpatPoints <- SpatialPointsDataFrame(coords, result, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
                                       match.ID = TRUE, bbox = NULL)

#get extent from existing raster and create a raster for the rasterize function
MaxNDVI_raster <- raster("MaxNDVI_Updated/MaxNDVI_9999.tif")
e <- extent(MaxNDVI_raster)
r <- raster(e, ncol=960, nrow=653, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
CoastDist_raster <- rasterize(SpatPoints, r, SpatPoints$Distance, fun=mean)
plot(CoastDist_raster)

library(rgdal)
CoastDist_forGEE_tif <- writeRaster(CoastDist_raster, "CoastDist_forGEE_09Mar2021", format= "GTiff")