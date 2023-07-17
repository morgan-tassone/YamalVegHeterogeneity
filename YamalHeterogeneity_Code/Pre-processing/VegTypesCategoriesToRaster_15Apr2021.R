########################################Create Geotiff from Vegetation Unit Categories###############################
setwd("~/UVA/MODIS Timeseries Project/Yamal Data")
library(raster)
library(terra)

#Vegetation Unit
VegType <- readRDS(file = "Veg Types/VegType_tbl_09March2021.RDS")

#0 = NA values
VegType[VegType == 0] <- NA
VegType_clean <- na.omit(VegType)

#Remove Pixel ID coordinate columns
VegType_clean <- subset(VegType_clean, select = -c(PixelID) )

#Remove values = 19, 19 = water
library(dplyr)
VegType_clean = filter(VegType_clean, VegType != 19)
unique(VegType_clean$VegType)

#Convert Veg Types into categories (based on Raynolds et al. 2006)
#Graminoid = 1
#Wetland = 2
#Erect shrubs = 3
#Prostrate shrubs = 4
VegType_clean$VegType <- ifelse(VegType_clean$VegType == 2, 1, VegType_clean$VegType)
VegType_clean$VegType <- ifelse(VegType_clean$VegType == 12, 2, VegType_clean$VegType)
VegType_clean$VegType <- ifelse(VegType_clean$VegType == 5, 1, VegType_clean$VegType)
VegType_clean$VegType <- ifelse(VegType_clean$VegType == 9, 3, VegType_clean$VegType)
VegType_clean$VegType <- ifelse(VegType_clean$VegType == 7, 1, VegType_clean$VegType)
VegType_clean$VegType <- ifelse(VegType_clean$VegType == 4, 4, VegType_clean$VegType)
VegType_clean$VegType <- ifelse(VegType_clean$VegType == 10, 3, VegType_clean$VegType)
VegType_clean$VegType <- ifelse(VegType_clean$VegType == 13, 2, VegType_clean$VegType)
VegType_clean$VegType <- ifelse(VegType_clean$VegType == 14, 2, VegType_clean$VegType)

####Create GeoTiff
library(sp)
library(raster)

###Create tables that only include the coordinates and only the soil texture values
VegCoords <- subset(VegType_clean, select = -c(VegType) )
VegResult <- subset(VegType_clean, select = -c(x,y) )

VegCoords <- as.data.frame(VegCoords)
VegResult <- as.data.frame(VegResult)

str(VegCoords)
str(VegResult)

#Create spatial points dataframe object to be used in the rasterize function
VegSpatPoints <- SpatialPointsDataFrame(VegCoords, VegResult, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
                                       match.ID = TRUE, bbox = NULL)

#get extent from existing raster and create a raster for the rasterize function
MaxNDVI_raster <- raster("MaxNDVI_Updated/MaxNDVI_9999.tif")
e <- extent(MaxNDVI_raster)
r <- raster(e, ncol=960, nrow=653, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
VegResult_raster <- rasterize(VegSpatPoints, r, VegSpatPoints$VegType, fun=mean)
plot(VegResult_raster)

#export the map
library(rgdal)
VegTypeCats_tif <- writeRaster(VegResult_raster, "VegTypeCats_27Apr2021", format= "GTiff")