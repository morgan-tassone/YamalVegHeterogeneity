############################Max and TI-NDVI Phenology Curve Data Prep###################################
#Load Max NDVI and TI-NDVI clean datasets
setwd("~/UVA/MODIS Timeseries Project/Yamal Data")

MaxNDVI_Slopes <- readRDS(file = "MaxNDVI_Updated/UpdatedMaxNDVI_Slope_22March2021.RDS")
MaxNDVI_PIs_Coords <- readRDS(file = "MaxNDVI_Updated/Updated_MaxNDVI_Clean_Coords_22March2021.RDS")
MaxNDVI_SS_PI <- cbind(MaxNDVI_PIs_Coords, MaxNDVI_Slopes)
colnames(MaxNDVI_SS_PI)[4] <- "MaxNDVI"

TINDVI_Slopes <- readRDS(file = "TI-NDVI_0.05_Updated/TINDVI0.05_Slope_27Feb2022.RDS")
TINDVI_PIs_Coords <- readRDS(file = "TI-NDVI_0.05_Updated/TINDVI_Clean_Coords_27Feb2022.RDS")
TINDVI_SS_PI <- cbind(TINDVI_PIs_Coords, TINDVI_Slopes)
colnames(TINDVI_SS_PI)[4] <- "TINDVI"

#Remove pixel IDs not included in both datasets
MaxNDVI_SS_PI2 <- MaxNDVI_SS_PI[(MaxNDVI_SS_PI$PixelID %in% TINDVI_SS_PI$PixelID),]
TINDVI_SS_PI2 <- TINDVI_SS_PI[(TINDVI_SS_PI$PixelID %in% MaxNDVI_SS_PI2$PixelID),]

#Delete unnecessary/duplicate rows from TI-NDVI dataframe
TINDVI_SS_PI2 <- subset(TINDVI_SS_PI2, select = -c(PixelID,x,y) )

#Combine into 1 dataframe
MaxNDVI_TINDVI_slopes <- cbind(MaxNDVI_SS_PI2, TINDVI_SS_PI2)

#Only keep rows where Max NDVI slope < 0 and TI-NDVI slope > 0
MaxNeg_TIPos <- MaxNDVI_TINDVI_slopes[(MaxNDVI_TINDVI_slopes$MaxNDVI < 0 & MaxNDVI_TINDVI_slopes$TINDVI > 0),]
#Only keep rows where Max NDVI slope > 0 and TI-NDVI slope > 0
MaxPos_TIPos <- MaxNDVI_TINDVI_slopes[(MaxNDVI_TINDVI_slopes$MaxNDVI > 0 & MaxNDVI_TINDVI_slopes$TINDVI > 0),]
#Only keep rows where Max NDVI slope > 0 and TI-NDVI slope < 0
MaxPos_TINeg <- MaxNDVI_TINDVI_slopes[(MaxNDVI_TINDVI_slopes$MaxNDVI > 0 & MaxNDVI_TINDVI_slopes$TINDVI < 0),]
#Only keep rows where Max NDVI slope < 0 and TI-NDVI slope < 0
MaxNeg_TINeg <- MaxNDVI_TINDVI_slopes[(MaxNDVI_TINDVI_slopes$MaxNDVI < 0 & MaxNDVI_TINDVI_slopes$TINDVI < 0),]

#Create raster for pixels where Max NDVI slope < 0 and TI-NDVI slope > 0
#Make a tif
library(sp)
library(raster)

coords <- subset(MaxNeg_TIPos, select = -c(PixelID, MaxNDVI, TINDVI) )
result <- subset(MaxNeg_TIPos, select = -c(PixelID, x,y, MaxNDVI) )

coords <- as.data.frame(coords)
result <- as.data.frame(result)

str(coords)
str(result) 

#Create spatial points dataframe object to be used in the rasterize function
SpatPoints <- SpatialPointsDataFrame(coords, result, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
                                     match.ID = TRUE, bbox = NULL)

#get extent from existing raster and create a raster for the rasterize function
MaxNDVI_raster <- raster("MaxNDVI_Updated/MaxNDVI_9999.tif")
e <- extent(MaxNDVI_raster)
r <- raster(e, ncol=960, nrow=653, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
MaxNeg_TIPos_raster <- rasterize(SpatPoints, r, SpatPoints$TINDVI, fun=mean)
plot(MaxNeg_TIPos_raster)

#export the map
library(rgdal)
MaxNeg_TIPos_tif <- writeRaster(MaxNeg_TIPos_raster, "MaxNeg_TIPos_22Sept2022", format= "GTiff")

#Create raster for pixels where Max NDVI slope > 0 and TI-NDVI slope > 0
coords2 <- subset(MaxPos_TIPos, select = -c(PixelID, MaxNDVI, TINDVI) )
result2 <- subset(MaxPos_TIPos, select = -c(PixelID, x,y, MaxNDVI) )

coords2 <- as.data.frame(coords2)
result2 <- as.data.frame(result2)

str(coords2)
str(result2) 

#Create spatial points dataframe object to be used in the rasterize function
SpatPoints2 <- SpatialPointsDataFrame(coords2, result2, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
                                     match.ID = TRUE, bbox = NULL)

MaxPos_TIPos_raster <- rasterize(SpatPoints2, r, SpatPoints2$TINDVI, fun=mean)
plot(MaxPos_TIPos_raster)

#export the map
MaxPos_TIPos_tif <- writeRaster(MaxPos_TIPos_raster, "MaxPos_TIPos_22Sept2022", format= "GTiff")

#Create raster for pixels where Max NDVI slope > 0 and TI-NDVI slope < 0
coords3 <- subset(MaxPos_TINeg, select = -c(PixelID, MaxNDVI, TINDVI) )
result3 <- subset(MaxPos_TINeg, select = -c(PixelID, x,y, MaxNDVI) )

coords3 <- as.data.frame(coords3)
result3 <- as.data.frame(result3)

str(coords3)
str(result3) 

#Create spatial points dataframe object to be used in the rasterize function
SpatPoints3 <- SpatialPointsDataFrame(coords3, result3, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
                                      match.ID = TRUE, bbox = NULL)

MaxPos_TINeg_raster <- rasterize(SpatPoints3, r, SpatPoints3$TINDVI, fun=mean)
plot(MaxPos_TINeg_raster)

#export the map
MaxPos_TINeg_tif <- writeRaster(MaxPos_TINeg_raster, "MaxPos_TINeg_22Sept2022", format= "GTiff")

#Create raster for pixels where Max NDVI slope < 0 and TI-NDVI slope < 0
coords4 <- subset(MaxNeg_TINeg, select = -c(PixelID, MaxNDVI, TINDVI) )
result4 <- subset(MaxNeg_TINeg, select = -c(PixelID, x,y, MaxNDVI) )

coords4 <- as.data.frame(coords4)
result4 <- as.data.frame(result4)

str(coords4)
str(result4) 

#Create spatial points dataframe object to be used in the rasterize function
SpatPoints4 <- SpatialPointsDataFrame(coords4, result4, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
                                      match.ID = TRUE, bbox = NULL)

MaxNeg_TINeg_raster <- rasterize(SpatPoints4, r, SpatPoints4$TINDVI, fun=mean)
plot(MaxNeg_TINeg_raster)

#export the map
MaxNeg_TINeg_tif <- writeRaster(MaxNeg_TINeg_raster, "MaxNeg_TINeg_22Sept2022", format= "GTiff")
