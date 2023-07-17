########################################Create Geotiff from Soil Texture Categories###############################
setwd("~/UVA/MODIS Timeseries Project/Yamal Data")
library(raster)
library(terra)

####Soil Texture
SoilText <- stack("SoilTexture/SoilText_final_04Nov2020.tif")
plot(SoilText)
SoilText_tbl <- as.data.frame(SoilText, xy = TRUE, cells = TRUE, na.rm = FALSE)
SoilText_tbl <- as.data.frame(SoilText)
str(SoilText_tbl)

colnames(SoilText_tbl)[1] <- "SoilTexture"

#Load Max NDVI dataset to get coordinates
####Max NDVI
#Create data tables for each year. Each year should have same # of obs and coordinates
MaxNDVI <- stack("MaxNDVI_Updated/UpdatedMaxNDVI_928m_22Mar2021.tif")

MaxNDVI2001 <- MaxNDVI[[1]]
#plot(MaxNDVI2001)
MaxNDVI2001_tbl <- as.data.frame(MaxNDVI2001, xy = TRUE, cells = TRUE, na.rm = FALSE)
MaxNDVI2001_tbl <- as.data.frame(MaxNDVI2001_tbl)
str(MaxNDVI2001_tbl)

coords <- subset(MaxNDVI2001_tbl, select = -c(NDVI) )

SoilTexture <- cbind(coords, SoilText_tbl)

#Convert the soil texture IDs into values associated with the soil texture types
#Sand = 1
#Clay loam = 2
#Loam = 3
#Sandy loam = 4
#Silt loam = 5

#Convert the soil texture IDs into soil texture types
SoilTexture$SoilTexture <- ifelse(SoilTexture$SoilTexture == 7021, 1, SoilTexture$SoilTexture)
SoilTexture$SoilTexture <- ifelse(SoilTexture$SoilTexture == 8505 | SoilTexture$SoilTexture == 8510, 2, SoilTexture$SoilTexture)
SoilTexture$SoilTexture <- ifelse(SoilTexture$SoilTexture == 7944 | SoilTexture$SoilTexture == 7952 | SoilTexture$SoilTexture == 7952
                                  | SoilTexture$SoilTexture == 8000 | SoilTexture$SoilTexture == 8099 | SoilTexture$SoilTexture == 8100 
                                  | SoilTexture$SoilTexture == 8112 | SoilTexture$SoilTexture == 8118 | SoilTexture$SoilTexture == 8120
                                  | SoilTexture$SoilTexture == 8123 | SoilTexture$SoilTexture == 8125 | SoilTexture$SoilTexture == 8126
                                  | SoilTexture$SoilTexture == 8129 | SoilTexture$SoilTexture == 8133 | SoilTexture$SoilTexture == 8134
                                  | SoilTexture$SoilTexture == 8140 | SoilTexture$SoilTexture == 8173 | SoilTexture$SoilTexture == 8174
                                  | SoilTexture$SoilTexture == 8178 | SoilTexture$SoilTexture == 8179 | SoilTexture$SoilTexture == 8182
                                  | SoilTexture$SoilTexture == 8195 | SoilTexture$SoilTexture == 8198 | SoilTexture$SoilTexture == 8239
                                  | SoilTexture$SoilTexture == 8348 | SoilTexture$SoilTexture == 8351 | SoilTexture$SoilTexture == 9073, 
                                  3, SoilTexture$SoilTexture)
SoilTexture$SoilTexture <- ifelse(SoilTexture$SoilTexture == 7061 | SoilTexture$SoilTexture == 8495 | SoilTexture$SoilTexture == 9008, 
                                  4, SoilTexture$SoilTexture)
SoilTexture$SoilTexture <- ifelse(SoilTexture$SoilTexture == 7073 | SoilTexture$SoilTexture == 8965 | SoilTexture$SoilTexture == 9027
                                  | SoilTexture$SoilTexture == 9030 | SoilTexture$SoilTexture == 9046 | SoilTexture$SoilTexture == 9059 
                                  | SoilTexture$SoilTexture == 9064 | SoilTexture$SoilTexture == 9069 | SoilTexture$SoilTexture == 9074
                                  | SoilTexture$SoilTexture == 9097 | SoilTexture$SoilTexture == 9108 | SoilTexture$SoilTexture == 9114
                                  | SoilTexture$SoilTexture == 9118 | SoilTexture$SoilTexture == 9119 | SoilTexture$SoilTexture == 9134
                                  | SoilTexture$SoilTexture == 9148 | SoilTexture$SoilTexture == 9170 | SoilTexture$SoilTexture == 9177
                                  | SoilTexture$SoilTexture == 9181 | SoilTexture$SoilTexture == 9182 | SoilTexture$SoilTexture == 9195
                                  | SoilTexture$SoilTexture == 9202, 
                                  5, SoilTexture$SoilTexture)

#Remove NA values
SoilTexture_Clean <- na.omit(SoilTexture)

####Create GeoTiff
library(sp)
library(raster)

###Create tables that only include the coordinates and only the soil texture values
STcoords <- subset(SoilTexture_Clean, select = -c(SoilTexture) )
STresult <- subset(SoilTexture_Clean, select = -c(x,y) )

STcoords <- as.data.frame(STcoords)
STresult <- as.data.frame(STresult)

str(STcoords)
str(STresult)

#Create spatial points dataframe object to be used in the rasterize function
STSpatPoints <- SpatialPointsDataFrame(STcoords, STresult, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
                                       match.ID = TRUE, bbox = NULL)

#get extent from existing raster and create a raster for the rasterize function
MaxNDVI_raster <- raster("MaxNDVI_Updated/MaxNDVI_9999.tif")
e <- extent(MaxNDVI_raster)
r <- raster(e, ncol=960, nrow=653, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
STresult_raster <- rasterize(STSpatPoints, r, STSpatPoints$SoilTexture, fun=mean)
plot(STresult_raster)

#export the map
library(rgdal)
SoilTextCats_tif <- writeRaster(STresult_raster, "SoilTextCats_15Apr2021", format= "GTiff")