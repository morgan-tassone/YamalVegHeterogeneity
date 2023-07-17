########################################Create Geotiff from Substrate Chemistry Categories###############################
setwd("~/UVA/MODIS Timeseries Project/Yamal Data")
library(raster)
library(terra)

####Substrate Chemistry
SubChem <- readRDS(file = "Substrate pH/SubChem_tbl_09March2021.RDS")

#0 = NA values
SubChem[SubChem == 0] <- NA
SubChem_clean <- na.omit(SubChem)

#Remove Pixel ID column
SubChem_clean <- subset(SubChem_clean, select = -c(PixelID) )

####Create GeoTiff
library(sp)
library(raster)

###Create tables that only include the coordinates and only the soil texture values
SubChemCoords <- subset(SubChem_clean, select = -c(SubChem) )
SubChemResult <- subset(SubChem_clean, select = -c(x,y) )

SubChemCoords <- as.data.frame(SubChemCoords)
SubChemResult <- as.data.frame(SubChemResult)

str(SubChemCoords)
str(SubChemResult)

#Create spatial points dataframe object to be used in the rasterize function
SCSpatPoints <- SpatialPointsDataFrame(SubChemCoords, SubChemResult, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
                                       match.ID = TRUE, bbox = NULL)

#get extent from existing raster and create a raster for the rasterize function
MaxNDVI_raster <- raster("MaxNDVI_Updated/MaxNDVI_9999.tif")
e <- extent(MaxNDVI_raster)
r <- raster(e, ncol=960, nrow=653, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
SCresult_raster <- rasterize(SCSpatPoints, r, SCSpatPoints$SubChem, fun=mean)
plot(SCresult_raster)

#export the map
library(rgdal)
SubChemCats_tif <- writeRaster(SCresult_raster, "SubChemCats_27Apr2021", format= "GTiff")