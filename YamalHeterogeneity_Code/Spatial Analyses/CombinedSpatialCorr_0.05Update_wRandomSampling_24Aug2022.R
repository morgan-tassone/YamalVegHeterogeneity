###########################Spearman's Spatial Corr with Both Max and TI-NDVI###############################
setwd("~/UVA/MODIS Timeseries Project/Yamal Data")

####Get 2001-2018 average Max NDVI for each pixel
MaxNDVI_clean <- readRDS(file = "MaxNDVI_Updated/Updated_MaxNDVI_Clean_22March2021.RDS")

#Remove NA values - if NaN included in mean calculation, mean = NaN
MaxNDVI_clean <- na.omit(MaxNDVI_clean)

##Create table with each pixel included in the clean dataset
library(dplyr)

MaxNDVI_PixelIDs <- MaxNDVI_clean %>%
  group_by(PixelID) %>%
  distinct(x, y)

#Split by pixel ID
MaxNDVI_split <- split(MaxNDVI_clean$NDVI, MaxNDVI_clean$PixelID)

library(pbapply)
MaxNDVI.avg <- pblapply(MaxNDVI_split, mean)

#Convert the list into a dataframe
MaxNDVI_avg_table <- as.data.frame(MaxNDVI.avg)
MaxNDVI_avg_table <- t(MaxNDVI_avg_table)
MaxNDVI_avg_table <- as.data.frame(MaxNDVI_avg_table)

#Extract Pixel IDs and add to avg Max NDVI dataframe
MaxNDVI_PIs <- subset(MaxNDVI_PixelIDs, select = c(PixelID) )
MaxNDVI_avg_table <- cbind(MaxNDVI_PIs, MaxNDVI_avg_table)
colnames(MaxNDVI_avg_table)[2] <- "MaxNDVI"

#Make mean max NDVI tif
MaxNDVI_coords <- subset(MaxNDVI_PixelIDs, select = c(x, y) )
MaxNDVI_mean_coords <- cbind(MaxNDVI_coords, MaxNDVI_avg_table)
colnames(MaxNDVI_mean_coords)[3] <- "MaxNDVI"

library(sp)
library(raster)

###Create tables that only include the coordinates and only the mean
MaxNDVI_Mean4Map <- subset(MaxNDVI_mean_coords, select = -c(x,y) )

MaxNDVI_coords <- as.data.frame(MaxNDVI_coords)
MaxNDVI_Mean4Map <- as.data.frame(MaxNDVI_Mean4Map)

str(MaxNDVI_coords)
str(MaxNDVI_Mean4Map)

#Create spatial points dataframe object to be used in the rasterize function
MaxNDVISpatPoints <- SpatialPointsDataFrame(MaxNDVI_coords, MaxNDVI_Mean4Map, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
                                       match.ID = TRUE, bbox = NULL)

#get extent from existing raster and create a raster for the rasterize function
MaxNDVI_raster <- raster("MaxNDVI_Updated/MaxNDVI_9999.tif")
e <- extent(MaxNDVI_raster)
r <- raster(e, ncol=960, nrow=653, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
MeanNDVI_raster <- rasterize(MaxNDVISpatPoints, r, MaxNDVISpatPoints$MaxNDVI, fun=mean)
plot(MeanNDVI_raster)

library(rgdal)
#MeanMaxNDVI_tif <- writeRaster(MeanNDVI_raster, "MeanNDVI_07June2021", format= "GTiff")

####Get 2001-2018 average TI-NDVI for each pixel
TINDVI_clean <- readRDS(file = "TI-NDVI_0.05_Updated/TINDVI_Clean_27Feb2022.RDS")

#Remove NA values - if NaN included in mean calculation, mean = NaN
TINDVI_clean <- na.omit(TINDVI_clean)

##Create table with each pixel included in the clean dataset
library(dplyr)

TINDVI_PixelIDs <- TINDVI_clean %>%
  group_by(PixelID) %>%
  distinct(x, y)

#Split by pixel ID
TINDVI_split <- split(TINDVI_clean$TINDVI, TINDVI_clean$PixelID)

library(pbapply)
TINDVI.avg <- pblapply(TINDVI_split, mean)

#Convert the list into a dataframe
TINDVI_avg_table <- as.data.frame(TINDVI.avg)
TINDVI_avg_table <- t(TINDVI_avg_table)
TINDVI_avg_table <- as.data.frame(TINDVI_avg_table)

#Extract Pixel IDs and add to avg TI-NDVI dataframe
TINDVI_PIs <- subset(TINDVI_PixelIDs, select = c(PixelID) )
TINDVI_avg_table <- cbind(TINDVI_PIs, TINDVI_avg_table)
colnames(TINDVI_avg_table)[2] <- "TINDVI"

#Make mean TI-NDVI tif
TINDVI_coords <- subset(TINDVI_PixelIDs, select = c(x, y) )
TINDVI_mean_coords <- cbind(TINDVI_coords, TINDVI_avg_table)
TINDVI_mean_coords <- subset(TINDVI_mean_coords, select = -c(PixelID) )

library(sp)
library(raster)

###Create tables that only include the coordinates and only the mean
TINDVI_Mean4Map <- subset(TINDVI_mean_coords, select = -c(x,y) )

TINDVI_coords <- as.data.frame(TINDVI_coords)
TINDVI_Mean4Map <- as.data.frame(TINDVI_Mean4Map)

str(TINDVI_coords)
str(TINDVI_Mean4Map)

#Create spatial points dataframe object to be used in the rasterize function
TINDVISpatPoints <- SpatialPointsDataFrame(TINDVI_coords, TINDVI_Mean4Map, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
                                            match.ID = TRUE, bbox = NULL)

#get extent from existing raster and create a raster for the rasterize function
MaxNDVI_raster <- raster("MaxNDVI_Updated/MaxNDVI_9999.tif")
e <- extent(MaxNDVI_raster)
r <- raster(e, ncol=960, nrow=653, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
TINDVI_raster <- rasterize(TINDVISpatPoints, r, TINDVISpatPoints$TINDVI, fun=mean)
plot(TINDVI_raster)

library(rgdal)
MeanTINDVI_tif <- writeRaster(TINDVI_raster, "MeanTI-NDVI0.05_24Aug2022", format= "GTiff")

####Get 2001-2018 average mean growing season precip for each pixel
MGSP_clean <- readRDS(file = "MGSP_Updated/MGSP_Clean_08March2021.RDS")

#Remove NA values - if NaN included in mean calculation, mean = NaN
MGSP_clean <- na.omit(MGSP_clean)

##Create table with each pixel included in the clean dataset
MGSP_PixelIDs <- MGSP_clean %>%
  group_by(PixelID) %>%
  distinct(x, y)

#Split by pixel ID
MGSP_split <- split(MGSP_clean$MGSP, MGSP_clean$PixelID)

MGSP.avg <- pblapply(MGSP_split, mean)

#Convert the list into a dataframe
MGSP_avg_table <- as.data.frame(MGSP.avg)
MGSP_avg_table <- t(MGSP_avg_table)
MGSP_avg_table <- as.data.frame(MGSP_avg_table)

#Extract Pixel IDs and add to avg MGSP dataframe
MGSP_PIs <- subset(MGSP_PixelIDs, select = c(PixelID) )
MGSP_avg_table <- cbind(MGSP_PIs, MGSP_avg_table)
colnames(MGSP_avg_table)[2] <- "MGSP"

# #Make mean MGSP tif
# MGSP_coords <- subset(MGSP_PixelIDs, select = c(x, y) )
# MGSP_mean_coords <- cbind(MGSP_coords, MGSP_avg_table)
# colnames(MGSP_mean_coords)[3] <- "MGSP"
# 
# library(sp)
# library(raster)
# 
# ###Create tables that only include the coordinates and only the MK test result
# MGSP_Mean4Map <- subset(MGSP_mean_coords, select = -c(x,y) )
# 
# MGSP_coords <- as.data.frame(MGSP_coords)
# MGSP_Mean4Map <- as.data.frame(MGSP_Mean4Map)
# 
# str(MGSP_coords)
# str(MGSP_Mean4Map)
# 
# #Create spatial points dataframe object to be used in the rasterize function
# MGSPSpatPoints <- SpatialPointsDataFrame(MGSP_coords, MGSP_Mean4Map, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
#                                            match.ID = TRUE, bbox = NULL)
# 
# #get extent from existing raster and create a raster for the rasterize function
# MaxNDVI_raster <- raster("MaxNDVI_Updated/MaxNDVI_9999.tif")
# e <- extent(MaxNDVI_raster)
# r <- raster(e, ncol=960, nrow=653, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# MGSP_raster <- rasterize(MGSPSpatPoints, r, MGSPSpatPoints$MGSP, fun=mean)
# plot(MGSP_raster)
# 
# library(rgdal)
# MeanMGSP_tif <- writeRaster(MGSP_raster, "MeanMGSP_07June2021", format= "GTiff")

####Get 2001-2018 average snow-free period onset date for each pixel
SFPO_clean <- readRDS(file = "SFPO_Updated/SFPO_Clean_09March2021.RDS")

#Remove NA values - if NaN included in mean calculation, mean = NaN
SFPO_clean <- na.omit(SFPO_clean)

##Create table with each pixel included in the clean dataset
SFPO_PixelIDs <- SFPO_clean %>%
  group_by(PixelID) %>%
  distinct(x, y)

#Split by pixel ID
SFPO_split <- split(SFPO_clean$SFPO, SFPO_clean$PixelID)

SFPO.avg <- pblapply(SFPO_split, mean)

#Convert the list into a dataframe
SFPO_avg_table <- as.data.frame(SFPO.avg)
SFPO_avg_table <- t(SFPO_avg_table)
SFPO_avg_table <- as.data.frame(SFPO_avg_table)

#Extract Pixel IDs and add to avg SFPO dataframe
SFPO_PIs <- subset(SFPO_PixelIDs, select = c(PixelID) )
SFPO_avg_table <- cbind(SFPO_PIs, SFPO_avg_table)
colnames(SFPO_avg_table)[2] <- "SFPO"

# #Make mean SFPO tif
# SFPO_coords <- subset(SFPO_PixelIDs, select = c(x, y) )
# SFPO_mean_coords <- cbind(SFPO_coords, SFPO_avg_table)
# colnames(SFPO_mean_coords)[3] <- "SFPO"
# 
# library(sp)
# library(raster)
# 
# ###Create tables that only include the coordinates and only the MK test result
# SFPO_Mean4Map <- subset(SFPO_mean_coords, select = -c(x,y) )
# 
# SFPO_coords <- as.data.frame(SFPO_coords)
# SFPO_Mean4Map <- as.data.frame(SFPO_Mean4Map)
# 
# str(SFPO_coords)
# str(SFPO_Mean4Map)
# 
# #Create spatial points dataframe object to be used in the rasterize function
# SFPOSpatPoints <- SpatialPointsDataFrame(SFPO_coords, SFPO_Mean4Map, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
#                                          match.ID = TRUE, bbox = NULL)
# 
# #get extent from existing raster and create a raster for the rasterize function
# MaxNDVI_raster <- raster("MaxNDVI_Updated/MaxNDVI_9999.tif")
# e <- extent(MaxNDVI_raster)
# r <- raster(e, ncol=960, nrow=653, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# SFPO_raster <- rasterize(SFPOSpatPoints, r, SFPOSpatPoints$SFPO, fun=mean)
# plot(SFPO_raster)
# 
# library(rgdal)
# #MeanSFPO_tif <- writeRaster(SFPO_raster, "MeanSFPO_07June2021", format= "GTiff")

####Get 2001-2018 average soil moisture for each pixel
SM_clean <- readRDS(file = "SoilMoisture_Updated/SM_Clean_09March2021.RDS")

#Remove NA values - if NaN included in mean calculation, mean = NaN
SM_clean <- na.omit(SM_clean)

##Create table with each pixel included in the clean dataset
SM_PixelIDs <- SM_clean %>%
  group_by(PixelID) %>%
  distinct(x, y)

#Split by pixel ID
SM_split <- split(SM_clean$SM, SM_clean$PixelID)

SM.avg <- pblapply(SM_split, mean)

#Convert the list into a dataframe
SM_avg_table <- as.data.frame(SM.avg)
SM_avg_table <- t(SM_avg_table)
SM_avg_table <- as.data.frame(SM_avg_table)

#Extract Pixel IDs and add to avg SM dataframe
SM_PIs <- subset(SM_PixelIDs, select = c(PixelID) )
SM_avg_table <- cbind(SM_PIs, SM_avg_table)
colnames(SM_avg_table)[2] <- "SM"

# #Make mean SM tif
# SM_coords <- subset(SM_PixelIDs, select = c(x, y) )
# SM_mean_coords <- cbind(SM_coords, SM_avg_table)
# colnames(SM_mean_coords)[3] <- "SM"
# 
# library(sp)
# library(raster)
# 
# ###Create tables that only include the coordinates and only the MK test result
# SM_Mean4Map <- subset(SM_mean_coords, select = -c(x,y) )
# 
# SM_coords <- as.data.frame(SM_coords)
# SM_Mean4Map <- as.data.frame(SM_Mean4Map)
# 
# str(SM_coords)
# str(SM_Mean4Map)
# 
# #Create spatial points dataframe object to be used in the rasterize function
# SMSpatPoints <- SpatialPointsDataFrame(SM_coords, SM_Mean4Map, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
#                                          match.ID = TRUE, bbox = NULL)
# 
# #get extent from existing raster and create a raster for the rasterize function
# MaxNDVI_raster <- raster("MaxNDVI_Updated/MaxNDVI_9999.tif")
# e <- extent(MaxNDVI_raster)
# r <- raster(e, ncol=960, nrow=653, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# SM_raster <- rasterize(SMSpatPoints, r, SMSpatPoints$SM, fun=mean)
# plot(SM_raster)
# 
# library(rgdal)
# #MeanSM_tif <- writeRaster(SM_raster, "MeanSoilMoisture_07June2021", format= "GTiff")

####Get 2001-2018 average SWI for each pixel
SWI_clean <- readRDS(file = "SWI_Updated/SWI_Clean_08March2021.RDS")

#Remove NA values - if NaN included in mean calculation, mean = NaN
SWI_clean <- na.omit(SWI_clean)

##Create table with each pixel included in the clean dataset
SWI_PixelIDs <- SWI_clean %>%
  group_by(PixelID) %>%
  distinct(x, y)

#Split by pixel ID
SWI_split <- split(SWI_clean$SWI, SWI_clean$PixelID)

SWI.avg <- pblapply(SWI_split, mean)

#Convert the list into a dataframe
SWI_avg_table <- as.data.frame(SWI.avg)
SWI_avg_table <- t(SWI_avg_table)
SWI_avg_table <- as.data.frame(SWI_avg_table)

#Extract Pixel IDs and add to avg SWI dataframe
SWI_PIs <- subset(SWI_PixelIDs, select = c(PixelID) )
SWI_avg_table <- cbind(SWI_PIs, SWI_avg_table)
colnames(SWI_avg_table)[2] <- "SWI"

# #Make mean SWI tif
# SWI_coords <- subset(SWI_PixelIDs, select = c(x, y) )
# SWI_mean_coords <- cbind(SWI_coords, SWI_avg_table)
# colnames(SWI_mean_coords)[3] <- "SWI"
# 
# library(sp)
# library(raster)
# 
# ###Create tables that only include the coordinates and only the MK test result
# SWI_Mean4Map <- subset(SWI_mean_coords, select = -c(x,y) )
# 
# SWI_coords <- as.data.frame(SWI_coords)
# SWI_Mean4Map <- as.data.frame(SWI_Mean4Map)
# 
# str(SWI_coords)
# str(SWI_Mean4Map)
# 
# #Create spatial points dataframe object to be used in the rasterize function
# SWISpatPoints <- SpatialPointsDataFrame(SWI_coords, SWI_Mean4Map, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
#                                        match.ID = TRUE, bbox = NULL)
# 
# #get extent from existing raster and create a raster for the rasterize function
# MaxNDVI_raster <- raster("MaxNDVI_Updated/MaxNDVI_9999.tif")
# e <- extent(MaxNDVI_raster)
# r <- raster(e, ncol=960, nrow=653, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# SWI_raster <- rasterize(SWISpatPoints, r, SWISpatPoints$SWI, fun=mean)
# plot(SWI_raster)
# 
# library(rgdal)
# #MeanSWI_tif <- writeRaster(SWI_raster, "MeanSWI_07June2021", format= "GTiff")

####Load distance from the coast dataset
CoastDist <- readRDS(file = "DistToCoast_Updated/CoastDist_tbl_09March2021.RDS")

#Remove NA values
CoastDist <- na.omit(CoastDist)

#Remove x,y coordinates from table
CoastDist_tbl <- subset(CoastDist, select = -c(x,y) )

####Load elevation dataset
Elevation <- readRDS(file = "TanDEM/Elev_tbl_09March2021.RDS")

#For elevation, NAs = 0. Added 500 to pixel values to not exclude pixels with 0m elevation
#Remove 0s, then subtract 500 from every value
Elevation[Elevation == 0] <- NA
Elevation <- na.omit(Elevation)
Elevation$Elevation <- Elevation$Elevation - 500

#Remove x,y coordinates from table
Elevation_tbl <- subset(Elevation, select = -c(x,y) )

####Load human modification dataset
HumanMod <- readRDS(file = "HumanMod/HumanMod_tbl_09March2021.RDS")

#Remove NA values
HumanMod <- na.omit(HumanMod)

#Remove x,y coordinates from table
HumanMod_tbl <- subset(HumanMod, select = -c(x,y) )

#Make all of the final datasets dataframes and round SFPO to a whole number
MaxNDVI_avg_table <- as.data.frame(MaxNDVI_avg_table)
str(MaxNDVI_avg_table)

MGSP_avg_table <- as.data.frame(MGSP_avg_table)
str(MGSP_avg_table)

SM_avg_table <- as.data.frame(SM_avg_table)
str(SM_avg_table)

SFPO_avg_table <- as.data.frame(SFPO_avg_table)
SFPO_avg_table$SFPO <- round(SFPO_avg_table$SFPO, digits = 0)
str(SFPO_avg_table)

SWI_avg_table <- as.data.frame(SWI_avg_table)
str(SWI_avg_table)

CoastDist_tbl <- as.data.frame(CoastDist_tbl)
str(CoastDist_tbl)

Elevation_tbl <- as.data.frame(Elevation_tbl)
str(Elevation_tbl)

HumanMod_tbl <- as.data.frame(HumanMod_tbl)
str(HumanMod_tbl)

#Remove pixel IDs from dataframes NOT present in the mean Max NDVI dataframe, and vice versa.
CoastDist_final <- CoastDist_tbl[(CoastDist_tbl$PixelID %in% MaxNDVI_avg_table$PixelID),]
Elev_final <- Elevation_tbl[(Elevation_tbl$PixelID %in% MaxNDVI_avg_table$PixelID),]
HumanMod_final <- HumanMod_tbl[(HumanMod_tbl$PixelID %in% MaxNDVI_avg_table$PixelID),]
MGSP_final <- MGSP_avg_table[(MGSP_avg_table$PixelID %in% MaxNDVI_avg_table$PixelID),]
SFPO_final <- SFPO_avg_table[(SFPO_avg_table$PixelID %in% MaxNDVI_avg_table$PixelID),]
SM_final <- SM_avg_table[(SM_avg_table$PixelID %in% MaxNDVI_avg_table$PixelID),]
SWI_final <- SWI_avg_table[(SWI_avg_table$PixelID %in% MaxNDVI_avg_table$PixelID),]
TINDVI_final <- TINDVI_avg_table[(TINDVI_avg_table$PixelID %in% MaxNDVI_avg_table$PixelID),]

#TI-NDVI now has fewest pixels - repeat steps
CoastDist_final2 <- CoastDist_final[(CoastDist_final$PixelID %in% TINDVI_final$PixelID),]
Elev_final2 <- Elev_final[(Elev_final$PixelID %in% TINDVI_final$PixelID),]
HumanMod_final2 <- HumanMod_final[(HumanMod_final$PixelID %in% TINDVI_final$PixelID),]
MGSP_final2 <- MGSP_final[(MGSP_final$PixelID %in% TINDVI_final$PixelID),]
MaxNDVI_final2 <- MaxNDVI_avg_table[(MaxNDVI_avg_table$PixelID %in% TINDVI_final$PixelID),]
SFPO_final2 <- SFPO_final[(SFPO_final$PixelID %in% TINDVI_final$PixelID),]
SM_final2 <- SM_final[(SM_final$PixelID %in% TINDVI_final$PixelID),]
SWI_final2 <- SWI_final[(SWI_final$PixelID %in% TINDVI_final$PixelID),]

#Elevation now has the fewest pixels - repeat steps
CoastDist_final3 <- CoastDist_final2[(CoastDist_final2$PixelID %in% Elev_final2$PixelID),]
HumanMod_final3 <- HumanMod_final2[(HumanMod_final2$PixelID %in% Elev_final2$PixelID),]
TINDVI_final3 <- TINDVI_final[(TINDVI_final$PixelID %in% Elev_final2$PixelID),]
MaxNDVI_final3 <- MaxNDVI_final2[(MaxNDVI_final2$PixelID %in% Elev_final2$PixelID),]
MGSP_final3 <- MGSP_final2[(MGSP_final2$PixelID %in% Elev_final2$PixelID),]
SFPO_final3 <- SFPO_final2[(SFPO_final2$PixelID %in% Elev_final2$PixelID),]
SM_final3 <- SM_final2[(SM_final2$PixelID %in% Elev_final2$PixelID),]
SWI_final3 <- SWI_final2[(SWI_final2$PixelID %in% Elev_final2$PixelID),]

#Human Mod now has the fewest pixels - repeat steps
CoastDist_final4 <- CoastDist_final3[(CoastDist_final3$PixelID %in% HumanMod_final3$PixelID),]
Elev_final4 <- Elev_final2[(Elev_final2$PixelID %in% HumanMod_final3$PixelID),]
MGSP_final4 <- MGSP_final3[(MGSP_final3$PixelID %in% HumanMod_final3$PixelID),]
MaxNDVI_final4 <- MaxNDVI_final3[(MaxNDVI_final3$PixelID %in% HumanMod_final3$PixelID),]
TINDVI_final4 <- TINDVI_final3[(TINDVI_final3$PixelID %in% HumanMod_final3$PixelID),]
SFPO_final4 <- SFPO_final3[(SFPO_final3$PixelID %in% HumanMod_final3$PixelID),]
SM_final4 <- SM_final3[(SM_final3$PixelID %in% HumanMod_final3$PixelID),]
SWI_final4 <- SWI_final3[(SWI_final3$PixelID %in% HumanMod_final3$PixelID),]

#MGSP now has the fewest pixels - repeat steps
CoastDist_final5 <- CoastDist_final4[(CoastDist_final4$PixelID %in% MGSP_final4$PixelID),]
Elev_final5 <- Elev_final4[(Elev_final4$PixelID %in% MGSP_final4$PixelID),]
HumanMod_final5 <- HumanMod_final3[(HumanMod_final3$PixelID %in% MGSP_final4$PixelID),]
MaxNDVI_final5 <- MaxNDVI_final4[(MaxNDVI_final4$PixelID %in% MGSP_final4$PixelID),]
TINDVI_final5 <- TINDVI_final4[(TINDVI_final4$PixelID %in% MGSP_final4$PixelID),]
SFPO_final5 <- SFPO_final4[(SFPO_final4$PixelID %in% MGSP_final4$PixelID),]
SM_final5 <- SM_final4[(SM_final4$PixelID %in% MGSP_final4$PixelID),]
SWI_final5 <- SWI_final4[(SWI_final4$PixelID %in% MGSP_final4$PixelID),]

#Compile the datasets
CoastDist_final_ValsOnly <- subset(CoastDist_final5, select = c(CoastDist) )
Elev_final_ValsOnly <- subset(Elev_final5, select = c(Elevation) )
HumanMod_final_ValsOnly <- subset(HumanMod_final5, select = c(HumanMod) )
MaxNDVI_final_ValsOnly <- subset(MaxNDVI_final5, select = c(MaxNDVI) )
MGSP_final_ValsOnly <- subset(MGSP_final4, select = c(MGSP) )
TINDVI_final_ValsOnly <- subset(TINDVI_final5, select = c(TINDVI) )
SFPO_final_ValsOnly <- subset(SFPO_final5, select = c(SFPO) )
SM_final_ValsOnly <- subset(SM_final5, select = c(SM) )
SWI_final_ValsOnly <- subset(SWI_final5, select = c(SWI) )

CompiledData_SC <- cbind(CoastDist_final_ValsOnly, Elev_final_ValsOnly, HumanMod_final_ValsOnly, MaxNDVI_final_ValsOnly, 
                         MGSP_final_ValsOnly,TINDVI_final_ValsOnly, SFPO_final_ValsOnly, SM_final_ValsOnly, SWI_final_ValsOnly)

names(CompiledData_SC ) <- c("CoastDist", "Elevation", "HumanMod","MaxNDVI", "Precip", "TI-NDVI", 
                             "SnowFree", "SoilMoisture", "SWI")

library(psych)
CorrResult1 <- corr.test(CompiledData_SC, y = NULL, use = "pairwise", method = "spearman", adjust = "bonferroni",
                         alpha = .05, ci = FALSE)

#CompiledData_sc_df <- as.data.frame(CompiledData_SC)
CorrResult2 <- cor(CompiledData_SC, method="spearman")

##############
library(Hmisc)
library(corrplot)
CompiledData_SC_Matrix <- as.matrix(CompiledData_SC)
str(CompiledData_SC_Matrix)
CorrResult3 <- rcorr(CompiledData_SC_Matrix, type = "spearman")

library(ggcorrplot) #make sure ggcorrplot2 is not loaded

p.mat <- CorrResult3$P
p.mat2 <- CorrResult1$p

ggcorrplot(CorrResult2, ggtheme = theme_minimal,lab = TRUE, hc.order = TRUE, legend.title = "r",
           type = "lower", outline.color = "black", pch.col = "black",
           lab_col = "black", lab_size = 4, digits = 2) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        legend.title.align = 0.25,
        panel.grid = element_blank())



# corrplot(M, type = 'lower', method = 'number')

##Run again on randomly sampled data and compare
set.seed(107)
library(caret)
inTrain <- createDataPartition(y = CompiledData_SC$MaxNDVI, p = .50, list = FALSE)
Training <- CompiledData_SC[ inTrain,]
Testing <- CompiledData_SC[-inTrain,]

CorrResult1_training <- corr.test(Training, y = NULL, use = "pairwise", method = "spearman", adjust = "bonferroni",
                         alpha = .05, ci = FALSE)

p.mat_training <- CorrResult1_training$p

library(ggcorrplot) #make sure ggcorrplot2 is not loaded

CorrResult2_training <- cor(Training, method="spearman")


ggcorrplot(CorrResult2_training, ggtheme = theme_minimal, lab = TRUE, hc.order = TRUE, legend.title = "r",
           type = "lower", outline.color = "black", pch.col = "black",
           lab_col = "black", lab_size = 4, digits = 2) +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        legend.title.align = 0.25,
        panel.grid = element_blank())
