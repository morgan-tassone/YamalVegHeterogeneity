##########################Max NDVI/Soil Moisture Anomaly Analysis#########################
setwd("~/UVA/MODIS Timeseries Project/Yamal Data")

#Load in cleaned Max NDVI dataset (only pixels with 15+ years of data)
MaxNDVI_clean <- readRDS(file = "MaxNDVI_Updated/Updated_MaxNDVI_Clean_22March2021.RDS")

#Remove NAs and use to add pixel IDs/coords to detrended dataset
MaxNDVI_clean <- na.omit(MaxNDVI_clean)

#Load in Max NDVI timeseries list
library(rlist)
MaxNDVI.TS <- list.load("MaxNDVI_Updated/Updated_NDVI_TS_list_22Mar2021.RData")

#Linear detrending
library(pracma)

LinDetrend <- function(x){
  detrend(x, tt='linear')
}

library(pbapply)
MaxNDVI.DT <- pblapply(MaxNDVI.TS, LinDetrend)

MaxNDVI_DT_df <- do.call(rbind, MaxNDVI.DT)
MaxNDVI_DT_df <- as.data.frame(MaxNDVI_DT_df)

#Combine detrended timeseries with year and pixel ID
MaxNDVI_DT_Combo <- cbind(MaxNDVI_clean, MaxNDVI_DT_df)

#Load Soil Moisture data (only pixels with 15+ years of data) and create timeseries list
SoilMoisture_clean <- readRDS(file = "SoilMoisture_Updated/SM_Clean_09March2021.RDS")

#Remove NAs and use to add pixel IDs/coords to detrended dataset
SoilMoisture_clean <- na.omit(SoilMoisture_clean)

SM.TS <- list.load("SoilMoisture_Updated/Updated_SM_TS_list.RData")

#Linear detrending
SM.DT <- pblapply(SM.TS, LinDetrend)

SM_DT_df <- do.call(rbind, SM.DT)
SM_DT_df <- as.data.frame(SM_DT_df)

#Combine detrended timeseries with year and pixel ID
SM_DT_Combo <- cbind(SoilMoisture_clean, SM_DT_df)

#Remove pixel IDs not included in both datasets
SM_DT_PIs <- SM_DT_Combo[(SM_DT_Combo$PixelID %in% MaxNDVI_DT_Combo$PixelID),]
MaxNDVI_DT_PIs <- MaxNDVI_DT_Combo[(MaxNDVI_DT_Combo$PixelID %in% SM_DT_PIs$PixelID),]

#Create tables of the PixelIDs and coordinates for later use
library(dplyr)

MaxNDVIanomaly_coords <- MaxNDVI_DT_PIs %>%
  group_by(x, y) %>%
  distinct(PixelID)

#Delete unneccessary columns
MaxNDVI_DT_PIs <- subset(MaxNDVI_DT_PIs, select = -c(x,y,NDVI) )
SM_DT_PIs <- subset(SM_DT_PIs, select = -c(x,y,SM) )

#Make each timeseries the same length
#Make a list split by pixel IDs (timeseries for each pixel)
MaxNDVI_DT_byPixel <-split(MaxNDVI_DT_PIs, MaxNDVI_DT_PIs$PixelID)

#Make a list split by pixel IDs (timeseries for each pixel)
SM_DT_byPixel <-split(SM_DT_PIs, SM_DT_PIs$PixelID)

#Make each element (i.e., pixel) in the lists have the same number of observations
MaxNDVI <- MaxNDVI_DT_byPixel
SM <- SM_DT_byPixel

start_clean <- Sys.time()
for (i in 1:length(SM)) {
  if(all(is.element(MaxNDVI[[i]]$Year, SM[[i]]$Year)) & all(is.element(SM[[i]]$Year, MaxNDVI[[i]]$Year)) == T) {
    next
  } else {
    years_missing_from_SM <- MaxNDVI[[i]]$Year[which(MaxNDVI[[i]]$Year %in% SM[[i]]$Year == F)]
    years_missing_from_MaxNDVI <- SM[[i]]$Year[which(SM[[i]]$Year %in% MaxNDVI[[i]]$Year == F)]
    years_to_cast_out <- c(years_missing_from_SM, years_missing_from_MaxNDVI)
    MaxNDVI[[i]] <- MaxNDVI[[i]][!(MaxNDVI[[i]]$Year %in% years_to_cast_out), ]
    SM[[i]] <- SM[[i]][!(SM[[i]]$Year %in% years_to_cast_out), ]
  }
}
fin_clean <- Sys.time()
cat('Loop took', round(fin_clean - start_clean, digits = 2), 'seconds to run')

#Check to make sure the loop worked
years_identical <- vector(length = length(SM))
for (i in 1:length(SM)) {
  years_identical[i] <- identical(sort(MaxNDVI[[i]]$Year), sort(SM[[i]]$Year))
}
which(years_identical  == FALSE) # indicates which matched pairs of elements from the lists have different numbers of observations
length(which(years_identical  == FALSE)) # indicates how many matched pairs of elements from the lists have different numbers of observations

#Run the correlation between the timeseries of each pixel
library(psych)

correlations <- vector(mode = 'list', length = length(SM))
start_cor <- Sys.time()
for (i in 1:length(SM)) {
  correlations[[i]] <- corr.test(MaxNDVI[[i]]$V1, y = SM[[i]]$V1,
                                 use = "pairwise",
                                 method = "spearman",
                                 alpha = .05,
                                 ci = FALSE)
  pixel_same <- identical(MaxNDVI[[i]]$PixelID, SM[[i]]$PixelID)
  if (pixel_same == T) {
    correlations[[i]][length(correlations[[i]]) + 1] <- unique(MaxNDVI[[i]]$PixelID)
    names(correlations[[i]])[12] <- "PixelID"
  } else {
    stop()
  }
}
fin_cor <- Sys.time()
cat('Loop took', round(fin_cor - start_cor, digits = 2), 'minutes to run')

hist(unlist(lapply(correlations, function(x) x[1])), main = "Correlations", ylab = 'Pixel Count', 
     xlab = "Spearman's Correlation Coefficient (r)")

CorrCoeffs <- unlist(lapply(correlations, function(x) x[1]))
CorrCoeffs <- as.data.frame(CorrCoeffs)

PixelIDs <- unlist(lapply(correlations, function(x) x[12]))
PixelIDs <- as.data.frame(PixelIDs)

Pvals <- unlist(lapply(correlations, function(x) x[4]))
Pvals <- as.data.frame(Pvals)

FinalCorrResult <- cbind(PixelIDs, Pvals, CorrCoeffs)

NegCorr <- FinalCorrResult[(FinalCorrResult$CorrCoeffs < 0),]
NegCorrSig <- NegCorr[(NegCorr$Pvals <= 0.05),]
PosCorr <- FinalCorrResult[(FinalCorrResult$CorrCoeffs > 0),]
PosCorrSig <- PosCorr[(PosCorr$Pvals <= 0.05),]

#Determine overall avg corr coeff
AvgCorrCoeff <- mean(FinalCorrResult$CorrCoeffs)

#To create a tif, need to add XY coordinates to the table, and remove pixel IDs
CorrResult_forTiff <- cbind(MaxNDVIanomaly_coords, FinalCorrResult)
CorrResult_forTiff <- subset(CorrResult_forTiff, select = -c(PixelID, PixelIDs) )

#Round r to 3 decimal places 
CorrResult_forTiff$CorrCoeffs <- round(CorrResult_forTiff$CorrCoeffs, digits = 3)

#Make map
library(sp)
library(raster)

###Create tables that only include the coordinates and only the MK test result, need to remame p-value to pvalue
coords <- subset(CorrResult_forTiff, select = -c(CorrCoeffs) )
result <- subset(CorrResult_forTiff, select = -c(x,y) )

coords <- as.data.frame(coords)
result <- as.data.frame(result)

str(coords)
str(result) 

#Create spatial points dataframe object to be used in the rasterize function
CorrSpatPoints <- SpatialPointsDataFrame(coords, result, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
                                         match.ID = TRUE, bbox = NULL)

#get extent from existing raster and create a raster for the rasterize function
MaxNDVI_raster <- raster("MaxNDVI_Updated/MaxNDVI_9999.tif")
e <- extent(MaxNDVI_raster)
r <- raster(e, ncol=960, nrow=653, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
CorrResult_raster <- rasterize(CorrSpatPoints, r, CorrSpatPoints$CorrCoeffs, fun=mean)
plot(CorrResult_raster)

#export the map
library(rgdal)
MaxNDVI_SM_AnomalyCorr_tif <- writeRaster(CorrResult_raster, "MaxNDVI_SM_AnomalyCorr_26Apr2021", format= "GTiff")
