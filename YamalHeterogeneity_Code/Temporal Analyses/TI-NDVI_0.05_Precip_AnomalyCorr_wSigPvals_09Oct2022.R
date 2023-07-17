##########################TI-NDVI/Precipitation Anomaly Analysis#########################
setwd("~/UVA/MODIS Timeseries Project/Yamal Data")

#Load in cleaned TI-NDVI dataset (only pixels with 15+ years of data)
TINDVI_clean <- readRDS(file = "TI-NDVI_0.05_Updated/TINDVI_Clean_27Feb2022.RDS")

#Remove NAs and use to add pixel IDs/coords to detrended dataset
TINDVI_clean <- na.omit(TINDVI_clean)

#Load in TI-NDVI timeseries list
library(rlist)
TINDVI.TS <- list.load("TI-NDVI_0.05_Updated/Updated0.05_TI-NDVI_TS_list.RData")

#Linear detrending
library(pracma)

LinDetrend <- function(x){
  detrend(x, tt='linear')
}

library(pbapply)
TINDVI.DT <- pblapply(TINDVI.TS, LinDetrend)

TINDVI_DT_df <- do.call(rbind, TINDVI.DT)
TINDVI_DT_df <- as.data.frame(TINDVI_DT_df)

#Combine detrended timeseries with year and pixel ID
TINDVI_DT_Combo <- cbind(TINDVI_clean, TINDVI_DT_df)

#Load Precipitation data (only pixels with 15+ years of data) and create timeseries list
MGSP_clean <- readRDS(file = "MGSP_Updated/MGSP_Clean_08March2021.RDS")

#Remove NAs and use to add pixel IDs/coords to detrended dataset
MGSP_clean <- na.omit(MGSP_clean)

MGSP.TS <- list.load("MGSP_Updated/Updated_MGSP_TS_list.RData")

#Linear detrending
MGSP.DT <- pblapply(MGSP.TS, LinDetrend)

MGSP_DT_df <- do.call(rbind, MGSP.DT)
MGSP_DT_df <- as.data.frame(MGSP_DT_df)

#Combine detrended timeseries with year and pixel ID
MGSP_DT_Combo <- cbind(MGSP_clean, MGSP_DT_df)

#Remove pixel IDs not included in both datasets#####
MGSP_DT_PIs <- MGSP_DT_Combo[(MGSP_DT_Combo$PixelID %in% TINDVI_DT_Combo$PixelID),]
TINDVI_DT_PIs <- TINDVI_DT_Combo[(TINDVI_DT_Combo$PixelID %in% MGSP_DT_PIs$PixelID),]

#Create tables of the PixelIDs and coordinates for later use
library(dplyr)

TINDVIanomaly_coords <- TINDVI_DT_PIs %>%
  group_by(x, y) %>%
  distinct(PixelID)

#Delete unneccessary columns
TINDVI_DT_PIs <- subset(TINDVI_DT_PIs, select = -c(x,y,TINDVI) )
MGSP_DT_PIs <- subset(MGSP_DT_PIs, select = -c(x,y,MGSP) )

#Make each timeseries the same length
#Make a list split by pixel IDs (timeseries for each pixel)
TINDVI_DT_byPixel <-split(TINDVI_DT_PIs, TINDVI_DT_PIs$PixelID)

#Make a list split by pixel IDs (timeseries for each pixel)
MGSP_DT_byPixel <-split(MGSP_DT_PIs, MGSP_DT_PIs$PixelID)

#Make each element (i.e., pixel) in the lists have the same number of observations
TINDVI <- TINDVI_DT_byPixel
MGSP <- MGSP_DT_byPixel

start_clean <- Sys.time()
for (i in 1:length(MGSP)) {
  if(all(is.element(TINDVI[[i]]$Year, MGSP[[i]]$Year)) & all(is.element(MGSP[[i]]$Year, TINDVI[[i]]$Year)) == T) {
    next
  } else {
    years_missing_from_SM <- TINDVI[[i]]$Year[which(TINDVI[[i]]$Year %in% MGSP[[i]]$Year == F)]
    years_missing_from_TINDVI <- MGSP[[i]]$Year[which(MGSP[[i]]$Year %in% TINDVI[[i]]$Year == F)]
    years_to_cast_out <- c(years_missing_from_SM, years_missing_from_TINDVI)
    TINDVI[[i]] <- TINDVI[[i]][!(TINDVI[[i]]$Year %in% years_to_cast_out), ]
    MGSP[[i]] <- MGSP[[i]][!(MGSP[[i]]$Year %in% years_to_cast_out), ]
  }
}
fin_clean <- Sys.time()
cat('Loop took', round(fin_clean - start_clean, digits = 2), 'seconds to run')

#Check to make sure the loop worked
years_identical <- vector(length = length(MGSP))
for (i in 1:length(MGSP)) {
  years_identical[i] <- identical(sort(TINDVI[[i]]$Year), sort(MGSP[[i]]$Year))
}
which(years_identical  == FALSE) # indicates which matched pairs of elements from the lists have different numbers of observations
length(which(years_identical  == FALSE)) # indicates how many matched pairs of elements from the lists have different numbers of observations

#Run the correlation between the timeseries of each pixel
library(psych)

correlations <- vector(mode = 'list', length = length(MGSP))
start_cor <- Sys.time()
for (i in 1:length(MGSP)) {
  correlations[[i]] <- corr.test(TINDVI[[i]]$V1, y = MGSP[[i]]$V1,
                                 use = "pairwise",
                                 method = "spearman",
                                 alpha = .05,
                                 ci = FALSE)
  pixel_same <- identical(TINDVI[[i]]$PixelID, MGSP[[i]]$PixelID)
  if (pixel_same == T) {
    correlations[[i]][length(correlations[[i]]) + 1] <- unique(TINDVI[[i]]$PixelID)
    names(correlations[[i]])[12] <- "PixelID"
  } else {
    stop()
  }
}
fin_cor <- Sys.time()
cat('Loop took', round(fin_cor - start_cor, digits = 2), 'minutes to run')

hist(unlist(lapply(correlations, function(x) x[1])), main = "Correlations", ylab = "Pixel Count", 
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

#To create a tif, need to add XY coordinates to the table, and remove pixel IDs
CorrResult_forTiff <- cbind(TINDVIanomaly_coords, FinalCorrResult)
CorrResult_forTiff <- subset(CorrResult_forTiff, select = -c(PixelID, PixelIDs, Pvals) )

#Round r to 3 decimal places 
CorrResult_forTiff$CorrCoeffs <- round(CorrResult_forTiff$CorrCoeffs, digits = 3)

CorrResult_neg <- CorrResult_forTiff[(CorrResult_forTiff$CorrCoeffs < 0),]
CorrResult_pos <- CorrResult_forTiff[(CorrResult_forTiff$CorrCoeffs > 0),]

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
TINDVI_MGSP_AnomalyCorr_tif <- writeRaster(CorrResult_raster, "TINDVI_0.05Update_MGSP_AnomalyCorr_09Oct2022", format= "GTiff")