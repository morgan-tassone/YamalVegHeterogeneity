##########################TI-NDVI/SWI Anomaly Analysis#########################
setwd("~/UVA/MODIS Timeseries Project/Yamal Data")

#Load in cleaned TI-NDVI dataset (only pixels with 15+ years of data)
TINDVI_clean <- readRDS(file = "TI-NDVI_0.05_Updated/TINDVI_Clean_27Feb2022.RDS")

#Remove NAs and use to add pixel IDs/coords to detrended dataset
TINDVI_clean <- na.omit(TINDVI_clean)

library(pbapply)

# #Create list of lists - each pixel ID contains a list of 2001-2018 NDVI values
# TINDVI_split <- split(TINDVI_clean$TINDVI, TINDVI_clean$PixelID)
# 
# #Create function to make the data into a timeseries and removes NA values
# TSobj <- function(x){
#   m <- ts(data = x, start = which.min(TINDVI_clean$Year), end = which.max(TINDVI_clean$Year), frequency = 1)
#   n <- x[!is.na(x)]
# }
# 
# TINDVI.TS <- pblapply(TINDVI_split, TSobj)
# 
# #Save TS list for other analyses
# library(rlist)
# list.save(TINDVI.TS, file="Updated0.05_TI-NDVI_TS_list.RData")

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

#Load SWI data (only pixels with 15+ years of data) and create timeseries list
SWI_clean <- readRDS(file = "SWI_Updated/SWI_Clean_08March2021.RDS")

#Remove NAs and use to add pixel IDs/coords to detrended dataset
SWI_clean <- na.omit(SWI_clean)

SWI.TS <- list.load("SWI_Updated/Updated_SWI_TS_list.RData")

#Linear detrending
SWI.DT <- pblapply(SWI.TS, LinDetrend)

SWI_DT_df <- do.call(rbind, SWI.DT)
SWI_DT_df <- as.data.frame(SWI_DT_df)

#Combine detrended timeseries with year and pixel ID
SWI_DT_Combo <- cbind(SWI_clean, SWI_DT_df)

#Remove pixel IDs not included in both datasets
SWI_DT_PIs <- SWI_DT_Combo[(SWI_DT_Combo$PixelID %in% TINDVI_DT_Combo$PixelID),]
TINDVI_DT_PIs <- TINDVI_DT_Combo[(TINDVI_DT_Combo$PixelID %in% SWI_DT_PIs$PixelID),]

#Create tables of the PixelIDs and coordinates for later use
library(dplyr)

TINDVIanomaly_coords <- TINDVI_DT_PIs %>%
  group_by(x, y) %>%
  distinct(PixelID)

#Delete unneccessary columns
TINDVI_DT_PIs <- subset(TINDVI_DT_PIs, select = -c(x,y,TINDVI) )
SWI_DT_PIs <- subset(SWI_DT_PIs, select = -c(x,y,SWI) )

#Make each timeseries the same length
#Make a list split by pixel IDs (timeseries for each pixel)
TINDVI_DT_byPixel <-split(TINDVI_DT_PIs, TINDVI_DT_PIs$PixelID)

#Make a list split by pixel IDs (timeseries for each pixel)
SWI_DT_byPixel <-split(SWI_DT_PIs, SWI_DT_PIs$PixelID)

#Make each element (i.e., pixel) in the lists have the same number of observations
TINDVI <- TINDVI_DT_byPixel
SWI <- SWI_DT_byPixel

start_clean <- Sys.time()
for (i in 1:length(SWI)) {
  if(all(is.element(TINDVI[[i]]$Year, SWI[[i]]$Year)) & all(is.element(SWI[[i]]$Year, TINDVI[[i]]$Year)) == T) {
    next
  } else {
    years_missing_from_SM <- TINDVI[[i]]$Year[which(TINDVI[[i]]$Year %in% SWI[[i]]$Year == F)]
    years_missing_from_TINDVI <- SWI[[i]]$Year[which(SWI[[i]]$Year %in% TINDVI[[i]]$Year == F)]
    years_to_cast_out <- c(years_missing_from_SM, years_missing_from_TINDVI)
    TINDVI[[i]] <- TINDVI[[i]][!(TINDVI[[i]]$Year %in% years_to_cast_out), ]
    SWI[[i]] <- SWI[[i]][!(SWI[[i]]$Year %in% years_to_cast_out), ]
  }
}
fin_clean <- Sys.time()
cat('Loop took', round(fin_clean - start_clean, digits = 2), 'seconds to run')

#Check to make sure the loop worked
years_identical <- vector(length = length(SWI))
for (i in 1:length(SWI)) {
  years_identical[i] <- identical(sort(TINDVI[[i]]$Year), sort(SWI[[i]]$Year))
}
which(years_identical  == FALSE) # indicates which matched pairs of elements from the lists have different numbers of observations
length(which(years_identical  == FALSE)) # indicates how many matched pairs of elements from the lists have different numbers of observations

#Run the correlation between the timeseries of each pixel
library(psych)

correlations <- vector(mode = 'list', length = length(SWI))
start_cor <- Sys.time()
for (i in 1:length(SWI)) {
  correlations[[i]] <- corr.test(TINDVI[[i]]$V1, y = SWI[[i]]$V1,
                                 use = "pairwise",
                                 method = "spearman",
                                 alpha = .05,
                                 ci = FALSE)
  pixel_same <- identical(TINDVI[[i]]$PixelID, SWI[[i]]$PixelID)
  if (pixel_same == T) {
    correlations[[i]][length(correlations[[i]]) + 1] <- unique(TINDVI[[i]]$PixelID)
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

#To create a tif, need to add XY coordinates to the table, and remove pixel IDs
CorrResult_forTiff <- cbind(TINDVIanomaly_coords, FinalCorrResult)
CorrResult_forTiff <- subset(CorrResult_forTiff, select = -c(PixelID, PixelIDs) )

#Round r to 3 decimal places 
CorrResult_forTiff$CorrCoeffs <- round(CorrResult_forTiff$CorrCoeffs, digits = 3)

CorrResult_neg <- CorrResult_forTiff[(CorrResult_forTiff$CorrCoeffs < 0),]
CorrResult_pos <- CorrResult_forTiff[(CorrResult_forTiff$CorrCoeffs > 0),]

#Make map
library(sp)
library(raster)

###Create tables that only include the coordinates and only the result
coords <- subset(CorrResult_forTiff, select = -c(CorrCoeffs,Pvals) )
result <- subset(CorrResult_forTiff, select = -c(x,y,Pvals) )

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
TINDVI_SWI_AnomalyCorr_tif <- writeRaster(CorrResult_raster, "TINDVI0.05_SWI_AnomalyCorr_22Sept2022", format= "GTiff")
