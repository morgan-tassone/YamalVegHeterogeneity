##########################Max NDVI/SWI Anomaly Analysis#########################
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

#Remove pixel IDs not included in both datasets#####
SWI_DT_PIs <- SWI_DT_Combo[(SWI_DT_Combo$PixelID %in% MaxNDVI_DT_Combo$PixelID),]
MaxNDVI_DT_PIs <- MaxNDVI_DT_Combo[(MaxNDVI_DT_Combo$PixelID %in% SWI_DT_PIs$PixelID),]

#Create tables of the PixelIDs and coordinates for later use
library(dplyr)

MaxNDVIanomaly_coords <- MaxNDVI_DT_PIs %>%
  group_by(x, y) %>%
  distinct(PixelID)

#Delete unneccessary columns
MaxNDVI_DT_PIs <- subset(MaxNDVI_DT_PIs, select = -c(x,y,NDVI) )
SWI_DT_PIs <- subset(SWI_DT_PIs, select = -c(x,y,SWI) )

#Make each timeseries the same length
#Make a list split by pixel IDs (timeseries for each pixel)
MaxNDVI_DT_byPixel <-split(MaxNDVI_DT_PIs, MaxNDVI_DT_PIs$PixelID)

#Make a list split by pixel IDs (timeseries for each pixel)
SWI_DT_byPixel <-split(SWI_DT_PIs, SWI_DT_PIs$PixelID)

#Make each element (i.e., pixel) in the lists have the same number of observations
MaxNDVI <- MaxNDVI_DT_byPixel
SWI <- SWI_DT_byPixel

start_clean <- Sys.time()
for (i in 1:length(SWI)) {
  if(all(is.element(MaxNDVI[[i]]$Year, SWI[[i]]$Year)) & all(is.element(SWI[[i]]$Year, MaxNDVI[[i]]$Year)) == T) {
    next
  } else {
    years_missing_from_SM <- MaxNDVI[[i]]$Year[which(MaxNDVI[[i]]$Year %in% SWI[[i]]$Year == F)]
    years_missing_from_MaxNDVI <- SWI[[i]]$Year[which(SWI[[i]]$Year %in% MaxNDVI[[i]]$Year == F)]
    years_to_cast_out <- c(years_missing_from_SM, years_missing_from_MaxNDVI)
    MaxNDVI[[i]] <- MaxNDVI[[i]][!(MaxNDVI[[i]]$Year %in% years_to_cast_out), ]
    SWI[[i]] <- SWI[[i]][!(SWI[[i]]$Year %in% years_to_cast_out), ]
  }
}
fin_clean <- Sys.time()
cat('Loop took', round(fin_clean - start_clean, digits = 2), 'seconds to run')

#Check to make sure the loop worked
years_identical <- vector(length = length(SWI))
for (i in 1:length(SWI)) {
  years_identical[i] <- identical(sort(MaxNDVI[[i]]$Year), sort(SWI[[i]]$Year))
}
which(years_identical  == FALSE) # indicates which matched pairs of elements from the lists have different numbers of observations
length(which(years_identical  == FALSE)) # indicates how many matched pairs of elements from the lists have different numbers of observations

#Run the correlation between the timeseries of each pixel
library(psych)

correlations <- vector(mode = 'list', length = length(SWI))
start_cor <- Sys.time()
for (i in 1:length(SWI)) {
  correlations[[i]] <- corr.test(MaxNDVI[[i]]$V1, y = SWI[[i]]$V1,
                                 use = "pairwise",
                                 method = "spearman",
                                 alpha = .05,
                                 ci = FALSE)
  pixel_same <- identical(MaxNDVI[[i]]$PixelID, SWI[[i]]$PixelID)
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

Pvals <- unlist(lapply(correlations, function(x) x[4]))
Pvals <- as.data.frame(Pvals)
SigPvals <- Pvals[(Pvals$Pvals <= 0.05),]

PixelIDs <- unlist(lapply(correlations, function(x) x[12]))
PixelIDs <- as.data.frame(PixelIDs)

FinalCorrResult <- cbind(PixelIDs, CorrCoeffs)

FinalCorrResult_Pvals <- cbind(PixelIDs, Pvals, CorrCoeffs)
#Limit dataset to pixels with significant correlations
FinalCorrResult_Sig <- FinalCorrResult_Pvals[(FinalCorrResult_Pvals$Pvals <= 0.05),]

#Determine # of pixels with negative corr and positive corr
NegCorr <- FinalCorrResult_Pvals[(FinalCorrResult_Pvals$CorrCoeffs < 0),]
#Significant negative correlations
NegCorrSig <- NegCorr[(NegCorr$Pvals <= 0.05),]
PosCorr <- FinalCorrResult_Pvals[(FinalCorrResult_Pvals$CorrCoeffs > 0),]
#Significant positive correlations
PosCorrSig <- PosCorr[(PosCorr$Pvals <= 0.05),]

#Determine # of significant pixels with negative corr and positive corr
NegCorr_sig <- FinalCorrResult_Sig[(FinalCorrResult_Sig$CorrCoeffs < 0),]
PosCorr_sig <- FinalCorrResult_Sig[(FinalCorrResult_Sig$CorrCoeffs > 0),]

# #Limit to pixels included in the random forest regression
# RFR_PixelIDs <- readRDS(file = "RFR_Final/MaxNDVI_RFR_PixelIDs.RDS")
# FinalCorr_RFRpixels <- FinalCorrResult[(FinalCorrResult$PixelID %in% RFR_PixelIDs$PixelID),]
# NegCorr_RFR <- FinalCorr_RFRpixels[(FinalCorr_RFRpixels$CorrCoeffs < 0),]
# PosCorr_RFR <- FinalCorr_RFRpixels[(FinalCorr_RFRpixels$CorrCoeffs > 0),]

# #Determine overall avg corr coeff
# AvgCorrCoeff <- mean(FinalCorr_RFRpixels$CorrCoeffs)

#To create a tif, need to add XY coordinates to the table, and remove pixel IDs
#Remove pixel IDs not in sig dataframe from coords dataframe
colnames(MaxNDVIanomaly_coords)[1] <- "PixelID"
colnames(FinalCorrResult_Sig)[1] <- "PixelID"
SigCoords <- MaxNDVIanomaly_coords[(MaxNDVIanomaly_coords$PixelID %in% FinalCorrResult_Sig$PixelID),]
CorrResult_forTiff <- cbind(SigCoords, FinalCorrResult_Sig$CorrCoeffs)
colnames(CorrResult_forTiff)[4] <- "CorrCoeffs"

#Round r to 3 decimal places 
CorrResult_forTiff$CorrCoeffs <- round(CorrResult_forTiff$CorrCoeffs, digits = 3)

# #Determine # of pixels with negative and positive correlations
# CorrResult_neg <- CorrResult_forTiff[(CorrResult_forTiff$CorrCoeffs < -0.1),]
# CorrResult_pos <- CorrResult_forTiff[(CorrResult_forTiff$CorrCoeffs > 0.1),]
# CorrResult_0 <- CorrResult_forTiff[(CorrResult_forTiff$CorrCoeffs == 0),]
# CorrResult_LittleToNoCorr <- CorrResult_forTiff[(CorrResult_forTiff$CorrCoeffs >= -0.1 & CorrResult_forTiff$CorrCoeffs <= 0.1),]
  
#Make map
library(sp)
library(raster)

###Create tables that only include the coordinates and only the MK test result, need to remame p-value to pvalue
coords <- subset(CorrResult_forTiff, select = -c(CorrCoeffs, PixelID) )
result <- subset(CorrResult_forTiff, select = -c(x,y,PixelID) )

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
MaxNDVI_SWI_AnomalyCorr_tif <- writeRaster(CorrResult_raster, "MaxNDVI_SWI_AnomalyCorr_SIGONLY_22Sept2021", format= "GTiff")