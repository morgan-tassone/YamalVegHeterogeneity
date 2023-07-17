########################Updated TI-NDVI Durbin-Watson, Sen's Slope, and Mann-Kendall Tests#############################
setwd("~/UVA/MODIS Timeseries Project/Yamal Data")

#Load in cleaned TI-NDVI dataset/coordinates (only pixels with 15+ years of data)
TINDVI_clean <- readRDS(file = "TI-NDVI_0.05_Updated/TINDVI_Clean_27Feb2022.RDS")
TINDVI_coords <- readRDS(file = "TI-NDVI_0.05_Updated/TINDVI_Clean_Coords_27Feb2022.RDS")

####Durbin-Watson Test for Serial Autocorrelation
library(pbapply)

#split the data
TINDVI_lst <- split(TINDVI_clean, f = TINDVI_clean$PixelID)

dw_lag1_2sided <- function(x){
  m <- lm(TINDVI ~ Year, data = x)
  dw <- car::durbinWatsonTest(m, max.lag=1,alternative=c("two.sided", "positive", "negative"))
  c(dw = dw$dw, p = dw$p, r = dw$r)
  #dw$dw
}

dw_lag1_2sided_out <- pbsapply(X = TINDVI_lst, FUN = dw_lag1_2sided)
dw_lag1_2sided_table <- as.data.frame(dw_lag1_2sided_out)
#transform the table so that dw, p, and r are columns not rows
dw_lag1_2sided_table <- t(dw_lag1_2sided_table)
#convert to a matrix
dw_lag1_2sided_table <- as.data.frame(dw_lag1_2sided_table)

#plot a histogram of the Durbin-Watson stats to see the frequency of pixels that exhibit autocorrelation
hist(dw_lag1_2sided_table$dw,
     main="TI-NDVI Durbin-Watson Stats", 
     xlab = "Durbin-Watson Stats",
     breaks=15)

#Determine the number of pixels with significant p-values
sum(dw_lag1_2sided_table$p < 0.05)

#export the table
#write.table(dw_lag1_2sided_table, file="TINDVI0.05_dw_lag1_2sided_27Feb2022.csv", sep=",")

###Calculate Sen's Slope
#Create list of lists - each pixel ID contains a list of 2001-2018 NDVI values
TINDVI_split <- split(TINDVI_clean$TINDVI, TINDVI_clean$PixelID)

#Create function to make the data into a timeseries and removes NA values
 TSobj <- function(x){
   m <- ts(data = x, start = which.min(TINDVI_clean$Year), end = which.max(TINDVI_clean$Year), frequency = 1)
   n <- x[!is.na(x)]
 }

#Apply the function
TINDVI.TS <- pblapply(TINDVI_split, TSobj)

#library(rlist)
#list.save(TINDVI.TS, file="Updated_TI-NDVI_TS_list.RData")
##WARNING: Exporting using rlist and re-importing can add duplicate pixel IDs.
#TINDVI.TS <- list.load("TI-NDVI_Updated/Updated_TI-NDVI_TS_list.Rdata")

library(trend)

##Create function
SS <- function(x){
  if(any(is.na(x))) c("estimates" = NA)
  else(sens.slope(x)[c("estimates")])
}

library(dplyr)
TINDVI.SS <- pblapply(TINDVI.TS, SS) 

#Convert the output list into a dataframe
TINDVI_SS <- as.data.frame(TINDVI.SS)
TINDVI_SS <- t(TINDVI_SS)
TINDVI_SS <- as.data.frame(TINDVI_SS)


TINDVI_SS$`Sen's slope` <- round(TINDVI_SS$`Sen's slope`, digits = 3)

##Plot histogram of slopes
hist(TINDVI_SS$`Sen's slope`,
     main="TI-NDVI 2001-2018 Sen's Slope",
     xlab = "Slope",
     xlim = c(-0.101, 0.151),
     breaks=50)

##Export Sens Slope table and R object
#write.table(TINDVI_SS, file
saveRDS(TINDVI_SS, file="TINDVI0.05_Slope_27Feb2022.RDS")

#Re-import exported slope data
#TINDVI_SS <- readRDS(file = "TI-NDVI_Updated/TINDVI_Slope_15March2021.RDS")

#####Non-Pre-whitened Mann-Kendall Trend Test
library(Kendall)

#Get p-value (sl)
MK_sl <- function(x){
  mk <- MannKendall(x)[c("sl")]
}

#Apply the function to the timeseries
TINDVI.MKsl <- pblapply(TINDVI.TS, MK_sl)

#Convert the output list into a dataframe
TINDVI_MKsl <- as.data.frame(TINDVI.MKsl)
TINDVI_MKsl <- t(TINDVI_MKsl)
TINDVI_MKsl <- as.data.frame(TINDVI_MKsl)

#Plot histogram of p-values.
hist(TINDVI_MKsl$V1,
     main="TI-NDVI Mann-Kendall Test P-values", 
     xlab = "P-value",
     #col="lightcyan3",
     breaks=35)

#Export as R object
saveRDS(TINDVI_MKsl, file="TINDVI0.05_MKSS_27Feb2022.RDS")

#Re-import exported slope data
#TINDVI_MKsl <- readRDS(file = "TI-NDVI_Updated/TINDVI_MKSS_15March2021.RDS")

#Export the data
#write.table(TINDVI_MKsl, file="Updated_TINDVI_MKpvals_07Mar2021.csv", sep=",")

#Determine the number of pixels with significant p-values
sum(TINDVI_MKsl$V1 < 0.05)

####Pre-whitened Mann-Kendall Test
library(modifiedmk)

#Create the function
MK_newtest <- function(x){
  mks <- mkttest(x)
}

#Apply the function to the timeseries object
TINDVI.MKout <- pblapply(TINDVI.TS, MK_newtest)

#Convert the output list into a dataframe
TINDVI_MKout <- as.data.frame(TINDVI.MKout)
TINDVI_MKout <- t(TINDVI_MKout)
TINDVI_MKout <- as.data.frame(TINDVI_MKout)

#Determine the number of pixels with significant p-values
sum(TINDVI_MKout$`P-value` < 0.05)

#Plot histogram of p-values.
hist(TINDVI_MKout$`P-value`,
     main="TI-NDVI Mann-Kendall Test P-values (New Function)", 
     xlab = "P-value",
     #col="lightcyan3",
     breaks=35)

#Plot histogram of slopes
hist(TINDVI_MKout$`Sen's slope`,
     main="TI-NDVI Sen's Slope (New Function)", 
     xlab = "Slope",
     xlim = c(-0.10, 0.15),
     #col="lightcyan3",
     breaks=50)

####Create a Mann-Kendall Results geotiff
#Combine slope, p-value, and coordinates into 1 dataframe
TINDVI_MKresult <- cbind(TINDVI_coords, TINDVI_MKsl, TINDVI_SS)

str(TINDVI_MKresult)
TINDVI_MKresult <- as.data.frame(TINDVI_MKresult)
str(TINDVI_MKresult)

TINDVI_MKresult <- subset(TINDVI_MKresult, select = -c(PixelID) )

#Round p-value to 3 decimal places and slope to 2 decimal places 
TINDVI_MKresult$V1 <- round(TINDVI_MKresult$V1, digits = 3)
TINDVI_MKresult$`Sen's slope` <- round(TINDVI_MKresult$`Sen's slope`, digits = 3)

####Categorize the data
TINDVI_MKresult$V1 <- ifelse(TINDVI_MKresult$V1 > 0.050000, "Insignificant", TINDVI_MKresult$V1)
TINDVI_MKresult$V1 <- ifelse(TINDVI_MKresult$V1 < 0.050000, "Significant", TINDVI_MKresult$V1)
TINDVI_MKresult$V1 <- ifelse(TINDVI_MKresult$V1 == 0.050000, "Marginal", TINDVI_MKresult$V1)
TINDVI_MKresult$`Sen's slope` <- ifelse(TINDVI_MKresult$`Sen's slope` > 0.000000, "Positive", TINDVI_MKresult$`Sen's slope`)
TINDVI_MKresult$`Sen's slope` <- ifelse(TINDVI_MKresult$`Sen's slope` < 0.000000, "Negative", TINDVI_MKresult$`Sen's slope`)
TINDVI_MKresult$`Sen's slope` <- ifelse(TINDVI_MKresult$`Sen's slope` == 0.000000, "No Change", TINDVI_MKresult$`Sen's slope`)

#Before converting to raster, need to convert significance/sign combinations into numeric values
#1 = significant and positive
#2 = insignificant and positive
#3 = marginal and positive
#4 = significant and negative
#5 = insignificant and negative
#6 = marginal and negative
#7 = no change

TINDVI_MKresult$V1 <- ifelse(TINDVI_MKresult$V1 == "Significant" & TINDVI_MKresult$`Sen's slope` =="Positive", 1, TINDVI_MKresult$V1)
TINDVI_MKresult$V1 <- ifelse(TINDVI_MKresult$V1 == "Insignificant" & TINDVI_MKresult$`Sen's slope` =="Positive", 2, TINDVI_MKresult$V1)
TINDVI_MKresult$V1 <- ifelse(TINDVI_MKresult$V1 == "Marginal" & TINDVI_MKresult$`Sen's slope` =="Positive", 3, TINDVI_MKresult$V1)
TINDVI_MKresult$V1 <- ifelse(TINDVI_MKresult$V1 == "Significant" & TINDVI_MKresult$`Sen's slope` =="Negative", 4, TINDVI_MKresult$V1)
TINDVI_MKresult$V1 <- ifelse(TINDVI_MKresult$V1 == "Insignificant" & TINDVI_MKresult$`Sen's slope` =="Negative", 5, TINDVI_MKresult$V1)
TINDVI_MKresult$V1 <- ifelse(TINDVI_MKresult$V1 == "Marginal" & TINDVI_MKresult$`Sen's slope` =="Negative", 6, TINDVI_MKresult$V1)
TINDVI_MKresult$V1 <- ifelse(TINDVI_MKresult$`Sen's slope` == "No Change", 7, TINDVI_MKresult$V1)

#Remove the 'slope' column
colnames(TINDVI_MKresult)[4] <- "slope"
TINDVI_MKresult <- subset(TINDVI_MKresult, select = -c(slope) )

#Make a tiff
library(sp)
library(raster)

###Create tables that only include the coordinates and only the MK test result
coords <- subset(TINDVI_MKresult, select = -c(V1) )
MKresult <- subset(TINDVI_MKresult, select = -c(x,y) )

coords <- as.data.frame(coords)
MKresult <- as.data.frame(MKresult)

str(coords)
str(MKresult) #need to make this numeric
MKresult$V1 <- as.numeric(as.character(MKresult$V1))
str(MKresult)

#Create spatial points dataframe object to be used in the rasterize function
MKSpatPoints <- SpatialPointsDataFrame(coords, MKresult, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
                                       match.ID = TRUE, bbox = NULL)

#get extent from existing raster and create a raster for the rasterize function
MaxNDVI_raster <- raster("MaxNDVI_Updated/MaxNDVI_9999.tif")
e <- extent(MaxNDVI_raster)
r <- raster(e, ncol=960, nrow=653, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
MKresult_raster <- rasterize(MKSpatPoints, r, MKSpatPoints$V1, fun=mean)
plot(MKresult_raster)

#export the map
library(rgdal)
Updated_TINDVI_MKresult_tif <- writeRaster(MKresult_raster, "Updated_TINDVI_MKresult_0.05_06Mar2022", format= "GTiff")
