########################Updated SFPO Durbin-Watson, Sen's Slope, and Mann-Kendall Tests#############################
setwd("~/UVA/MODIS Timeseries Project/Yamal Data")

#Load in cleaned Max NDVI dataset/coordinates (only pixels with 15+ years of data)
SFPO_clean <- readRDS(file = "SFPO_Updated/SFPO_Clean_09March2021.RDS")
SFPO_coords <- readRDS(file = "SFPO_Updated/SFPO_Clean_Coords_09March2021.RDS")

####Durbin-Watson Test for Serial Autocorrelation
library(pbapply)

#split the data
SFPO_lst <- split(SFPO_clean, f = SFPO_clean$PixelID)

dw_lag1_2sided <- function(x){
  m <- lm(SFPO ~ Year, data = x)
  dw <- car::durbinWatsonTest(m, max.lag=1,alternative=c("two.sided", "positive", "negative"))
  c(dw = dw$dw, p = dw$p, r = dw$r)
  #dw$dw
}

dw_lag1_2sided_out <- pbsapply(X = SFPO_lst, FUN = dw_lag1_2sided)
dw_lag1_2sided_table <- as.data.frame(dw_lag1_2sided_out)
#transform the table so that dw, p, and r are columns not rows
dw_lag1_2sided_table <- t(dw_lag1_2sided_table)
#convert to a matrix
dw_lag1_2sided_table <- as.data.frame(dw_lag1_2sided_table)

#plot a histogram of the Durbin-Watson stats to see the frequency of pixels that exhibit autocorrelation
hist(dw_lag1_2sided_table$dw,
     main="SFPO Durbin-Watson Stats", 
     xlab = "Durbin-Watson Stats",
     breaks=15)

#Determine the number of pixels with significant p-values
sum(dw_lag1_2sided_table$p < 0.05)

#export the table
library(readxl)
#write.table(dw_lag1_2sided_table, file="SFPO_dw_lag1_2sided_08March2021.csv", sep=",")

####Calculate Sen's Slope
#Create list of lists - each pixel ID contains a list of 2001-2018 values
SFPO_split <- split(SFPO_clean$SFPO, SFPO_clean$PixelID)

#Create function to make the data into a timeseries and removes NA values
TSobj <- function(x){
  m <- ts(data = x, start = which.min(SFPO_clean$Year), end = which.max(SFPO_clean$Year), frequency = 1)
  n <- x[!is.na(x)]
}

#Apply the function
SFPO.TS <- pblapply(SFPO_split, TSobj)

#Save the timeseries list for use in other analyses
#WARNING: exporting and re-loading using R list can add duplicate pixel IDs
#library(rlist)
#list.save(SFPO.TS, file="Updated_SFPO_TS_list.RData")
#SFPO.TS <- list.load("SFPO_Updated/Updated_SFPO_TS_list.Rdata")

library(trend)

##Create function
SS <- function(x){
  if(any(is.na(x))) c("estimates" = NA)
  else(sens.slope(x)[c("estimates")])
}

library(dplyr)
SFPO.SS <- pblapply(SFPO.TS, SS) 

#Convert the output list into a dataframe
SFPO_SS <- as.data.frame(SFPO.SS)
SFPO_SS <- t(SFPO_SS)
SFPO_SS <- as.data.frame(SFPO_SS)

##Plot histogram of slopes
hist(SFPO_SS$`Sen's slope`,
     main="SFPO 2001-2018 Sen's Slope",
     xlab = "Slope",
     xlim = c(-2,2),
     breaks=50)

#Export as an R object
#saveRDS(SFPO_SS, file="SFPO_Slope_10March2021.RDS")

##Export Sens Slope table
#write.table(SFPO_SS, file="Updated_SFPO_SensSlope_09Mar2021.csv", sep=",")

#Reload slope R object
SFPO_SS <- readRDS(file = "SFPO_Updated/SFPO_Slope_10March2021.RDS")

#####Non-Pre-whitened Mann-Kendall Trend Test
library(Kendall)

#Get p-value (sl)
MK_sl <- function(x){
  mk <- MannKendall(x)[c("sl")]
}

#Apply the function to the timeseries
SFPO.MKsl <- pblapply(SFPO.TS, MK_sl)

#Convert the output list into a dataframe
SFPO_MKsl <- as.data.frame(SFPO.MKsl)
SFPO_MKsl <- t(SFPO_MKsl)
SFPO_MKsl <- as.data.frame(SFPO_MKsl)

#Export as an R object
#saveRDS(SFPO_MKsl, file="SFPO_MKsl_10March2021.RDS")

#Reload MK R Object
SFPO_MKsl <- readRDS(file = "SFPO_Updated/SFPO_MKsl_10March2021.RDS")

#Plot histogram of p-values.
hist(SFPO_MKsl$V1,
     main="SFPO Mann-Kendall Test P-values", 
     xlab = "P-value",
     #col="lightcyan3",
     breaks=35)

#Determine the number of pixels with significant p-values
sum(SFPO_MKsl$V1 < 0.05)

####Pre-whitened Mann-Kendall Test
library(modifiedmk)

#Create the function
MK_newtest <- function(x){
  mks <- mkttest(x)
}

#Apply the function to the timeseries object
SFPO.MKout <- pblapply(SFPO.TS, MK_newtest)

#Convert the output list into a dataframe
SFPO_MKout <- as.data.frame(SFPO.MKout)
SFPO_MKout <- t(SFPO_MKout)
SFPO_MKout <- as.data.frame(SFPO_MKout)

#Determine the number of pixels with significant p-values
sum(SFPO_MKout$`P-value` < 0.05)

#Plot histogram of p-values.
hist(SFPO_MKout$`P-value`,
     main="SFPO Mann-Kendall Test P-values (New Function)", 
     xlab = "P-value",
     #col="lightcyan3",
     breaks=35)

#Plot histogram of slopes
hist(SFPO_MKout$`Sen's slope`,
     main="SFPO Sen's Slope (New Function)", 
     xlab = "Slope",
     #xlim = c(-0.010, 0.010),
     #col="lightcyan3",
     breaks=50)

#Get same result from both functions.
####Create a Mann-Kendall Results geotiff
#Combine slope, p-value, and coordinates into 1 dataframe
SFPO_MKresult <- cbind(SFPO_coords, SFPO_MKsl, SFPO_SS)

str(SFPO_MKresult)
SFPO_MKresult <- as.data.frame(SFPO_MKresult)
str(SFPO_MKresult)

SFPO_MKresult <- subset(SFPO_MKresult, select = -c(PixelID) )

#Round to 3 decimal places 
SFPO_MKresult$V1 <- round(SFPO_MKresult$V1, digits = 3)
SFPO_MKresult$`Sen's slope` <- round(SFPO_MKresult$`Sen's slope`, digits = 3)

####Categorize the data
SFPO_MKresult$V1 <- ifelse(SFPO_MKresult$V1 > 0.050000, "Insignificant", SFPO_MKresult$V1)
SFPO_MKresult$V1 <- ifelse(SFPO_MKresult$V1 < 0.050000, "Significant", SFPO_MKresult$V1)
SFPO_MKresult$V1 <- ifelse(SFPO_MKresult$V1 == 0.050000, "Marginal", SFPO_MKresult$V1)
SFPO_MKresult$`Sen's slope` <- ifelse(SFPO_MKresult$`Sen's slope` > 0.000000, "Positive", SFPO_MKresult$`Sen's slope`)
SFPO_MKresult$`Sen's slope` <- ifelse(SFPO_MKresult$`Sen's slope` < 0.000000, "Negative", SFPO_MKresult$`Sen's slope`)
SFPO_MKresult$`Sen's slope` <- ifelse(SFPO_MKresult$`Sen's slope` == 0.000000, "No Change", SFPO_MKresult$`Sen's slope`)

#Before converting to raster, need to convert significance/sign combinations into numeric values
#1 = significant and positive
#2 = insignificant and positive
#3 = marginal and positive
#4 = significant and negative
#5 = insignificant and negative
#6 = marginal and negative
#7 = no change

SFPO_MKresult$V1 <- ifelse(SFPO_MKresult$V1 == "Significant" & SFPO_MKresult$`Sen's slope` =="Positive", 1, SFPO_MKresult$V1)
SFPO_MKresult$V1 <- ifelse(SFPO_MKresult$V1 == "Insignificant" & SFPO_MKresult$`Sen's slope` =="Positive", 2, SFPO_MKresult$V1)
SFPO_MKresult$V1 <- ifelse(SFPO_MKresult$V1 == "Marginal" & SFPO_MKresult$`Sen's slope` =="Positive", 3, SFPO_MKresult$V1)
SFPO_MKresult$V1 <- ifelse(SFPO_MKresult$V1 == "Significant" & SFPO_MKresult$`Sen's slope` =="Negative", 4, SFPO_MKresult$V1)
SFPO_MKresult$V1 <- ifelse(SFPO_MKresult$V1 == "Insignificant" & SFPO_MKresult$`Sen's slope` =="Negative", 5, SFPO_MKresult$V1)
SFPO_MKresult$V1 <- ifelse(SFPO_MKresult$V1 == "Marginal" & SFPO_MKresult$`Sen's slope` =="Negative", 6, SFPO_MKresult$V1)
SFPO_MKresult$V1 <- ifelse(SFPO_MKresult$`Sen's slope` == "No Change", 7, SFPO_MKresult$V1)

table(SFPO_MKresult$V1)

#Remove the 'slope' column
colnames(SFPO_MKresult)[4] <- "slope"
SFPO_MKresult <- subset(SFPO_MKresult, select = -c(slope) )

#Make a tiff
library(sp)
library(raster)

###Create tables that only include the coordinates and only the MK test result
coords <- subset(SFPO_MKresult, select = -c(V1) )
MKresult <- subset(SFPO_MKresult, select = -c(x,y) )

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
Updated_SFPO_MKresult_tif <- writeRaster(MKresult_raster, "Updated_SFPO_MKresult_06June2021", format= "GTiff")
