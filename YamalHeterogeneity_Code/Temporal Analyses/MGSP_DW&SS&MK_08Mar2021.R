########################Updated MGSP Durbin-Watson, Sen's Slope, and Mann-Kendall Tests#############################
setwd("~/UVA/MODIS Timeseries Project/Yamal Data")

#Load in cleaned Max NDVI dataset/coordinates (only pixels with 15+ years of data)
MGSP_clean <- readRDS(file = "MGSP_Updated/MGSP_Clean_08March2021.RDS")
MGSP_coords <- readRDS(file = "MGSP_Updated/MGSP_Clean_Coords_08March2021.RDS")

####Durbin-Watson Test for Serial Autocorrelation
library(pbapply)

#split the data
MGSP_lst <- split(MGSP_clean, f = MGSP_clean$PixelID)

dw_lag1_2sided <- function(x){
  m <- lm(MGSP ~ Year, data = x)
  dw <- car::durbinWatsonTest(m, max.lag=1,alternative=c("two.sided", "positive", "negative"))
  c(dw = dw$dw, p = dw$p, r = dw$r)
  #dw$dw
}

dw_lag1_2sided_out <- pbsapply(X = MGSP_lst, FUN = dw_lag1_2sided)
dw_lag1_2sided_table <- as.data.frame(dw_lag1_2sided_out)
#transform the table so that dw, p, and r are columns not rows
dw_lag1_2sided_table <- t(dw_lag1_2sided_table)
#convert to a matrix
dw_lag1_2sided_table <- as.data.frame(dw_lag1_2sided_table)

#plot a histogram of the Durbin-Watson stats to see the frequency of pixels that exhibit autocorrelation
hist(dw_lag1_2sided_table$dw,
     main="MGSP Durbin-Watson Stats", 
     xlab = "Durbin-Watson Stats",
     breaks=15)

#Determine the number of pixels with significant p-values
sum(dw_lag1_2sided_table$p < 0.05)

#export the table
library(readxl)
#write.table(dw_lag1_2sided_table, file="MGSP_dw_lag1_2sided_08March2021.csv", sep=",")

####Calculate Sen's Slope
#Create list of lists - each pixel ID contains a list of 2001-2018 values
MGSP_split <- split(MGSP_clean$MGSP, MGSP_clean$PixelID)

#Create function to make the data into a timeseries and removes NA values
TSobj <- function(x){
  m <- ts(data = x, start = which.min(MGSP_clean$Year), end = which.max(MGSP_clean$Year), frequency = 1)
  n <- x[!is.na(x)]
}

#Apply the function
MGSP.TS <- pblapply(MGSP_split, TSobj)

#Save the timeseries list for use in other analyses
##WARNING: Using rlist to save/reload the timeseries list can add duplicate pixel IDs.
#library(rlist)
#list.save(MGSP.TS, file="Updated_MGSP_TS_list.RData")
#MGSP.TS <- list.load("MGSP_Updated/Updated_MGSP_TS_list.Rdata")

# library(trend)
# 
# ##Create function
# SS <- function(x){
#   if(any(is.na(x))) c("estimates" = NA)
#   else(sens.slope(x)[c("estimates")])
# }
# 
# library(dplyr)
# MGSP.SS <- pblapply(MGSP.TS, SS) 
# 
# #Convert the output list into a dataframe
# MGSP_SS <- as.data.frame(MGSP.SS)
# MGSP_SS <- t(MGSP_SS)
# MGSP_SS <- as.data.frame(MGSP_SS)
# 
# ##Plot histogram of slopes
# hist(MGSP_SS$`Sen's slope`,
#      main="MGSP 2001-2018 Sen's Slope",
#      xlab = "Slope",
#      #xlim = c(-0.010, 0.010),
#      breaks=50)

#Export as an R object
#saveRDS(MGSP_SS, file="MGSP_Slope_10March2021.RDS")

#Reload slope R object
MGSP_SS <- readRDS(file = "MGSP_Updated/MGSP_Slope_10March2021.RDS")

##Export Sens Slope table
#write.table(MGSP_SS, file="Updated_MGSP_SensSlope_08Mar2021.csv", sep=",")

#####Non-Pre-whitened Mann-Kendall Trend Test
library(Kendall)

#Get p-value (sl)
MK_sl <- function(x){
  mk <- MannKendall(x)[c("sl")]
}

#Apply the function to the timeseries
MGSP.MKsl <- pblapply(MGSP.TS, MK_sl)

#Convert the output list into a dataframe
MGSP_MKsl <- as.data.frame(MGSP.MKsl)
MGSP_MKsl <- t(MGSP_MKsl)
MGSP_MKsl <- as.data.frame(MGSP_MKsl)

#Export as an R object
#saveRDS(MGSP_MKsl, file="MGSP_MKsl_10March2021.RDS")

#Plot histogram of p-values.
hist(MGSP_MKsl$V1,
     main="MGSP Mann-Kendall Test P-values", 
     xlab = "P-value",
     #col="lightcyan3",
     breaks=35)

#Determine the number of pixels with significant p-values
sum(MGSP_MKsl$V1 < 0.05)

####Pre-whitened Mann-Kendall Test
library(modifiedmk)

#Create the function
MK_newtest <- function(x){
  mks <- mkttest(x)
}

#Apply the function to the timeseries object
MGSP.MKout <- pblapply(MGSP.TS, MK_newtest)

#Convert the output list into a dataframe
MGSP_MKout <- as.data.frame(MGSP.MKout)
MGSP_MKout <- t(MGSP_MKout)
MGSP_MKout <- as.data.frame(MGSP_MKout)

#Determine the number of pixels with significant p-values
sum(MGSP_MKout$`P-value` < 0.05)

#Plot histogram of p-values.
hist(MGSP_MKout$`P-value`,
     main="MGSP Mann-Kendall Test P-values (New Function)", 
     xlab = "P-value",
     #col="lightcyan3",
     breaks=35)

#Plot histogram of slopes
hist(MGSP_MKout$`Sen's slope`,
     main="MGSP Sen's Slope (New Function)", 
     xlab = "Slope",
     #xlim = c(-0.010, 0.010),
     #col="lightcyan3",
     breaks=50)

#Get same result from both functions.
####Create a Mann-Kendall Results geotiff
#Combine slope, p-value, and coordinates into 1 dataframe
MGSP_MKresult <- cbind(MGSP_coords, MGSP_MKsl, MGSP_SS)

str(MGSP_MKresult)
MGSP_MKresult <- as.data.frame(MGSP_MKresult)
str(MGSP_MKresult)

MGSP_MKresult <- subset(MGSP_MKresult, select = -c(PixelID) )

#Round p-values to 3 and slopes to 6 decimal places 
MGSP_MKresult$V1 <- round(MGSP_MKresult$V1, digits = 3)
MGSP_MKresult$`Sen's slope` <- round(MGSP_MKresult$`Sen's slope`, digits = 6)

####Categorize the data
MGSP_MKresult$V1 <- ifelse(MGSP_MKresult$V1 > 0.050000, "Insignificant", MGSP_MKresult$V1)
MGSP_MKresult$V1 <- ifelse(MGSP_MKresult$V1 < 0.050000, "Significant", MGSP_MKresult$V1)
MGSP_MKresult$V1 <- ifelse(MGSP_MKresult$V1 == 0.050000, "Marginal", MGSP_MKresult$V1)
MGSP_MKresult$`Sen's slope` <- ifelse(MGSP_MKresult$`Sen's slope` > 0.000000, "Positive", MGSP_MKresult$`Sen's slope`)
MGSP_MKresult$`Sen's slope` <- ifelse(MGSP_MKresult$`Sen's slope` < 0.000000, "Negative", MGSP_MKresult$`Sen's slope`)
MGSP_MKresult$`Sen's slope` <- ifelse(MGSP_MKresult$`Sen's slope` == 0.000000, "No Change", MGSP_MKresult$`Sen's slope`)

#Before converting to raster, need to convert significance/sign combinations into numeric values
#1 = significant and positive
#2 = insignificant and positive
#3 = marginal and positive
#4 = significant and negative
#5 = insignificant and negative
#6 = marginal and negative
#7 = no change

MGSP_MKresult$V1 <- ifelse(MGSP_MKresult$V1 == "Significant" & MGSP_MKresult$`Sen's slope` =="Positive", 1, MGSP_MKresult$V1)
MGSP_MKresult$V1 <- ifelse(MGSP_MKresult$V1 == "Insignificant" & MGSP_MKresult$`Sen's slope` =="Positive", 2, MGSP_MKresult$V1)
MGSP_MKresult$V1 <- ifelse(MGSP_MKresult$V1 == "Marginal" & MGSP_MKresult$`Sen's slope` =="Positive", 3, MGSP_MKresult$V1)
MGSP_MKresult$V1 <- ifelse(MGSP_MKresult$V1 == "Significant" & MGSP_MKresult$`Sen's slope` =="Negative", 4, MGSP_MKresult$V1)
MGSP_MKresult$V1 <- ifelse(MGSP_MKresult$V1 == "Insignificant" & MGSP_MKresult$`Sen's slope` =="Negative", 5, MGSP_MKresult$V1)
MGSP_MKresult$V1 <- ifelse(MGSP_MKresult$V1 == "Marginal" & MGSP_MKresult$`Sen's slope` =="Negative", 6, MGSP_MKresult$V1)
MGSP_MKresult$V1 <- ifelse(MGSP_MKresult$`Sen's slope` == "No Change", 7, MGSP_MKresult$V1)

table(MGSP_MKresult$V1)

#Remove the 'slope' column
colnames(MGSP_MKresult)[4] <- "slope"
MGSP_MKresult <- subset(MGSP_MKresult, select = -c(slope) )

#Make a tiff
library(sp)
library(raster)

###Create tables that only include the coordinates and only the MK test result
coords <- subset(MGSP_MKresult, select = -c(V1) )
MKresult <- subset(MGSP_MKresult, select = -c(x,y) )

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
Updated_MGSP_MKresult_tif <- writeRaster(MKresult_raster, "Updated_MGSP_MKresult_000slope_21Sept2021", format= "GTiff")

