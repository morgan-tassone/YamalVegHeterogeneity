########################Updated Soil Moisture Durbin-Watson, Sen's Slope, and Mann-Kendall Tests#########################
setwd("~/UVA/MODIS Timeseries Project/Yamal Data")

#Load in cleaned Max NDVI dataset/coordinates (only pixels with 15+ years of data)
SM_clean <- readRDS(file = "SoilMoisture_Updated/SM_Clean_09March2021.RDS")
SM_coords <- readRDS(file = "SoilMoisture_Updated/SM_Clean_Coords_09March2021.RDS")

####Durbin-Watson Test for Serial Autocorrelation
library(pbapply)

#split the data
SM_lst <- split(SM_clean, f = SM_clean$PixelID)

dw_lag1_2sided <- function(x){
  m <- lm(SM ~ Year, data = x)
  dw <- car::durbinWatsonTest(m, max.lag=1,alternative=c("two.sided", "positive", "negative"))
  c(dw = dw$dw, p = dw$p, r = dw$r)
  #dw$dw
}

dw_lag1_2sided_out <- pbsapply(X = SM_lst, FUN = dw_lag1_2sided)
dw_lag1_2sided_table <- as.data.frame(dw_lag1_2sided_out)
#transform the table so that dw, p, and r are columns not rows
dw_lag1_2sided_table <- t(dw_lag1_2sided_table)
#convert to a matrix
dw_lag1_2sided_table <- as.data.frame(dw_lag1_2sided_table)

#plot a histogram of the Durbin-Watson stats to see the frequency of pixels that exhibit autocorrelation
hist(dw_lag1_2sided_table$dw,
     main="SM Durbin-Watson Stats", 
     xlab = "Durbin-Watson Stats",
     breaks=15)

#Determine the number of pixels with significant p-values
sum(dw_lag1_2sided_table$p < 0.05)

#export the table
library(readxl)
#write.table(dw_lag1_2sided_table, file="SM_dw_lag1_2sided_08March2021.csv", sep=",")

####Calculate Sen's Slope
#Create list of lists - each pixel ID contains a list of 2001-2018 values
SM_split <- split(SM_clean$SM, SM_clean$PixelID)

#Create function to make the data into a timeseries and removes NA values
TSobj <- function(x){
  m <- ts(data = x, start = which.min(SM_clean$Year), end = which.max(SM_clean$Year), frequency = 1)
  n <- x[!is.na(x)]
}

#Apply the function
SM.TS <- pblapply(SM_split, TSobj)

#Save the timeseries list for use in other analyses
##WARNING: using rlist to export/reload the timeseries list can cause pixel ID duplicates
#library(rlist)
#list.save(SM.TS, file="Updated_SM_TS_list.RData")
#SM.TS <- list.load("SoilMoisture_Updated/Updated_SM_TS_list.Rdata")

library(trend)

##Create function
SS <- function(x){
  if(any(is.na(x))) c("estimates" = NA)
  else(sens.slope(x)[c("estimates")])
}

library(dplyr)
SM.SS <- pblapply(SM.TS, SS) 

#Convert the output list into a dataframe
SM_SS <- as.data.frame(SM.SS)
SM_SS <- t(SM_SS)
SM_SS <- as.data.frame(SM_SS)

##Plot histogram of slopes
hist(SM_SS$`Sen's slope`,
     main="SM 2001-2018 Sen's Slope",
     xlab = "Slope",
     xlim = c(-3,1),
     breaks=50)

#Determine Sen's slope outliers
summary(SM_SS$`Sen's slope`)
IQR_val <- -0.1550 - -1.3358
Lower_bound <- -1.3358 - (1.5*1.1808)
Upper_bound <- -0.155 + (1.5 *1.1808)

#See boxplot
boxplot(SM_SS$`Sen's slope`,
        ylab = "SM Sen's Slope"
)

#Export as an R object
#saveRDS(SM_SS, file="SM_Slope_10March2021.RDS")

##Export Sens Slope table
#write.table(SM_SS, file="Updated_SM_SensSlope_09Mar2021.csv", sep=",")

#Re-import slope R object
SM_SS <- readRDS(file = "SoilMoisture_Updated/SM_Slope_10March2021.RDS")

#####Non-Pre-whitened Mann-Kendall Trend Test
library(Kendall)

#Get p-value (sl)
MK_sl <- function(x){
  mk <- MannKendall(x)[c("sl")]
}

#Apply the function to the timeseries
SM.MKsl <- pblapply(SM.TS, MK_sl)

#Convert the output list into a dataframe
SM_MKsl <- as.data.frame(SM.MKsl)
SM_MKsl <- t(SM_MKsl)
SM_MKsl <- as.data.frame(SM_MKsl)

#Export as an R object
#saveRDS(SM_MKsl, file="SM_MKsl_10March2021.RDS")

#Plot histogram of p-values.
hist(SM_MKsl$V1,
     main="SM Mann-Kendall Test P-values", 
     xlab = "P-value",
     #col="lightcyan3",
     breaks=35)

#Determine the number of pixels with significant p-values
sum(SM_MKsl$V1 < 0.05)

####Pre-whitened Mann-Kendall Test
library(modifiedmk)

#Create the function
MK_newtest <- function(x){
  mks <- mkttest(x)
}

#Apply the function to the timeseries object
SM.MKout <- pblapply(SM.TS, MK_newtest)

#Convert the output list into a dataframe
SM_MKout <- as.data.frame(SM.MKout)
SM_MKout <- t(SM_MKout)
SM_MKout <- as.data.frame(SM_MKout)

#Determine the number of pixels with significant p-values
sum(SM_MKout$`P-value` < 0.05)

#Plot histogram of p-values.
hist(SM_MKout$`P-value`,
     main="SM Mann-Kendall Test P-values (New Function)", 
     xlab = "P-value",
     #col="lightcyan3",
     breaks=35)

#Plot histogram of slopes
hist(SM_MKout$`Sen's slope`,
     main="SM Sen's Slope (New Function)", 
     xlab = "Slope",
     #xlim = c(-0.010, 0.010),
     #col="lightcyan3",
     breaks=50)

#Get same result from both functions.
####Create a Mann-Kendall Results geotiff
#Combine slope, p-value, and coordinates into 1 dataframe
SM_MKresult <- cbind(SM_coords, SM_MKsl, SM_SS)

str(SM_MKresult)
SM_MKresult <- as.data.frame(SM_MKresult)
str(SM_MKresult)

SM_MKresult <- subset(SM_MKresult, select = -c(PixelID) )

#Round p-values to 3 and slopes to 6 decimal places 
SM_MKresult$V1 <- round(SM_MKresult$V1, digits = 3)
SM_MKresult$`Sen's slope` <- round(SM_MKresult$`Sen's slope`, digits = 6)

####Categorize the data
SM_MKresult$V1 <- ifelse(SM_MKresult$V1 > 0.050000, "Insignificant", SM_MKresult$V1)
SM_MKresult$V1 <- ifelse(SM_MKresult$V1 < 0.050000, "Significant", SM_MKresult$V1)
SM_MKresult$V1 <- ifelse(SM_MKresult$V1 == 0.050000, "Marginal", SM_MKresult$V1)
SM_MKresult$`Sen's slope` <- ifelse(SM_MKresult$`Sen's slope` > 0.000000, "Positive", SM_MKresult$`Sen's slope`)
SM_MKresult$`Sen's slope` <- ifelse(SM_MKresult$`Sen's slope` < 0.000000, "Negative", SM_MKresult$`Sen's slope`)
SM_MKresult$`Sen's slope` <- ifelse(SM_MKresult$`Sen's slope` == 0.000000, "No Change", SM_MKresult$`Sen's slope`)

#Before converting to raster, need to convert significance/sign combinations into numeric values
#1 = significant and positive
#2 = insignificant and positive
#3 = marginal and positive
#4 = significant and negative
#5 = insignificant and negative
#6 = marginal and negative
#7 = no change

SM_MKresult$V1 <- ifelse(SM_MKresult$V1 == "Significant" & SM_MKresult$`Sen's slope` =="Positive", 1, SM_MKresult$V1)
SM_MKresult$V1 <- ifelse(SM_MKresult$V1 == "Insignificant" & SM_MKresult$`Sen's slope` =="Positive", 2, SM_MKresult$V1)
SM_MKresult$V1 <- ifelse(SM_MKresult$V1 == "Marginal" & SM_MKresult$`Sen's slope` =="Positive", 3, SM_MKresult$V1)
SM_MKresult$V1 <- ifelse(SM_MKresult$V1 == "Significant" & SM_MKresult$`Sen's slope` =="Negative", 4, SM_MKresult$V1)
SM_MKresult$V1 <- ifelse(SM_MKresult$V1 == "Insignificant" & SM_MKresult$`Sen's slope` =="Negative", 5, SM_MKresult$V1)
SM_MKresult$V1 <- ifelse(SM_MKresult$V1 == "Marginal" & SM_MKresult$`Sen's slope` =="Negative", 6, SM_MKresult$V1)
SM_MKresult$V1 <- ifelse(SM_MKresult$`Sen's slope` == "No Change", 7, SM_MKresult$V1)

table(SM_MKresult$V1)

#Remove the 'slope' column
colnames(SM_MKresult)[4] <- "slope"
SM_MKresult <- subset(SM_MKresult, select = -c(slope) )

#Make a tiff
library(sp)
library(raster)

###Create tables that only include the coordinates and only the MK test result
coords <- subset(SM_MKresult, select = -c(V1) )
MKresult <- subset(SM_MKresult, select = -c(x,y) )

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
#Updated_SM_MKresult_tif <- writeRaster(MKresult_raster, "Updated_SM_MKresult_06June2021", format= "GTiff")
