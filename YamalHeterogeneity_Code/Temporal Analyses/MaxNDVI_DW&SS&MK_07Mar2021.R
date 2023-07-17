########################Updated Max NDVI Durbin-Watson, Sen's Slope, and Mann-Kendall Tests#############################
setwd("~/UVA/MODIS Timeseries Project/Yamal Data")

#Load in cleaned Max NDVI dataset/coordinates (only pixels with 15+ years of data)
MaxNDVI_clean <- readRDS(file = "MaxNDVI_Updated/Updated_MaxNDVI_Clean_22March2021.RDS")
MaxNDVI_coords <- readRDS(file = "MaxNDVI_Updated/Updated_MaxNDVI_Clean_Coords_22March2021.RDS")

####Durbin-Watson Test for Serial Autocorrelation
library(pbapply)

#split the data
MaxNDVI_lst <- split(MaxNDVI_clean, f = MaxNDVI_clean$PixelID)

dw_lag1_2sided <- function(x){
  m <- lm(NDVI ~ Year, data = x)
  dw <- car::durbinWatsonTest(m, max.lag=1,alternative=c("two.sided", "positive", "negative"))
  c(dw = dw$dw, p = dw$p, r = dw$r)
  #dw$dw
}

dw_lag1_2sided_out <- pbsapply(X = MaxNDVI_lst, FUN = dw_lag1_2sided)
dw_lag1_2sided_table <- as.data.frame(dw_lag1_2sided_out)
#transform the table so that dw, p, and r are columns not rows
dw_lag1_2sided_table <- t(dw_lag1_2sided_table)
#convert to a matrix
dw_lag1_2sided_table <- as.data.frame(dw_lag1_2sided_table)

#plot a histogram of the Durbin-Watson stats to see the frequency of pixels that exhibit autocorrelation
hist(dw_lag1_2sided_table$dw,
     main="Max NDVI Durbin-Watson Stats", 
     xlab = "Durbin-Watson Stats",
     breaks=15)

#Determine the number of pixels with significant p-values
sum(dw_lag1_2sided_table$p < 0.05)

#export the table
library(readxl)
#write.table(dw_lag1_2sided_table, file="MaxNDVI_dw_lag1_2sided_07March2021.csv", sep=",")

####Calculate Sen's Slope
#Create list of lists - each pixel ID contains a list of 2001-2018 NDVI values
MaxNDVI_split <- split(MaxNDVI_clean$NDVI, MaxNDVI_clean$PixelID)

#Create function to make the data into a timeseries and removes NA values
TSobj <- function(x){
  m <- ts(data = x, start = which.min(MaxNDVI_clean$Year), end = which.max(MaxNDVI_clean$Year), frequency = 1)
  n <- x[!is.na(x)]
}

#Apply the function
MaxNDVI.TS <- pblapply(MaxNDVI_split, TSobj)

#Save the timeseries list for use in other analyses
library(rlist)
##WARNING: this can add duplicates, better not to use.
#list.save(MaxNDVI.TS, file="Updated_NDVI_TS_list_22Mar2021.RData")
#MaxNDVI.TS <- list.load("MaxNDVI_Updated/Updated_NDVI_TS_list_22Mar2021.Rdata")

library(trend)

##Create function
SS <- function(x){
  if(any(is.na(x))) c("estimates" = NA)
  else(sens.slope(x)[c("estimates")])
}

library(dplyr)
MaxNDVI.SS <- pblapply(MaxNDVI.TS, SS) 

#Convert the output list into a dataframe
MaxNDVI_SS <- as.data.frame(MaxNDVI.SS)
MaxNDVI_SS <- t(MaxNDVI_SS)
MaxNDVI_SS <- as.data.frame(MaxNDVI_SS)

##Plot histogram of slopes
hist(MaxNDVI_SS$`Sen's slope`,
     main="Max NDVI 2001-2018 Sen's Slope",
     xlab = "Slope",
     xlim = c(-0.010, 0.010),
     breaks=50)

##Export Sens Slope table
#write.table(MaxNDVI_SS, file="Updated_MaxNDVI_SensSlope_07Mar2021.csv", sep=",")

#Export as an R object
#saveRDS(MaxNDVI_SS, file="UpdatedMaxNDVI_Slope_22March2021.RDS")

#Reload Max NDVI SS data
MaxNDVI_SS <- readRDS(file = "MaxNDVI_Updated/UpdatedMaxNDVI_Slope_22March2021.RDS")

#####Non-Pre-whitened Mann-Kendall Trend Test
library(Kendall)

#Get p-value (sl)
MK_sl <- function(x){
  mk <- MannKendall(x)[c("sl")]
}

#Apply the function to the timeseries
MaxNDVI.MKsl <- pblapply(MaxNDVI.TS, MK_sl)

#Convert the output list into a dataframe
MaxNDVI_MKsl <- as.data.frame(MaxNDVI.MKsl)
MaxNDVI_MKsl <- t(MaxNDVI_MKsl)
MaxNDVI_MKsl <- as.data.frame(MaxNDVI_MKsl)

#Plot histogram of p-values.
hist(MaxNDVI_MKsl$V1,
     main="Max NDVI Mann-Kendall Test P-values", 
     xlab = "P-value",
     #col="lightcyan3",
     breaks=35)

#Export the data
#write.table(MaxNDVI_MKsl, file="Updated_MaxNDVI_MKpvals_07Mar2021.csv", sep=",")

#Export as an R object
saveRDS(MaxNDVI_MKsl, file="UpdatedMaxNDVI_MKresults_22March2021.RDS")

#Determine the number of pixels with significant p-values
sum(MaxNDVI_MKsl$V1 < 0.05)

####Pre-whitened Mann-Kendall Test
library(modifiedmk)

#Create the function
MK_newtest <- function(x){
  mks <- mkttest(x)
}

#Apply the function to the timeseries object
MaxNDVI.MKout <- pblapply(MaxNDVI.TS, MK_newtest)

#Convert the output list into a dataframe
MaxNDVI_MKout <- as.data.frame(MaxNDVI.MKout)
MaxNDVI_MKout <- t(MaxNDVI_MKout)
MaxNDVI_MKout <- as.data.frame(MaxNDVI_MKout)

#Determine the number of pixels with significant p-values
sum(MaxNDVI_MKout$`P-value` < 0.05)

#Plot histogram of p-values.
hist(MaxNDVI_MKout$`P-value`,
     main="Max NDVI Mann-Kendall Test P-values (New Function)", 
     xlab = "P-value",
     #col="lightcyan3",
     breaks=35)

#Plot histogram of slopes
hist(MaxNDVI_MKout$`Sen's slope`,
     main="Max NDVI Sen's Slope (New Function)", 
     xlab = "Slope",
     xlim = c(-0.010, 0.010),
     #col="lightcyan3",
     breaks=50)

#Combine Sen's slopes from previous and new function and compare difference
BothSlopes <- cbind(MaxNDVI_SS, MaxNDVI_MKout$`Sen's slope`)
colnames(BothSlopes)[1] <- "SlopeMethod1"
colnames(BothSlopes)[2] <- "SlopeMethod2"

BothSlopes$compare <- ifelse(BothSlopes$SlopeMethod1 > BothSlopes$SlopeMethod2, 'Method1Greater',
                    ifelse(BothSlopes$SlopeMethod1 < BothSlopes$SlopeMethod2, 'Method2Greater', 'NoDiff'))

unique(BothSlopes$compare)

#Combine p-values from previous and new function and compare difference
BothPvals <- cbind(MaxNDVI_MKsl$V1, MaxNDVI_MKout$`P-value`)
BothPvals <- as.data.frame(BothPvals)
str(BothPvals)
#BothPvals <- format(BothPvals, scientific = F)
#BothPvals <- as.numeric(BothPvals)
Method1_Pvals <- round(BothPvals[,1], digits = 3)
Method2_Pvals <- round(BothPvals[,2], digits = 3)
BothPvals <- cbind(Method1_Pvals, Method2_Pvals)
colnames(BothPvals)[1] <- "PvalsMethod1"
colnames(BothPvals)[2] <- "PvalsMethod2"
BothPvals <- as.data.frame(BothPvals)

BothPvals$compare <- ifelse(BothPvals$PvalsMethod1 > BothPvals$PvalsMethod2, 'Method1Greater',
                             ifelse(BothPvals$PvalsMethod1 < BothPvals$PvalsMethod2, 'Method2Greater', 'NoDiff'))

unique(BothPvals$compare)
table(BothPvals$compare)

#1,142 differences in p-values, but very small (1 vs. 0.970). Both methods found the same # of trends significant.

####Create a Mann-Kendall Results geotiff
#Combine slope, p-value, and coordinates into 1 dataframe
MaxNDVI_MKresult <- cbind(MaxNDVI_coords, MaxNDVI_MKsl, MaxNDVI_SS)

str(MaxNDVI_MKresult)
MaxNDVI_MKresult <- as.data.frame(MaxNDVI_MKresult)
str(MaxNDVI_MKresult)

MaxNDVI_MKresult <- subset(MaxNDVI_MKresult, select = -c(PixelID) )

#Round to 3 decimal places 
MaxNDVI_MKresult$V1 <- round(MaxNDVI_MKresult$V1, digits = 3)
MaxNDVI_MKresult$`Sen's slope` <- round(MaxNDVI_MKresult$`Sen's slope`, digits = 3)

####Categorize the data
MaxNDVI_MKresult$V1 <- ifelse(MaxNDVI_MKresult$V1 > 0.050000, "Insignificant", MaxNDVI_MKresult$V1)
MaxNDVI_MKresult$V1 <- ifelse(MaxNDVI_MKresult$V1 < 0.050000, "Significant", MaxNDVI_MKresult$V1)
MaxNDVI_MKresult$V1 <- ifelse(MaxNDVI_MKresult$V1 == 0.050000, "Marginal", MaxNDVI_MKresult$V1)
MaxNDVI_MKresult$`Sen's slope` <- ifelse(MaxNDVI_MKresult$`Sen's slope` > 0.000000, "Positive", MaxNDVI_MKresult$`Sen's slope`)
MaxNDVI_MKresult$`Sen's slope` <- ifelse(MaxNDVI_MKresult$`Sen's slope` < 0.000000, "Negative", MaxNDVI_MKresult$`Sen's slope`)
MaxNDVI_MKresult$`Sen's slope` <- ifelse(MaxNDVI_MKresult$`Sen's slope` == 0.000000, "No Change", MaxNDVI_MKresult$`Sen's slope`)

#Before converting to raster, need to convert significance/sign combinations into numeric values
#1 = significant and positive
#2 = insignificant and positive
#3 = marginal and positive
#4 = significant and negative
#5 = insignificant and negative
#6 = marginal and negative
#7 = no change

MaxNDVI_MKresult$V1 <- ifelse(MaxNDVI_MKresult$V1 == "Significant" & MaxNDVI_MKresult$`Sen's slope` =="Positive", 1, MaxNDVI_MKresult$V1)
MaxNDVI_MKresult$V1 <- ifelse(MaxNDVI_MKresult$V1 == "Insignificant" & MaxNDVI_MKresult$`Sen's slope` =="Positive", 2, MaxNDVI_MKresult$V1)
MaxNDVI_MKresult$V1 <- ifelse(MaxNDVI_MKresult$V1 == "Marginal" & MaxNDVI_MKresult$`Sen's slope` =="Positive", 3, MaxNDVI_MKresult$V1)
MaxNDVI_MKresult$V1 <- ifelse(MaxNDVI_MKresult$V1 == "Significant" & MaxNDVI_MKresult$`Sen's slope` =="Negative", 4, MaxNDVI_MKresult$V1)
MaxNDVI_MKresult$V1 <- ifelse(MaxNDVI_MKresult$V1 == "Insignificant" & MaxNDVI_MKresult$`Sen's slope` =="Negative", 5, MaxNDVI_MKresult$V1)
MaxNDVI_MKresult$V1 <- ifelse(MaxNDVI_MKresult$V1 == "Marginal" & MaxNDVI_MKresult$`Sen's slope` =="Negative", 6, MaxNDVI_MKresult$V1)
MaxNDVI_MKresult$V1 <- ifelse(MaxNDVI_MKresult$`Sen's slope` == "No Change", 7, MaxNDVI_MKresult$V1)

#Remove the 'slope' column
colnames(MaxNDVI_MKresult)[4] <- "slope"
MaxNDVI_MKresult <- subset(MaxNDVI_MKresult, select = -c(slope) )

#Make a tiff
library(sp)
library(raster)

###Create tables that only include the coordinates and only the MK test result
coords <- subset(MaxNDVI_MKresult, select = -c(V1) )
MKresult <- subset(MaxNDVI_MKresult, select = -c(x,y) )

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
Updated_MaxNDVI_MKresult_tif <- writeRaster(MKresult_raster, "Updated_MaxNDVI_MKresult_000slope_20Sept2021", format= "GTiff")
