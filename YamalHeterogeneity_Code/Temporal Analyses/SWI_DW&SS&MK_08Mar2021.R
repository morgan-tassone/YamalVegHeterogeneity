########################Updated SWI Durbin-Watson, Sen's Slope, and Mann-Kendall Tests#############################
setwd("~/UVA/MODIS Timeseries Project/Yamal Data")

#Load in cleaned Max NDVI dataset/coordinates (only pixels with 15+ years of data)
SWI_clean <- readRDS(file = "SWI_Updated/SWI_Clean_08March2021.RDS")
SWI_coords <- readRDS(file = "SWI_Updated/SWI_Clean_Coords_08March2021.RDS")

####Durbin-Watson Test for Serial Autocorrelation
library(pbapply)

#split the data
SWI_lst <- split(SWI_clean, f = SWI_clean$PixelID)

dw_lag1_2sided <- function(x){
  m <- lm(SWI ~ Year, data = x)
  dw <- car::durbinWatsonTest(m, max.lag=1,alternative=c("two.sided", "positive", "negative"))
  c(dw = dw$dw, p = dw$p, r = dw$r)
  #dw$dw
}

dw_lag1_2sided_out <- pbsapply(X = SWI_lst, FUN = dw_lag1_2sided)
dw_lag1_2sided_table <- as.data.frame(dw_lag1_2sided_out)
#transform the table so that dw, p, and r are columns not rows
dw_lag1_2sided_table <- t(dw_lag1_2sided_table)
#convert to a matrix
dw_lag1_2sided_table <- as.data.frame(dw_lag1_2sided_table)

#plot a histogram of the Durbin-Watson stats to see the frequency of pixels that exhibit autocorrelation
hist(dw_lag1_2sided_table$dw,
     main="SWI Durbin-Watson Stats", 
     xlab = "Durbin-Watson Stats",
     breaks=15)

#Determine the number of pixels with significant p-values
sum(dw_lag1_2sided_table$p < 0.05)

#export the table
library(readxl)
#write.table(dw_lag1_2sided_table, file="SWI_dw_lag1_2sided_08March2021.csv", sep=",")

####Calculate Sen's Slope
#Create list of lists - each pixel ID contains a list of 2001-2018 NDVI values
SWI_split <- split(SWI_clean$SWI, SWI_clean$PixelID)

#Create function to make the data into a timeseries and removes NA values
TSobj <- function(x){
  m <- ts(data = x, start = which.min(SWI_clean$Year), end = which.max(SWI_clean$Year), frequency = 1)
  n <- x[!is.na(x)]
}

#Apply the function
SWI.TS <- pblapply(SWI_split, TSobj)

#Save the timeseries list for use in other analyses
##WARNING: Reloading after exporting using rlist can add duplicate pixel IDs
#library(rlist)
#list.save(SWI.TS, file="Updated_SWI_TS_list.RData")
#SWI.TS <- list.load("SWI_Updated/Updated_SWI_TS_list.Rdata")

# library(trend)
# 
# ##Create function
# SS <- function(x){
#   if(any(is.na(x))) c("estimates" = NA)
#   else(sens.slope(x)[c("estimates")])
# }
# 
# SWI.SS <- pblapply(SWI.TS, SS) 
# 
# #Convert the output list into a dataframe
# SWI_SS <- as.data.frame(SWI.SS)
# SWI_SS <- t(SWI_SS)
# SWI_SS <- as.data.frame(SWI_SS)
# 
# ##Plot histogram of slopes
# hist(SWI_SS$`Sen's slope`,
#      main="SWI 2001-2018 Sen's Slope",
#      xlab = "Slope",
#      #xlim = c(-0.010, 0.010),
#      breaks=50)

#Reload SWI SS data
SWI_SS <- readRDS(file = "SWI_Updated/SWI_Slope_10March2021.RDS")

#Determine Sen's slope outliers
summary(SWI_SS$`Sen's slope`)
IQR_val <- 0.28222 - 0.06139
Lower_bound <- 0.06139 - (1.5*0.22083)
Upper_bound <- 0.28222 + (1.5 *0.22083)

#See boxplot
boxplot(SWI_SS$`Sen's slope`,
        ylab = "SWI Sen's Slope"
)

# #See where pixels with slopes > 0.5 Deg C/year are located
# #Add pixel IDs and coordinates to Sen's slope dataset
# SWI_SS_Coords <- cbind(SWI_coords, SWI_SS)
# SWI_SS_GT05 <- SWI_SS_Coords[(SWI_SS_Coords$`Sen's slope` >= 0.5 & SWI_SS_Coords$`Sen's slope` <= 1.0),]
# 
# #Determine # of these pixels that correspond with positive Max NDVI slopes
# MaxNDVI_SS <- readRDS(file = "MaxNDVI_Updated/UpdatedMaxNDVI_Slope_22March2021.RDS")
# MaxNDVI_PixelIDs <- readRDS(file = "MaxNDVI_Updated/Updated_MaxNDVI_Clean_Coords_22March2021.RDS")
# MaxNDVI_SS_PIs <- cbind(MaxNDVI_PixelIDs, MaxNDVI_SS)
# colnames(MaxNDVI_SS_PIs)[4] <- "MaxNDVI"
# 
# SWI_GT05_final <- SWI_SS_GT05[(SWI_SS_GT05$PixelID %in% MaxNDVI_SS_PIs$PixelID),]
# MaxNDVI_GT05_final <- MaxNDVI_SS_PIs[(MaxNDVI_SS_PIs$PixelID %in% SWI_GT05_final$PixelID),]
# 
# MaxNDVI_GT05_final <- MaxNDVI_GT05_final[(MaxNDVI_GT05_final$MaxNDVI > 0.000000),]
# 
# #Create tif
# SWI_SS_GT05 <- as.data.frame(SWI_SS_GT05)
# str(SWI_SS_GT05)
# 
# SWI_SS_GT05result <- subset(SWI_SS_GT05, select = -c(PixelID) )
# colnames(SWI_SS_GT05result)[3] <- "slope"
# 
# library(sp)
# library(raster)
# 
# coords_GT05 <- subset(SWI_SS_GT05result, select = -c(slope) )
# result_GT05 <- subset(SWI_SS_GT05result, select = -c(x,y) )
# 
# coords_GT05 <- as.data.frame(coords_GT05)
# result_GT05 <- as.data.frame(result_GT05)
# 
# str(coords_GT05)
# str(result_GT05) 
# 
# #Create spatial points dataframe object to be used in the rasterize function
# GT05SpatPoints <- SpatialPointsDataFrame(coords_GT05, result_GT05, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
#                                        match.ID = TRUE, bbox = NULL)
# 
# #get extent from existing raster and create a raster for the rasterize function
# MaxNDVI_raster <- raster("MaxNDVI_Updated/MaxNDVI_9999.tif")
# e <- extent(MaxNDVI_raster)
# r <- raster(e, ncol=960, nrow=653, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# GT05result_raster <- rasterize(GT05SpatPoints, r, GT05SpatPoints$slope, fun=mean)
# plot(GT05result_raster)
# 
# library(rgdal)
# GT05result_raster_tif <- writeRaster(GT05result_raster, "SWIslopes_GT05_06June2021", format= "GTiff")


#Export Sen's slope as an R object
#saveRDS(SWI_SS, file="SWI_Slope_10March2021.RDS")

##Export Sens Slope table
#write.table(SWI_SS, file="Updated_SWI_SensSlope_08Mar2021.csv", sep=",")

#####Non-Pre-whitened Mann-Kendall Trend Test
library(Kendall)

#Get p-value (sl)
MK_sl <- function(x){
  mk <- MannKendall(x)[c("sl")]
}

#Apply the function to the timeseries
SWI.MKsl <- pblapply(SWI.TS, MK_sl)

#Convert the output list into a dataframe
SWI_MKsl <- as.data.frame(SWI.MKsl)
SWI_MKsl <- t(SWI_MKsl)
SWI_MKsl <- as.data.frame(SWI_MKsl)

#Plot histogram of p-values.
hist(SWI_MKsl$V1,
     main="SWI Mann-Kendall Test P-values", 
     xlab = "P-value",
     #col="lightcyan3",
     breaks=35)

#Determine the number of pixels with significant p-values
sum(SWI_MKsl$V1 < 0.05)

#Export as R object
#saveRDS(SWI_MKsl, file="SWI_MKsl_10March2021.RDS")

####Pre-whitened Mann-Kendall Test
library(modifiedmk)

#Create the function
MK_newtest <- function(x){
  mks <- mkttest(x)
}

#Apply the function to the timeseries object
SWI.MKout <- pblapply(SWI.TS, MK_newtest)

#Convert the output list into a dataframe
SWI_MKout <- as.data.frame(SWI.MKout)
SWI_MKout <- t(SWI_MKout)
SWI_MKout <- as.data.frame(SWI_MKout)

#Determine the number of pixels with significant p-values
sum(SWI_MKout$`P-value` < 0.05)

#Plot histogram of p-values.
hist(SWI_MKout$`P-value`,
     main="SWI Mann-Kendall Test P-values (New Function)", 
     xlab = "P-value",
     #col="lightcyan3",
     breaks=35)

#Plot histogram of slopes
hist(SWI_MKout$`Sen's slope`,
     main="SWI Sen's Slope (New Function)", 
     xlab = "Slope",
     #xlim = c(-0.010, 0.010),
     #col="lightcyan3",
     breaks=50)

#Get same result from both functions.
####Create a Mann-Kendall Results geotiff
#Combine slope, p-value, and coordinates into 1 dataframe
SWI_MKresult <- cbind(SWI_coords, SWI_MKsl, SWI_SS)

str(SWI_MKresult)
SWI_MKresult <- as.data.frame(SWI_MKresult)
str(SWI_MKresult)

SWI_MKresult <- subset(SWI_MKresult, select = -c(PixelID) )

#Round to 3 decimal places 
SWI_MKresult$V1 <- round(SWI_MKresult$V1, digits = 3)
SWI_MKresult$`Sen's slope` <- round(SWI_MKresult$`Sen's slope`, digits = 3)

####Categorize the data
SWI_MKresult$V1 <- ifelse(SWI_MKresult$V1 > 0.050000, "Insignificant", SWI_MKresult$V1)
SWI_MKresult$V1 <- ifelse(SWI_MKresult$V1 < 0.050000, "Significant", SWI_MKresult$V1)
SWI_MKresult$V1 <- ifelse(SWI_MKresult$V1 == 0.050000, "Marginal", SWI_MKresult$V1)
SWI_MKresult$`Sen's slope` <- ifelse(SWI_MKresult$`Sen's slope` > 0.000000, "Positive", SWI_MKresult$`Sen's slope`)
SWI_MKresult$`Sen's slope` <- ifelse(SWI_MKresult$`Sen's slope` < 0.000000, "Negative", SWI_MKresult$`Sen's slope`)
SWI_MKresult$`Sen's slope` <- ifelse(SWI_MKresult$`Sen's slope` == 0.000000, "No Change", SWI_MKresult$`Sen's slope`)

#Before converting to raster, need to convert significance/sign combinations into numeric values
#1 = significant and positive
#2 = insignificant and positive
#3 = marginal and positive
#4 = significant and negative
#5 = insignificant and negative
#6 = marginal and negative
#7 = no change

SWI_MKresult$V1 <- ifelse(SWI_MKresult$V1 == "Significant" & SWI_MKresult$`Sen's slope` =="Positive", 1, SWI_MKresult$V1)
SWI_MKresult$V1 <- ifelse(SWI_MKresult$V1 == "Insignificant" & SWI_MKresult$`Sen's slope` =="Positive", 2, SWI_MKresult$V1)
SWI_MKresult$V1 <- ifelse(SWI_MKresult$V1 == "Marginal" & SWI_MKresult$`Sen's slope` =="Positive", 3, SWI_MKresult$V1)
SWI_MKresult$V1 <- ifelse(SWI_MKresult$V1 == "Significant" & SWI_MKresult$`Sen's slope` =="Negative", 4, SWI_MKresult$V1)
SWI_MKresult$V1 <- ifelse(SWI_MKresult$V1 == "Insignificant" & SWI_MKresult$`Sen's slope` =="Negative", 5, SWI_MKresult$V1)
SWI_MKresult$V1 <- ifelse(SWI_MKresult$V1 == "Marginal" & SWI_MKresult$`Sen's slope` =="Negative", 6, SWI_MKresult$V1)
SWI_MKresult$V1 <- ifelse(SWI_MKresult$`Sen's slope` == "No Change", 7, SWI_MKresult$V1)

SWI_CatTable <- table(SWI_MKresult$V1)
SWI_CatTable

#Remove the 'slope' column
colnames(SWI_MKresult)[4] <- "slope"
SWI_MKresult <- subset(SWI_MKresult, select = -c(slope) )

#Make a tiff
library(sp)
library(raster)

###Create tables that only include the coordinates and only the MK test result
coords <- subset(SWI_MKresult, select = -c(V1) )
MKresult <- subset(SWI_MKresult, select = -c(x,y) )

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
Updated_SWI_MKresult_tif <- writeRaster(MKresult_raster, "Updated_SWI_MKresult_000slope_21Sept2021", format= "GTiff")

