###########################################Data Prep##############################################
setwd("~/UVA/MODIS Timeseries Project/Yamal Data")
library(raster)
library(terra)

####Max NDVI
#Create data tables for each year. Each year should have same # of obs and coordinates
MaxNDVI <- stack("MaxNDVI_Updated/UpdatedMaxNDVI_928m_22Mar2021.tif")

MaxNDVI2001 <- MaxNDVI[[1]]
#plot(MaxNDVI2001)
MaxNDVI2001_tbl <- as.data.frame(MaxNDVI2001, xy = TRUE, cells = TRUE, na.rm = FALSE)
MaxNDVI2001_tbl <- as.data.frame(MaxNDVI2001_tbl)
str(MaxNDVI2001_tbl)

MaxNDVI2002 <- MaxNDVI[[2]]
MaxNDVI2002_tbl <- as.data.frame(MaxNDVI2002, xy = TRUE, cells = TRUE, na.rm = FALSE)
MaxNDVI2002_tbl <- as.data.frame(MaxNDVI2002_tbl)
str(MaxNDVI2002_tbl)

MaxNDVI2003 <- MaxNDVI[[3]]
MaxNDVI2003_tbl <- as.data.frame(MaxNDVI2003, xy = TRUE, cells = TRUE, na.rm = FALSE)
MaxNDVI2003_tbl <- as.data.frame(MaxNDVI2003_tbl)
str(MaxNDVI2003_tbl)

MaxNDVI2004 <- MaxNDVI[[4]]
MaxNDVI2004_tbl <- as.data.frame(MaxNDVI2004, xy = TRUE, cells = TRUE, na.rm = FALSE)
MaxNDVI2004_tbl <- as.data.frame(MaxNDVI2004_tbl)
str(MaxNDVI2004_tbl)

MaxNDVI2005 <- MaxNDVI[[5]]
MaxNDVI2005_tbl <- as.data.frame(MaxNDVI2005, xy = TRUE, cells = TRUE, na.rm = FALSE)
MaxNDVI2005_tbl <- as.data.frame(MaxNDVI2005_tbl)
str(MaxNDVI2005_tbl)

MaxNDVI2006 <- MaxNDVI[[6]]
MaxNDVI2006_tbl <- as.data.frame(MaxNDVI2006, xy = TRUE, cells = TRUE, na.rm = FALSE)
MaxNDVI2006_tbl <- as.data.frame(MaxNDVI2006_tbl)
str(MaxNDVI2006_tbl)

MaxNDVI2007 <- MaxNDVI[[7]]
MaxNDVI2007_tbl <- as.data.frame(MaxNDVI2007, xy = TRUE, cells = TRUE, na.rm = FALSE)
MaxNDVI2007_tbl <- as.data.frame(MaxNDVI2007_tbl)
str(MaxNDVI2007_tbl)

MaxNDVI2008 <- MaxNDVI[[8]]
MaxNDVI2008_tbl <- as.data.frame(MaxNDVI2008, xy = TRUE, cells = TRUE, na.rm = FALSE)
MaxNDVI2008_tbl <- as.data.frame(MaxNDVI2008_tbl)
str(MaxNDVI2008_tbl)

MaxNDVI2009 <- MaxNDVI[[9]]
MaxNDVI2009_tbl <- as.data.frame(MaxNDVI2009, xy = TRUE, cells = TRUE, na.rm = FALSE)
MaxNDVI2009_tbl <- as.data.frame(MaxNDVI2009_tbl)
str(MaxNDVI2009_tbl)

MaxNDVI2010 <- MaxNDVI[[10]]
MaxNDVI2010_tbl <- as.data.frame(MaxNDVI2010, xy = TRUE, cells = TRUE, na.rm = FALSE)
MaxNDVI2010_tbl <- as.data.frame(MaxNDVI2010_tbl)
str(MaxNDVI2010_tbl)

MaxNDVI2011 <- MaxNDVI[[11]]
MaxNDVI2011_tbl <- as.data.frame(MaxNDVI2011, xy = TRUE, cells = TRUE, na.rm = FALSE)
MaxNDVI2011_tbl <- as.data.frame(MaxNDVI2011_tbl)
str(MaxNDVI2011_tbl)

MaxNDVI2012 <- MaxNDVI[[12]]
MaxNDVI2012_tbl <- as.data.frame(MaxNDVI2012, xy = TRUE, cells = TRUE, na.rm = FALSE)
MaxNDVI2012_tbl <- as.data.frame(MaxNDVI2012_tbl)
str(MaxNDVI2012_tbl)

MaxNDVI2013 <- MaxNDVI[[13]]
MaxNDVI2013_tbl <- as.data.frame(MaxNDVI2013, xy = TRUE, cells = TRUE, na.rm = FALSE)
MaxNDVI2013_tbl <- as.data.frame(MaxNDVI2013_tbl)
str(MaxNDVI2013_tbl)

MaxNDVI2014 <- MaxNDVI[[14]]
MaxNDVI2014_tbl <- as.data.frame(MaxNDVI2014, xy = TRUE, cells = TRUE, na.rm = FALSE)
MaxNDVI2014_tbl <- as.data.frame(MaxNDVI2014_tbl)
str(MaxNDVI2005_tbl)

MaxNDVI2015 <- MaxNDVI[[15]]
MaxNDVI2015_tbl <- as.data.frame(MaxNDVI2015, xy = TRUE, cells = TRUE, na.rm = FALSE)
MaxNDVI2015_tbl <- as.data.frame(MaxNDVI2015_tbl)
str(MaxNDVI2015_tbl)

MaxNDVI2016 <- MaxNDVI[[16]]
MaxNDVI2016_tbl <- as.data.frame(MaxNDVI2016, xy = TRUE, cells = TRUE, na.rm = FALSE)
MaxNDVI2016_tbl <- as.data.frame(MaxNDVI2016_tbl)
str(MaxNDVI2016_tbl)

MaxNDVI2017 <- MaxNDVI[[17]]
MaxNDVI2017_tbl <- as.data.frame(MaxNDVI2017, xy = TRUE, cells = TRUE, na.rm = FALSE)
MaxNDVI2017_tbl <- as.data.frame(MaxNDVI2017_tbl)
str(MaxNDVI2017_tbl)

MaxNDVI2018 <- MaxNDVI[[18]]
MaxNDVI2018_tbl <- as.data.frame(MaxNDVI2018, xy = TRUE, cells = TRUE, na.rm = FALSE)
MaxNDVI2018_tbl <- as.data.frame(MaxNDVI2018_tbl)
str(MaxNDVI2018_tbl)

#Add year and pixel ID columns to each table, re-order columns
MaxNDVI2001_tbl$Year <- rep(2001,nrow(MaxNDVI2001_tbl))
MaxNDVI2001_tbl$PixelID <- seq.int(nrow(MaxNDVI2001_tbl))
MaxNDVI2001_tbl <- MaxNDVI2001_tbl[, c(5, 4, 1, 2, 3)]

MaxNDVI2002_tbl$Year <- rep(2002,nrow(MaxNDVI2002_tbl))
MaxNDVI2002_tbl$PixelID <- seq.int(nrow(MaxNDVI2002_tbl))
MaxNDVI2002_tbl <- MaxNDVI2002_tbl[, c(5, 4, 1, 2, 3)]
colnames(MaxNDVI2002_tbl)[5] <- "NDVI"

MaxNDVI2003_tbl$Year <- rep(2003,nrow(MaxNDVI2003_tbl))
MaxNDVI2003_tbl$PixelID <- seq.int(nrow(MaxNDVI2003_tbl))
MaxNDVI2003_tbl <- MaxNDVI2003_tbl[, c(5, 4, 1, 2, 3)]
colnames(MaxNDVI2003_tbl)[5] <- "NDVI"

MaxNDVI2004_tbl$Year <- rep(2004,nrow(MaxNDVI2004_tbl))
MaxNDVI2004_tbl$PixelID <- seq.int(nrow(MaxNDVI2004_tbl))
MaxNDVI2004_tbl <- MaxNDVI2004_tbl[, c(5, 4, 1, 2, 3)]
colnames(MaxNDVI2004_tbl)[5] <- "NDVI"

MaxNDVI2005_tbl$Year <- rep(2005,nrow(MaxNDVI2005_tbl))
MaxNDVI2005_tbl$PixelID <- seq.int(nrow(MaxNDVI2005_tbl))
MaxNDVI2005_tbl <- MaxNDVI2005_tbl[, c(5, 4, 1, 2, 3)]
colnames(MaxNDVI2005_tbl)[5] <- "NDVI"

MaxNDVI2006_tbl$Year <- rep(2006,nrow(MaxNDVI2006_tbl))
MaxNDVI2006_tbl$PixelID <- seq.int(nrow(MaxNDVI2006_tbl))
MaxNDVI2006_tbl <- MaxNDVI2006_tbl[, c(5, 4, 1, 2, 3)]
colnames(MaxNDVI2006_tbl)[5] <- "NDVI"

MaxNDVI2007_tbl$Year <- rep(2007,nrow(MaxNDVI2007_tbl))
MaxNDVI2007_tbl$PixelID <- seq.int(nrow(MaxNDVI2007_tbl))
MaxNDVI2007_tbl <- MaxNDVI2007_tbl[, c(5, 4, 1, 2, 3)]
colnames(MaxNDVI2007_tbl)[5] <- "NDVI"

MaxNDVI2008_tbl$Year <- rep(2008,nrow(MaxNDVI2008_tbl))
MaxNDVI2008_tbl$PixelID <- seq.int(nrow(MaxNDVI2008_tbl))
MaxNDVI2008_tbl <- MaxNDVI2008_tbl[, c(5, 4, 1, 2, 3)]
colnames(MaxNDVI2008_tbl)[5] <- "NDVI"

MaxNDVI2009_tbl$Year <- rep(2009,nrow(MaxNDVI2009_tbl))
MaxNDVI2009_tbl$PixelID <- seq.int(nrow(MaxNDVI2009_tbl))
MaxNDVI2009_tbl <- MaxNDVI2009_tbl[, c(5, 4, 1, 2, 3)]
colnames(MaxNDVI2009_tbl)[5] <- "NDVI"

MaxNDVI2010_tbl$Year <- rep(2010,nrow(MaxNDVI2010_tbl))
MaxNDVI2010_tbl$PixelID <- seq.int(nrow(MaxNDVI2010_tbl))
MaxNDVI2010_tbl <- MaxNDVI2010_tbl[, c(5, 4, 1, 2, 3)]
colnames(MaxNDVI2010_tbl)[5] <- "NDVI"

MaxNDVI2011_tbl$Year <- rep(2011,nrow(MaxNDVI2011_tbl))
MaxNDVI2011_tbl$PixelID <- seq.int(nrow(MaxNDVI2011_tbl))
MaxNDVI2011_tbl <- MaxNDVI2011_tbl[, c(5, 4, 1, 2, 3)]
colnames(MaxNDVI2011_tbl)[5] <- "NDVI"

MaxNDVI2012_tbl$Year <- rep(2012,nrow(MaxNDVI2012_tbl))
MaxNDVI2012_tbl$PixelID <- seq.int(nrow(MaxNDVI2012_tbl))
MaxNDVI2012_tbl <- MaxNDVI2012_tbl[, c(5, 4, 1, 2, 3)]
colnames(MaxNDVI2012_tbl)[5] <- "NDVI"

MaxNDVI2013_tbl$Year <- rep(2013,nrow(MaxNDVI2013_tbl))
MaxNDVI2013_tbl$PixelID <- seq.int(nrow(MaxNDVI2013_tbl))
MaxNDVI2013_tbl <- MaxNDVI2013_tbl[, c(5, 4, 1, 2, 3)]
colnames(MaxNDVI2013_tbl)[5] <- "NDVI"

MaxNDVI2014_tbl$Year <- rep(2014,nrow(MaxNDVI2014_tbl))
MaxNDVI2014_tbl$PixelID <- seq.int(nrow(MaxNDVI2014_tbl))
MaxNDVI2014_tbl <- MaxNDVI2014_tbl[, c(5, 4, 1, 2, 3)]
colnames(MaxNDVI2014_tbl)[5] <- "NDVI"

MaxNDVI2015_tbl$Year <- rep(2015,nrow(MaxNDVI2015_tbl))
MaxNDVI2015_tbl$PixelID <- seq.int(nrow(MaxNDVI2015_tbl))
MaxNDVI2015_tbl <- MaxNDVI2015_tbl[, c(5, 4, 1, 2, 3)]
colnames(MaxNDVI2015_tbl)[5] <- "NDVI"

MaxNDVI2016_tbl$Year <- rep(2016,nrow(MaxNDVI2016_tbl))
MaxNDVI2016_tbl$PixelID <- seq.int(nrow(MaxNDVI2016_tbl))
MaxNDVI2016_tbl <- MaxNDVI2016_tbl[, c(5, 4, 1, 2, 3)]
colnames(MaxNDVI2016_tbl)[5] <- "NDVI"

MaxNDVI2017_tbl$Year <- rep(2017,nrow(MaxNDVI2017_tbl))
MaxNDVI2017_tbl$PixelID <- seq.int(nrow(MaxNDVI2017_tbl))
MaxNDVI2017_tbl <- MaxNDVI2017_tbl[, c(5, 4, 1, 2, 3)]
colnames(MaxNDVI2017_tbl)[5] <- "NDVI"

MaxNDVI2018_tbl$Year <- rep(2018,nrow(MaxNDVI2018_tbl))
MaxNDVI2018_tbl$PixelID <- seq.int(nrow(MaxNDVI2018_tbl))
MaxNDVI2018_tbl <- MaxNDVI2018_tbl[, c(5, 4, 1, 2, 3)]
colnames(MaxNDVI2018_tbl)[5] <- "NDVI"

MaxNDVI_joined <- rbind(MaxNDVI2001_tbl, MaxNDVI2002_tbl, MaxNDVI2003_tbl, MaxNDVI2004_tbl, MaxNDVI2005_tbl, 
                        MaxNDVI2006_tbl, MaxNDVI2007_tbl, MaxNDVI2008_tbl, MaxNDVI2009_tbl, MaxNDVI2010_tbl, 
                        MaxNDVI2011_tbl, MaxNDVI2012_tbl, MaxNDVI2013_tbl, MaxNDVI2014_tbl, MaxNDVI2015_tbl, 
                        MaxNDVI2016_tbl, MaxNDVI2017_tbl, MaxNDVI2018_tbl)

str(MaxNDVI_joined)

#####sort the data by pixel ID, this results in a timeseries for each pixel.######
MaxNDVI_sorted <- MaxNDVI_joined[order(MaxNDVI_joined$PixelID),]

str(MaxNDVI_sorted)

library(dplyr)

#Remove pixels with timeseries missing more than 3 years of data
MaxNDVI_clean <- MaxNDVI_sorted %>%
  group_by(PixelID) %>%
  filter(sum(!is.na(NDVI))>=15)

str(MaxNDVI_clean) #the MaxNDVI_clean dataset is a tibble, so convert to a dataframe.

MaxNDVI_clean <- as.data.frame(MaxNDVI_clean)
str(MaxNDVI_clean)

#Create table with coordinates of each pixel included in the clean dataset
MaxNDVI_coords <- MaxNDVI_clean %>%
  group_by(x, y) %>%
  distinct(PixelID)

#Save files to reload when needed
saveRDS(MaxNDVI_clean, file="Updated_MaxNDVI_Clean_22March2021.RDS")
saveRDS(MaxNDVI_coords, file="Updated_MaxNDVI_Clean_Coords_22March2021.RDS")

#Test reload
#Reload_test <- readRDS(file = "MaxNDVI_Clean_06March2021.RDS")

####TI-NDVI
#Create data tables for each year. Each year should have same # of obs and coordinates
TINDVI <- stack("TI-NDVI_0.05_Updated/TINDVI_Updated_928m_27Feb2021.tif")

TINDVI2001 <- TINDVI[[1]]
TINDVI2001_tbl <- as.data.frame(TINDVI2001, xy = TRUE, cells = TRUE, na.rm = FALSE)
TINDVI2001_tbl <- as.data.frame(TINDVI2001_tbl)
str(TINDVI2001_tbl)

TINDVI2002 <- TINDVI[[2]]
TINDVI2002_tbl <- as.data.frame(TINDVI2002, xy = TRUE, cells = TRUE, na.rm = FALSE)
TINDVI2002_tbl <- as.data.frame(TINDVI2002_tbl)
str(TINDVI2002_tbl)

TINDVI2003 <- TINDVI[[3]]
TINDVI2003_tbl <- as.data.frame(TINDVI2003, xy = TRUE, cells = TRUE, na.rm = FALSE)
TINDVI2003_tbl <- as.data.frame(TINDVI2003_tbl)
str(TINDVI2003_tbl)

TINDVI2004 <- TINDVI[[4]]
TINDVI2004_tbl <- as.data.frame(TINDVI2004, xy = TRUE, cells = TRUE, na.rm = FALSE)
TINDVI2004_tbl <- as.data.frame(TINDVI2004_tbl)
str(TINDVI2004_tbl)

TINDVI2005 <- TINDVI[[5]]
TINDVI2005_tbl <- as.data.frame(TINDVI2005, xy = TRUE, cells = TRUE, na.rm = FALSE)
TINDVI2005_tbl <- as.data.frame(TINDVI2005_tbl)
str(TINDVI2005_tbl)

TINDVI2006 <- TINDVI[[6]]
TINDVI2006_tbl <- as.data.frame(TINDVI2006, xy = TRUE, cells = TRUE, na.rm = FALSE)
TINDVI2006_tbl <- as.data.frame(TINDVI2006_tbl)
str(TINDVI2006_tbl)

TINDVI2007 <- TINDVI[[7]]
TINDVI2007_tbl <- as.data.frame(TINDVI2007, xy = TRUE, cells = TRUE, na.rm = FALSE)
TINDVI2007_tbl <- as.data.frame(TINDVI2007_tbl)
str(TINDVI2007_tbl)

TINDVI2008 <- TINDVI[[8]]
TINDVI2008_tbl <- as.data.frame(TINDVI2008, xy = TRUE, cells = TRUE, na.rm = FALSE)
TINDVI2008_tbl <- as.data.frame(TINDVI2008_tbl)
str(TINDVI2008_tbl)

TINDVI2009 <- TINDVI[[9]]
TINDVI2009_tbl <- as.data.frame(TINDVI2009, xy = TRUE, cells = TRUE, na.rm = FALSE)
TINDVI2009_tbl <- as.data.frame(TINDVI2009_tbl)
str(TINDVI2009_tbl)

TINDVI2010 <- TINDVI[[10]]
TINDVI2010_tbl <- as.data.frame(TINDVI2010, xy = TRUE, cells = TRUE, na.rm = FALSE)
TINDVI2010_tbl <- as.data.frame(TINDVI2010_tbl)
str(TINDVI2010_tbl)

TINDVI2011 <- TINDVI[[11]]
TINDVI2011_tbl <- as.data.frame(TINDVI2011, xy = TRUE, cells = TRUE, na.rm = FALSE)
TINDVI2011_tbl <- as.data.frame(TINDVI2011_tbl)
str(TINDVI2011_tbl)

TINDVI2012 <- TINDVI[[12]]
TINDVI2012_tbl <- as.data.frame(TINDVI2012, xy = TRUE, cells = TRUE, na.rm = FALSE)
TINDVI2012_tbl <- as.data.frame(TINDVI2012_tbl)
str(TINDVI2012_tbl)

TINDVI2013 <- TINDVI[[13]]
TINDVI2013_tbl <- as.data.frame(TINDVI2013, xy = TRUE, cells = TRUE, na.rm = FALSE)
TINDVI2013_tbl <- as.data.frame(TINDVI2013_tbl)
str(TINDVI2013_tbl)

TINDVI2014 <- TINDVI[[14]]
TINDVI2014_tbl <- as.data.frame(TINDVI2014, xy = TRUE, cells = TRUE, na.rm = FALSE)
TINDVI2014_tbl <- as.data.frame(TINDVI2014_tbl)
str(TINDVI2005_tbl)

TINDVI2015 <- TINDVI[[15]]
TINDVI2015_tbl <- as.data.frame(TINDVI2015, xy = TRUE, cells = TRUE, na.rm = FALSE)
TINDVI2015_tbl <- as.data.frame(TINDVI2015_tbl)
str(TINDVI2015_tbl)

TINDVI2016 <- TINDVI[[16]]
TINDVI2016_tbl <- as.data.frame(TINDVI2016, xy = TRUE, cells = TRUE, na.rm = FALSE)
TINDVI2016_tbl <- as.data.frame(TINDVI2016_tbl)
str(TINDVI2016_tbl)

TINDVI2017 <- TINDVI[[17]]
TINDVI2017_tbl <- as.data.frame(TINDVI2017, xy = TRUE, cells = TRUE, na.rm = FALSE)
TINDVI2017_tbl <- as.data.frame(TINDVI2017_tbl)
str(TINDVI2017_tbl)

TINDVI2018 <- TINDVI[[18]]
TINDVI2018_tbl <- as.data.frame(TINDVI2018, xy = TRUE, cells = TRUE, na.rm = FALSE)
TINDVI2018_tbl <- as.data.frame(TINDVI2018_tbl)
str(TINDVI2018_tbl)

#Add year and pixel ID columns to each table, re-order columns
TINDVI2001_tbl$Year <- rep(2001,nrow(TINDVI2001_tbl))
TINDVI2001_tbl$PixelID <- seq.int(nrow(TINDVI2001_tbl))
TINDVI2001_tbl <- TINDVI2001_tbl[, c(5, 4, 1, 2, 3)]
colnames(TINDVI2001_tbl)[5] <- "TINDVI"

TINDVI2002_tbl$Year <- rep(2002,nrow(TINDVI2002_tbl))
TINDVI2002_tbl$PixelID <- seq.int(nrow(TINDVI2002_tbl))
TINDVI2002_tbl <- TINDVI2002_tbl[, c(5, 4, 1, 2, 3)]
colnames(TINDVI2002_tbl)[5] <- "TINDVI"

TINDVI2003_tbl$Year <- rep(2003,nrow(TINDVI2003_tbl))
TINDVI2003_tbl$PixelID <- seq.int(nrow(TINDVI2003_tbl))
TINDVI2003_tbl <- TINDVI2003_tbl[, c(5, 4, 1, 2, 3)]
colnames(TINDVI2003_tbl)[5] <- "TINDVI"

TINDVI2004_tbl$Year <- rep(2004,nrow(TINDVI2004_tbl))
TINDVI2004_tbl$PixelID <- seq.int(nrow(TINDVI2004_tbl))
TINDVI2004_tbl <- TINDVI2004_tbl[, c(5, 4, 1, 2, 3)]
colnames(TINDVI2004_tbl)[5] <- "TINDVI"

TINDVI2005_tbl$Year <- rep(2005,nrow(TINDVI2005_tbl))
TINDVI2005_tbl$PixelID <- seq.int(nrow(TINDVI2005_tbl))
TINDVI2005_tbl <- TINDVI2005_tbl[, c(5, 4, 1, 2, 3)]
colnames(TINDVI2005_tbl)[5] <- "TINDVI"

TINDVI2006_tbl$Year <- rep(2006,nrow(TINDVI2006_tbl))
TINDVI2006_tbl$PixelID <- seq.int(nrow(TINDVI2006_tbl))
TINDVI2006_tbl <- TINDVI2006_tbl[, c(5, 4, 1, 2, 3)]
colnames(TINDVI2006_tbl)[5] <- "TINDVI"

TINDVI2007_tbl$Year <- rep(2007,nrow(TINDVI2007_tbl))
TINDVI2007_tbl$PixelID <- seq.int(nrow(TINDVI2007_tbl))
TINDVI2007_tbl <- TINDVI2007_tbl[, c(5, 4, 1, 2, 3)]
colnames(TINDVI2007_tbl)[5] <- "TINDVI"

TINDVI2008_tbl$Year <- rep(2008,nrow(TINDVI2008_tbl))
TINDVI2008_tbl$PixelID <- seq.int(nrow(TINDVI2008_tbl))
TINDVI2008_tbl <- TINDVI2008_tbl[, c(5, 4, 1, 2, 3)]
colnames(TINDVI2008_tbl)[5] <- "TINDVI"

TINDVI2009_tbl$Year <- rep(2009,nrow(TINDVI2009_tbl))
TINDVI2009_tbl$PixelID <- seq.int(nrow(TINDVI2009_tbl))
TINDVI2009_tbl <- TINDVI2009_tbl[, c(5, 4, 1, 2, 3)]
colnames(TINDVI2009_tbl)[5] <- "TINDVI"

TINDVI2010_tbl$Year <- rep(2010,nrow(TINDVI2010_tbl))
TINDVI2010_tbl$PixelID <- seq.int(nrow(TINDVI2010_tbl))
TINDVI2010_tbl <- TINDVI2010_tbl[, c(5, 4, 1, 2, 3)]
colnames(TINDVI2010_tbl)[5] <- "TINDVI"

TINDVI2011_tbl$Year <- rep(2011,nrow(TINDVI2011_tbl))
TINDVI2011_tbl$PixelID <- seq.int(nrow(TINDVI2011_tbl))
TINDVI2011_tbl <- TINDVI2011_tbl[, c(5, 4, 1, 2, 3)]
colnames(TINDVI2011_tbl)[5] <- "TINDVI"

TINDVI2012_tbl$Year <- rep(2012,nrow(TINDVI2012_tbl))
TINDVI2012_tbl$PixelID <- seq.int(nrow(TINDVI2012_tbl))
TINDVI2012_tbl <- TINDVI2012_tbl[, c(5, 4, 1, 2, 3)]
colnames(TINDVI2012_tbl)[5] <- "TINDVI"

TINDVI2013_tbl$Year <- rep(2013,nrow(TINDVI2013_tbl))
TINDVI2013_tbl$PixelID <- seq.int(nrow(TINDVI2013_tbl))
TINDVI2013_tbl <- TINDVI2013_tbl[, c(5, 4, 1, 2, 3)]
colnames(TINDVI2013_tbl)[5] <- "TINDVI"

TINDVI2014_tbl$Year <- rep(2014,nrow(TINDVI2014_tbl))
TINDVI2014_tbl$PixelID <- seq.int(nrow(TINDVI2014_tbl))
TINDVI2014_tbl <- TINDVI2014_tbl[, c(5, 4, 1, 2, 3)]
colnames(TINDVI2014_tbl)[5] <- "TINDVI"

TINDVI2015_tbl$Year <- rep(2015,nrow(TINDVI2015_tbl))
TINDVI2015_tbl$PixelID <- seq.int(nrow(TINDVI2015_tbl))
TINDVI2015_tbl <- TINDVI2015_tbl[, c(5, 4, 1, 2, 3)]
colnames(TINDVI2015_tbl)[5] <- "TINDVI"

TINDVI2016_tbl$Year <- rep(2016,nrow(TINDVI2016_tbl))
TINDVI2016_tbl$PixelID <- seq.int(nrow(TINDVI2016_tbl))
TINDVI2016_tbl <- TINDVI2016_tbl[, c(5, 4, 1, 2, 3)]
colnames(TINDVI2016_tbl)[5] <- "TINDVI"

TINDVI2017_tbl$Year <- rep(2017,nrow(TINDVI2017_tbl))
TINDVI2017_tbl$PixelID <- seq.int(nrow(TINDVI2017_tbl))
TINDVI2017_tbl <- TINDVI2017_tbl[, c(5, 4, 1, 2, 3)]
colnames(TINDVI2017_tbl)[5] <- "TINDVI"

TINDVI2018_tbl$Year <- rep(2018,nrow(TINDVI2018_tbl))
TINDVI2018_tbl$PixelID <- seq.int(nrow(TINDVI2018_tbl))
TINDVI2018_tbl <- TINDVI2018_tbl[, c(5, 4, 1, 2, 3)]
colnames(TINDVI2018_tbl)[5] <- "TINDVI"

TINDVI_joined <- rbind(TINDVI2001_tbl, TINDVI2002_tbl, TINDVI2003_tbl, TINDVI2004_tbl, TINDVI2005_tbl, 
                        TINDVI2006_tbl, TINDVI2007_tbl, TINDVI2008_tbl, TINDVI2009_tbl, TINDVI2010_tbl, 
                        TINDVI2011_tbl, TINDVI2012_tbl, TINDVI2013_tbl, TINDVI2014_tbl, TINDVI2015_tbl, 
                        TINDVI2016_tbl, TINDVI2017_tbl, TINDVI2018_tbl)

str(TINDVI_joined)

#####sort the data by pixel ID, this results in a timeseries for each pixel.######
TINDVI_sorted <- TINDVI_joined[order(TINDVI_joined$PixelID),]

str(TINDVI_sorted)

library(dplyr)

#Remove pixels with timeseries missing more than 3 years of data
TINDVI_clean <- TINDVI_sorted %>%
  group_by(PixelID) %>%
  filter(sum(!is.na(TINDVI))>=15)

str(TINDVI_clean) #the TINDVI_clean dataset is a tibble, so convert to a dataframe.

TINDVI_clean <- as.data.frame(TINDVI_clean)
str(TINDVI_clean)

#Create table with coordinates of each pixel included in the clean dataset
TINDVI_coords <- TINDVI_clean %>%
  group_by(x, y) %>%
  distinct(PixelID)

#Save files to reload when needed
saveRDS(TINDVI_clean, file="TINDVI_Clean_27Feb2022.RDS")
saveRDS(TINDVI_coords, file="TINDVI_Clean_Coords_27Feb2022.RDS")

####SWI
SWI <- stack("SWI_Updated/SWI_2001-2018_14Oct2020.tif")
SWI2001 <- SWI[[1]]
SWI2001_tbl <- as.data.frame(SWI2001, xy = TRUE, cells = TRUE, na.rm = FALSE)
SWI2001_tbl <- as.data.frame(SWI2001_tbl)
str(SWI2001_tbl)

SWI2002 <- SWI[[2]]
SWI2002_tbl <- as.data.frame(SWI2002, xy = TRUE, cells = TRUE, na.rm = FALSE)
SWI2002_tbl <- as.data.frame(SWI2002_tbl)
str(SWI2002_tbl)

SWI2003 <- SWI[[3]]
SWI2003_tbl <- as.data.frame(SWI2003, xy = TRUE, cells = TRUE, na.rm = FALSE)
SWI2003_tbl <- as.data.frame(SWI2003_tbl)
str(SWI2003_tbl)

SWI2004 <- SWI[[4]]
SWI2004_tbl <- as.data.frame(SWI2004, xy = TRUE, cells = TRUE, na.rm = FALSE)
SWI2004_tbl <- as.data.frame(SWI2004_tbl)
str(SWI2004_tbl)

SWI2005 <- SWI[[5]]
SWI2005_tbl <- as.data.frame(SWI2005, xy = TRUE, cells = TRUE, na.rm = FALSE)
SWI2005_tbl <- as.data.frame(SWI2005_tbl)
str(SWI2005_tbl)

SWI2006 <- SWI[[6]]
SWI2006_tbl <- as.data.frame(SWI2006, xy = TRUE, cells = TRUE, na.rm = FALSE)
SWI2006_tbl <- as.data.frame(SWI2006_tbl)
str(SWI2006_tbl)

SWI2007 <- SWI[[7]]
SWI2007_tbl <- as.data.frame(SWI2007, xy = TRUE, cells = TRUE, na.rm = FALSE)
SWI2007_tbl <- as.data.frame(SWI2007_tbl)
str(SWI2007_tbl)

SWI2008 <- SWI[[8]]
SWI2008_tbl <- as.data.frame(SWI2008, xy = TRUE, cells = TRUE, na.rm = FALSE)
SWI2008_tbl <- as.data.frame(SWI2008_tbl)
str(SWI2008_tbl)

SWI2009 <- SWI[[9]]
SWI2009_tbl <- as.data.frame(SWI2009, xy = TRUE, cells = TRUE, na.rm = FALSE)
SWI2009_tbl <- as.data.frame(SWI2009_tbl)
str(SWI2009_tbl)

SWI2010 <- SWI[[10]]
SWI2010_tbl <- as.data.frame(SWI2010, xy = TRUE, cells = TRUE, na.rm = FALSE)
SWI2010_tbl <- as.data.frame(SWI2010_tbl)
str(SWI2010_tbl)

SWI2011 <- SWI[[11]]
SWI2011_tbl <- as.data.frame(SWI2011, xy = TRUE, cells = TRUE, na.rm = FALSE)
SWI2011_tbl <- as.data.frame(SWI2011_tbl)
str(SWI2011_tbl)

SWI2012 <- SWI[[12]]
SWI2012_tbl <- as.data.frame(SWI2012, xy = TRUE, cells = TRUE, na.rm = FALSE)
SWI2012_tbl <- as.data.frame(SWI2012_tbl)
str(SWI2012_tbl)

SWI2013 <- SWI[[13]]
SWI2013_tbl <- as.data.frame(SWI2013, xy = TRUE, cells = TRUE, na.rm = FALSE)
SWI2013_tbl <- as.data.frame(SWI2013_tbl)
str(SWI2013_tbl)

SWI2014 <- SWI[[14]]
SWI2014_tbl <- as.data.frame(SWI2014, xy = TRUE, cells = TRUE, na.rm = FALSE)
SWI2014_tbl <- as.data.frame(SWI2014_tbl)
str(SWI2014_tbl)

SWI2015 <- SWI[[15]]
SWI2015_tbl <- as.data.frame(SWI2015, xy = TRUE, cells = TRUE, na.rm = FALSE)
SWI2015_tbl <- as.data.frame(SWI2015_tbl)
str(SWI2015_tbl)

SWI2016 <- SWI[[16]]
SWI2016_tbl <- as.data.frame(SWI2016, xy = TRUE, cells = TRUE, na.rm = FALSE)
SWI2016_tbl <- as.data.frame(SWI2016_tbl)
str(SWI2016_tbl)

SWI2017 <- SWI[[17]]
SWI2017_tbl <- as.data.frame(SWI2017, xy = TRUE, cells = TRUE, na.rm = FALSE)
SWI2017_tbl <- as.data.frame(SWI2017_tbl)
str(SWI2017_tbl)

SWI2018 <- SWI[[18]]
SWI2018_tbl <- as.data.frame(SWI2018, xy = TRUE, cells = TRUE, na.rm = FALSE)
SWI2018_tbl <- as.data.frame(SWI2018_tbl)
str(SWI2018_tbl)

#Add year and pixel ID columns to each table, re-order columns
SWI2001_tbl$Year <- rep(2001,nrow(SWI2001_tbl))
SWI2001_tbl$PixelID <- seq.int(nrow(SWI2001_tbl))
SWI2001_tbl <- SWI2001_tbl[, c(5, 4, 1, 2, 3)]
colnames(SWI2001_tbl)[5] <- "SWI"

SWI2002_tbl$Year <- rep(2002,nrow(SWI2002_tbl))
SWI2002_tbl$PixelID <- seq.int(nrow(SWI2002_tbl))
SWI2002_tbl <- SWI2002_tbl[, c(5, 4, 1, 2, 3)]
colnames(SWI2002_tbl)[5] <- "SWI"

SWI2003_tbl$Year <- rep(2003,nrow(SWI2003_tbl))
SWI2003_tbl$PixelID <- seq.int(nrow(SWI2003_tbl))
SWI2003_tbl <- SWI2003_tbl[, c(5, 4, 1, 2, 3)]
colnames(SWI2003_tbl)[5] <- "SWI"

SWI2004_tbl$Year <- rep(2004,nrow(SWI2004_tbl))
SWI2004_tbl$PixelID <- seq.int(nrow(SWI2004_tbl))
SWI2004_tbl <- SWI2004_tbl[, c(5, 4, 1, 2, 3)]
colnames(SWI2004_tbl)[5] <- "SWI"

SWI2005_tbl$Year <- rep(2005,nrow(SWI2005_tbl))
SWI2005_tbl$PixelID <- seq.int(nrow(SWI2005_tbl))
SWI2005_tbl <- SWI2005_tbl[, c(5, 4, 1, 2, 3)]
colnames(SWI2005_tbl)[5] <- "SWI"

SWI2006_tbl$Year <- rep(2006,nrow(SWI2006_tbl))
SWI2006_tbl$PixelID <- seq.int(nrow(SWI2006_tbl))
SWI2006_tbl <- SWI2006_tbl[, c(5, 4, 1, 2, 3)]
colnames(SWI2006_tbl)[5] <- "SWI"

SWI2007_tbl$Year <- rep(2007,nrow(SWI2007_tbl))
SWI2007_tbl$PixelID <- seq.int(nrow(SWI2007_tbl))
SWI2007_tbl <- SWI2007_tbl[, c(5, 4, 1, 2, 3)]
colnames(SWI2007_tbl)[5] <- "SWI"

SWI2008_tbl$Year <- rep(2008,nrow(SWI2008_tbl))
SWI2008_tbl$PixelID <- seq.int(nrow(SWI2008_tbl))
SWI2008_tbl <- SWI2008_tbl[, c(5, 4, 1, 2, 3)]
colnames(SWI2008_tbl)[5] <- "SWI"

SWI2009_tbl$Year <- rep(2009,nrow(SWI2009_tbl))
SWI2009_tbl$PixelID <- seq.int(nrow(SWI2009_tbl))
SWI2009_tbl <- SWI2009_tbl[, c(5, 4, 1, 2, 3)]
colnames(SWI2009_tbl)[5] <- "SWI"

SWI2010_tbl$Year <- rep(2010,nrow(SWI2010_tbl))
SWI2010_tbl$PixelID <- seq.int(nrow(SWI2010_tbl))
SWI2010_tbl <- SWI2010_tbl[, c(5, 4, 1, 2, 3)]
colnames(SWI2010_tbl)[5] <- "SWI"

SWI2011_tbl$Year <- rep(2011,nrow(SWI2011_tbl))
SWI2011_tbl$PixelID <- seq.int(nrow(SWI2011_tbl))
SWI2011_tbl <- SWI2011_tbl[, c(5, 4, 1, 2, 3)]
colnames(SWI2011_tbl)[5] <- "SWI"

SWI2012_tbl$Year <- rep(2012,nrow(SWI2012_tbl))
SWI2012_tbl$PixelID <- seq.int(nrow(SWI2012_tbl))
SWI2012_tbl <- SWI2012_tbl[, c(5, 4, 1, 2, 3)]
colnames(SWI2012_tbl)[5] <- "SWI"

SWI2013_tbl$Year <- rep(2013,nrow(SWI2013_tbl))
SWI2013_tbl$PixelID <- seq.int(nrow(SWI2013_tbl))
SWI2013_tbl <- SWI2013_tbl[, c(5, 4, 1, 2, 3)]
colnames(SWI2013_tbl)[5] <- "SWI"

SWI2014_tbl$Year <- rep(2014,nrow(SWI2014_tbl))
SWI2014_tbl$PixelID <- seq.int(nrow(SWI2014_tbl))
SWI2014_tbl <- SWI2014_tbl[, c(5, 4, 1, 2, 3)]
colnames(SWI2014_tbl)[5] <- "SWI"

SWI2015_tbl$Year <- rep(2015,nrow(SWI2015_tbl))
SWI2015_tbl$PixelID <- seq.int(nrow(SWI2015_tbl))
SWI2015_tbl <- SWI2015_tbl[, c(5, 4, 1, 2, 3)]
colnames(SWI2015_tbl)[5] <- "SWI"

SWI2016_tbl$Year <- rep(2016,nrow(SWI2016_tbl))
SWI2016_tbl$PixelID <- seq.int(nrow(SWI2016_tbl))
SWI2016_tbl <- SWI2016_tbl[, c(5, 4, 1, 2, 3)]
colnames(SWI2016_tbl)[5] <- "SWI"

SWI2017_tbl$Year <- rep(2017,nrow(SWI2017_tbl))
SWI2017_tbl$PixelID <- seq.int(nrow(SWI2017_tbl))
SWI2017_tbl <- SWI2017_tbl[, c(5, 4, 1, 2, 3)]
colnames(SWI2017_tbl)[5] <- "SWI"

SWI2018_tbl$Year <- rep(2018,nrow(SWI2018_tbl))
SWI2018_tbl$PixelID <- seq.int(nrow(SWI2018_tbl))
SWI2018_tbl <- SWI2018_tbl[, c(5, 4, 1, 2, 3)]
colnames(SWI2018_tbl)[5] <- "SWI"

SWI_joined <- rbind(SWI2001_tbl, SWI2002_tbl, SWI2003_tbl, SWI2004_tbl, SWI2005_tbl, 
                        SWI2006_tbl, SWI2007_tbl, SWI2008_tbl, SWI2009_tbl, SWI2010_tbl, 
                        SWI2011_tbl, SWI2012_tbl, SWI2013_tbl, SWI2014_tbl, SWI2015_tbl, 
                        SWI2016_tbl, SWI2017_tbl, SWI2018_tbl)

str(SWI_joined)

#####sort the data by pixel ID, this results in a timeseries for each pixel.######
SWI_sorted <- SWI_joined[order(SWI_joined$PixelID),]

str(SWI_sorted)

#Remove pixels with timeseries missing more than 3 years of data
SWI_clean <- SWI_sorted %>%
  group_by(PixelID) %>%
  filter(sum(!is.na(SWI))>=15)

str(SWI_clean) #convert to a dataframe.

SWI_clean <- as.data.frame(SWI_clean)
str(SWI_clean)

#Create table with coordinates of each pixel included in the clean dataset
SWI_coords <- SWI_clean %>%
  group_by(x, y) %>%
  distinct(PixelID)

#Save files to reload when needed
#saveRDS(SWI_clean, file="SWI_Clean_08March2021.RDS")
#saveRDS(SWI_coords, file="SWI_Clean_Coords_08March2021.RDS")

####MGSP
MGSP <- stack("MGSP_Updated/MAP_1kmH2Omask_15Oct2020.tif")
MGSP2001 <- MGSP[[1]]
MGSP2001_tbl <- as.data.frame(MGSP2001, xy = TRUE, cells = TRUE, na.rm = FALSE)
MGSP2001_tbl <- as.data.frame(MGSP2001_tbl)
str(MGSP2001_tbl)

MGSP2002 <- MGSP[[2]]
MGSP2002_tbl <- as.data.frame(MGSP2002, xy = TRUE, cells = TRUE, na.rm = FALSE)
MGSP2002_tbl <- as.data.frame(MGSP2002_tbl)
str(MGSP2002_tbl)

MGSP2003 <- MGSP[[3]]
MGSP2003_tbl <- as.data.frame(MGSP2003, xy = TRUE, cells = TRUE, na.rm = FALSE)
MGSP2003_tbl <- as.data.frame(MGSP2003_tbl)
str(MGSP2003_tbl)

MGSP2004 <- MGSP[[4]]
MGSP2004_tbl <- as.data.frame(MGSP2004, xy = TRUE, cells = TRUE, na.rm = FALSE)
MGSP2004_tbl <- as.data.frame(MGSP2004_tbl)
str(MGSP2004_tbl)

MGSP2005 <- MGSP[[5]]
MGSP2005_tbl <- as.data.frame(MGSP2005, xy = TRUE, cells = TRUE, na.rm = FALSE)
MGSP2005_tbl <- as.data.frame(MGSP2005_tbl)
str(MGSP2005_tbl)

MGSP2006 <- MGSP[[6]]
MGSP2006_tbl <- as.data.frame(MGSP2006, xy = TRUE, cells = TRUE, na.rm = FALSE)
MGSP2006_tbl <- as.data.frame(MGSP2006_tbl)
str(MGSP2006_tbl)

MGSP2007 <- MGSP[[7]]
MGSP2007_tbl <- as.data.frame(MGSP2007, xy = TRUE, cells = TRUE, na.rm = FALSE)
MGSP2007_tbl <- as.data.frame(MGSP2007_tbl)
str(MGSP2007_tbl)

MGSP2008 <- MGSP[[8]]
MGSP2008_tbl <- as.data.frame(MGSP2008, xy = TRUE, cells = TRUE, na.rm = FALSE)
MGSP2008_tbl <- as.data.frame(MGSP2008_tbl)
str(MGSP2008_tbl)

MGSP2009 <- MGSP[[9]]
MGSP2009_tbl <- as.data.frame(MGSP2009, xy = TRUE, cells = TRUE, na.rm = FALSE)
MGSP2009_tbl <- as.data.frame(MGSP2009_tbl)
str(MGSP2009_tbl)

MGSP2010 <- MGSP[[10]]
MGSP2010_tbl <- as.data.frame(MGSP2010, xy = TRUE, cells = TRUE, na.rm = FALSE)
MGSP2010_tbl <- as.data.frame(MGSP2010_tbl)
str(MGSP2010_tbl)

MGSP2011 <- MGSP[[11]]
MGSP2011_tbl <- as.data.frame(MGSP2011, xy = TRUE, cells = TRUE, na.rm = FALSE)
MGSP2011_tbl <- as.data.frame(MGSP2011_tbl)
str(MGSP2011_tbl)

MGSP2012 <- MGSP[[12]]
MGSP2012_tbl <- as.data.frame(MGSP2012, xy = TRUE, cells = TRUE, na.rm = FALSE)
MGSP2012_tbl <- as.data.frame(MGSP2012_tbl)
str(MGSP2012_tbl)

MGSP2013 <- MGSP[[13]]
MGSP2013_tbl <- as.data.frame(MGSP2013, xy = TRUE, cells = TRUE, na.rm = FALSE)
MGSP2013_tbl <- as.data.frame(MGSP2013_tbl)
str(MGSP2013_tbl)

MGSP2014 <- MGSP[[14]]
MGSP2014_tbl <- as.data.frame(MGSP2014, xy = TRUE, cells = TRUE, na.rm = FALSE)
MGSP2014_tbl <- as.data.frame(MGSP2014_tbl)
str(MGSP2014_tbl)

MGSP2015 <- MGSP[[15]]
MGSP2015_tbl <- as.data.frame(MGSP2015, xy = TRUE, cells = TRUE, na.rm = FALSE)
MGSP2015_tbl <- as.data.frame(MGSP2015_tbl)
str(MGSP2015_tbl)

MGSP2016 <- MGSP[[16]]
MGSP2016_tbl <- as.data.frame(MGSP2016, xy = TRUE, cells = TRUE, na.rm = FALSE)
MGSP2016_tbl <- as.data.frame(MGSP2016_tbl)
str(MGSP2016_tbl)

MGSP2017 <- MGSP[[17]]
MGSP2017_tbl <- as.data.frame(MGSP2017, xy = TRUE, cells = TRUE, na.rm = FALSE)
MGSP2017_tbl <- as.data.frame(MGSP2017_tbl)
str(MGSP2017_tbl)

MGSP2018 <- MGSP[[18]]
MGSP2018_tbl <- as.data.frame(MGSP2018, xy = TRUE, cells = TRUE, na.rm = FALSE)
MGSP2018_tbl <- as.data.frame(MGSP2018_tbl)
str(MGSP2018_tbl)

#Add year and pixel ID columns to each table, re-order columns
MGSP2001_tbl$Year <- rep(2001,nrow(MGSP2001_tbl))
MGSP2001_tbl$PixelID <- seq.int(nrow(MGSP2001_tbl))
MGSP2001_tbl <- MGSP2001_tbl[, c(5, 4, 1, 2, 3)]
colnames(MGSP2001_tbl)[5] <- "MGSP"

MGSP2002_tbl$Year <- rep(2002,nrow(MGSP2002_tbl))
MGSP2002_tbl$PixelID <- seq.int(nrow(MGSP2002_tbl))
MGSP2002_tbl <- MGSP2002_tbl[, c(5, 4, 1, 2, 3)]
colnames(MGSP2002_tbl)[5] <- "MGSP"

MGSP2003_tbl$Year <- rep(2003,nrow(MGSP2003_tbl))
MGSP2003_tbl$PixelID <- seq.int(nrow(MGSP2003_tbl))
MGSP2003_tbl <- MGSP2003_tbl[, c(5, 4, 1, 2, 3)]
colnames(MGSP2003_tbl)[5] <- "MGSP"

MGSP2004_tbl$Year <- rep(2004,nrow(MGSP2004_tbl))
MGSP2004_tbl$PixelID <- seq.int(nrow(MGSP2004_tbl))
MGSP2004_tbl <- MGSP2004_tbl[, c(5, 4, 1, 2, 3)]
colnames(MGSP2004_tbl)[5] <- "MGSP"

MGSP2005_tbl$Year <- rep(2005,nrow(MGSP2005_tbl))
MGSP2005_tbl$PixelID <- seq.int(nrow(MGSP2005_tbl))
MGSP2005_tbl <- MGSP2005_tbl[, c(5, 4, 1, 2, 3)]
colnames(MGSP2005_tbl)[5] <- "MGSP"

MGSP2006_tbl$Year <- rep(2006,nrow(MGSP2006_tbl))
MGSP2006_tbl$PixelID <- seq.int(nrow(MGSP2006_tbl))
MGSP2006_tbl <- MGSP2006_tbl[, c(5, 4, 1, 2, 3)]
colnames(MGSP2006_tbl)[5] <- "MGSP"

MGSP2007_tbl$Year <- rep(2007,nrow(MGSP2007_tbl))
MGSP2007_tbl$PixelID <- seq.int(nrow(MGSP2007_tbl))
MGSP2007_tbl <- MGSP2007_tbl[, c(5, 4, 1, 2, 3)]
colnames(MGSP2007_tbl)[5] <- "MGSP"

MGSP2008_tbl$Year <- rep(2008,nrow(MGSP2008_tbl))
MGSP2008_tbl$PixelID <- seq.int(nrow(MGSP2008_tbl))
MGSP2008_tbl <- MGSP2008_tbl[, c(5, 4, 1, 2, 3)]
colnames(MGSP2008_tbl)[5] <- "MGSP"

MGSP2009_tbl$Year <- rep(2009,nrow(MGSP2009_tbl))
MGSP2009_tbl$PixelID <- seq.int(nrow(MGSP2009_tbl))
MGSP2009_tbl <- MGSP2009_tbl[, c(5, 4, 1, 2, 3)]
colnames(MGSP2009_tbl)[5] <- "MGSP"

MGSP2010_tbl$Year <- rep(2010,nrow(MGSP2010_tbl))
MGSP2010_tbl$PixelID <- seq.int(nrow(MGSP2010_tbl))
MGSP2010_tbl <- MGSP2010_tbl[, c(5, 4, 1, 2, 3)]
colnames(MGSP2010_tbl)[5] <- "MGSP"

MGSP2011_tbl$Year <- rep(2011,nrow(MGSP2011_tbl))
MGSP2011_tbl$PixelID <- seq.int(nrow(MGSP2011_tbl))
MGSP2011_tbl <- MGSP2011_tbl[, c(5, 4, 1, 2, 3)]
colnames(MGSP2011_tbl)[5] <- "MGSP"

MGSP2012_tbl$Year <- rep(2012,nrow(MGSP2012_tbl))
MGSP2012_tbl$PixelID <- seq.int(nrow(MGSP2012_tbl))
MGSP2012_tbl <- MGSP2012_tbl[, c(5, 4, 1, 2, 3)]
colnames(MGSP2012_tbl)[5] <- "MGSP"

MGSP2013_tbl$Year <- rep(2013,nrow(MGSP2013_tbl))
MGSP2013_tbl$PixelID <- seq.int(nrow(MGSP2013_tbl))
MGSP2013_tbl <- MGSP2013_tbl[, c(5, 4, 1, 2, 3)]
colnames(MGSP2013_tbl)[5] <- "MGSP"

MGSP2014_tbl$Year <- rep(2014,nrow(MGSP2014_tbl))
MGSP2014_tbl$PixelID <- seq.int(nrow(MGSP2014_tbl))
MGSP2014_tbl <- MGSP2014_tbl[, c(5, 4, 1, 2, 3)]
colnames(MGSP2014_tbl)[5] <- "MGSP"

MGSP2015_tbl$Year <- rep(2015,nrow(MGSP2015_tbl))
MGSP2015_tbl$PixelID <- seq.int(nrow(MGSP2015_tbl))
MGSP2015_tbl <- MGSP2015_tbl[, c(5, 4, 1, 2, 3)]
colnames(MGSP2015_tbl)[5] <- "MGSP"

MGSP2016_tbl$Year <- rep(2016,nrow(MGSP2016_tbl))
MGSP2016_tbl$PixelID <- seq.int(nrow(MGSP2016_tbl))
MGSP2016_tbl <- MGSP2016_tbl[, c(5, 4, 1, 2, 3)]
colnames(MGSP2016_tbl)[5] <- "MGSP"

MGSP2017_tbl$Year <- rep(2017,nrow(MGSP2017_tbl))
MGSP2017_tbl$PixelID <- seq.int(nrow(MGSP2017_tbl))
MGSP2017_tbl <- MGSP2017_tbl[, c(5, 4, 1, 2, 3)]
colnames(MGSP2017_tbl)[5] <- "MGSP"

MGSP2018_tbl$Year <- rep(2018,nrow(MGSP2018_tbl))
MGSP2018_tbl$PixelID <- seq.int(nrow(MGSP2018_tbl))
MGSP2018_tbl <- MGSP2018_tbl[, c(5, 4, 1, 2, 3)]
colnames(MGSP2018_tbl)[5] <- "MGSP"

MGSP_joined <- rbind(MGSP2001_tbl, MGSP2002_tbl, MGSP2003_tbl, MGSP2004_tbl, MGSP2005_tbl, 
                    MGSP2006_tbl, MGSP2007_tbl, MGSP2008_tbl, MGSP2009_tbl, MGSP2010_tbl, 
                    MGSP2011_tbl, MGSP2012_tbl, MGSP2013_tbl, MGSP2014_tbl, MGSP2015_tbl, 
                    MGSP2016_tbl, MGSP2017_tbl, MGSP2018_tbl)

str(MGSP_joined)

#####sort the data by pixel ID, this results in a timeseries for each pixel.######
MGSP_sorted <- MGSP_joined[order(MGSP_joined$PixelID),]

str(MGSP_sorted)

#Remove pixels with timeseries missing more than 3 years of data
MGSP_clean <- MGSP_sorted %>%
  group_by(PixelID) %>%
  filter(sum(!is.na(MGSP))>=15)

str(MGSP_clean) #convert to a dataframe.

MGSP_clean <- as.data.frame(MGSP_clean)
str(MGSP_clean)

#Create table with coordinates of each pixel included in the clean dataset
MGSP_coords <- MGSP_clean %>%
  group_by(x, y) %>%
  distinct(PixelID)

#Save files to reload when needed
#saveRDS(MGSP_clean, file="MGSP_Clean_08March2021.RDS")
#saveRDS(MGSP_coords, file="MGSP_Clean_Coords_08March2021.RDS")

####SFPO
SFPO <- stack("SFPO_Updated/SFPO_scaled_2000-2018_16Oct2020.tif")
SFPO2001 <- SFPO[[1]]
SFPO2001_tbl <- as.data.frame(SFPO2001, xy = TRUE, cells = TRUE, na.rm = FALSE)
SFPO2001_tbl <- as.data.frame(SFPO2001_tbl)
str(SFPO2001_tbl)

SFPO2002 <- SFPO[[2]]
SFPO2002_tbl <- as.data.frame(SFPO2002, xy = TRUE, cells = TRUE, na.rm = FALSE)
SFPO2002_tbl <- as.data.frame(SFPO2002_tbl)
str(SFPO2002_tbl)

SFPO2003 <- SFPO[[3]]
SFPO2003_tbl <- as.data.frame(SFPO2003, xy = TRUE, cells = TRUE, na.rm = FALSE)
SFPO2003_tbl <- as.data.frame(SFPO2003_tbl)
str(SFPO2003_tbl)

SFPO2004 <- SFPO[[4]]
SFPO2004_tbl <- as.data.frame(SFPO2004, xy = TRUE, cells = TRUE, na.rm = FALSE)
SFPO2004_tbl <- as.data.frame(SFPO2004_tbl)
str(SFPO2004_tbl)

SFPO2005 <- SFPO[[5]]
SFPO2005_tbl <- as.data.frame(SFPO2005, xy = TRUE, cells = TRUE, na.rm = FALSE)
SFPO2005_tbl <- as.data.frame(SFPO2005_tbl)
str(SFPO2005_tbl)

SFPO2006 <- SFPO[[6]]
SFPO2006_tbl <- as.data.frame(SFPO2006, xy = TRUE, cells = TRUE, na.rm = FALSE)
SFPO2006_tbl <- as.data.frame(SFPO2006_tbl)
str(SFPO2006_tbl)

SFPO2007 <- SFPO[[7]]
SFPO2007_tbl <- as.data.frame(SFPO2007, xy = TRUE, cells = TRUE, na.rm = FALSE)
SFPO2007_tbl <- as.data.frame(SFPO2007_tbl)
str(SFPO2007_tbl)

SFPO2008 <- SFPO[[8]]
SFPO2008_tbl <- as.data.frame(SFPO2008, xy = TRUE, cells = TRUE, na.rm = FALSE)
SFPO2008_tbl <- as.data.frame(SFPO2008_tbl)
str(SFPO2008_tbl)

SFPO2009 <- SFPO[[9]]
SFPO2009_tbl <- as.data.frame(SFPO2009, xy = TRUE, cells = TRUE, na.rm = FALSE)
SFPO2009_tbl <- as.data.frame(SFPO2009_tbl)
str(SFPO2009_tbl)

SFPO2010 <- SFPO[[10]]
SFPO2010_tbl <- as.data.frame(SFPO2010, xy = TRUE, cells = TRUE, na.rm = FALSE)
SFPO2010_tbl <- as.data.frame(SFPO2010_tbl)
str(SFPO2010_tbl)

SFPO2011 <- SFPO[[11]]
SFPO2011_tbl <- as.data.frame(SFPO2011, xy = TRUE, cells = TRUE, na.rm = FALSE)
SFPO2011_tbl <- as.data.frame(SFPO2011_tbl)
str(SFPO2011_tbl)

SFPO2012 <- SFPO[[12]]
SFPO2012_tbl <- as.data.frame(SFPO2012, xy = TRUE, cells = TRUE, na.rm = FALSE)
SFPO2012_tbl <- as.data.frame(SFPO2012_tbl)
str(SFPO2012_tbl)

SFPO2013 <- SFPO[[13]]
SFPO2013_tbl <- as.data.frame(SFPO2013, xy = TRUE, cells = TRUE, na.rm = FALSE)
SFPO2013_tbl <- as.data.frame(SFPO2013_tbl)
str(SFPO2013_tbl)

SFPO2014 <- SFPO[[14]]
SFPO2014_tbl <- as.data.frame(SFPO2014, xy = TRUE, cells = TRUE, na.rm = FALSE)
SFPO2014_tbl <- as.data.frame(SFPO2014_tbl)
str(SFPO2014_tbl)

SFPO2015 <- SFPO[[15]]
SFPO2015_tbl <- as.data.frame(SFPO2015, xy = TRUE, cells = TRUE, na.rm = FALSE)
SFPO2015_tbl <- as.data.frame(SFPO2015_tbl)
str(SFPO2015_tbl)

SFPO2016 <- SFPO[[16]]
SFPO2016_tbl <- as.data.frame(SFPO2016, xy = TRUE, cells = TRUE, na.rm = FALSE)
SFPO2016_tbl <- as.data.frame(SFPO2016_tbl)
str(SFPO2016_tbl)

SFPO2017 <- SFPO[[17]]
SFPO2017_tbl <- as.data.frame(SFPO2017, xy = TRUE, cells = TRUE, na.rm = FALSE)
SFPO2017_tbl <- as.data.frame(SFPO2017_tbl)
str(SFPO2017_tbl)

SFPO2018 <- SFPO[[18]]
SFPO2018_tbl <- as.data.frame(SFPO2018, xy = TRUE, cells = TRUE, na.rm = FALSE)
SFPO2018_tbl <- as.data.frame(SFPO2018_tbl)
str(SFPO2018_tbl)

#Add year and pixel ID columns to each table, re-order columns
SFPO2001_tbl$Year <- rep(2001,nrow(SFPO2001_tbl))
SFPO2001_tbl$PixelID <- seq.int(nrow(SFPO2001_tbl))
SFPO2001_tbl <- SFPO2001_tbl[, c(5, 4, 1, 2, 3)]
colnames(SFPO2001_tbl)[5] <- "SFPO"

SFPO2002_tbl$Year <- rep(2002,nrow(SFPO2002_tbl))
SFPO2002_tbl$PixelID <- seq.int(nrow(SFPO2002_tbl))
SFPO2002_tbl <- SFPO2002_tbl[, c(5, 4, 1, 2, 3)]
colnames(SFPO2002_tbl)[5] <- "SFPO"

SFPO2003_tbl$Year <- rep(2003,nrow(SFPO2003_tbl))
SFPO2003_tbl$PixelID <- seq.int(nrow(SFPO2003_tbl))
SFPO2003_tbl <- SFPO2003_tbl[, c(5, 4, 1, 2, 3)]
colnames(SFPO2003_tbl)[5] <- "SFPO"

SFPO2004_tbl$Year <- rep(2004,nrow(SFPO2004_tbl))
SFPO2004_tbl$PixelID <- seq.int(nrow(SFPO2004_tbl))
SFPO2004_tbl <- SFPO2004_tbl[, c(5, 4, 1, 2, 3)]
colnames(SFPO2004_tbl)[5] <- "SFPO"

SFPO2005_tbl$Year <- rep(2005,nrow(SFPO2005_tbl))
SFPO2005_tbl$PixelID <- seq.int(nrow(SFPO2005_tbl))
SFPO2005_tbl <- SFPO2005_tbl[, c(5, 4, 1, 2, 3)]
colnames(SFPO2005_tbl)[5] <- "SFPO"

SFPO2006_tbl$Year <- rep(2006,nrow(SFPO2006_tbl))
SFPO2006_tbl$PixelID <- seq.int(nrow(SFPO2006_tbl))
SFPO2006_tbl <- SFPO2006_tbl[, c(5, 4, 1, 2, 3)]
colnames(SFPO2006_tbl)[5] <- "SFPO"

SFPO2007_tbl$Year <- rep(2007,nrow(SFPO2007_tbl))
SFPO2007_tbl$PixelID <- seq.int(nrow(SFPO2007_tbl))
SFPO2007_tbl <- SFPO2007_tbl[, c(5, 4, 1, 2, 3)]
colnames(SFPO2007_tbl)[5] <- "SFPO"

SFPO2008_tbl$Year <- rep(2008,nrow(SFPO2008_tbl))
SFPO2008_tbl$PixelID <- seq.int(nrow(SFPO2008_tbl))
SFPO2008_tbl <- SFPO2008_tbl[, c(5, 4, 1, 2, 3)]
colnames(SFPO2008_tbl)[5] <- "SFPO"

SFPO2009_tbl$Year <- rep(2009,nrow(SFPO2009_tbl))
SFPO2009_tbl$PixelID <- seq.int(nrow(SFPO2009_tbl))
SFPO2009_tbl <- SFPO2009_tbl[, c(5, 4, 1, 2, 3)]
colnames(SFPO2009_tbl)[5] <- "SFPO"

SFPO2010_tbl$Year <- rep(2010,nrow(SFPO2010_tbl))
SFPO2010_tbl$PixelID <- seq.int(nrow(SFPO2010_tbl))
SFPO2010_tbl <- SFPO2010_tbl[, c(5, 4, 1, 2, 3)]
colnames(SFPO2010_tbl)[5] <- "SFPO"

SFPO2011_tbl$Year <- rep(2011,nrow(SFPO2011_tbl))
SFPO2011_tbl$PixelID <- seq.int(nrow(SFPO2011_tbl))
SFPO2011_tbl <- SFPO2011_tbl[, c(5, 4, 1, 2, 3)]
colnames(SFPO2011_tbl)[5] <- "SFPO"

SFPO2012_tbl$Year <- rep(2012,nrow(SFPO2012_tbl))
SFPO2012_tbl$PixelID <- seq.int(nrow(SFPO2012_tbl))
SFPO2012_tbl <- SFPO2012_tbl[, c(5, 4, 1, 2, 3)]
colnames(SFPO2012_tbl)[5] <- "SFPO"

SFPO2013_tbl$Year <- rep(2013,nrow(SFPO2013_tbl))
SFPO2013_tbl$PixelID <- seq.int(nrow(SFPO2013_tbl))
SFPO2013_tbl <- SFPO2013_tbl[, c(5, 4, 1, 2, 3)]
colnames(SFPO2013_tbl)[5] <- "SFPO"

SFPO2014_tbl$Year <- rep(2014,nrow(SFPO2014_tbl))
SFPO2014_tbl$PixelID <- seq.int(nrow(SFPO2014_tbl))
SFPO2014_tbl <- SFPO2014_tbl[, c(5, 4, 1, 2, 3)]
colnames(SFPO2014_tbl)[5] <- "SFPO"

SFPO2015_tbl$Year <- rep(2015,nrow(SFPO2015_tbl))
SFPO2015_tbl$PixelID <- seq.int(nrow(SFPO2015_tbl))
SFPO2015_tbl <- SFPO2015_tbl[, c(5, 4, 1, 2, 3)]
colnames(SFPO2015_tbl)[5] <- "SFPO"

SFPO2016_tbl$Year <- rep(2016,nrow(SFPO2016_tbl))
SFPO2016_tbl$PixelID <- seq.int(nrow(SFPO2016_tbl))
SFPO2016_tbl <- SFPO2016_tbl[, c(5, 4, 1, 2, 3)]
colnames(SFPO2016_tbl)[5] <- "SFPO"

SFPO2017_tbl$Year <- rep(2017,nrow(SFPO2017_tbl))
SFPO2017_tbl$PixelID <- seq.int(nrow(SFPO2017_tbl))
SFPO2017_tbl <- SFPO2017_tbl[, c(5, 4, 1, 2, 3)]
colnames(SFPO2017_tbl)[5] <- "SFPO"

SFPO2018_tbl$Year <- rep(2018,nrow(SFPO2018_tbl))
SFPO2018_tbl$PixelID <- seq.int(nrow(SFPO2018_tbl))
SFPO2018_tbl <- SFPO2018_tbl[, c(5, 4, 1, 2, 3)]
colnames(SFPO2018_tbl)[5] <- "SFPO"

SFPO_joined <- rbind(SFPO2001_tbl, SFPO2002_tbl, SFPO2003_tbl, SFPO2004_tbl, SFPO2005_tbl, 
                     SFPO2006_tbl, SFPO2007_tbl, SFPO2008_tbl, SFPO2009_tbl, SFPO2010_tbl, 
                     SFPO2011_tbl, SFPO2012_tbl, SFPO2013_tbl, SFPO2014_tbl, SFPO2015_tbl, 
                     SFPO2016_tbl, SFPO2017_tbl, SFPO2018_tbl)

str(SFPO_joined)

#####sort the data by pixel ID, this results in a timeseries for each pixel.######
SFPO_sorted <- SFPO_joined[order(SFPO_joined$PixelID),]

str(SFPO_sorted)

#Remove pixels with timeseries missing more than 3 years of data
SFPO_clean <- SFPO_sorted %>%
  group_by(PixelID) %>%
  filter(sum(!is.na(SFPO))>=15)

str(SFPO_clean) #convert to a dataframe.

SFPO_clean <- as.data.frame(SFPO_clean)
str(SFPO_clean)

#Create table with coordinates of each pixel included in the clean dataset
SFPO_coords <- SFPO_clean %>%
  group_by(x, y) %>%
  distinct(PixelID)

#Save files to reload when needed
#saveRDS(SFPO_clean, file="SFPO_Clean_09March2021.RDS")
#saveRDS(SFPO_coords, file="SFPO_Clean_Coords_09March2021.RDS")

####Soil Moisture
SoilMoist <- stack("SoilMoisture_Updated/SM_H2Omask_final_10Nov2020.tif")
SM2001 <- SoilMoist[[1]]
SM2001_tbl <- as.data.frame(SM2001, xy = TRUE, cells = TRUE, na.rm = FALSE)
SM2001_tbl <- as.data.frame(SM2001_tbl)
str(SM2001_tbl)

SM2002 <- SoilMoist[[2]]
SM2002_tbl <- as.data.frame(SM2002, xy = TRUE, cells = TRUE, na.rm = FALSE)
SM2002_tbl <- as.data.frame(SM2002_tbl)
str(SM2002_tbl)

SM2003 <- SoilMoist[[3]]
SM2003_tbl <- as.data.frame(SM2003, xy = TRUE, cells = TRUE, na.rm = FALSE)
SM2003_tbl <- as.data.frame(SM2003_tbl)
str(SM2003_tbl)

SM2004 <- SoilMoist[[4]]
SM2004_tbl <- as.data.frame(SM2004, xy = TRUE, cells = TRUE, na.rm = FALSE)
SM2004_tbl <- as.data.frame(SM2004_tbl)
str(SM2004_tbl)

SM2004 <- SoilMoist[[4]]
SM2004_tbl <- as.data.frame(SM2004, xy = TRUE, cells = TRUE, na.rm = FALSE)
SM2004_tbl <- as.data.frame(SM2004_tbl)
str(SM2004_tbl)

SM2005 <- SoilMoist[[5]]
SM2005_tbl <- as.data.frame(SM2005, xy = TRUE, cells = TRUE, na.rm = FALSE)
SM2005_tbl <- as.data.frame(SM2005_tbl)
str(SM2005_tbl)

SM2006 <- SoilMoist[[6]]
SM2006_tbl <- as.data.frame(SM2006, xy = TRUE, cells = TRUE, na.rm = FALSE)
SM2006_tbl <- as.data.frame(SM2006_tbl)
str(SM2006_tbl)

SM2007 <- SoilMoist[[7]]
SM2007_tbl <- as.data.frame(SM2007, xy = TRUE, cells = TRUE, na.rm = FALSE)
SM2007_tbl <- as.data.frame(SM2007_tbl)
str(SM2007_tbl)

SM2008 <- SoilMoist[[8]]
SM2008_tbl <- as.data.frame(SM2008, xy = TRUE, cells = TRUE, na.rm = FALSE)
SM2008_tbl <- as.data.frame(SM2008_tbl)
str(SM2008_tbl)

SM2009 <- SoilMoist[[9]]
SM2009_tbl <- as.data.frame(SM2009, xy = TRUE, cells = TRUE, na.rm = FALSE)
SM2009_tbl <- as.data.frame(SM2009_tbl)
str(SM2009_tbl)

SM2010 <- SoilMoist[[10]]
SM2010_tbl <- as.data.frame(SM2010, xy = TRUE, cells = TRUE, na.rm = FALSE)
SM2010_tbl <- as.data.frame(SM2010_tbl)
str(SM2010_tbl)

SM2011 <- SoilMoist[[11]]
SM2011_tbl <- as.data.frame(SM2011, xy = TRUE, cells = TRUE, na.rm = FALSE)
SM2011_tbl <- as.data.frame(SM2011_tbl)
str(SM2011_tbl)

SM2012 <- SoilMoist[[12]]
SM2012_tbl <- as.data.frame(SM2012, xy = TRUE, cells = TRUE, na.rm = FALSE)
SM2012_tbl <- as.data.frame(SM2012_tbl)
str(SM2012_tbl)

SM2013 <- SoilMoist[[13]]
SM2013_tbl <- as.data.frame(SM2013, xy = TRUE, cells = TRUE, na.rm = FALSE)
SM2013_tbl <- as.data.frame(SM2013_tbl)
str(SM2013_tbl)

SM2014 <- SoilMoist[[14]]
SM2014_tbl <- as.data.frame(SM2014, xy = TRUE, cells = TRUE, na.rm = FALSE)
SM2014_tbl <- as.data.frame(SM2014_tbl)
str(SM2014_tbl)

SM2015 <- SoilMoist[[15]]
SM2015_tbl <- as.data.frame(SM2015, xy = TRUE, cells = TRUE, na.rm = FALSE)
SM2015_tbl <- as.data.frame(SM2015_tbl)
str(SM2015_tbl)

SM2016 <- SoilMoist[[16]]
SM2016_tbl <- as.data.frame(SM2016, xy = TRUE, cells = TRUE, na.rm = FALSE)
SM2016_tbl <- as.data.frame(SM2016_tbl)
str(SM2016_tbl)

SM2017 <- SoilMoist[[17]]
SM2017_tbl <- as.data.frame(SM2017, xy = TRUE, cells = TRUE, na.rm = FALSE)
SM2017_tbl <- as.data.frame(SM2017_tbl)
str(SM2017_tbl)

SM2018 <- SoilMoist[[18]]
SM2018_tbl <- as.data.frame(SM2018, xy = TRUE, cells = TRUE, na.rm = FALSE)
SM2018_tbl <- as.data.frame(SM2018_tbl)
str(SM2018_tbl)

#Add year and pixel ID columns to each table, re-order columns
SM2001_tbl$Year <- rep(2001,nrow(SM2001_tbl))
SM2001_tbl$PixelID <- seq.int(nrow(SM2001_tbl))
SM2001_tbl <- SM2001_tbl[, c(5, 4, 1, 2, 3)]
colnames(SM2001_tbl)[5] <- "SM"

SM2002_tbl$Year <- rep(2002,nrow(SM2002_tbl))
SM2002_tbl$PixelID <- seq.int(nrow(SM2002_tbl))
SM2002_tbl <- SM2002_tbl[, c(5, 4, 1, 2, 3)]
colnames(SM2002_tbl)[5] <- "SM"

SM2003_tbl$Year <- rep(2003,nrow(SM2003_tbl))
SM2003_tbl$PixelID <- seq.int(nrow(SM2003_tbl))
SM2003_tbl <- SM2003_tbl[, c(5, 4, 1, 2, 3)]
colnames(SM2003_tbl)[5] <- "SM"

SM2004_tbl$Year <- rep(2004,nrow(SM2004_tbl))
SM2004_tbl$PixelID <- seq.int(nrow(SM2004_tbl))
SM2004_tbl <- SM2004_tbl[, c(5, 4, 1, 2, 3)]
colnames(SM2004_tbl)[5] <- "SM"

SM2005_tbl$Year <- rep(2005,nrow(SM2005_tbl))
SM2005_tbl$PixelID <- seq.int(nrow(SM2005_tbl))
SM2005_tbl <- SM2005_tbl[, c(5, 4, 1, 2, 3)]
colnames(SM2005_tbl)[5] <- "SM"

SM2006_tbl$Year <- rep(2006,nrow(SM2006_tbl))
SM2006_tbl$PixelID <- seq.int(nrow(SM2006_tbl))
SM2006_tbl <- SM2006_tbl[, c(5, 4, 1, 2, 3)]
colnames(SM2006_tbl)[5] <- "SM"

SM2007_tbl$Year <- rep(2007,nrow(SM2007_tbl))
SM2007_tbl$PixelID <- seq.int(nrow(SM2007_tbl))
SM2007_tbl <- SM2007_tbl[, c(5, 4, 1, 2, 3)]
colnames(SM2007_tbl)[5] <- "SM"

SM2008_tbl$Year <- rep(2008,nrow(SM2008_tbl))
SM2008_tbl$PixelID <- seq.int(nrow(SM2008_tbl))
SM2008_tbl <- SM2008_tbl[, c(5, 4, 1, 2, 3)]
colnames(SM2008_tbl)[5] <- "SM"

SM2009_tbl$Year <- rep(2009,nrow(SM2009_tbl))
SM2009_tbl$PixelID <- seq.int(nrow(SM2009_tbl))
SM2009_tbl <- SM2009_tbl[, c(5, 4, 1, 2, 3)]
colnames(SM2009_tbl)[5] <- "SM"

SM2010_tbl$Year <- rep(2010,nrow(SM2010_tbl))
SM2010_tbl$PixelID <- seq.int(nrow(SM2010_tbl))
SM2010_tbl <- SM2010_tbl[, c(5, 4, 1, 2, 3)]
colnames(SM2010_tbl)[5] <- "SM"

SM2011_tbl$Year <- rep(2011,nrow(SM2011_tbl))
SM2011_tbl$PixelID <- seq.int(nrow(SM2011_tbl))
SM2011_tbl <- SM2011_tbl[, c(5, 4, 1, 2, 3)]
colnames(SM2011_tbl)[5] <- "SM"

SM2012_tbl$Year <- rep(2012,nrow(SM2012_tbl))
SM2012_tbl$PixelID <- seq.int(nrow(SM2012_tbl))
SM2012_tbl <- SM2012_tbl[, c(5, 4, 1, 2, 3)]
colnames(SM2012_tbl)[5] <- "SM"

SM2013_tbl$Year <- rep(2013,nrow(SM2013_tbl))
SM2013_tbl$PixelID <- seq.int(nrow(SM2013_tbl))
SM2013_tbl <- SM2013_tbl[, c(5, 4, 1, 2, 3)]
colnames(SM2013_tbl)[5] <- "SM"

SM2014_tbl$Year <- rep(2014,nrow(SM2014_tbl))
SM2014_tbl$PixelID <- seq.int(nrow(SM2014_tbl))
SM2014_tbl <- SM2014_tbl[, c(5, 4, 1, 2, 3)]
colnames(SM2014_tbl)[5] <- "SM"

SM2015_tbl$Year <- rep(2015,nrow(SM2015_tbl))
SM2015_tbl$PixelID <- seq.int(nrow(SM2015_tbl))
SM2015_tbl <- SM2015_tbl[, c(5, 4, 1, 2, 3)]
colnames(SM2015_tbl)[5] <- "SM"

SM2016_tbl$Year <- rep(2016,nrow(SM2016_tbl))
SM2016_tbl$PixelID <- seq.int(nrow(SM2016_tbl))
SM2016_tbl <- SM2016_tbl[, c(5, 4, 1, 2, 3)]
colnames(SM2016_tbl)[5] <- "SM"

SM2017_tbl$Year <- rep(2017,nrow(SM2017_tbl))
SM2017_tbl$PixelID <- seq.int(nrow(SM2017_tbl))
SM2017_tbl <- SM2017_tbl[, c(5, 4, 1, 2, 3)]
colnames(SM2017_tbl)[5] <- "SM"

SM2018_tbl$Year <- rep(2018,nrow(SM2018_tbl))
SM2018_tbl$PixelID <- seq.int(nrow(SM2018_tbl))
SM2018_tbl <- SM2018_tbl[, c(5, 4, 1, 2, 3)]
colnames(SM2018_tbl)[5] <- "SM"

SM_joined <- rbind(SM2001_tbl, SM2002_tbl, SM2003_tbl, SM2004_tbl, SM2005_tbl, 
                   SM2006_tbl, SM2007_tbl, SM2008_tbl, SM2009_tbl, SM2010_tbl, 
                   SM2011_tbl, SM2012_tbl, SM2013_tbl, SM2014_tbl, SM2015_tbl, 
                   SM2016_tbl, SM2017_tbl, SM2018_tbl)

str(SM_joined)

#####sort the data by pixel ID, this results in a timeseries for each pixel.######
SM_sorted <- SM_joined[order(SM_joined$PixelID),]

str(SM_sorted)

#Remove pixels with timeseries missing more than 3 years of data
SM_clean <- SM_sorted %>%
  group_by(PixelID) %>%
  filter(sum(!is.na(SM))>=15)

str(SM_clean) #convert to a dataframe.

SM_clean <- as.data.frame(SM_clean)
str(SM_clean)

#Create table with coordinates of each pixel included in the clean dataset
SM_coords <- SM_clean %>%
  group_by(x, y) %>%
  distinct(PixelID)

#Save files to reload when needed
#saveRDS(SM_clean, file="SM_Clean_09March2021.RDS")
#saveRDS(SM_coords, file="SM_Clean_Coords_09March2021.RDS")

####Elevation
Elev <- stack("TanDEM/DEM500_1kmH2Omask_11Nov2020.tif")
Elev_tbl <- as.data.frame(Elev, xy = TRUE, cells = TRUE, na.rm = FALSE)
Elev_tbl <- as.data.frame(Elev_tbl)
str(Elev_tbl)

Elev_tbl$PixelID <- seq.int(nrow(Elev_tbl))
Elev_tbl <- Elev_tbl[, c(4, 1, 2, 3)]
colnames(Elev_tbl)[4] <- "Elevation"

#saveRDS(Elev_tbl, file="Elev_tbl_09March2021.RDS")

####DistToCoast
CoastDist <- stack("DistToCoast_Updated/DistToCoast_forTable_04Mar2021.tif")
CoastDist_tbl <- as.data.frame(CoastDist, xy = TRUE, cells = TRUE, na.rm = FALSE)
CoastDist_tbl <- as.data.frame(CoastDist_tbl)
str(CoastDist_tbl)

CoastDist_tbl$PixelID <- seq.int(nrow(CoastDist_tbl))
CoastDist_tbl <- CoastDist_tbl[, c(4, 1, 2, 3)]
colnames(CoastDist_tbl)[4] <- "CoastDist"

#saveRDS(CoastDist_tbl, file="CoastDist_tbl_09March2021.RDS")

####Human Mod
HumanMod <- stack("HumanMod/HumanMod_928m_H2Omask_10Nov2020.tif")
HumanMod_tbl <- as.data.frame(HumanMod, xy = TRUE, cells = TRUE, na.rm = FALSE)
HumanMod_tbl <- as.data.frame(HumanMod_tbl)
str(HumanMod_tbl)

HumanMod_tbl$PixelID <- seq.int(nrow(HumanMod_tbl))
HumanMod_tbl <- HumanMod_tbl[, c(4, 1, 2, 3)]
colnames(HumanMod_tbl)[4] <- "HumanMod"

#saveRDS(HumanMod_tbl, file="HumanMod_tbl_09March2021.RDS")

####Land Age
LandAge <- stack("Landscape Age/LandAge_H2Omask_projected_500_28Oct2020.tif")
LandAge_tbl <- as.data.frame(LandAge, xy = TRUE, cells = TRUE, na.rm = FALSE)
LandAge_tbl <- as.data.frame(LandAge_tbl)
str(LandAge_tbl)

LandAge_tbl$PixelID <- seq.int(nrow(LandAge_tbl))
LandAge_tbl <- LandAge_tbl[, c(4, 1, 2, 3)]
colnames(LandAge_tbl)[4] <- "LandAge"

#saveRDS(LandAge_tbl, file="LandAge_tbl_09March2021.RDS")

####Veg Types
VegType <- stack("Veg Types/VegType_H2O_projected_29Oct2020.tif")
VegType_tbl <- as.data.frame(VegType, xy = TRUE, cells = TRUE, na.rm = FALSE)
VegType_tbl <- as.data.frame(VegType_tbl)
str(VegType_tbl)

VegType_tbl$PixelID <- seq.int(nrow(VegType_tbl))
VegType_tbl <- VegType_tbl[, c(4, 1, 2, 3)]
colnames(VegType_tbl)[4] <- "VegType"

#saveRDS(VegType_tbl, file="VegType_tbl_09March2021.RDS")

####Sub Chem
SubChem <- stack("Substrate pH/SubpH_H2Omask_projected_500_28Oct2020.tif")
SubChem_tbl <- as.data.frame(SubChem, xy = TRUE, cells = TRUE, na.rm = FALSE)
SubChem_tbl <- as.data.frame(SubChem_tbl)
str(SubChem_tbl)

SubChem_tbl$PixelID <- seq.int(nrow(SubChem_tbl))
SubChem_tbl <- SubChem_tbl[, c(4, 1, 2, 3)]
colnames(SubChem_tbl)[4] <- "SubChem"

#saveRDS(SubChem_tbl, file="SubChem_tbl_09March2021.RDS")

####Soil Texture
SoilText <- stack("SoilTexture/SoilText_final_04Nov2020.tif")
plot(SoilText)
SoilText_tbl <- as.data.frame(SoilText, xy = TRUE, cells = TRUE, na.rm = FALSE)
SoilText_tbl <- as.data.frame(SoilText)
str(SoilText_tbl)

SoilText_tbl$PixelID <- seq.int(nrow(SoilText_tbl))
SoilText_tbl <- SoilText_tbl[, c(2, 1)]
colnames(SoilText_tbl)[1] <- "SoilTexture"

#saveRDS(SoilText_tbl, file="SoilText_tbl_09March2021.RDS")
