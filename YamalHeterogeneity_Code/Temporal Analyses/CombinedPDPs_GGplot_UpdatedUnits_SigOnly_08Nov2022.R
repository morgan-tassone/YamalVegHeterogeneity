setwd("~/UVA/MODIS Timeseries Project/Yamal Data")

library(randomForest)
library(ggplot2)
library(caret)
library(pdp)

####Max NDVI####
#Load final model
MaxNDVI_RFRmodel <- get(load("RFR_SigOnly/MaxNDVI/MaxNDVI_UpdatedRFRmodel_25boot_SigOnly_07Nov2023.Rdata"))

Rsq <- mean(MaxNDVI_RFRmodel$rsq)

####Max NDVI - Coast Distance (no outliers included)
Max_CoastDist_PDP <- readRDS(file = "RFR_SigOnly/MaxNDVI/MaxNDVI_CoastDist_PDPdata_07Nov2023.RDS")
Max_CoastDist_PDP$CoastDist <- Max_CoastDist_PDP$CoastDist * 0.001
Max_CoastDist_PDP$yhat <- Max_CoastDist_PDP$yhat * 10000

#library(ggplot2)
Max_CoastDist_Plot <- ggplot(Max_CoastDist_PDP, aes(CoastDist, yhat)) +
  geom_line(size = 1) +
  #use stat smooth to get a smooth line between data points
  #stat_smooth(aes(y=yhat, x=CoastDist), method = lm, formula = y ~ poly(x, 10), se = FALSE) +
  labs(x = "Distance from the Coast (km)",
       y = expression(paste("Max NDVI  ",yr^-1," (x ",10^-4,")"))) +
  # scale_y_continuous(breaks = seq(6.0,8.5,0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        panel.grid = element_blank())

plot(Max_CoastDist_Plot)

####Max NDVI - human mod
Max_HumanMod_PDP <- readRDS(file = "RFR_SigOnly/MaxNDVI/MaxNDVI_HumanMod_PDPdata_07Nov2023.RDS")
Max_HumanMod_PDP$yhat <- Max_HumanMod_PDP$yhat * 1000
Max_HumanMod_PDP$HumanMod <- Max_HumanMod_PDP$HumanMod * 100
#add ID points to segment data for graphing
Max_HumanMod_PDP$PointID <- seq.int(nrow(Max_HumanMod_PDP))
Max_HumanMod_PDP$HumanMod <- round(Max_HumanMod_PDP$HumanMod, digits = 3)

MaxNDVI_HumanMod_PosOutliers <- Max_HumanMod_PDP[(Max_HumanMod_PDP$HumanMod > 3.5),]
MaxNDVI_HumanMod_NegOutliers <- Max_HumanMod_PDP[(Max_HumanMod_PDP$HumanMod < 2.0),]
#Keep only data values not included in the above datasets
MaxNDVI_HumanMod_Norm <- Max_HumanMod_PDP[!(Max_HumanMod_PDP$PointID %in% MaxNDVI_HumanMod_PosOutliers$PointID) & 
                                  !(Max_HumanMod_PDP$PointID %in% MaxNDVI_HumanMod_NegOutliers$PointID),]

Max_HumanMod_plot <- ggplot() +
  geom_line(data = MaxNDVI_HumanMod_Norm, aes(x = HumanMod, y = yhat), size = 1, color = "black") +
  geom_line(data = Max_HumanMod_PDP, aes(x = HumanMod, y = yhat), size = 0.25, linetype="longdash", color = "black") +
  #geom_line(data = MaxNDVI_SWI_NegOutliers, aes(x = SummerWarmth, y = yhat), size = 0.5, color = "black") +
  #use stat smooth to get a smooth line between data points
  #stat_smooth(aes(y=yhat, x=CoastDist), method = lm, formula = y ~ poly(x, 10), se = FALSE) +
  labs(x = "Human Modification (% pixel area)",
       y = expression(paste("Max NDVI  ",yr^-1," (x ",10^-3,")"))) +
  #scale_y_continuous(breaks = seq(6.0,8.5,0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = 'black'),
        panel.grid = element_blank())

plot(Max_HumanMod_plot)

####Max NDVI - soil moisture (no outliers included)
Max_SoilMoisture_PDP <- readRDS(file = "RFR_SigOnly/MaxNDVI/MaxNDVI_SoilMoisture_PDPdata_07Nov2023.RDS")
Max_SoilMoisture_PDP$yhat <- Max_SoilMoisture_PDP$yhat * 10000

Max_SM_plot <- ggplot(Max_SoilMoisture_PDP, aes(SoilMoisture, yhat)) +
  geom_line(size = 1) +
  #use stat smooth to get a smooth line between data points
  #stat_smooth(aes(y=yhat, x=CoastDist), method = lm, formula = y ~ poly(x, 10), se = FALSE) +
  labs(x = expression(paste("Soil Moisture (mm ",yr^-1,")")),
       y = expression(paste("Max NDVI  ",yr^-1," (x ",10^-4,")"))) +
  #scale_y_continuous(breaks = seq(6.0,8.5,0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        panel.grid = element_blank())

plot(Max_SM_plot)

####Max NDVI - precipitation
Max_Precip_PDP <- readRDS(file = "RFR_SigOnly/MaxNDVI/MaxNDVI_Precip_PDPdata_07Nov2023.RDS")
Max_Precip_PDP$yhat <- Max_Precip_PDP$yhat * 10000
#add ID points to segment data for graphing
Max_Precip_PDP$PointID <- seq.int(nrow(Max_Precip_PDP))

MaxNDVI_Precip_PosOutliers <- Max_Precip_PDP[(Max_Precip_PDP$Precipitation > 1.410545),]
MaxNDVI_Precip_NegOutliers <- Max_Precip_PDP[(Max_Precip_PDP$Precipitation < -1.525015),]
#Keep only data values not included in the above datasets
MaxNDVI_Precip_Norm <- Max_Precip_PDP[!(Max_Precip_PDP$PointID %in% MaxNDVI_Precip_PosOutliers$PointID) & 
                                  !(Max_Precip_PDP$PointID %in% MaxNDVI_Precip_NegOutliers$PointID),]

Max_Precip_plot <- ggplot() +
  geom_line(data = MaxNDVI_Precip_Norm, aes(x = Precipitation, y = yhat), size = 1, color = "black") +
  geom_line(data = Max_Precip_PDP, aes(x = Precipitation, y = yhat), size = 0.25, linetype="longdash", color = "black") +
  #geom_line(data = MaxNDVI_SWI_NegOutliers, aes(x = SummerWarmth, y = yhat), size = 0.5, color = "black") +
  #use stat smooth to get a smooth line between data points
  #stat_smooth(aes(y=yhat, x=CoastDist), method = lm, formula = y ~ poly(x, 10), se = FALSE) +
  labs(x = expression(paste("Precipitation (mm ",yr^-1,")")),
       y = expression(paste("Max NDVI  ",yr^-1," (x ",10^-4,")"))) +
  #scale_y_continuous(breaks = seq(6.0,8.5,0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = 'black'),
        panel.grid = element_blank())

plot(Max_Precip_plot)

####Max NDVI - elevation
Max_Elevation_PDP <- readRDS(file = "RFR_SigOnly/MaxNDVI/MaxNDVI_Elevation_PDPdata_07Nov2023.RDS")
Max_Elevation_PDP$yhat <- Max_Elevation_PDP$yhat * 10000
#add ID points to segment data for graphing
Max_Elevation_PDP$PointID <- seq.int(nrow(Max_Elevation_PDP))

MaxNDVI_Elevation_PosOutliers <- Max_Elevation_PDP[(Max_Elevation_PDP$Elevation > 65),]
MaxNDVI_Elevation_NegOutliers <- Max_Elevation_PDP[(Max_Elevation_PDP$Elevation < -39),]
#Keep only data values not included in the above datasets
MaxNDVI_Elevation_Norm <- Max_Elevation_PDP[!(Max_Elevation_PDP$PointID %in% MaxNDVI_Elevation_PosOutliers$PointID) & 
                                  !(Max_Elevation_PDP$PointID %in% MaxNDVI_Elevation_NegOutliers$PointID),]

Max_Elev_plot <- ggplot() +
  geom_line(data = MaxNDVI_Elevation_Norm, aes(x = Elevation, y = yhat), size = 1, color = "black") +
  geom_line(data = Max_Elevation_PDP, aes(x = Elevation, y = yhat), size = 0.25, linetype="longdash", color = "black") +
  #geom_line(data = MaxNDVI_SWI_NegOutliers, aes(x = SummerWarmth, y = yhat), size = 0.5, color = "black") +
  #use stat smooth to get a smooth line between data points
  #stat_smooth(aes(y=yhat, x=CoastDist), method = lm, formula = y ~ poly(x, 10), se = FALSE) +
  labs(x = expression(paste("Elevation (m)")),
       y = expression(paste("Max NDVI  ",yr^-1," (x ",10^-4,")"))) +
  #scale_y_continuous(breaks = seq(6.0,8.5,0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = 'black'),
        panel.grid = element_blank())

plot(Max_Elev_plot)

####Max NDVI - SWI
Max_SWI_PDP <- readRDS(file = "RFR_SigOnly/MaxNDVI/MaxNDVI_SummerWarmth_PDPdata_07Nov2023.RDS")
Max_SWI_PDP$yhat <- Max_SWI_PDP$yhat * 1000
#add ID points to segment data for graphing
Max_SWI_PDP$PointID <- seq.int(nrow(Max_SWI_PDP))

MaxNDVI_SWI_PosOutliers <- Max_SWI_PDP[(Max_SWI_PDP$SummerWarmth > 0.57821),]
MaxNDVI_SWI_NegOutliers <- Max_SWI_PDP[(Max_SWI_PDP$SummerWarmth < -0.26987),]
#Keep only data values not included in the above datasets
MaxNDVI_SWI_Norm <- Max_SWI_PDP[!(Max_SWI_PDP$PointID %in% MaxNDVI_SWI_PosOutliers$PointID) & 
                                  !(Max_SWI_PDP$PointID %in% MaxNDVI_SWI_NegOutliers$PointID),]

Max_SWI_plot <- ggplot() +
  geom_line(data = MaxNDVI_SWI_Norm, aes(x = SummerWarmth, y = yhat), size = 1, color = "black") +
  geom_line(data = Max_SWI_PDP, aes(x = SummerWarmth, y = yhat), size = 0.25, linetype="longdash", color = "black") +
  #geom_line(data = MaxNDVI_SWI_NegOutliers, aes(x = SummerWarmth, y = yhat), size = 0.5, color = "black") +
  #use stat smooth to get a smooth line between data points
  #stat_smooth(aes(y=yhat, x=CoastDist), method = lm, formula = y ~ poly(x, 10), se = FALSE) +
  labs(x = expression(Summer~Warmth~Index~Slope~(degree*C~months~yr^-1)),
       y = expression(paste("Max NDVI  ",yr^-1," (x ",10^-3,")"))) +
  #scale_y_continuous(breaks = seq(6.0,8.5,0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = 'black'),
        panel.grid = element_blank())

plot(Max_SWI_plot)

library(ggpubr)

ggarrange(Max_CoastDist_Plot,
          Max_Precip_plot,
          Max_SWI_plot,
          Max_HumanMod_plot,
          Max_Elev_plot,
          Max_SM_plot,
          ncol = 3,
          nrow = 2)

####TI-NDVI####
#Load final model
TINDVI_RFRmodel <- get(load("RFR_SigOnly/TI-NDVI/TI-NDVI_UpdatedRFRmodel_25boot_SigOnly_07Nov2023.Rdata"))


Rsq2 <- mean(TINDVI_RFRmodel$rsq)

####TI-NDVI - Coast Distance (no outliers included)
TI_CoastDist_PDP <- readRDS(file = "RFR_SigOnly/TI-NDVI/TI-NDVI_CoastDist_PDPdata_07Nov2023.RDS")
TI_CoastDist_PDP$CoastDist <- TI_CoastDist_PDP$CoastDist * 0.001
TI_CoastDist_PDP$yhat <- TI_CoastDist_PDP$yhat * 100

TI_CoastDist_Plot <- ggplot(TI_CoastDist_PDP, aes(CoastDist, yhat)) +
  geom_line(size = 1) +
  #use stat smooth to get a smooth line between data points
  #stat_smooth(aes(y=yhat, x=CoastDist), method = lm, formula = y ~ poly(x, 10), se = FALSE) +
  labs(x = "Distance from the Coast (km)",
       y = expression(paste("TI-NDVI  ",yr^-1," (x ",10^-2,")"))) +
  #scale_y_continuous(breaks = seq(6.0,8.5,0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        panel.grid = element_blank())

plot(TI_CoastDist_Plot)

####TI-NDVI - Precip
TI_Precip_PDP <- readRDS(file = "RFR_SigOnly/TI-NDVI/TI-NDVI_Precip_PDPdata_07Nov2023.RDS")
TI_Precip_PDP$yhat <- TI_Precip_PDP$yhat * 100
#add ID points to segment data for graphing
TI_Precip_PDP$PointID <- seq.int(nrow(TI_Precip_PDP))

TINDVI_Precip_PosOutliers <- TI_Precip_PDP[(TI_Precip_PDP$Precipitation > 1.45423),]
TINDVI_Precip_NegOutliers <- TI_Precip_PDP[(TI_Precip_PDP$Precipitation < -1.49762),]
#Keep only data values not included in the above datasets
TINDVI_Precip_Norm <- TI_Precip_PDP[!(TI_Precip_PDP$PointID %in% TINDVI_Precip_PosOutliers$PointID) & 
                                        !(TI_Precip_PDP$PointID %in% TINDVI_Precip_NegOutliers$PointID),]

TI_Precip_plot <- ggplot() +
  geom_line(data = TINDVI_Precip_Norm, aes(x = Precipitation, y = yhat), size = 1, color = "black") +
  geom_line(data = TI_Precip_PDP, aes(x = Precipitation, y = yhat), size = 0.25, linetype="longdash", color = "black") +
  #geom_line(data = MaxNDVI_SWI_NegOutliers, aes(x = SummerWarmth, y = yhat), size = 0.5, color = "black") +
  #use stat smooth to get a smooth line between data points
  #stat_smooth(aes(y=yhat, x=CoastDist), method = lm, formula = y ~ poly(x, 10), se = FALSE) +
  labs(x = expression(paste("Precipitation (mm ",yr^-1,")")),
       y = expression(paste("TI-NDVI  ",yr^-1," (x ",10^-2,")"))) +
  #scale_y_continuous(breaks = seq(6.0,8.5,0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = 'black'),
        panel.grid = element_blank())

plot(TI_Precip_plot)

####TI-NDVI - Elevation
TI_Elevation_PDP <- readRDS(file = "RFR_SigOnly/TI-NDVI/TI-NDVI_Elevation_PDPdata_07Nov2023.RDS")
TI_Elevation_PDP$yhat <- TI_Elevation_PDP$yhat * 100
#add ID points to segment data for graphing
TI_Elevation_PDP$PointID <- seq.int(nrow(TI_Elevation_PDP))

TINDVI_Elevation_PosOutliers <- TI_Elevation_PDP[(TI_Elevation_PDP$Elevation > 55.5),]
TINDVI_Elevation_NegOutliers <- TI_Elevation_PDP[(TI_Elevation_PDP$Elevation < -28.5),]
#Keep only data values not included in the above datasets
TINDVI_Elevation_Norm <- TI_Elevation_PDP[!(TI_Elevation_PDP$PointID %in% TINDVI_Elevation_PosOutliers$PointID) & 
                                              !(TI_Elevation_PDP$PointID %in% TINDVI_Elevation_NegOutliers$PointID),]

TI_Elev_plot <- ggplot() +
  geom_line(data = TINDVI_Elevation_Norm, aes(x = Elevation, y = yhat), size = 1, color = "black") +
  geom_line(data = TI_Elevation_PDP, aes(x = Elevation, y = yhat), size = 0.25, linetype="longdash", color = "black") +
  #geom_line(data = MaxNDVI_SWI_NegOutliers, aes(x = SummerWarmth, y = yhat), size = 0.5, color = "black") +
  #use stat smooth to get a smooth line between data points
  #stat_smooth(aes(y=yhat, x=CoastDist), method = lm, formula = y ~ poly(x, 10), se = FALSE) +
  labs(x = expression(paste("Elevation (m)")),
       y = expression(paste("TI-NDVI  ",yr^-1," (x ",10^-2,")"))) +
  #scale_y_continuous(breaks = seq(6.0,8.5,0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = 'black'),
        panel.grid = element_blank())

plot(TI_Elev_plot)

####TI NDVI - Human mod
TI_HumanMod_PDP <- readRDS(file = "RFR_SigOnly/TI-NDVI/TI-NDVI_HumanMod_PDPdata_07Nov2023.RDS")
TI_HumanMod_PDP$yhat <- TI_HumanMod_PDP$yhat * 100
TI_HumanMod_PDP$HumanMod <- TI_HumanMod_PDP$HumanMod * 100
#add ID points to segment data for graphing
TI_HumanMod_PDP$PointID <- seq.int(nrow(TI_HumanMod_PDP))
TI_HumanMod_PDP$HumanMod <- round(TI_HumanMod_PDP$HumanMod, digits = 3)

TINDVI_HumanMod_PosOutliers <- TI_HumanMod_PDP[(TI_HumanMod_PDP$HumanMod > 3.5),]
TINDVI_HumanMod_NegOutliers <- TI_HumanMod_PDP[(TI_HumanMod_PDP$HumanMod < 2.0),]
#Keep only data values not included in the above datasets
TINDVI_HumanMod_Norm <- TI_HumanMod_PDP[!(TI_HumanMod_PDP$PointID %in% TINDVI_HumanMod_PosOutliers$PointID) & 
                                            !(TI_HumanMod_PDP$PointID %in% TINDVI_HumanMod_NegOutliers$PointID),]

TI_HumanMod_plot <- ggplot() +
  geom_line(data = TINDVI_HumanMod_Norm, aes(x = HumanMod, y = yhat), size = 1.25, color = "black") +
  geom_line(data = TI_HumanMod_PDP, aes(x = HumanMod, y = yhat), size = 0.25, linetype="longdash", color = "black") +
  #geom_line(data = MaxNDVI_SWI_NegOutliers, aes(x = SummerWarmth, y = yhat), size = 0.5, color = "black") +
  #use stat smooth to get a smooth line between data points
  #stat_smooth(aes(y=yhat, x=CoastDist), method = lm, formula = y ~ poly(x, 10), se = FALSE) +
  labs(x = "Human Modification (% pixel area)",
       y = expression(paste("TI-NDVI  ",yr^-1," (x ",10^-2,")"))) +
  #scale_y_continuous(breaks = seq(0.5,2.5,0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = 'black'),
        panel.grid = element_blank())

plot(TI_HumanMod_plot)

####TI-NDVI - SWI
TI_SWI_PDP <- readRDS(file = "RFR_SigOnly/TI-NDVI/TI-NDVI_SummerWarmth_PDPdata_07Nov2023.RDS")
TI_SWI_PDP$yhat <- TI_SWI_PDP$yhat * 100
#add ID points to segment data for graphing
TI_SWI_PDP$PointID <- seq.int(nrow(TI_SWI_PDP))

TINDVI_SWI_PosOutliers <- TI_SWI_PDP[(TI_SWI_PDP$SummerWarmth > 0.61745),]
TINDVI_SWI_NegOutliers <- TI_SWI_PDP[(TI_SWI_PDP$SummerWarmth < -0.20535),]
#Keep only data values not included in the above datasets
TINDVI_SWI_Norm <- TI_SWI_PDP[!(TI_SWI_PDP$PointID %in% TINDVI_SWI_PosOutliers$PointID) & 
                                  !(TI_SWI_PDP$PointID %in% TINDVI_SWI_NegOutliers$PointID),]

TI_SWI_plot <- ggplot() +
  geom_line(data = TINDVI_SWI_Norm, aes(x = SummerWarmth, y = yhat), size = 1, color = "black") +
  geom_line(data = TI_SWI_PDP, aes(x = SummerWarmth, y = yhat), size = 0.25, linetype="longdash", color = "black") +
  #geom_line(data = MaxNDVI_SWI_NegOutliers, aes(x = SummerWarmth, y = yhat), size = 0.5, color = "black") +
  #use stat smooth to get a smooth line between data points
  #stat_smooth(aes(y=yhat, x=CoastDist), method = lm, formula = y ~ poly(x, 10), se = FALSE) +
  labs(x = expression(Summer~Warmth~Index~Slope~(degree*C~months~yr^-1)),
       y = expression(paste("TI-NDVI  ",yr^-1," (x ",10^-2,")"))) +
  #scale_y_continuous(breaks = seq(2.5,4.0,0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = 'black'),
        panel.grid = element_blank())

plot(TI_SWI_plot)

####TI-NDVI - Soil Moisture
TI_SoilMoisture_PDP <- readRDS(file = "RFR_SigOnly/TI-NDVI/TI-NDVI_SoilMoisture_PDPdata_07Nov2023.RDS")
TI_SoilMoisture_PDP$yhat <- TI_SoilMoisture_PDP$yhat * 100

TI_SM_plot <- ggplot(TI_SoilMoisture_PDP, aes(SoilMoisture, yhat)) +
  geom_line(size = 1) +
  #use stat smooth to get a smooth line between data points
  #stat_smooth(aes(y=yhat, x=CoastDist), method = lm, formula = y ~ poly(x, 10), se = FALSE) +
  labs(x = expression(paste("Soil Moisture (mm ",yr^-1,")")),
       y = expression(paste("TI-NDVI  ",yr^-1," (x ",10^-2,")"))) +
  #scale_y_continuous(breaks = seq(6.0,8.5,0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        panel.grid = element_blank())

plot(TI_SM_plot)

ggarrange(TI_CoastDist_Plot,
          TI_HumanMod_plot,
          TI_SWI_plot,
          TI_SM_plot,
          TI_Precip_plot,
          TI_Elev_plot,
          ncol = 3,
          nrow = 2)

