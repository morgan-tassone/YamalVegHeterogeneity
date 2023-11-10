###################################Max NDVI Random Forest Regression Prep Code##############################################
setwd("~/UVA/MODIS Timeseries Project/Yamal Data")

####Load datasets of slopes/values, combine with pixel IDs where needed. 
MaxNDVI_SS_SigOnly <- readRDS(file = "MaxNDVI_Updated/UpdatedMaxNDVI_SigOnly_07Nov2023.RDS")
MaxNDVI_SS_PIs <- subset(MaxNDVI_SS_SigOnly, select = -c(pval) )
colnames(MaxNDVI_SS_PIs)[2] <- "MaxNDVI"

CoastDist_PIs <- readRDS(file = "DistToCoast_Updated/CoastDist_tbl_09March2021.RDS")
CoastDist_PIs <- subset(CoastDist_PIs, select = -c(x,y) )

Elevation_PIs <- readRDS(file = "TanDEM/Elev_tbl_09March2021.RDS")
Elevation_PIs <- subset(Elevation_PIs, select = -c(x,y) )

HumanMod_PIs <- readRDS(file = "HumanMod/HumanMod_tbl_09March2021.RDS")
HumanMod_PIs <- subset(HumanMod_PIs, select = -c(x,y) )

LandAge_PIs <- readRDS(file = "Landscape Age/LandAge_tbl_09March2021.RDS")
LandAge_PIs <- subset(LandAge_PIs, select = -c(x,y) )

MGSP_SS <- readRDS(file = "MGSP_Updated/MGSP_Slope_10March2021.RDS")
MGSP_PixelIDs <- readRDS(file = "MGSP_Updated/MGSP_Clean_Coords_08March2021.RDS")
MGSP_SS_PIs <- cbind(MGSP_PixelIDs, MGSP_SS)
MGSP_SS_PIs <- subset(MGSP_SS_PIs, select = -c(x,y) )
colnames(MGSP_SS_PIs)[2] <- "MGSP"

SFPO_SS <- readRDS(file = "SFPO_Updated/SFPO_Slope_10March2021.RDS")
SFPO_PixelIDs <- readRDS(file = "SFPO_Updated/SFPO_Clean_Coords_09March2021.RDS")
SFPO_SS_PIs <- cbind(SFPO_PixelIDs, SFPO_SS)
SFPO_SS_PIs <- subset(SFPO_SS_PIs, select = -c(x,y) )
colnames(SFPO_SS_PIs)[2] <- "SFPO"

SM_SS <- readRDS(file = "SoilMoisture_Updated/SM_Slope_10March2021.RDS")
SM_PixelIDs <- readRDS(file = "SoilMoisture_Updated/SM_Clean_Coords_09March2021.RDS")
SM_SS_PIs <- cbind(SM_PixelIDs, SM_SS)
SM_SS_PIs <- subset(SM_SS_PIs, select = -c(x,y) )
colnames(SM_SS_PIs)[2] <- "SM"

SoilText_PIs <- readRDS(file = "SoilTexture/SoilText_tbl_09March2021.RDS")
colnames(SoilText_PIs)[2] <- "SoilTexture"

SubChem_PIs <- readRDS(file = "Substrate pH/SubChem_tbl_09March2021.RDS")
SubChem_PIs <- subset(SubChem_PIs, select = -c(x,y) )

SWI_SS <- readRDS(file = "SWI_Updated/SWI_Slope_10March2021.RDS")
SWI_PixelIDs <- readRDS(file = "SWI_Updated/SWI_Clean_Coords_08March2021.RDS")
SWI_SS_PIs <- cbind(SWI_PixelIDs, SWI_SS)
SWI_SS_PIs <- subset(SWI_SS_PIs, select = -c(x,y) )
colnames(SWI_SS_PIs)[2] <- "SWI"

VegType_PIs <- readRDS(file = "Veg Types/VegType_tbl_09March2021.RDS")
VegType_PIs <- subset(VegType_PIs, select = -c(x,y) )

#Remove NAs from tables and convert to dataframes
MaxNDVI_SS_PIs <- na.omit(MaxNDVI_SS_PIs) #no change - NAs removed when creating timeseries 15 yrs +

CoastDist_PIs <- na.omit(CoastDist_PIs)

#For elevation, NAs = 0. Added 500 to pixel values to not exclude pixels with 0m elevation
#Remove 0s, then subtract 500 from every value
Elevation_PIs[Elevation_PIs == 0] <- NA
Elevation_PIs <- na.omit(Elevation_PIs)
Elevation_PIs$Elevation <- Elevation_PIs$Elevation - 500

HumanMod_PIs <- na.omit(HumanMod_PIs)

#For land age, NAs = 0. Added 500 to pixel values to not exclude pixels with 0 (recently disturbed) values
#Remove 0s, then subtract 500 from every value
LandAge_PIs[LandAge_PIs == 0] <- NA
LandAge_PIs <- na.omit(LandAge_PIs)
LandAge_PIs$LandAge <- LandAge_PIs$LandAge - 500

MGSP_SS_PIs <- na.omit(MGSP_SS_PIs) #no change - NAs removed when creating timeseries 15 yrs +

SFPO_SS_PIs <- na.omit(SFPO_SS_PIs) #no change - NAs removed when creating timeseries 15 yrs +

SM_SS_PIs <- na.omit(SM_SS_PIs) #no change - NAs removed when creating timeseries 15 yrs +

SoilText_PIs <- na.omit(SoilText_PIs)
colnames(SoilText_PIs)[1] <- "PixelID"
str(SoilText_PIs)

#Convert the soil texture IDs into soil texture types
SoilText_PIs$SoilTexture <- ifelse(SoilText_PIs$SoilTexture == 7021, "Sand", SoilText_PIs$SoilTexture)
SoilText_PIs$SoilTexture <- ifelse(SoilText_PIs$SoilTexture == 8505 | SoilText_PIs$SoilTexture == 8510, "ClayLoam", SoilText_PIs$SoilTexture)
SoilText_PIs$SoilTexture <- ifelse(SoilText_PIs$SoilTexture == 7944 | SoilText_PIs$SoilTexture == 7952 | SoilText_PIs$SoilTexture == 7952
                                  | SoilText_PIs$SoilTexture == 8000 | SoilText_PIs$SoilTexture == 8099 | SoilText_PIs$SoilTexture == 8100 
                                  | SoilText_PIs$SoilTexture == 8112 | SoilText_PIs$SoilTexture == 8118 | SoilText_PIs$SoilTexture == 8120
                                  | SoilText_PIs$SoilTexture == 8123 | SoilText_PIs$SoilTexture == 8125 | SoilText_PIs$SoilTexture == 8126
                                  | SoilText_PIs$SoilTexture == 8129 | SoilText_PIs$SoilTexture == 8133 | SoilText_PIs$SoilTexture == 8134
                                  | SoilText_PIs$SoilTexture == 8140 | SoilText_PIs$SoilTexture == 8173 | SoilText_PIs$SoilTexture == 8174
                                  | SoilText_PIs$SoilTexture == 8178 | SoilText_PIs$SoilTexture == 8179 | SoilText_PIs$SoilTexture == 8182
                                  | SoilText_PIs$SoilTexture == 8195 | SoilText_PIs$SoilTexture == 8198 | SoilText_PIs$SoilTexture == 8239
                                  | SoilText_PIs$SoilTexture == 8348 | SoilText_PIs$SoilTexture == 8351 | SoilText_PIs$SoilTexture == 9073, 
                                  "Loam", SoilText_PIs$SoilTexture)
SoilText_PIs$SoilTexture <- ifelse(SoilText_PIs$SoilTexture == 7061 | SoilText_PIs$SoilTexture == 8495 | SoilText_PIs$SoilTexture == 9008, 
                                  "SandyLoam", SoilText_PIs$SoilTexture)
SoilText_PIs$SoilTexture <- ifelse(SoilText_PIs$SoilTexture == 7073 | SoilText_PIs$SoilTexture == 8965 | SoilText_PIs$SoilTexture == 9027
                                  | SoilText_PIs$SoilTexture == 9030 | SoilText_PIs$SoilTexture == 9046 | SoilText_PIs$SoilTexture == 9059 
                                  | SoilText_PIs$SoilTexture == 9064 | SoilText_PIs$SoilTexture == 9069 | SoilText_PIs$SoilTexture == 9074
                                  | SoilText_PIs$SoilTexture == 9097 | SoilText_PIs$SoilTexture == 9108 | SoilText_PIs$SoilTexture == 9114
                                  | SoilText_PIs$SoilTexture == 9118 | SoilText_PIs$SoilTexture == 9119 | SoilText_PIs$SoilTexture == 9134
                                  | SoilText_PIs$SoilTexture == 9148 | SoilText_PIs$SoilTexture == 9170 | SoilText_PIs$SoilTexture == 9177
                                  | SoilText_PIs$SoilTexture == 9181 | SoilText_PIs$SoilTexture == 9182 | SoilText_PIs$SoilTexture == 9195
                                  | SoilText_PIs$SoilTexture == 9202, 
                                  "SiltLoam", SoilText_PIs$SoilTexture)

#For sub chem, NAs = 0
SubChem_PIs[SubChem_PIs == 0] <- NA
SubChem_PIs <- na.omit(SubChem_PIs)
#Convert sub chem IDs into categories
SubChem_PIs$SubChem <- ifelse(SubChem_PIs$SubChem == 2, "Acidic", SubChem_PIs$SubChem)
SubChem_PIs$SubChem <- ifelse(SubChem_PIs$SubChem == 3, "Circum", SubChem_PIs$SubChem)
SubChem_PIs$SubChem <- ifelse(SubChem_PIs$SubChem == 5, "Saline", SubChem_PIs$SubChem)

SWI_SS_PIs <- na.omit(SWI_SS_PIs) #no change - NAs removed when creating timeseries 15 yrs +

#For veg type, NAs = 0
VegType_PIs[VegType_PIs == 0] <- NA
VegType_PIs <- na.omit(VegType_PIs)
#Convert Veg Types into categories (based on Raynolds et al. 2006)
VegType_PIs$VegType <- ifelse(VegType_PIs$VegType == 2, "Graminoid", VegType_PIs$VegType)
VegType_PIs$VegType <- ifelse(VegType_PIs$VegType == 12, "Wetland", VegType_PIs$VegType)
VegType_PIs$VegType <- ifelse(VegType_PIs$VegType == 5, "Graminoid", VegType_PIs$VegType)
VegType_PIs$VegType <- ifelse(VegType_PIs$VegType == 9, "ErectShrub", VegType_PIs$VegType)
VegType_PIs$VegType <- ifelse(VegType_PIs$VegType == 7, "Graminoid", VegType_PIs$VegType)
VegType_PIs$VegType <- ifelse(VegType_PIs$VegType == 4, "ProstrateShrub", VegType_PIs$VegType)
VegType_PIs$VegType <- ifelse(VegType_PIs$VegType == 10, "ErectShrub", VegType_PIs$VegType)
VegType_PIs$VegType <- ifelse(VegType_PIs$VegType == 13, "Wetland", VegType_PIs$VegType)
VegType_PIs$VegType <- ifelse(VegType_PIs$VegType == 14, "Wetland", VegType_PIs$VegType)
#Remove 19, associated with water
library(dplyr)
VegType_PIs = filter(VegType_PIs, VegType != 19)

#Need each dataset to have the same number of data points and data associated with the same pixels
#Start with MaxNDVI, which has the fewest pixels (17,093)
#Remove pixel IDs from dataframes NOT present in the MaxNDVI dataframe, and vice versa.
CoastDist_v1 <- CoastDist_PIs[(CoastDist_PIs$PixelID %in% MaxNDVI_SS_PIs$PixelID),]
Elevation_v1 <- Elevation_PIs[(Elevation_PIs$PixelID %in% MaxNDVI_SS_PIs$PixelID),]
HumanMod_v1 <- HumanMod_PIs[(HumanMod_PIs$PixelID %in% MaxNDVI_SS_PIs$PixelID),]
LandAge_v1 <- LandAge_PIs[(LandAge_PIs$PixelID %in% MaxNDVI_SS_PIs$PixelID),]
ST_v1 <- SoilText_PIs[(SoilText_PIs$PixelID %in% MaxNDVI_SS_PIs$PixelID),]
MGSP_v1 <- MGSP_SS_PIs[(MGSP_SS_PIs$PixelID %in% MaxNDVI_SS_PIs$PixelID),]
SFPO_v1 <- SFPO_SS_PIs[(SFPO_SS_PIs$PixelID %in% MaxNDVI_SS_PIs$PixelID),]
SM_v1 <- SM_SS_PIs[(SM_SS_PIs$PixelID %in% MaxNDVI_SS_PIs$PixelID),]
SubChem_v1 <- SubChem_PIs[(SubChem_PIs$PixelID %in% MaxNDVI_SS_PIs$PixelID),]
SWI_v1 <- SWI_SS_PIs[(SWI_SS_PIs$PixelID %in% MaxNDVI_SS_PIs$PixelID),]
VegType_v1 <- VegType_PIs[(VegType_PIs$PixelID %in% MaxNDVI_SS_PIs$PixelID),]

#Soil Texture now has the fewest pixels (16,840)
#Remove pixel IDs from dataframes NOT present in the soil texture dataframe, and vice versa.
CoastDist_v2 <- CoastDist_v1[(CoastDist_v1$PixelID %in% ST_v1$PixelID),]
Elevation_v2 <- Elevation_v1[(Elevation_v1$PixelID %in% ST_v1$PixelID),]
HumanMod_v2 <- HumanMod_v1[(HumanMod_v1$PixelID %in% ST_v1$PixelID),]
LandAge_v2 <- LandAge_v1[(LandAge_v1$PixelID %in% ST_v1$PixelID),]
MGSP_v2 <- MGSP_v1[(MGSP_v1$PixelID %in% ST_v1$PixelID),]
SFPO_v2 <- SFPO_v1[(SFPO_v1$PixelID %in% ST_v1$PixelID),]
SM_v2 <- SM_v1[(SM_v1$PixelID %in% ST_v1$PixelID),]
MaxNDVI_v2 <- MaxNDVI_SS_PIs[(MaxNDVI_SS_PIs$PixelID %in% ST_v1$PixelID),]
SubChem_v2 <- SubChem_v1[(SubChem_v1$PixelID %in% ST_v1$PixelID),]
SWI_v2 <- SWI_v1[(SWI_v1$PixelID %in% ST_v1$PixelID),]
VegType_v2 <- VegType_v1[(VegType_v1$PixelID %in% ST_v1$PixelID),]

#VegType now has the fewest pixels (16,782)
#Remove pixel IDs from dataframes NOT present in the veg type dataframe, and vice versa.
CoastDist_v3 <- CoastDist_v2[(CoastDist_v2$PixelID %in% VegType_v2$PixelID),]
Elevation_v3 <- Elevation_v2[(Elevation_v2$PixelID %in% VegType_v2$PixelID),]
HumanMod_v3 <- HumanMod_v2[(HumanMod_v2$PixelID %in% VegType_v2$PixelID),]
LandAge_v3 <- LandAge_v2[(LandAge_v2$PixelID %in% VegType_v2$PixelID),]
MaxNDVI_v3 <- MaxNDVI_v2[(MaxNDVI_v2$PixelID %in% VegType_v2$PixelID),]
MGSP_v3 <- MGSP_v2[(MGSP_v2$PixelID %in% VegType_v2$PixelID),]
SFPO_v3 <- SFPO_v2[(SFPO_v2$PixelID %in% VegType_v2$PixelID),]
SM_v3 <- SM_v2[(SM_v2$PixelID %in% VegType_v2$PixelID),]
ST_v3 <- ST_v1[(ST_v1$PixelID %in% VegType_v2$PixelID),]
SubChem_v3 <- SubChem_v2[(SubChem_v2$PixelID %in% VegType_v2$PixelID),]
SWI_v3 <- SWI_v2[(SWI_v2$PixelID %in% VegType_v2$PixelID),]

#Elevation now has the fewest pixels (16,740)
#Remove pixel IDs from dataframes NOT present in the elevation dataframe, and vice versa.
CoastDist_v4 <- CoastDist_v3[(CoastDist_v3$PixelID %in% Elevation_v3$PixelID),]
HumanMod_v4 <- HumanMod_v3[(HumanMod_v3$PixelID %in% Elevation_v3$PixelID),]
LandAge_v4 <- LandAge_v3[(LandAge_v3$PixelID %in% Elevation_v3$PixelID),]
MaxNDVI_v4 <- MaxNDVI_v3[(MaxNDVI_v3$PixelID %in% Elevation_v3$PixelID),]
MGSP_v4 <- MGSP_v3[(MGSP_v3$PixelID %in% Elevation_v3$PixelID),]
SFPO_v4 <- SFPO_v3[(SFPO_v3$PixelID %in% Elevation_v3$PixelID),]
SM_v4 <- SM_v3[(SM_v3$PixelID %in% Elevation_v3$PixelID),]
ST_v4 <- ST_v3[(ST_v3$PixelID %in% Elevation_v3$PixelID),]
SubChem_v4 <- SubChem_v3[(SubChem_v3$PixelID %in% Elevation_v3$PixelID),]
SWI_v4 <- SWI_v3[(SWI_v3$PixelID %in% Elevation_v3$PixelID),]
VegType_v4 <- VegType_v2[(VegType_v2$PixelID %in% Elevation_v3$PixelID),]

#Human mod now has the fewest pixels (16,711)
#Remove pixel IDs from dataframes NOT present in the human mod dataframe, and vice versa.
CoastDist_v5 <- CoastDist_v4[(CoastDist_v4$PixelID %in% HumanMod_v4$PixelID),]
Elevation_v5 <- Elevation_v3[(Elevation_v3$PixelID %in% HumanMod_v4$PixelID),]
LandAge_v5 <- LandAge_v4[(LandAge_v4$PixelID %in% HumanMod_v4$PixelID),]
MaxNDVI_v5 <- MaxNDVI_v4[(MaxNDVI_v4$PixelID %in% HumanMod_v4$PixelID),]
MGSP_v5 <- MGSP_v4[(MGSP_v4$PixelID %in% HumanMod_v4$PixelID),]
SFPO_v5 <- SFPO_v4[(SFPO_v4$PixelID %in% HumanMod_v4$PixelID),]
SM_v5 <- SM_v4[(SM_v4$PixelID %in% HumanMod_v4$PixelID),]
ST_v5 <- ST_v4[(ST_v4$PixelID %in% HumanMod_v4$PixelID),]
SubChem_v5 <- SubChem_v4[(SubChem_v4$PixelID %in% HumanMod_v4$PixelID),]
SWI_v5 <- SWI_v4[(SWI_v4$PixelID %in% HumanMod_v4$PixelID),]
VegType_v5 <- VegType_v4[(VegType_v4$PixelID %in% HumanMod_v4$PixelID),]

#MGSP now has the fewest pixels (16,710)
#Remove pixel IDs from dataframes NOT present in the MGSP dataframe, and vice versa.
CoastDist_v6 <- CoastDist_v5[(CoastDist_v5$PixelID %in% MGSP_v5$PixelID),]
Elevation_v6 <- Elevation_v5[(Elevation_v5$PixelID %in% MGSP_v5$PixelID),]
HumanMod_v6 <- HumanMod_v4[(HumanMod_v4$PixelID %in% MGSP_v5$PixelID),]
LandAge_v6 <- LandAge_v5[(LandAge_v5$PixelID %in% MGSP_v5$PixelID),]
MaxNDVI_v6 <- MaxNDVI_v5[(MaxNDVI_v5$PixelID %in% MGSP_v5$PixelID),]
SFPO_v6 <- SFPO_v5[(SFPO_v5$PixelID %in% MGSP_v5$PixelID),]
SM_v6 <- SM_v5[(SM_v5$PixelID %in% MGSP_v5$PixelID),]
ST_v6 <- ST_v5[(ST_v5$PixelID %in% MGSP_v5$PixelID),]
SubChem_v6 <- SubChem_v5[(SubChem_v5$PixelID %in% MGSP_v5$PixelID),]
SWI_v6 <- SWI_v5[(SWI_v5$PixelID %in% MGSP_v5$PixelID),]
VegType_v6 <- VegType_v5[(VegType_v5$PixelID %in% MGSP_v5$PixelID),]

#Export one dataset for Pixel IDs
saveRDS(MaxNDVI_v6, file="MaxNDVI_RFR_PixelIDs_SigOnly_07Nov2023.RDS")

#Remove PixelID columns from the final datasets
CoastDist_v6 <- subset(CoastDist_v6, select = -c(PixelID) )
Elevation_v6 <- subset(Elevation_v6, select = -c(PixelID) )
HumanMod_v6 <- subset(HumanMod_v6, select = -c(PixelID) )
LandAge_v6 <- subset(LandAge_v6, select = -c(PixelID) )
MGSP_v5 <- subset(MGSP_v5, select = -c(PixelID) )
MaxNDVI_v6 <- subset(MaxNDVI_v6, select = -c(PixelID) )
SFPO_v6 <- subset(SFPO_v6, select = -c(PixelID) )
SM_v6 <- subset(SM_v6, select = -c(PixelID) )
ST_v6 <- subset(ST_v6, select = -c(PixelID) )
SubChem_v6 <- subset(SubChem_v6, select = -c(PixelID) )
SWI_v6 <- subset(SWI_v6, select = -c(PixelID) )
VegType_v6 <- subset(VegType_v6, select = -c(PixelID) )

#Datasets now have same number of matching pixels. Combine into 1 dataframe and rename columns
RFRinput <-cbind(MaxNDVI_v6, CoastDist_v6, Elevation_v6, HumanMod_v6, LandAge_v6, MGSP_v5, SFPO_v6,
                 SM_v6, ST_v6, SubChem_v6, SWI_v6, VegType_v6)

#Rename columns for RFR export interpretation
colnames(RFRinput)[6] <- "Precipitation"
colnames(RFRinput)[7] <- "SnowFreeDate"
colnames(RFRinput)[8] <- "SoilMoisture"
colnames(RFRinput)[10] <- "SubstrateChem"
colnames(RFRinput)[11] <- "SummerWarmth"
colnames(RFRinput)[12] <- "VegUnit"

#####Determine outlier values
summary(RFRinput$SummerWarmth)
SWI_IQR <- 0.26018 - 0.04816
SWI_LB <- 0.04816 - (1.5*SWI_IQR)
SWI_UB <-0.26018 + (1.5 *SWI_IQR)

#Determine number of pixels with SWI slopes >= 0.5
SWI_GTE05 <- RFRinput[(RFRinput$SummerWarmth >= 0.5),]

summary(RFRinput$SoilMoisture)
SM_IQR <- -0.3710 - -1.3401
SM_LB <- -1.3401 - (1.5*SM_IQR)
SM_UB <- -0.3710 + (1.5 *SM_IQR)

boxplot(RFRinput$SoilMoisture,
        ylab = "SM Sen's Slope"
)

summary(RFRinput$Precipitation)
MGSP_IQR <- 0.30971 - -0.42418
MGSP_LB <- -0.42418 - (1.5*MGSP_IQR)
MGSP_UB <- 0.30971 + (1.5 *MGSP_IQR)

boxplot(RFRinput$Precipitation,
        ylab = "MGSP Sen's Slope"
)

summary(RFRinput$HumanMod)
HM_IQR <- 0.02515 - 0.02515
HM_LB <- 0.02515 - (1.5*HM_IQR)
HM_UB <- 0.02515 + (1.5 *HM_IQR)

boxplot(RFRinput$HumanMod,
        ylab = "Human Modification"
)

summary(RFRinput$Elevation)
Elev_IQR <- 26.00 - 0.00
Elev_LB <- 0.00 - (1.5*Elev_IQR)
Elev_UB <- 26.00 + (1.5 *Elev_IQR)

boxplot(RFRinput$Elevation,
        ylab = "Elevation"
)

#Determine number of pixels with elevations between -12 and -15 m below sea level
Elev_Neg13to15m <- RFRinput[(RFRinput$Elevation >= -15 & RFRinput$Elevation <= -13),]

summary(RFRinput$CoastDist)
CD_IQR <- 55851 - 16150
CD_LB <- 16150 - (1.5*CD_IQR)
CD_UB <- 55851 + (1.5 *CD_IQR)

boxplot(RFRinput$CoastDist,
        ylab = "Coast Distance"
)

summary(RFRinput$MaxNDVI)
MaxNDVI_IQR <- 0.0015800 - -0.0001375
MaxNDVI_LB <- -0.0001375 - (1.5*MaxNDVI_IQR)
MaxNDVI_UB <- 0.0015800 + (1.5 *MaxNDVI_IQR)

boxplot(RFRinput$MaxNDVI,
        ylab = "Max NDVI Slope"
)

#Export as an R object and table
saveRDS(RFRinput, file="RFRinput_MaxNDVI_SigOnly_07Nov2023.RDS")
write.table(RFRinput, file="RFRinput_MaxNDVI_SigOnly_07Nov2023.csv", sep=",")

#Determine if input variables are correlated
# library(caret)
# 
# str(RFRinput)
# correlation <- cor(RFRinput)
# 
# findCorrelation(
#   cor(RFRinput),
#   cutoff = 0.75,
#   verbose = FALSE,
#   names = FALSE,
# )

#Run correlations between the discrete/continuous input variables to determine if any are highly correlated
#Highly correlated if r > 0.75
library(readxl)

SlopesForCor <- read_excel("RFR_SigOnly/MaxNDVI/RFRinput_MaxNDVI_SigOnly_07Nov2023.xlsx", 
                           na = "NA")
SlopesForCor <- data.matrix(SlopesForCor)
SlopesForCor <- subset(SlopesForCor, select = -c(LandAge, SoilTexture, SubstrateChem, VegUnit) )

library(psych)
CorrResult <- corr.test(SlopesForCor, y = NULL, use = "pairwise", method = "spearman", adjust = "bonferroni",
                        alpha = .05, ci = FALSE)

pval <- CorrResult$p

#SlopesForCorTest <- na.omit(SlopesForCor)

# #Determine number of pixels where SWI slopes between 0.5 and 1 correspond with positive Max NDVI Slopes
# SWI_GT05_test <- RFRinput[(RFRinput$SummerWarmth >= 0.5 & RFRinput$SummerWarmth <= 1.0),]
# SWIGT05_MaxPos <- SWI_GT05_test[(SWI_GT05_test$MaxNDVI > 0.000000),]
# 
# #Make tif from Coast Distance values included in RFR
# CoastDist <- readRDS(file = "DistToCoast_Updated/CoastDist_tbl_09March2021.RDS")
# CoastDist <- na.omit(CoastDist)
# 
# CoastDist_InRFR <- CoastDist[(CoastDist$PixelID %in% CoastDist_v8$PixelID),]
# 
# library(sp)
# library(raster)
# 
# CoastDist_coords <- subset(CoastDist_InRFR, select = c(x, y) )
# CoastDist_vals <- subset(CoastDist_InRFR, select = c(CoastDist) )
# 
# CoastDist_coords <- as.data.frame(CoastDist_coords)
# CoastDist_vals <- as.data.frame(CoastDist_vals)
# 
# str(CoastDist_coords)
# str(CoastDist_vals)
# 
# #Create spatial points dataframe object to be used in the rasterize function
# CDSpatPoints <- SpatialPointsDataFrame(CoastDist_coords, CoastDist_vals, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
#                                          match.ID = TRUE, bbox = NULL)
# 
# #get extent from existing raster and create a raster for the rasterize function
# MaxNDVI_raster <- raster("MaxNDVI_Updated/MaxNDVI_9999.tif")
# e <- extent(MaxNDVI_raster)
# r <- raster(e, ncol=960, nrow=653, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# CD_raster <- rasterize(CDSpatPoints, r, CDSpatPoints$CoastDist, fun=mean)
# plot(CD_raster)
# 
# library(rgdal)
# CDraster_tif <- writeRaster(CD_raster, "CoastDistRFR_07June2021", format= "GTiff")