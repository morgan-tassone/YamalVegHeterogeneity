#############################Chi-square Tests of Independence (Max NDVI vs. categorical variables)##################################
setwd("~/UVA/MODIS Timeseries Project/Yamal Data")

MaxNDVI_clean <- readRDS(file = "MaxNDVI_Updated/Updated_MaxNDVI_Clean_22March2021.RDS")

#Remove NA values - if NaN included in mean calculation, mean = NaN
MaxNDVI_clean <- na.omit(MaxNDVI_clean)

#Split by pixel ID
MaxNDVI_split <- split(MaxNDVI_clean$NDVI, MaxNDVI_clean$PixelID)

library(pbapply)
MaxNDVI.avg <- pblapply(MaxNDVI_split, mean)

#Convert the list into a dataframe
MaxNDVI_avg_table <- as.data.frame(MaxNDVI.avg)
MaxNDVI_avg_table <- t(MaxNDVI_avg_table)
MaxNDVI_avg_table <- as.data.frame(MaxNDVI_avg_table)

#Round to 3 decimal places
MaxNDVI_avg_table$V1 <- round(MaxNDVI_avg_table$V1, digits = 3)

hist(MaxNDVI_avg_table$V1,
     main="Mean Max NDVI 2001-2018", 
     xlab = "Mean NDVI",
     breaks=25)


#####Determine Jenk's natural breaks for Max NDVI categories
#Convert dataframe into a vector
MeanMaxNDVI_vector <- MaxNDVI_avg_table[,1]
class(MeanMaxNDVI_vector)

library(BAMMtools)
#Break Max NDVI into 3 categories (low, mid-, and high)
Jenks_out <- getJenksBreaks(MeanMaxNDVI_vector, 4, subset = NULL)

min(MeanMaxNDVI_vector)
max(MeanMaxNDVI_vector)

#Mean Max NDVI categories:
#Low = 0.116 - 0.533
#Mid = 0.534 - 0.624
#High = 0.625 - 0.797

MaxNDVI_avg_table$V1 <- ifelse(MaxNDVI_avg_table$V1 >= 0.116 & MaxNDVI_avg_table$V1 <= 0.533,"Low", MaxNDVI_avg_table$V1)
MaxNDVI_avg_table$V1 <- ifelse(MaxNDVI_avg_table$V1 >= 0.534 & MaxNDVI_avg_table$V1 <= 0.624,"Mid", MaxNDVI_avg_table$V1)
MaxNDVI_avg_table$V1 <- ifelse(MaxNDVI_avg_table$V1 >= 0.625 & MaxNDVI_avg_table$V1 <= 0.797,"High", MaxNDVI_avg_table$V1)

####Mean Max NDVI vs. Landscape Age
LandAge <- readRDS(file = "Landscape Age/LandAge_tbl_09March2021.RDS")

#NAs = 0, remove 0 and subtract 500 from every value to get correct values
LandAge[LandAge == 0] <- NA
LandAge_clean <- na.omit(LandAge)
LandAge_corrVals <- LandAge_clean[, 4] - 500
LandAge_corrVals <- as.data.frame(LandAge_corrVals)

#Create table with only Land Age Pixel IDs, then combine with the correct land age values
LandAge_PIs <- subset(LandAge_clean, select = c(PixelID) )
LandAge_PIs_corrVals <- cbind(LandAge_PIs, LandAge_corrVals)
colnames(LandAge_PIs_corrVals )[2] <- "LandAge"

#Convert land ages into older, younger, and recent_disturbance
LandAge_PIs_corrVals$LandAge <- ifelse(LandAge_PIs_corrVals$LandAge == 70, "Older", LandAge_PIs_corrVals$LandAge)
LandAge_PIs_corrVals$LandAge <- ifelse(LandAge_PIs_corrVals$LandAge == 25, "Younger", LandAge_PIs_corrVals$LandAge)
LandAge_PIs_corrVals$LandAge <- ifelse(LandAge_PIs_corrVals$LandAge == 0, "Recent", LandAge_PIs_corrVals$LandAge)

#Import Max NDVI coordinates/Pixel IDs to ensure datasets match
MaxNDVI_coords <- readRDS(file = "MaxNDVI_Updated/Updated_MaxNDVI_Clean_Coords_22March2021.RDS")

#Extract Pixel IDs and add to avg Max NDVI dataframe
MaxNDVI_PIs <- subset(MaxNDVI_coords, select = c(PixelID) )
MaxNDVI_avg_table <- cbind(MaxNDVI_PIs, MaxNDVI_avg_table)
colnames(MaxNDVI_avg_table)[2] <- "MaxNDVI"

#Remove pixel IDs from dataframes NOT present in the Max NDVI dataframe, and vice versa.
LandAge_final <- LandAge_PIs_corrVals[(LandAge_PIs_corrVals$PixelID %in% MaxNDVI_avg_table$PixelID),]
MaxNDVI_final <-  MaxNDVI_avg_table[(MaxNDVI_avg_table$PixelID %in% LandAge_final$PixelID),]

#Run Chi Square Test between landscape age and Max NDVI categories
#Create a contingency table
ContTable_LA <- table(LandAge_final$LandAge, MaxNDVI_final$MaxNDVI)
View(ContTable_LA)

#Run Chi Square Test to see if land age and mean Max NDVI are related
MaxNDVI_LandAge_chisq <- chisq.test(ContTable_LA)
MaxNDVI_LandAge_chisq

library(RColorBrewer)
#display.brewer.all()

#Results were significant, so check out residuals to see what relationship contributed to this.
round(MaxNDVI_LandAge_chisq$residuals, 3)

#Plot the residuals
library(corrplot)
corrplot(MaxNDVI_LandAge_chisq$residuals, is.cor = FALSE, col = brewer.pal(n = 4, name = "RdBu"),
         cl.ratio = 0.5, cl.align = "r", tl.col = "black")

#Run using stratified random samples of 2/3 and 1/2 of the data
Max_LA_Table <- cbind (LandAge_final$LandAge, MaxNDVI_final$MaxNDVI)
Max_LA_Table <- as.data.frame(Max_LA_Table)

set.seed(107)
library(caret)
inTrain <- createDataPartition(y = Max_LA_Table$V2, p = .67, list = FALSE)
Training <- Max_LA_Table[ inTrain,]
Testing <- Max_LA_Table[-inTrain,]

#Create a contingency table
ContTable_LA_training <- table(Training$V1, Training$V2)
View(ContTable_LA_training)

#Run Chi Square Test to see if land age and mean Max NDVI are related
MaxNDVI_LandAge_chisq_2 <- chisq.test(ContTable_LA_training)
MaxNDVI_LandAge_chisq_2

#Results were significant, so check out residuals to see what relationship contributed to this.
round(MaxNDVI_LandAge_chisq_2$residuals, 3)


####Mean Max NDVI vs. Soil Texture
SoilText <- readRDS(file = "SoilTexture/SoilText_tbl_09March2021.RDS")
SoilText_clean <- na.omit(SoilText)
colnames(SoilText_clean)[1] <- "PixelID"
colnames(SoilText_clean)[2] <- "SoilText"

#Convert the soil texture IDs into soil texture types
SoilText_clean$SoilText <- ifelse(SoilText_clean$SoilText == 7021, "Sand", SoilText_clean$SoilText)
SoilText_clean$SoilText <- ifelse(SoilText_clean$SoilText == 8505 | SoilText_clean$SoilText == 8510, "ClayLoam", SoilText_clean$SoilText)
SoilText_clean$SoilText <- ifelse(SoilText_clean$SoilText == 7944 | SoilText_clean$SoilText == 7952 | SoilText_clean$SoilText == 7952
                                  | SoilText_clean$SoilText == 8000 | SoilText_clean$SoilText == 8099 | SoilText_clean$SoilText == 8100 
                                  | SoilText_clean$SoilText == 8112 | SoilText_clean$SoilText == 8118 | SoilText_clean$SoilText == 8120
                                  | SoilText_clean$SoilText == 8123 | SoilText_clean$SoilText == 8125 | SoilText_clean$SoilText == 8126
                                  | SoilText_clean$SoilText == 8129 | SoilText_clean$SoilText == 8133 | SoilText_clean$SoilText == 8134
                                  | SoilText_clean$SoilText == 8140 | SoilText_clean$SoilText == 8173 | SoilText_clean$SoilText == 8174
                                  | SoilText_clean$SoilText == 8178 | SoilText_clean$SoilText == 8179 | SoilText_clean$SoilText == 8182
                                  | SoilText_clean$SoilText == 8195 | SoilText_clean$SoilText == 8198 | SoilText_clean$SoilText == 8239
                                  | SoilText_clean$SoilText == 8348 | SoilText_clean$SoilText == 8351 | SoilText_clean$SoilText == 9073, 
                                  "Loam", SoilText_clean$SoilText)
SoilText_clean$SoilText <- ifelse(SoilText_clean$SoilText == 7061 | SoilText_clean$SoilText == 8495 | SoilText_clean$SoilText == 9008, 
                                  "SandyLoam", SoilText_clean$SoilText)
SoilText_clean$SoilText <- ifelse(SoilText_clean$SoilText == 7073 | SoilText_clean$SoilText == 8965 | SoilText_clean$SoilText == 9027
                                  | SoilText_clean$SoilText == 9030 | SoilText_clean$SoilText == 9046 | SoilText_clean$SoilText == 9059 
                                  | SoilText_clean$SoilText == 9064 | SoilText_clean$SoilText == 9069 | SoilText_clean$SoilText == 9074
                                  | SoilText_clean$SoilText == 9097 | SoilText_clean$SoilText == 9108 | SoilText_clean$SoilText == 9114
                                  | SoilText_clean$SoilText == 9118 | SoilText_clean$SoilText == 9119 | SoilText_clean$SoilText == 9134
                                  | SoilText_clean$SoilText == 9148 | SoilText_clean$SoilText == 9170 | SoilText_clean$SoilText == 9177
                                  | SoilText_clean$SoilText == 9181 | SoilText_clean$SoilText == 9182 | SoilText_clean$SoilText == 9195
                                  | SoilText_clean$SoilText == 9202, 
                                  "SiltLoam", SoilText_clean$SoilText)


#Remove pixel IDs from dataframes NOT present in the Max NDVI dataframe, and vice versa.
SoilText_final <- SoilText_clean[(SoilText_clean$PixelID %in% MaxNDVI_avg_table$PixelID),]
MaxNDVI_final2 <-  MaxNDVI_avg_table[(MaxNDVI_avg_table$PixelID %in% SoilText_final$PixelID),]

MaxST_Table_NumOccTest <- cbind(SoilText_final, MaxNDVI_final2)

#Run Chi Square Test between soil texture and Max NDVI categories
#Create a contingency table
ContTable_SoilTex <- table(SoilText_final$SoilText, MaxNDVI_final2$MaxNDVI)
View(ContTable_SoilTex)

#Run Chi Square Test to see if soilText and mean Max NDVI are related
MaxNDVI_SoilText_chisq <- chisq.test(ContTable_SoilTex)
MaxNDVI_SoilText_chisq

#Results were significant, so check out residuals to see what relationship contributed to this.
round(MaxNDVI_SoilText_chisq$residuals, 3)

#Plot the residuals
corrplot(MaxNDVI_SoilText_chisq$residuals, is.cor = FALSE, col = brewer.pal(n = 4, name = "RdBu"),
         cl.ratio = 0.5, cl.align = "r", tl.col = "black", tl.srt = 90)

#Run using stratified random samples of 2/3 and 1/2 of the data
Max_ST_Table <- cbind (SoilText_final$SoilText, MaxNDVI_final2$MaxNDVI)
Max_ST_Table <- as.data.frame(Max_ST_Table)

set.seed(107)
library(caret)
inTrain <- createDataPartition(y = Max_ST_Table$V2, p = .50, list = FALSE)
Training2 <- Max_ST_Table[ inTrain,]
Testing2 <- Max_ST_Table[-inTrain,]

#Create a contingency table
ContTable_ST_training <- table(Training2$V1, Training2$V2)
View(ContTable_ST_training)

#Run Chi Square Test to see if land age and mean Max NDVI are related
MaxNDVI_SoilText_chisq_2 <- chisq.test(ContTable_ST_training)
MaxNDVI_SoilText_chisq_2

#Results were significant, so check out residuals to see what relationship contributed to this.
round(MaxNDVI_SoilText_chisq_2$residuals, 3)

####Mean Max NDVI vs. Substrate Chemistry
SubChem <- readRDS(file = "Substrate pH/SubChem_tbl_09March2021.RDS")

#0 = NA values
SubChem[SubChem == 0] <- NA
SubChem_clean <- na.omit(SubChem)

#Remove xy coordinate columns
SubChem_clean <- subset(SubChem_clean, select = -c(x,y) )

#Convert values into categories
SubChem_clean$SubChem <- ifelse(SubChem_clean$SubChem == 2, "Acidic", SubChem_clean$SubChem)
SubChem_clean$SubChem <- ifelse(SubChem_clean$SubChem == 3, "Circumneutral", SubChem_clean$SubChem)
SubChem_clean$SubChem <- ifelse(SubChem_clean$SubChem == 5, "Saline", SubChem_clean$SubChem)

#Remove pixel IDs from dataframes NOT present in the Max NDVI dataframe, and vice versa.
SubChem_final <- SubChem_clean[(SubChem_clean$PixelID %in% MaxNDVI_avg_table$PixelID),]
MaxNDVI_final3 <-  MaxNDVI_avg_table[(MaxNDVI_avg_table$PixelID %in% SubChem_final$PixelID),]

#Run Chi Square Test between substrate chem and Max NDVI categories
#Create a contingency table
ContTable_SC <- table(SubChem_final$SubChem, MaxNDVI_final3$MaxNDVI)
View(ContTable_SC)

#Run Chi Square Test to see if sub pH and mean Max NDVI are related
MaxNDVI_SubChem_chisq <- chisq.test(ContTable_SC)
MaxNDVI_SubChem_chisq

#Results were significant, so check out residuals to see what relationship contributed to this.
round(MaxNDVI_SubChem_chisq$residuals, 3)

#Plot the residuals
corrplot(MaxNDVI_SubChem_chisq$residuals, is.cor = FALSE, col = brewer.pal(n = 4, name = "RdBu"),
         cl.ratio = 0.5, cl.align = "r", tl.col = "black", tl.srt = 90, order = "AOE")

#Run using stratified random samples of 2/3 and 1/2 of the data
Max_SC_Table <- cbind (SubChem_final$SubChem, MaxNDVI_final3$MaxNDVI)
Max_SC_Table <- as.data.frame(Max_SC_Table)

set.seed(107)
library(caret)
inTrain <- createDataPartition(y = Max_SC_Table$V2, p = .50, list = FALSE)
Training3 <- Max_SC_Table[ inTrain,]

#Create a contingency table
ContTable_SC_training <- table(Training3$V1, Training3$V2)
View(ContTable_SC_training)

#Run Chi Square Test
MaxNDVI_SubChem_chisq_2 <- chisq.test(ContTable_SC_training)
MaxNDVI_SubChem_chisq_2

#Results were significant, so check out residuals to see what relationship contributed to this.
round(MaxNDVI_SubChem_chisq_2$residuals, 3)


####Mean Max NDVI vs. Vegetation Unit
VegType <- readRDS(file = "Veg Types/VegType_tbl_09March2021.RDS")

#0 = NA values
VegType[VegType == 0] <- NA
VegType_clean <- na.omit(VegType)

#Remove xy coordinate columns
VegType_clean <- subset(VegType_clean, select = -c(x,y) )

#Remove values = 19, 19 = water
library(dplyr)
VegType_clean = filter(VegType_clean, VegType != 19)
unique(VegType_clean$VegType)

#Convert Veg Types into categories (based on Raynolds et al. 2006)
VegType_clean$VegType <- ifelse(VegType_clean$VegType == 2, "Graminoid", VegType_clean$VegType)
VegType_clean$VegType <- ifelse(VegType_clean$VegType == 12, "Wetland", VegType_clean$VegType)
VegType_clean$VegType <- ifelse(VegType_clean$VegType == 5, "Graminoid", VegType_clean$VegType)
VegType_clean$VegType <- ifelse(VegType_clean$VegType == 9, "ErectShrub", VegType_clean$VegType)
VegType_clean$VegType <- ifelse(VegType_clean$VegType == 7, "Graminoid", VegType_clean$VegType)
VegType_clean$VegType <- ifelse(VegType_clean$VegType == 4, "ProstrateShrub", VegType_clean$VegType)
VegType_clean$VegType <- ifelse(VegType_clean$VegType == 10, "ErectShrub", VegType_clean$VegType)
VegType_clean$VegType <- ifelse(VegType_clean$VegType == 13, "Wetland", VegType_clean$VegType)
VegType_clean$VegType <- ifelse(VegType_clean$VegType == 14, "Wetland", VegType_clean$VegType)

#Remove pixel IDs from dataframes NOT present in the Max NDVI dataframe, and vice versa.
VegType_final <- VegType_clean[(VegType_clean$PixelID %in% MaxNDVI_avg_table$PixelID),]
MaxNDVI_final4 <-  MaxNDVI_avg_table[(MaxNDVI_avg_table$PixelID %in% VegType_final$PixelID),]

#Run Chi Square Test between veg type and Max NDVI categories
#Create a contingency table
ContTable_VT <- table(VegType_final$VegType, MaxNDVI_final4$MaxNDVI)
View(ContTable_VT)

#Run Chi Square Test 
MaxNDVI_VegType_chisq <- chisq.test(ContTable_VT)
MaxNDVI_VegType_chisq

#Results were significant, so check out residuals to see what relationship contributed to this.
round(MaxNDVI_VegType_chisq$residuals, 3)

#Plot the residuals
corrplot(MaxNDVI_VegType_chisq$residuals, is.cor = FALSE, col = brewer.pal(n = 4, name = "RdBu"),
         cl.ratio = 0.5, cl.align = "r", tl.col = "black", tl.srt = 90)

#Run using stratified random samples of 2/3 and 1/2 of the data
Max_VT_Table <- cbind (VegType_final$VegType, MaxNDVI_final4$MaxNDVI)
Max_VT_Table <- as.data.frame(Max_VT_Table)

set.seed(107)
library(caret)
inTrain <- createDataPartition(y = Max_VT_Table$V2, p = .50, list = FALSE)
Training4 <- Max_VT_Table[ inTrain,]

#Create a contingency table
ContTable_VT_training <- table(Training4$V1, Training4$V2)
View(ContTable_VT_training)

#Run Chi Square Test
MaxNDVI_VegType_chisq_2 <- chisq.test(ContTable_VT_training)
MaxNDVI_VegType_chisq_2

#Results were significant, so check out residuals to see what relationship contributed to this.
round(MaxNDVI_VegType_chisq_2$residuals, 3)

