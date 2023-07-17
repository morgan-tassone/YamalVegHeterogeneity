library(readxl)
library(doParallel)
library(randomForest)
library(caret)
library(pdp)

RFR_input <- read_excel("./RFRinput_MaxNDVI_09Apr2021.xlsx",
                        na = "NA")

as.data.frame(RFR_input)

RFR_input$LandAge = as.factor(RFR_input$LandAge)
RFR_input$SoilTexture = as.factor(RFR_input$SoilTexture)
RFR_input$SubstrateChem = as.factor(RFR_input$SubstrateChem)
RFR_input$VegUnit = as.factor(RFR_input$VegUnit)

as.data.frame(RFR_input)
str(RFR_input)

#Set up cores for parallel processing
numCores <- as.numeric(Sys.getenv('SLURM_CPUS_PER_TASK')) - 1
registerDoParallel(cores=numCores)

#Set control method and bootstrap interations, set seed for reproducable results, set mtry values to test
control <- trainControl(method = "boot", number=25, search="grid")
set.seed(2)
tunegrid <- expand.grid(.mtry=c(2:5))

#Run random forest regression
RFRslope_model2 <- train(MaxNDVI~., data=RFR_input, method='rf', tuneGrid=tunegrid, trControl=control, importance=TRUE)

finalModel2 <- RFRslope_model2$finalModel

#Export the data associated with Coast Dist PDP plot (no ICE)
CoastDistPDP <- partial(RFRslope_model2, pred.var = "CoastDist", plot = FALSE)
saveRDS(CoastDistPDP, file="MaxNDVI_CoastDist_PDPdata_09Apr2021.RDS")

#Export the data associated with Coast Dist PDP plot (with ICE)
CoastDistPDP_ice <- partial(RFRslope_model2, pred.var = "CoastDist", ice= TRUE, plot = FALSE)
saveRDS(CoastDistPDP_ice, file="MaxNDVI_CoastDist_PDPdataICE_09Apr2021.RDS")

#Export the data associated with Elevation PDP plot (no ICE)
ElevationPDP <- partial(RFRslope_model2, pred.var = "Elevation", plot = FALSE)
saveRDS(ElevationPDP, file="MaxNDVI_Elevation_PDPdata_09Apr2021.RDS")

#Export the data associated with Elevation PDP plot (with ICE)
ElevationPDP_ice <- partial(RFRslope_model2, pred.var = "Elevation", ice= TRUE, plot = FALSE)
saveRDS(ElevationPDP_ice, file="MaxNDVI_Elevation_PDPdataICE_09Apr2021.RDS")

#Export the data associated with Human Mod PDP plot (no ICE)
HumanModPDP <- partial(RFRslope_model2, pred.var = "HumanMod", plot = FALSE)
saveRDS(HumanModPDP, file="MaxNDVI_HumanMod_PDPdata_09Apr2021.RDS")

#Export the data associated with HumanMod PDP plot (with ICE)
HumanModPDP_ice <- partial(RFRslope_model2, pred.var = "HumanMod", ice= TRUE, plot = FALSE)
saveRDS(HumanModPDP_ice, file="MaxNDVI_HumanMod_PDPdataICE_09Apr2021.RDS")

#Export the data associated with Land Age PDP plot (no ICE)
LandAgePDP <- partial(RFRslope_model2, pred.var = "LandAge", plot = FALSE)
saveRDS(LandAgePDP, file="MaxNDVI_LandAge_PDPdata_09Apr2021.RDS")

#Export the data associated with Land Age PDP plot (with ICE)
LandAgePDP_ice <- partial(RFRslope_model2, pred.var = "LandAge", ice= TRUE, plot = FALSE)
saveRDS(LandAgePDP_ice, file="MaxNDVI_LandAge_PDPdataICE_09Apr2021.RDS")

#Export the data associated with Precipitation PDP plot (no ICE)
PrecipPDP <- partial(RFRslope_model2, pred.var = "Precipitation", plot = FALSE)
saveRDS(PrecipPDP, file="MaxNDVI_Precip_PDPdata_09Apr2021.RDS")

#Export the data associated with Precipitation PDP plot (with ICE)
PrecipPDP_ice <- partial(RFRslope_model2, pred.var = "Precipitation", ice= TRUE, plot = FALSE)
saveRDS(PrecipPDP_ice, file="MaxNDVI_Precip_PDPdataICE_09Apr2021.RDS")

#Export the data associated with Snow Free Date PDP plot (no ICE)
SnowFreePDP <- partial(RFRslope_model2, pred.var = "SnowFreeDate", plot = FALSE)
saveRDS(SnowFreePDP, file="MaxNDVI_SnowFree_PDPdata_09Apr2021.RDS")

#Export the data associated with Snow Free Date PDP plot (with ICE)
SnowFreePDP_ice <- partial(RFRslope_model2, pred.var = "SnowFreeDate", ice= TRUE, plot = FALSE)
saveRDS(SnowFreePDP_ice, file="MaxNDVI_SnowFree_PDPdataICE_09Apr2021.RDS")

#Export the data associated with Soil Moisture PDP plot (no ICE)
SoilMoisturePDP <- partial(RFRslope_model2, pred.var = "SoilMoisture", plot = FALSE)
saveRDS(SoilMoisturePDP, file="MaxNDVI_SoilMoisture_PDPdata_09Apr2021.RDS")

#Export the data associated with Soil Moisture PDP plot (with ICE)
SoilMoisturePDP_ice <- partial(RFRslope_model2, pred.var = "SoilMoisture", ice= TRUE, plot = FALSE)
saveRDS(SoilMoisturePDP_ice, file="MaxNDVI_SoilMoisture_PDPdataICE_09Apr2021.RDS")

#Export the data associated with Soil Texture PDP plot (no ICE)
SoilTexturePDP <- partial(RFRslope_model2, pred.var = "SoilTexture", plot = FALSE)
saveRDS(SoilTexturePDP, file="MaxNDVI_SoilTexture_PDPdata_09Apr2021.RDS")

#Export the data associated with Soil Texture PDP plot (with ICE)
SoilTexturePDP_ice <- partial(RFRslope_model2, pred.var = "SoilTexture", ice= TRUE, plot = FALSE)
saveRDS(SoilTexturePDP_ice, file="MaxNDVI_SoilTexture_PDPdataICE_09Apr2021.RDS")

#Export the data associated with Substrate Chem PDP plot (no ICE)
SubChemPDP <- partial(RFRslope_model2, pred.var = "SubstrateChem", plot = FALSE)
saveRDS(SubChemPDP, file="MaxNDVI_SubChem_PDPdata_09Apr2021.RDS")

#Export the data associated with Substrate Chem PDP plot (with ICE)
SubChemPDP_ice <- partial(RFRslope_model2, pred.var = "SubstrateChem", ice= TRUE, plot = FALSE)
saveRDS(SubChemPDP_ice, file="MaxNDVI_SubChem_PDPdataICE_09Apr2021.RDS")

#Export the data associated with SWI PDP plot (no ICE)
SummerWarmthPDP <- partial(RFRslope_model2, pred.var = "SummerWarmth", plot = FALSE)
saveRDS(SummerWarmthPDP, file="MaxNDVI_SummerWarmth_PDPdata_09Apr2021.RDS")

#Export the data associated with SWI PDP plot (with ICE)
SummerWarmthPDP_ice <- partial(RFRslope_model2, pred.var = "SummerWarmth", ice= TRUE, plot = FALSE)
saveRDS(SummerWarmthPDP_ice, file="MaxNDVI_SummerWarmth_PDPdataICE_09Apr2021.RDS")

#Export the data associated with VegUnit PDP plot (no ICE)
VegUnitPDP <- partial(RFRslope_model2, pred.var = "VegUnit", plot = FALSE)
saveRDS(VegUnitPDP, file="MaxNDVI_VegUnit_PDPdata_09Apr2021.RDS")

#Export the data associated with VegUnit PDP plot (with ICE)
VegUnitPDP_ice <- partial(RFRslope_model2, pred.var = "VegUnit", ice= TRUE, plot = FALSE)
saveRDS(VegUnitPDP_ice, file="MaxNDVI_VegUnit_PDPdataICE_09Apr2021.RDS")

jpeg(file="MaxNDVI_IncMSEmodel_09Apr2021.jpeg")
varImpPlot(finalModel2, type = 1, main = "Max NDVI Random Forest Regression Results")
dev.off()

save(finalModel2, file="MaxNDVI_UpdatedRFRmodel_25boot_09Apr2021.Rdata")