library(readxl)
library(doParallel)
library(randomForest)
library(caret)
library(pdp)

RFR_input <- read_excel("./RFRinput_TINDVI0.05_24Apr2022.xlsx",
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
RFRslope_model2 <- train(TINDVI~., data=RFR_input, method='rf', tuneGrid=tunegrid, trControl=control, importance=TRUE)

finalModel2 <- RFRslope_model2$finalModel

#Export the data associated with Coast Dist PDP plot (no ICE)
CoastDistPDP <- partial(RFRslope_model2, pred.var = "CoastDist", plot = FALSE)
saveRDS(CoastDistPDP, file="TI-NDVI_CoastDist_PDPdata_24Apr2022.RDS")

#Export the data associated with Elevation PDP plot (no ICE)
ElevationPDP <- partial(RFRslope_model2, pred.var = "Elevation", plot = FALSE)
saveRDS(ElevationPDP, file="TI-NDVI_Elevation_PDPdata_24Apr2022.RDS")

#Export the data associated with Human Mod PDP plot (no ICE)
HumanModPDP <- partial(RFRslope_model2, pred.var = "HumanMod", plot = FALSE)
saveRDS(HumanModPDP, file="TI-NDVI_HumanMod_PDPdata_24Apr2022.RDS")

#Export the data associated with Land Age PDP plot (no ICE)
LandAgePDP <- partial(RFRslope_model2, pred.var = "LandAge", plot = FALSE)
saveRDS(LandAgePDP, file="TI-NDVI_LandAge_PDPdata_24Apr2022.RDS")

#Export the data associated with Precipitation PDP plot (no ICE)
PrecipPDP <- partial(RFRslope_model2, pred.var = "Precipitation", plot = FALSE)
saveRDS(PrecipPDP, file="TI-NDVI_Precip_PDPdata_24Apr2022.RDS")

#Export the data associated with Snow Free Date PDP plot (no ICE)
SnowFreePDP <- partial(RFRslope_model2, pred.var = "SnowFreeDate", plot = FALSE)
saveRDS(SnowFreePDP, file="TI-NDVI_SnowFree_PDPdata_24Apr2022.RDS")

#Export the data associated with Soil Moisture PDP plot (no ICE)
SoilMoisturePDP <- partial(RFRslope_model2, pred.var = "SoilMoisture", plot = FALSE)
saveRDS(SoilMoisturePDP, file="TI-NDVI_SoilMoisture_PDPdata_24Apr2022.RDS")

#Export the data associated with Soil Texture PDP plot (no ICE)
SoilTexturePDP <- partial(RFRslope_model2, pred.var = "SoilTexture", plot = FALSE)
saveRDS(SoilTexturePDP, file="TI-NDVI_SoilTexture_PDPdata_24Apr2022.RDS")

#Export the data associated with Substrate Chem PDP plot (no ICE)
SubChemPDP <- partial(RFRslope_model2, pred.var = "SubstrateChem", plot = FALSE)
saveRDS(SubChemPDP, file="TI-NDVI_SubChem_PDPdata_24Apr2022.RDS")

#Export the data associated with SWI PDP plot (no ICE)
SummerWarmthPDP <- partial(RFRslope_model2, pred.var = "SummerWarmth", plot = FALSE)
saveRDS(SummerWarmthPDP, file="TI-NDVI_SummerWarmth_PDPdata_24Apr2022.RDS")

#Export the data associated with VegUnit PDP plot (no ICE)
VegUnitPDP <- partial(RFRslope_model2, pred.var = "VegUnit", plot = FALSE)
saveRDS(VegUnitPDP, file="TI-NDVI_VegUnit_PDPdata_24Apr2022.RDS")

jpeg(file="TI-NDVI_IncMSEmodel_24Apr2022.jpeg")
varImpPlot(finalModel2, type = 1, main = "TI-NDVI 0.05 Random Forest Regression Results")
dev.off()

save(finalModel2, file="TI-NDVI_UpdatedRFRmodel_25boot_24Apr2022.Rdata")