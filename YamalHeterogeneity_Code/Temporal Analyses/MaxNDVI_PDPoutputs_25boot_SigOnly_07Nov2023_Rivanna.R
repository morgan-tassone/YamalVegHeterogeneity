library(readxl)
library(doParallel)
library(randomForest)
library(caret)
library(pdp)

RFR_input <- read_excel("./RFRinput_MaxNDVI_SigOnly_07Nov2023.xlsx",
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

#Set control method and bootstrap iterations, set seed for reproducible results, set mtry values to test
control <- trainControl(method = "boot", number=25, search="grid")
set.seed(2)
tunegrid <- expand.grid(.mtry=c(2:5))

#Run random forest regression
RFRslope_model_SigOnly <- train(MaxNDVI~., data=RFR_input, method='rf', tuneGrid=tunegrid, trControl=control, importance=TRUE)

finalModel_SigOnly <- RFRslope_model_SigOnly$finalModel

#Export the data associated with Coast Dist PDP plot (no ICE)
CoastDistPDP <- partial(RFRslope_model_SigOnly, pred.var = "CoastDist", plot = FALSE)
saveRDS(CoastDistPDP, file="MaxNDVI_CoastDist_PDPdata_07Nov2023.RDS")


#Export the data associated with Elevation PDP plot (no ICE)
ElevationPDP <- partial(RFRslope_model_SigOnly, pred.var = "Elevation", plot = FALSE)
saveRDS(ElevationPDP, file="MaxNDVI_Elevation_PDPdata_07Nov2023.RDS")


#Export the data associated with Human Mod PDP plot (no ICE)
HumanModPDP <- partial(RFRslope_model_SigOnly, pred.var = "HumanMod", plot = FALSE)
saveRDS(HumanModPDP, file="MaxNDVI_HumanMod_PDPdata_07Nov2023.RDS")


#Export the data associated with Land Age PDP plot (no ICE)
LandAgePDP <- partial(RFRslope_model_SigOnly, pred.var = "LandAge", plot = FALSE)
saveRDS(LandAgePDP, file="MaxNDVI_LandAge_PDPdata_07Nov2023.RDS")

#Export the data associated with Precipitation PDP plot (no ICE)
PrecipPDP <- partial(RFRslope_model_SigOnly, pred.var = "Precipitation", plot = FALSE)
saveRDS(PrecipPDP, file="MaxNDVI_Precip_PDPdata_07Nov2023.RDS")

#Export the data associated with Snow Free Date PDP plot (no ICE)
SnowFreePDP <- partial(RFRslope_model_SigOnly, pred.var = "SnowFreeDate", plot = FALSE)
saveRDS(SnowFreePDP, file="MaxNDVI_SnowFree_PDPdata_07Nov2023.RDS")

#Export the data associated with Soil Moisture PDP plot (no ICE)
SoilMoisturePDP <- partial(RFRslope_model_SigOnly, pred.var = "SoilMoisture", plot = FALSE)
saveRDS(SoilMoisturePDP, file="MaxNDVI_SoilMoisture_PDPdata_07Nov2023.RDS")

#Export the data associated with Soil Texture PDP plot (no ICE)
SoilTexturePDP <- partial(RFRslope_model_SigOnly, pred.var = "SoilTexture", plot = FALSE)
saveRDS(SoilTexturePDP, file="MaxNDVI_SoilTexture_PDPdata_07Nov2023.RDS")

#Export the data associated with Substrate Chem PDP plot (no ICE)
SubChemPDP <- partial(RFRslope_model_SigOnly, pred.var = "SubstrateChem", plot = FALSE)
saveRDS(SubChemPDP, file="MaxNDVI_SubChem_PDPdata_07Nov2023.RDS")

#Export the data associated with SWI PDP plot (no ICE)
SummerWarmthPDP <- partial(RFRslope_model_SigOnly, pred.var = "SummerWarmth", plot = FALSE)
saveRDS(SummerWarmthPDP, file="MaxNDVI_SummerWarmth_PDPdata_07Nov2023.RDS")

#Export the data associated with VegUnit PDP plot (no ICE)
VegUnitPDP <- partial(RFRslope_model_SigOnly, pred.var = "VegUnit", plot = FALSE)
saveRDS(VegUnitPDP, file="MaxNDVI_VegUnit_PDPdata_07Nov2023.RDS")

jpeg(file="MaxNDVI_IncMSEmodel_SigOnly_07Nov2023.jpeg")
varImpPlot(finalModel_SigOnly, type = 1, main = "Max NDVI (Significant Trends) Random Forest Regression Results")
dev.off()

save(finalModel_SigOnly, file="MaxNDVI_UpdatedRFRmodel_25boot_SigOnly_07Nov2023.Rdata")