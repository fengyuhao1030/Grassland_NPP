rm(list = ls())
library(readr)
library(data.table)
library(randomForest)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
outputPath_1 <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path),'\\Models')
outputPath_2 <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path),'\\Tables')

sourceData <- read.csv(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),'\\Data\\Example data.csv'))
sourceData$Formation <- as.factor(sourceData$Formation)
MAP2 <- sourceData$MAP^2
sourceData <- cbind(sourceData,MAP2)
meanMAT <- mean(sourceData[,'MAT']); sdMAT <- sd(sourceData[,'MAT'])
meanMAP <- mean(sourceData[,'MAP']); sdMAP <- sd(sourceData[,'MAP'])
meanMAP2 <- mean(sourceData[,'MAP2']); sdMAP2 <- sd(sourceData[,'MAP2'])
meanSOL <- mean(sourceData[,'SOL']); sdSOL <- sd(sourceData[,'SOL'])
meanPET <- mean(sourceData[,'PET']); sdPET <- sd(sourceData[,'PET'])
meanAI <- mean(sourceData[,'AI']); sdAI <- sd(sourceData[,'AI'])
meanSTN <- mean(sourceData[,'STN']); sdSTN <- sd(sourceData[,'STN'])
meanSOC <- mean(sourceData[,'SOC']); sdSOC <- sd(sourceData[,'SOC'])
meanPH <- mean(sourceData[,'PH']); sdPH <- sd(sourceData[,'PH'])
meanBD <- mean(sourceData[,'BD']); sdBD <- sd(sourceData[,'BD'])
meanCN <- mean(sourceData[,'CN']); sdCN <- sd(sourceData[,'CN'])
meanSILT <- mean(sourceData[,'SILT']); sdSILT <- sd(sourceData[,'SILT'])
meanCLAY <- mean(sourceData[,'CLAY']); sdCLAY <- sd(sourceData[,'CLAY'])
meanElev <- mean(sourceData[,'ELEV']); sdElev <- sd(sourceData[,'ELEV'])
meanASP <- mean(sourceData[,'ASP']); sdASP <- sd(sourceData[,'ASP'])
meanSLOPE <- mean(sourceData[,'SLOPE']); sdSLOPE <- sd(sourceData[,'SLOPE'])
sourceData$MAT <- (sourceData$MAT - meanMAT)/sdMAT
sourceData$MAP <- (sourceData$MAP - meanMAP)/sdMAP
sourceData$MAP2 <- (sourceData$MAP2 - meanMAP2)/sdMAP2
sourceData$SOL <- (sourceData$SOL - meanSOL)/sdSOL
sourceData$PET <- (sourceData$PET - meanPET)/sdPET
sourceData$AI <- (sourceData$AI - meanAI)/sdAI
sourceData$STN <- (sourceData$STN - meanSTN)/sdSTN
sourceData$SOC <- (sourceData$SOC - meanSOC)/sdSOC
sourceData$PH <- (sourceData$PH - meanPH)/sdPH
sourceData$BD <- (sourceData$BD - meanBD)/sdBD
sourceData$CN <- (sourceData$CN - meanCN)/sdCN
sourceData$SILT <- (sourceData$SILT - meanSILT)/sdSILT
sourceData$CLAY <- (sourceData$CLAY - meanCLAY)/sdCLAY
sourceData$ELEV <- (sourceData$ELEV - meanElev)/sdElev
sourceData$ASP <- (sourceData$ASP - meanASP)/sdASP
sourceData$SLOPE <- (sourceData$SLOPE - meanSLOPE)/sdSLOPE

# Generate random forest models
rfTreeNum <- 200
rfVarNum <- 4  
for(i in seq(1,500)){
  selectIndex <- sample(seq(1,nrow(sourceData)),floor(0.3*nrow(sourceData)),replace = FALSE)
  trainData <- sourceData[-selectIndex,]
  testData <- sourceData[selectIndex,]
  assign(paste0('rfModel_',as.character(i)),randomForest(ANPP ~ MAT+MAP+MAP2+SOL+PET+AI+STN+SOC+PH+BD+CN+SILT+CLAY+ELEV+ASP+SLOPE+Formation,
                                                         data = trainData,ntree = rfTreeNum,mtry = rfVarNum))
  save(list = paste0('rfModel_',as.character(i)),file = paste0(outputPath_1,'\\rfModel_',as.character(i)))
  rm(list = paste0('rfModel_',as.character(i)))
  print(paste0('rfModel_',as.character(i)))
}

# Predict
targetFileName <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path),'\\Data\\Data for prediction (73W_70W_53S_50S).csv')
targetData <- read_csv(targetFileName)
MAP2 <- targetData$MAP^2
targetData <- cbind(targetData,MAP2)
targetData$MAT <- (targetData$MAT - meanMAT)/sdMAT
targetData$MAP <- (targetData$MAP - meanMAP)/sdMAP
targetData$MAP2 <- (targetData$MAP2 - meanMAP2)/sdMAP2
targetData$SOL <- (targetData$SOL - meanSOL)/sdSOL
targetData$PET <- (targetData$PET - meanPET)/sdPET
targetData$AI <- (targetData$AI - meanAI)/sdAI
targetData$STN <- (targetData$STN - meanSTN)/sdSTN
targetData$SOC <- (targetData$SOC - meanSOC)/sdSOC
targetData$PH <- (targetData$PH - meanPH)/sdPH
targetData$BD <- (targetData$BD - meanBD)/sdBD
targetData$CN <- (targetData$CN - meanCN)/sdCN
targetData$SILT <- (targetData$SILT - meanSILT)/sdSILT
targetData$CLAY <- (targetData$CLAY - meanCLAY)/sdCLAY
targetData$ELEV <- (targetData$ELEV - meanElev)/sdElev
targetData$ASP <- (targetData$ASP - meanASP)/sdASP
targetData$SLOPE <- (targetData$SLOPE - meanSLOPE)/sdSLOPE
targetData$Formation <- factor(targetData$Formation,levels = levels(sourceData$Formation))
# Predict
resultMatrix <- cbind(targetData$POINT_X,targetData$POINT_Y)
colnames(resultMatrix) <- c('Lon','Lat')
for(j in seq(1,500)){
  load(paste0(outputPath_1,'\\rfModel_',as.character(j)))
  predictValues <- predict(get(paste0('rfModel_',as.character(j))),targetData)
  resultMatrix <- cbind(resultMatrix,predictValues)
  rm(list = paste0('rfModel_',as.character(j)))
  print(as.character(j))
}
outputFileName <- paste0(outputPath_2,'\\Result.csv')
fwrite(resultMatrix,file = outputFileName,row.names = FALSE)