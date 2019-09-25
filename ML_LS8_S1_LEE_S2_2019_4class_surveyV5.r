setwd("C:/MasseyOffice1/Research/R-Script2019/")
rm(list=ls())

detachAllPackages <- function() {
  
  basic.packages <- c("package:ggplot2","package:lattice","package:caret","package:foreach","package:iterators","package:parallel","package:doParallel","package:reshape","package:plyr","package:utils","package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}


library(caret)
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
library(reshape)
library(plyr)
library(utils)
library(dplyr)
set.seed(7)

control <- trainControl(method="LOOCV")
name_fmask='WithMasking'
metric <- "Accuracy"

library(readxl)
ML_LS8_S1_LEE_S2_2019_4class_surveyV5 <- read_excel("C:/MasseyOffice1/Research/R-Script2019/ML_LS8_S1_LEE_S2_2019_4class_surveyV5.xlsx")

ML_LS8_S1_LEE_S2_2019_4class_surveyV5$Phase <- as.factor(ML_LS8_S1_LEE_S2_2019_4class_surveyV5$Phase)
set.seed(7)
# 
# NR <- filter(ML_LS8_S1_LEE_S2_2019_4class_surveyV5, Phase == "1")
# VEG <- filter(ML_LS8_S1_LEE_S2_2019_4class_surveyV5, Phase == "2")
# REPR <- filter(ML_LS8_S1_LEE_S2_2019_4class_surveyV5, Phase == "3")
# RIPE <- filter(ML_LS8_S1_LEE_S2_2019_4class_surveyV5, Phase == "4")
# 
# numberofrows <- c(nrow(NR),nrow(VEG),nrow(REPR),nrow(RIPE))
# minrows <- min(numberofrows)
# index <- sample(1:nrow(NR), minrows)
# NR_sample <- NR[index, ]
# index <- sample(1:nrow(VEG), minrows)
# VEG_sample <- VEG[index, ]
# index <- sample(1:nrow(REPR), minrows)
# REPR_sample <- REPR[index, ]
# index <- sample(1:nrow(RIPE), minrows)
# RIPE_sample <- RIPE[index, ]
# 
# ML_LS8_S1_LEE_S2_2019_4class_surveyV5_sampling_down <- rbind(NR_sample, VEG_sample,REPR_sample,RIPE_sample)
# ML_LS8_S1_LEE_S2_2019_4class_surveyV5_sampling_down <-  ML_LS8_S1_LEE_S2_2019_4class_surveyV5_sampling_down[sample(1:nrow(ML_LS8_S1_LEE_S2_2019_4class_surveyV5_sampling_down)), ]
ML_LS8_S1_LEE_S2_2019_4class_surveyV5_sampling_down <- ML_LS8_S1_LEE_S2_2019_4class_surveyV5
rice_fmask <- ML_LS8_S1_LEE_S2_2019_4class_surveyV5_sampling_down
trainIndex <- createDataPartition(rice_fmask$Phase, p=0.7, list=FALSE)
train.data <- rice_fmask[ trainIndex,]
test.data <- rice_fmask[-trainIndex,]
# write.csv(train.data,file="ML_LS8_S1_LEE_S2_2019_4class_surveyV5 train data ML.csv")
# write.csv(test.data,file="ML_LS8_S1_LEE_S2_2019_4class_surveyV5 test data ML.csv")

rice <-ML_LS8_S1_LEE_S2_2019_4class_surveyV5_sampling_down
rice$PHASE <- factor(rice$Phase)
print(summary(rice))



allModelsList <- c("Phase ~ S2_B02 + S2_B03 + S2_B04 + S2_B05 + S2_B06 + S2_B07 + S2_B08 + S2_B11 + S2_B12 + S2_B8A"
                  
)
library(stringr)

NoOfCoef <- str_count(allModelsList,"\\+") + 1
# 
# 
# control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# # run the RFE algorithm
# results <- rfe(train.data[,5:28], train.data[[29]], sizes=c(1:24), rfeControl=control)
# # summarize the results
# print(results)
# print(results$optVariables)
# # list the chosen features
# predictors(results)
# # plot the results
# plot(results, type=c("g", "o"))

#rice_all_indexes <- rice[,c("Blue","Green","Red","NIR","SWIR1","SWIR2")]

# for (limit in c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
#   x05 <- findCorrelation(rice_all_indexes, cutoff = limit, exact = FALSE)
#   formula05 <- rice_all_indexes[,-x05]
#   formula05_colnames <- colnames(formula05)
#   formula05_colnumber <- length(formula05_colnames)
#   formula05List <- gsub("~  [+]","~", paste(c("Phase ~ ", formula05_colnames), collapse=" + "))
#   if (formula05List %in% allModelsList==FALSE){
#     allModelsList <- append(allModelsList,formula05List)
#     NoOfCoef <- append(NoOfCoef,formula05_colnumber)
#   }
# 
# }

allModelsList



#Leave One Out Cross Validation
name_data='LOOCV'

#model <- names(getModelInfo())
#model_filter <- c('AdaBag','amdai','avNNet','bagFDA','bagFDAGCV','bayesglm','BstLm','bstSm','bstTree','C5.0','C5.0Rules','C5.0Tree','cforest','CSimca','ctree','ctree2','dnn','earth','elm','fda','gam','gamLoess','gamSpline','gbm','gcvEarth','glmnet','hda','hdda','hdrda','J48','JRip','kernelpls','kknn','knn','lda','lda2','Linda','LMT','loclda','LogitBoost','lvq','mda','Mlda','mlpSGD','monmlp','multinom','naive_bayes','nnet','OneR','ownn','pam','parRF','PART','partDSA','pcaNNet','pda','PenalizedLDA','plr','pls','polr','qda','QdaCov','rda','regLogistic','rf','RFlda','rlda','rmda','rpart','rpart1SE','rpart2','rpartScore','rrlda','RSimca','sda','sdwd','simpls','snn','sparseLDA','spls','stepLDA','stepQDA','svmLinear','svmLinear2','svmLinear3','svmPoly','svmRadial','svmRadialCost','svmRadialSigma','treebag','vglmAdjCat','widekernelpls','wsrf')
#model_filter <- c('amdai','avNNet','bagFDA','bagFDAGCV','bayesglm','BstLm','bstSm','bstTree','C5.0','C5.0Rules','C5.0Tree','cforest','CSimca','ctree','ctree2','dnn','earth','elm','fda','gam','gamLoess','gamSpline','gbm','gcvEarth','glmnet','hda','hdda','hdrda','J48','JRip','kernelpls','kknn','knn','lda','lda2','Linda','LMT','loclda','LogitBoost','lvq','mda','Mlda','mlpSGD','monmlp','multinom','naive_bayes','nnet','OneR','ownn','pam','parRF','PART','partDSA','pcaNNet','pda','PenalizedLDA','plr','pls','polr','qda','QdaCov','rda','regLogistic','rf','RFlda','rlda','rmda','rpart','rpart1SE','rpart2','rpartScore','rrlda','RSimca','sda','sdwd','simpls','snn','sparseLDA','spls','stepLDA','stepQDA','svmLinear','svmLinear2','svmLinear3','svmPoly','svmRadial','svmRadialCost','svmRadialSigma','treebag','vglmAdjCat','widekernelpls','wsrf')
#model_filter <- c('avNNet','bagFDA','bagFDAGCV','bayesglm','BstLm','bstSm','bstTree','C5.0','C5.0Rules','C5.0Tree','cforest','CSimca','ctree','ctree2','dnn','earth','elm','fda','gam','gamLoess','gamSpline','gbm','gcvEarth','glmnet','hda','hdda','hdrda','J48','JRip','kernelpls','kknn','knn','lda','lda2','Linda','LMT','loclda','LogitBoost','lvq','mda','Mlda','mlpSGD','monmlp','multinom','naive_bayes','nnet','OneR','ownn','pam','parRF','PART','partDSA','pcaNNet','pda','PenalizedLDA','plr','pls','polr','qda','QdaCov','rda','regLogistic','rf','RFlda','rlda','rmda','rpart','rpart1SE','rpart2','rpartScore','rrlda','RSimca','sda','sdwd','simpls','snn','sparseLDA','spls','stepLDA','stepQDA','svmLinear','svmLinear2','svmLinear3','svmPoly','svmRadial','svmRadialCost','svmRadialSigma','treebag','vglmAdjCat','widekernelpls','wsrf')

model_filter <- c('C5.0','C5.0Rules','C5.0Tree','C5.0Cost','nnet','fda','rf','svmLinear','svmPoly','svmRadial')
model_filter <- c('nnet','rf','svmLinear','svmPoly','svmRadial')

model_filter <- c('svmRadial','rf')

mainDir <- paste("C:/MasseyOffice1/Research/R-Script2019/ML_LS8_S1_LEE_S2_2019_4class_surveyV5/",collapse = "",sep = "")
if (file.exists(mainDir)==FALSE) {
  dir.create(file.path(mainDir), showWarnings = FALSE)
}
for (model_ML in model_filter) {
  
  subDir <- model_ML
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  setwd(file.path(mainDir, subDir))
  
  print (model_ML)
  set.seed(7)
  detachAllPackages()
  
  for(i_formula in 1:length(allModelsList)) {
    formula_current <- as.formula(allModelsList[[i_formula]])
    
    for(i_preName in 1:1) {
      
      if (i_preName==1) {
        preName_current ='noProc'
        preName_current_string <- "noProc"
      }
      if (i_preName==2) {
        preName_current =c('center','scale')
        preName_current_string <- "center_scale"
      }
      if (i_preName==3) {
        preName_current =c('corr')
        preName_current_string <- "corr"
      }
      if (i_preName==4) {
        preName_current =c('pca')
        preName_current_string <- "pca"
      }
      if (i_preName==5) {
        preName_current =c("BoxCox", "center", "scale", "pca")
        preName_current_string <- "BoxCox_center_scale_pca"
      }
      numbering <- sprintf("%04d", i_formula)
      filename <- paste(name_fmask,"_",name_data,"_", "ML_LS8_S1_LEE_S2_2019_4class_surveyV5_",model_ML,"_",preName_current_string,"_",numbering,".csv", sep="")
      if (file.exists(filename)) {
        next
      }
      print(paste(model_ML,i_formula,format(Sys.time(), "%a %b %d %X %Y"),as.character(formula_current[3]),sep=" | "))
      
      if (model_ML=="fda") {
        TuneGrid <- expand.grid(.degree = c(1,2,3),
                                .nprune = (1:100) * 2)
      } else if (model_ML=="nnet") {
        TuneGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1),decay = c(0, 0.0001,0.001,0.01,0.1))
      } else  if (model_ML=="rf") {
        TuneGrid <-  expand.grid(mtry  = seq(from = 1, to = 20, by = 1))
      } else  if (model_ML=="svmLinear") {
        TuneGrid <-  expand.grid(C= 2^c(0:10))
      } else  if (model_ML=="svmPoly") {
        TuneGrid <- expand.grid(degree=(1:5), scale=c(0.001,0.01,0.1), C=c(0.01,0.1,1,3,5,10,20))
      }else  if (model_ML=="svmRadial") {
        TuneGrid <- expand.grid(sigma= 2^c(-25, -20, -15,-10, -5, 0), C= 2^c(0:10))
      }
      
      model_other <-  c('nnet','fda','rf','svmLinear','svmPoly','svmRadial')
      if (any(model_other == model_ML) ) {
        if (preName_current=="noProc") {
          allModelsResults <- train(formula_current, data=train.data, tuneGrid = TuneGrid, method=model_ML, metric=metric, trControl=control)
        } else {
          allModelsResults <- train(formula_current, data=train.data, tuneGrid = TuneGrid, preProcess = c(preName_current), method=model_ML, metric=metric, trControl=control)
        } 
      } else {
        if (preName_current=="noProc") {
          allModelsResults <- train(formula_current, data=train.data, method=model_ML, metric=metric, trControl=control)
        } else {
          allModelsResults <- train(formula_current, data=train.data, preProcess = c(preName_current), method=model_ML, metric=metric, trControl=control)
        }
      }
      filename_save <- paste(name_fmask,"_",name_data,"_", "ML_LS8_S1_LEE_S2_2019_4class_surveyV5_",model_ML,"_",preName_current_string,"_",numbering,".rds", sep="")
      saveRDS(allModelsResults, filename_save)
      model_varimp <-  c('nnet','fda','rf')
      if (any(model_varimp == model_ML) ) {
        allModelsResults_varImp <- varImp(allModelsResults)
        allModelsResults_varImp_1 <- allModelsResults_varImp$importance
        filename_varImp <- paste("varImp_",name_fmask,"_",name_data,"_", "ML_LS8_S1_LEE_S2_2019_4class_surveyV5_",model_ML,"_",preName_current_string,"_",numbering,"_all.csv", sep="")
        write.csv(allModelsResults_varImp_1, file = filename_varImp)
      }
      allModelsResults_predictions <- predict(allModelsResults,test.data)
      
      allModelsResults_confusionMatrix <- confusionMatrix(allModelsResults_predictions, test.data$Phase)
      
      
      filename_rfc_overall <- paste("ConMat_overall_",name_fmask,"_",name_data,"_", "ML_LS8_S1_LEE_S2_2019_4class_surveyV5_",model_ML,"_",preName_current_string,"_",numbering,".csv", sep="")
      filename_rfc_table <- paste("ConMat_table_",name_fmask,"_",name_data,"_", "ML_LS8_S1_LEE_S2_2019_4class_surveyV5_",model_ML,"_",preName_current_string,"_",numbering,".csv", sep="")
      filename_rfc_class <- paste("ConMat_class_",name_fmask,"_",name_data,"_", "ML_LS8_S1_LEE_S2_2019_4class_surveyV5_",model_ML,"_",preName_current_string,"_",numbering,".csv", sep="")
      
      write.csv(allModelsResults_confusionMatrix$table, file = filename_rfc_table)
      write.csv(allModelsResults_confusionMatrix$overall, file = filename_rfc_overall)
      write.csv(allModelsResults_confusionMatrix$byClass, file = filename_rfc_class)
      
      dfTest  <- as.data.frame(allModelsResults_confusionMatrix$overall)
      
      y =paste("test.",names(allModelsResults_confusionMatrix$overall),sep="")
      
      rownames(dfTest) <- y
      
      #CM
      dfTest_CM  <- as.data.frame(allModelsResults_confusionMatrix$byClass)
      
      dfTest_CM <- melt(allModelsResults_confusionMatrix$byClass)
      y <- paste(dfTest_CM[,1],dfTest_CM[,2],sep='.')
      y <- gsub(": ", ".", y)
      
      
      rownames(dfTest_CM) <- paste("testCM.",y,sep="")
      dfTest_CM$X1 <- NULL
      dfTest_CM$X2 <- NULL
      
      dfCoefNum   <- as.data.frame(allModelsResults["results"])
      
      
      dfLengthData_train <- as.data.frame(c(nrow(train.data)))
      colnames(dfLengthData_train) <- c("Count.Train")
      
      dfLengthData_test <- as.data.frame(c(nrow(test.data)))
      colnames(dfLengthData_test) <- c("Count.Test")
      names_formula <- paste(as.character(formula_current)[c(2,1,3)], collapse = " ")
      NoOfCoef_current <- NoOfCoef[[i_formula]]
      results <- data.frame( model = names_formula, Method=model_ML,
                             dfLengthData_train,
                             dfLengthData_test,
                             NoOfCoef = NoOfCoef_current,
                             preProc=preName_current_string,
                             dfCoefNum,
                             t(dfTest),
                             t(dfTest_CM),
                             stringsAsFactors=FALSE
      )
      
      write.csv(results, file = filename)
      
      model_varimp <-  c('nnet','fda','rf')
      if (any(model_varimp == model_ML) ) {
        allModelsResults_varImp <- varImp(allModelsResults)
        allModelsResults_varImp_1 <- allModelsResults_varImp$importance
        filename_varImp <- paste("varImp_",name_fmask,"_",name_data,"_", "ML_LS8_S1_LEE_S2_2019_4class_surveyV5_",model_ML,"_",preName_current_string,".csv", sep="")
        write.csv(allModelsResults_varImp_1, file = filename_varImp)
      }
      
    } 
  } 
}

setwd("C:/MasseyOffice1/Research/R-Script2019/")
