rm(list=ls())


filename_recap <- paste("C:/MasseyOffice1/Research/R-Script/SR_output_model_LS8_2018_indramayu_edit/SR_output_model_LS8_2018_indramayu_edit.csv",sep="")
results <- data.frame(namefile=character(),
                      size=character(),
                      masking=character(), 
                      sampling=character(),
                      source=character(),
                      method=character(),
                      Count.Train = integer(),
                      Count.Test = integer(),
                      NoOfCoef = integer(),
                      preProc = character(),
                      model=character(),
                      results.Accuracy = double(),
                      results.Kappa = double(),
                      test.Accuracy = double(),
                      test.Kappa = double(),
                      test.AccuracyLower = double(),
                      test.AccuracyUpper = double(),
                      test.AccuracyNull = double(),
                      test.AccuracyPValue = double(),
                      test.McnemarPValue = double(),
                      testCM.Class.Non.rice.Sensitivity = double(),
                      testCM.Class.Reproductive.Sensitivity = double(),
                      testCM.Class.Ripening.Sensitivity = double(),
                      testCM.Class.Vegetative.Sensitivity = double(),
                      testCM.Class.Non.rice.Specificity = double(),
                      testCM.Class.Reproductive.Specificity = double(),
                      testCM.Class.Ripening.Specificity = double(),
                      testCM.Class.Vegetative.Specificity = double(),
                      testCM.Class.Non.rice.Pos.Pred.Value = double(),
                      testCM.Class.Reproductive.Pos.Pred.Value = double(),
                      testCM.Class.Ripening.Pos.Pred.Value = double(),
                      testCM.Class.Vegetative.Pos.Pred.Value = double(),
                      testCM.Class.Non.rice.Neg.Pred.Value = double(),
                      testCM.Class.Reproductive.Neg.Pred.Value = double(),
                      testCM.Class.Ripening.Neg.Pred.Value = double(),
                      testCM.Class.Vegetative.Neg.Pred.Value = double(),
                      testCM.Class.Non.rice.Precision = double(),
                      testCM.Class.Reproductive.Precision = double(),
                      testCM.Class.Ripening.Precision = double(),
                      testCM.Class.Vegetative.Precision = double(),
                      testCM.Class.Non.rice.Recall = double(),
                      testCM.Class.Reproductive.Recall = double(),
                      testCM.Class.Ripening.Recall = double(),
                      testCM.Class.Vegetative.Recall = double(),
                      testCM.Class.Non.rice.F1 = double(),
                      testCM.Class.Reproductive.F1 = double(),
                      testCM.Class.Ripening.F1 = double(),
                      testCM.Class.Vegetative.F1 = double(),
                      testCM.Class.Non.rice.Prevalence = double(),
                      testCM.Class.Reproductive.Prevalence = double(),
                      testCM.Class.Ripening.Prevalence = double(),
                      testCM.Class.Vegetative.Prevalence = double(),
                      testCM.Class.Non.rice.Detection.Rate = double(),
                      testCM.Class.Reproductive.Detection.Rate = double(),
                      testCM.Class.Ripening.Detection.Rate = double(),
                      testCM.Class.Vegetative.Detection.Rate = double(),
                      testCM.Class.Non.rice.Detection.Prevalence = double(),
                      testCM.Class.Reproductive.Detection.Prevalence = double(),
                      testCM.Class.Ripening.Detection.Prevalence = double(),
                      testCM.Class.Vegetative.Detection.Prevalence = double(),
                      testCM.Class.Non.rice.Balanced.Accuracy = double(),
                      testCM.Class.Reproductive.Balanced.Accuracy = double(),
                      testCM.Class.Ripening.Balanced.Accuracy = double(),
                      testCM.Class.Vegetative.Balanced.Accuracy = double(),
                      
                      stringsAsFactors = FALSE)
model <- c('avNNet','bagFDA','bagFDAGCV','bayesglm','BstLm','bstSm','bstTree','C5.0','C5.0Rules','C5.0Tree','cforest','CSimca','ctree','ctree2','dnn','earth','elm','fda','gam','gamLoess','gamSpline','gbm','gcvEarth','glmnet','hda','hdda','hdrda','J48','JRip','kernelpls','kknn','knn','lda','lda2','Linda','LMT','loclda','LogitBoost','lvq','mda','Mlda','mlpSGD','monmlp','multinom','naive_bayes','nnet','OneR','ownn','pam','parRF','PART','partDSA','pcaNNet','pda','PenalizedLDA','plr','pls','polr','qda','QdaCov','rda','regLogistic','rf','RFlda','rlda','rmda','rpart','rpart1SE','rpart2','rpartScore','rrlda','RSimca','sda','sdwd','simpls','snn','sparseLDA','spls','stepLDA','stepQDA','svmLinear','svmLinear2','svmLinear3','svmPoly','svmRadial','svmRadialCost','svmRadialSigma','treebag','vglmAdjCat','widekernelpls','wsrf')

mainDir_root <- "C:/MasseyOffice1/Research/R-Script/SR_output_model_LS8_2018_indramayu_edit"
listfolder <- list.dirs(path = mainDir_root, full.names = FALSE, recursive = FALSE)


mainDir <- paste("C:/MasseyOffice1/Research/R-Script/SR_output_model_LS8_2018_indramayu_edit/",collapse = "",sep = "")
folder_split <- 100
for (model_ML in model) {
  subDir <- model_ML
  if (file.exists(file.path(mainDir, subDir))){
    setwd(file.path(mainDir, subDir))
    files <- list.files(pattern = glob2rx("WithMasking*.csv"))
    if (length(files) != 0) {
      for (i in 1:length(files)){
        
        filename <-files[i]
        print(filename)
        temp <- substr(filename,1,nchar(filename)-4)
        temp_model <- strsplit(temp,"_")
        
        file_model <- read.csv(filename)
        
        max_row <- file_model[which.max(file_model$results.Accuracy),]
        
        iterate1 <- 0
        for (j in 1:ncol(max_row)){
          if (names(max_row)[j] == 'results.Accuracy') {
            iterate1 <- j
          }
        }
        
        iterate2 <- 0
        for (j in 1:ncol(max_row)){
          if (names(max_row)[j] == 'test.Accuracy') {
            iterate2 <- j
          }
        }
        
        results[nrow(results) + 1, ] <- c(as.character(filename),
                                          as.character(folder_split), 
                                          as.character(temp_model[[1]][1]), 
                                          as.character(temp_model[[1]][2]),
                                          as.character(temp_model[[1]][3]), 
                                          as.character(temp_model[[1]][4]), 
                                          as.integer(max_row$Count.Train),
                                          as.integer(max_row$Count.Test),
                                          as.integer(max_row$NoOfCoef),
                                          as.character(max_row$preProc),
                                          as.character(as.character(max_row$model)),
                                          max_row[iterate1],
                                          max_row[iterate1+1],
                                          max_row[iterate2+0],
                                          max_row[iterate2+1],
                                          max_row[iterate2+2],
                                          max_row[iterate2+3],
                                          max_row[iterate2+4],
                                          max_row[iterate2+5],
                                          max_row[iterate2+6],
                                          max_row[iterate2+7],
                                          max_row[iterate2+8],
                                          max_row[iterate2+9],
                                          max_row[iterate2+10],
                                          max_row[iterate2+11],
                                          max_row[iterate2+12],
                                          max_row[iterate2+13],
                                          max_row[iterate2+14],
                                          max_row[iterate2+15],
                                          max_row[iterate2+16],
                                          max_row[iterate2+17],
                                          max_row[iterate2+18],
                                          max_row[iterate2+19],
                                          max_row[iterate2+20],
                                          max_row[iterate2+21],
                                          max_row[iterate2+22],
                                          max_row[iterate2+23],
                                          max_row[iterate2+24],
                                          max_row[iterate2+25],
                                          max_row[iterate2+26],
                                          max_row[iterate2+27],
                                          max_row[iterate2+28],
                                          max_row[iterate2+29],
                                          max_row[iterate2+30],
                                          max_row[iterate2+31],
                                          max_row[iterate2+32],
                                          max_row[iterate2+33],
                                          max_row[iterate2+34],
                                          max_row[iterate2+35],
                                          max_row[iterate2+36],
                                          max_row[iterate2+37],
                                          max_row[iterate2+38],
                                          max_row[iterate2+39],
                                          max_row[iterate2+40],
                                          max_row[iterate2+41],
                                          max_row[iterate2+42],
                                          max_row[iterate2+43],
                                          max_row[iterate2+44],
                                          max_row[iterate2+45],
                                          max_row[iterate2+46],
                                          max_row[iterate2+47],
                                          max_row[iterate2+48],
                                          max_row[iterate2+49],
                                          max_row[iterate2+50]
                                          
        )
      }
    }
  } 
  
}
write.csv(results, file = filename_recap)

setwd("C:/MasseyOffice1/Research/R-Script/")