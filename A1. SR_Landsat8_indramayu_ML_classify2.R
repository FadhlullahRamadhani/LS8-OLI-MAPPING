
rm(list=ls())
gc()
memory.size(max=F)

rm(list=ls())
gc()
memory.size(max=F)
rm(list=ls(all=TRUE))
gc(reset=TRUE)

library(raster)
library(sp)
library(rgdal)
library(zoo)
library(signal)
library(pracma)
library(ggplot2)
library(purrr)
library(readxl)
library(caret)
library(reshape)
library(plyr)
library(utils)

get_free_ram <- function(){
  if(Sys.info()[["sysname"]] == "Windows"){
    x <- system2("wmic", args =  "OS get FreePhysicalMemory /Value", stdout = TRUE)
    x <- x[grepl("FreePhysicalMemory", x)]
    x <- gsub("FreePhysicalMemory=", "", x, fixed = TRUE)
    x <- gsub("\r", "", x, fixed = TRUE)
    as.integer(x)
  } else {
    stop("Only supported on Windows OS")
  }
}

detachAllPackages <- function() {
  
  basic.packages <- c("package:ggplot2","package:sp","package:raster","package:lattice","package:caret","package:foreach","package:iterators","package:parallel","package:doParallel","package:reshape","package:plyr","package:utils","package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

setwd("C:/MasseyOffice1/Research/R-Script/")
#'B4','B3','B2','B5'

mainDir <- "C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/LS8-ORI-FULL/"
setwd(mainDir)

current_folder <- mainDir

setwd(file.path(current_folder))
files <- list.files(pattern = "\\.tif$")
for (i in 1:length(files)){
  
  filename_original <-files[i]
  print(filename_original)
  
  #select(['B1','B2','B3','B4','B5','B6','B7']);
  
  Red_SR <- raster(filename_original,band=4) #B4: Red (0.64 - 0.67 µm)
  Green_SR <- raster(filename_original,band=3) #B3: Green (0.53 - 0.59 µm)
  Blue_SR <- raster(filename_original,band=2) #B2: Blue (0.45 - 0.51 µm)
  NIR_SR <- raster(filename_original,band=5) #B5: Near Infrared (0.85 - 0.88 µm)
  SWIR1_SR <- raster(filename_original,band=6)
  SWIR2_SR <- raster(filename_original,band=7)

  Red_SR[Red_SR==0] <- NA

  Green_SR[Green_SR==0] <- NA

  Blue_SR[Blue_SR==0] <- NA
  
  NIR_SR[NIR_SR==0] <- NA
  
  SWIR1_SR[SWIR1_SR==0] <- NA
  
  SWIR2_SR[SWIR2_SR==0] <- NA
  
  Red_SR <- Red_SR * 0.0001
  Green_SR <- Green_SR * 0.0001
  Blue_SR <- Blue_SR * 0.0001
  NIR_SR <- NIR_SR * 0.0001
  SWIR1_SR <-SWIR1_SR * 0.0001
  SWIR2_SR <-SWIR2_SR * 0.0001
  
  
  Red_raster <- Red_SR
  Red_raster_df <- as.data.frame(Red_raster)
  Red_raster_df_template <- as.data.frame(Red_raster)
  Red_raster_df[is.na(Red_raster_df)] <- -999
  
  Green_raster_df <- as.data.frame(Green_SR)
  Green_raster_df[is.na(Green_raster_df)] <- -999
  
  Blue_raster_df <- as.data.frame(Blue_SR)
  Blue_raster_df[is.na(Blue_raster_df)] <- -999
  
  NIR_raster_df <- as.data.frame(NIR_SR)
  NIR_raster_df[is.na(NIR_raster_df)] <- -999

  SWIR1_raster_df <- as.data.frame(SWIR1_SR)
  SWIR1_raster_df[is.na(SWIR1_raster_df)] <- -999
  
  SWIR2_raster_df <- as.data.frame(SWIR2_SR)
  SWIR2_raster_df[is.na(SWIR2_raster_df)] <- -999
  
  indramayu_classify_df <- data.frame(Red=Red_raster_df,Green=Green_raster_df,Blue=Blue_raster_df,NIR=NIR_raster_df,SWIR1=SWIR1_raster_df,SWIR12=SWIR2_raster_df,stringsAsFactors=FALSE)
  colnames(indramayu_classify_df) <- c("Red","Green","Blue","NIR","SWIR1","SWIR2")
  
  model <- c('fda','nnet','rf','svmLinear','svmPoly','svmRadial')
  #model <- c('fda')
  folder <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/CLASSIFY/",collapse = "",sep = "")
  
  dir.create(folder, showWarnings = FALSE)
  
  for (model_ML in model) {
    
    
    print (model_ML)
    set.seed(7)
    detachAllPackages()
    
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
        
        
        folder <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/CLASSIFY/",model_ML,"/",collapse = "",sep = "")
        
        dir.create(folder, showWarnings = FALSE)
        
        filename_original_noext = substr(filename_original,1,nchar(filename_original)-4)
        filename_result <- paste(folder,filename_original_noext,"_classify_",model_ML,"_",preName_current_string, sep="")
        filename_tif <- paste(folder,filename_original_noext,"_classify_",model_ML,"_",preName_current_string,".tif", sep="")
        print(format(Sys.time(), "%a %b %d %X %Y")) 
        print(filename_tif)
        if (file.exists(filename_tif)) {
          next
        }
        
        
        #WithMasking_LOOCV_LS8_nnet_center_scale_100
        mainDir1 <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/MODEL/",model_ML,"/",collapse = "",sep = "")
        
        filename <- paste(mainDir1,"WithMasking_LOOCV_LS8_",model_ML,"_",preName_current_string,"_0005.rds", sep="")
        
        allModelsResults <- readRDS(filename)
        
        if (get_free_ram()<1000000) {
          print(get_free_ram())
          print("sleep 60s")
          Sys.sleep(60)
          next
        }
        
        size_df <-  dim(indramayu_classify_df)[1]
        divided <- as.integer(size_df/5)
        for (it in 1:5) {
          size_df_0 <- (it-1)*(divided)+1
          size_df_1 <- (it)*(divided)
          print (paste(format(Sys.time(), "%a %b %d %Y %X"),"-->predicting",it,"==",size_df_0,"-",size_df_1,sep=""))
          if (it==1){
            result1 <- predict(allModelsResults,indramayu_classify_df[size_df_0:size_df_1,])
            allModelsResults_predictions <- result1
          } else {
            result1 <- predict(allModelsResults,indramayu_classify_df[size_df_0:size_df_1,])
            allModelsResults_predictions[size_df_0:size_df_1] <- predict(allModelsResults,indramayu_classify_df[size_df_0:size_df_1,])
          }
        }
        
  
        
        allModelsResults_predictions_coded <- match(allModelsResults_predictions, cbind("Bare land", "Vegetative", "Reproductive", "Ripening"))
        
        #encoded
        
        unique(allModelsResults_predictions)
        
        allModelsResults_predictions_filter <- Red_raster_df_template
        
        allModelsResults_predictions_filter[!is.na(Red_raster_df_template)] <- allModelsResults_predictions_coded[!is.na(Red_raster_df_template)]
        
        allModelsResults_predictions_filter <- as.matrix(allModelsResults_predictions_filter)
        
        result_temp <- raster(Red_raster)
        
        nrows=Red_raster[[1]]@nrows
        ncols=Red_raster[[1]]@ncols

        temp1<-matrix(allModelsResults_predictions_filter,nrows,ncols,byrow=TRUE)
        allModelsResults_predictions_filter <- NA
        temp2 <- raster(temp1)
        extent(temp2) <- Red_raster[[1]]
        projection(temp2) <- projection(Red_raster[[1]])
        raster_classify <- temp2
        
        writeRaster(raster_classify, filename=filename_result, format="GTiff", overwrite=TRUE)  
        
        gc()    
    }
  }
}

