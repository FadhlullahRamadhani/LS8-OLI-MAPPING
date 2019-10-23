
rm(list=ls())
library(raster)


setwd("C:/MasseyOffice1/Research/R-Script/")
#'B4','B3','B2','B5'
#EVI_indramayu_LC81200652018002LGN00
#indramayu_fmask_LC81200652018002LGN00

mainDir <- "C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/LS8-ORI-FULL/"
setwd(mainDir)

files <- list.files(pattern = "\\.tif$")
temp1 <- strsplit(files, "-")
temp2 <- temp1[[1]]
region <- temp2[2]
len_filename <- nchar(files[1])

temp3 <- substr(files, len_filename-11, len_filename-4)
datels8 <- unique(temp3)

for (i_datels8 in datels8){
  
  
  model <- c('fda','nnet','rf','svmLinear','svmPoly','svmRadial')
  #model <- c('fda')
  folder <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/CLASSIFY/",collapse = "",sep = "")
  
  dir.create(folder, showWarnings = FALSE)
  
  for (model_ML in model) {
    
    
    print (model_ML)
    set.seed(7)
    
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
      folder_classify <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/CLASSIFY/",collapse = "",sep = "")
      dir.create(folder_classify, showWarnings = FALSE)
      folder_classify <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/CLASSIFY/",model_ML,"/",collapse = "",sep = "")
      dir.create(folder_classify, showWarnings = FALSE)
      setwd(folder_classify)
      
      search_str <- paste("*",i_datels8,".*",model_ML,".*[.]tif$",sep = "")
      input.rasters <- lapply(list.files(pattern=search_str), raster)
      
      folder_merge <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/CLASSIFY-MERGE/",collapse = "",sep = "")
      dir.create(folder_merge, showWarnings = FALSE)
      
      folder_merge <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/CLASSIFY-MERGE/",model_ML,"/",collapse = "",sep = "")
      dir.create(folder_merge, showWarnings = FALSE)
      
      filename_result <- paste(folder_merge,region,"-",i_datels8,"-",model_ML,"_merge", sep="")
      filename_tif <- paste(folder_merge,region,"-",i_datels8,"-",model_ML,"_merge",".tif", sep="")
      print(format(Sys.time(), "%a %b %d %X %Y"))       
      print(filename_tif)
      if (file.exists(filename_tif)) {
        next
      }
      
      mosaic1 <- mosaic(input.rasters[[1]],input.rasters[[2]],fun=max)
      
      writeRaster(mosaic1, filename=filename_result, format="GTiff", overwrite=TRUE)  
      
      png(file = paste(filename_result, '.png', sep=""), width = 1200, height = 1200)
      plot(mosaic1)
      dev.off()
      
    }
  }
}

