
rm(list=ls())
library(raster)
library(rgdal)

setwd("C:/MasseyOffice1/Research/R-Script/")

mainDir <- "C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm/LS8-ORI-FULL/"
setwd(mainDir)

files <- list.files(pattern = "\\.tif$")
temp1 <- strsplit(files, "-")
temp2 <- temp1[[1]]
region <- temp2[2]
len_filename <- nchar(files[1])

temp3 <- substr(files, len_filename-11, len_filename-4)
datels8 <- unique(temp3)

for(i_datels8 in 1:1) {
  datels8_initial <- datels8[i_datels8]
  datels8_last <- datels8[i_datels8+1]
  datels8_combine <- paste(datels8_initial,"-",datels8_last,sep = "")

  model <- c('fda','nnet','rf','svmLinear','svmPoly','svmRadial')
  #model <- c('fda')
  folder <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm/CLASSIFY/",collapse = "",sep = "")
  
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
       folder_result_paddy <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm/CHANGE-DETECTION/",model_ML,"/",collapse = "",sep = "")
       dir.create(folder_result_paddy, showWarnings = FALSE)
       
        filename_result_paddy <- paste(folder_result_paddy,"paddy-",datels8_combine,"_changedetection_",model_ML, sep="")
        filename_tif_paddy <- paste(folder_result_paddy,"paddy-",datels8_combine,"_changedetection_",model_ML,".tif", sep="")
        print(format(Sys.time(), "%a %b %d %X %Y")) 
        print(filename_tif_paddy)
        if (file.exists(filename_tif_paddy)) {
          next
        }
        
        folder_clip_paddy <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm/CLIP-PADDY-MASK/",model_ML,"/",collapse = "",sep = "")
        setwd(folder_clip_paddy)
        search_str <- paste("*",datels8_initial,".*",model_ML,".*",".*[.]tif$",sep = "")
        input.rasters <- lapply(list.files(pattern=search_str), raster)
        raster_current_initial <- input.rasters[[1]]
        
        search_str <- paste("*",datels8_last,".*",model_ML,".*",".*",".*[.]tif$",sep = "")
        input.rasters <- lapply(list.files(pattern=search_str), raster)
          raster_current_last <- input.rasters[[1]]
        
        rast_stack <- stack(raster_current_initial,raster_current_last)
        fun_raster <- function(x) {
            result <- as.numeric(paste(x[1],"0",x[2],sep = ""))
          return(result)
        }
        
        raster_mask <- calc(rast_stack, fun_raster)
        
        writeRaster(raster_mask, filename=filename_result_paddy, format="GTiff", overwrite=TRUE)  
        
        png(file = paste(filename_result_paddy, '.png', sep=""), width = 1200, height = 1200)
        plot(raster_mask)
        dev.off()
     
    }
  }
}

