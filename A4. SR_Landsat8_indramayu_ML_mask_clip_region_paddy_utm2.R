
rm(list=ls())
library(raster)
library(rgdal)
indramayu_region <- readOGR("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/SHP","indramayu_land_utm")
indramayu_paddy <- readOGR("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/SHP","paddy_utm")

setwd("C:/MasseyOffice1/Research/R-Script/")

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
        
        folder_clip_paddy <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/CLIP-PADDY-MASK/",model_ML,"/",collapse = "",sep = "")
        dir.create(folder_clip_paddy, showWarnings = FALSE)
        
        filename_result_paddy <- paste(folder_clip_paddy,"paddy-",i_datels8,"_clip_paddy_mask_",model_ML, sep="")
        filename_tif_paddy <- paste(folder_clip_paddy,"paddy-",i_datels8,"_clip_paddy_mask_",model_ML,".tif", sep="")
        if (file.exists(filename_tif_paddy)) {
          next
        }
        
        folder_mask <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/LS8-FMASK-MERGE/",collapse = "",sep = "")
        dir.create(folder_mask, showWarnings = FALSE)
        setwd(folder_mask)
        search_str <- paste("*",i_datels8,".*",".*[.]tif$",sep = "")
        fmask.rasters <- lapply(list.files(pattern=search_str), raster)
        fmask_current <- fmask.rasters[[1]]
        folder_merge <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/CLASSIFY-MERGE/",collapse = "",sep = "")
        dir.create(folder_merge, showWarnings = FALSE)
        
        folder_merge <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/CLASSIFY-MERGE/",model_ML,"/",collapse = "",sep = "")
        dir.create(folder_merge, showWarnings = FALSE)
          setwd(folder_merge)
        
        search_str <- paste("*",i_datels8,".*",model_ML,".*[.]tif$",sep = "")
        input.rasters <- lapply(list.files(pattern=search_str), raster)
        raster_current <- input.rasters[[1]]

        rast_stack <- stack(raster_current,fmask_current)
        fun_raster <- function(x) {
          if (x[2]== 322) {
            result <- x[1]
          }  else {
            result <- x[2]
          }
          return(result)
        }
        #fun_raster(c(1,0))
        raster_mask <- calc(rast_stack, fun_raster)
        #plot(raster_mask)
        #REGION-MASK
        folder_clip_region_mask <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/REGION-MASK/",collapse = "",sep = "")
        dir.create(folder_clip_region_mask, showWarnings = FALSE)
        folder_clip_region_mask <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/REGION-MASK/",model_ML,"/",collapse = "",sep = "")
        dir.create(folder_clip_region_mask, showWarnings = FALSE)
        
        filename_result_region_mask <- paste(folder_clip_region_mask,region,"-",i_datels8,"_region_mask_",model_ML, sep="")
        filename_tif_region_mask <- paste(folder_clip_region_mask,region,"-",i_datels8,"_region_mask_",model_ML,".tif", sep="")
       
        writeRaster(raster_mask, filename=filename_result_region_mask, format="GTiff", overwrite=TRUE)  
        
        png(file = paste(filename_result_region_mask, '.png', sep=""), width = 1200, height = 1200)
        plot(raster_mask)
        dev.off()
        
        
        #region
        folder_clip_region <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/CLIP-REGION-MASK/",collapse = "",sep = "")
        dir.create(folder_clip_region, showWarnings = FALSE)
        
        folder_clip_region <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/CLIP-REGION-MASK/",model_ML,"/",collapse = "",sep = "")
        dir.create(folder_clip_region, showWarnings = FALSE)
        
        filename_result_region <- paste(folder_clip_region,region,"-",i_datels8,"_clip_region_mask_",model_ML, sep="")
        filename_tif_region <- paste(folder_clip_region,region,"-",i_datels8,"_clip_region_mask_",model_ML,".tif", sep="")
        
        print(format(Sys.time(), "%a %b %d %X %Y"))       
        print(filename_tif_region)
        
        clip_region1 <- crop(raster_mask, extent(indramayu_region))
        clip_region1 <- mask(clip_region1, indramayu_region)
        
        writeRaster(clip_region1, filename=filename_result_region, format="GTiff", overwrite=TRUE)  
        
        png(file = paste(filename_result_region, '.png', sep=""), width = 1200, height = 1200)
        plot(clip_region1)
        dev.off()
        
        #paddy
        folder_clip_paddy <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/CLIP-PADDY-MASK/",collapse = "",sep = "")
        dir.create(folder_clip_paddy, showWarnings = FALSE)
        folder_clip_paddy <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/CLIP-PADDY-MASK/",model_ML,"/",collapse = "",sep = "")
        dir.create(folder_clip_paddy, showWarnings = FALSE)
        
        filename_result_paddy <- paste(folder_clip_paddy,"paddy-",i_datels8,"_clip_paddy_mask_",model_ML, sep="")
        filename_tif_paddy <- paste(folder_clip_paddy,"paddy-",i_datels8,"_clip_paddy_mask_",model_ML,".tif", sep="")
        
        print(format(Sys.time(), "%a %b %d %X %Y"))       
        print(filename_tif_paddy)
        
        clip_paddy1 <- crop(raster_mask, extent(indramayu_paddy))
        clip_paddy1 <- mask(clip_paddy1, indramayu_paddy)
        
        writeRaster(clip_paddy1, filename=filename_result_paddy, format="GTiff", overwrite=TRUE)  
        
        png(file = paste(filename_result_paddy, '.png', sep=""), width = 1200, height = 1200)
        plot(clip_paddy1)
        dev.off()
 
    }
  }
}

