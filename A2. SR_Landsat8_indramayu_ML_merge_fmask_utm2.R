
rm(list=ls())
library(raster)

mainDir <- "C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/LS8-FMASK/"
setwd(mainDir)

files <- list.files(pattern = "\\.tif$")
temp1 <- strsplit(files, "-")
temp2 <- temp1[[1]]

region <- temp2[1]

temp3 <- substr(files, 26, 33)
datels8 <- unique(temp3)

for (i_datels8 in datels8){

        search_str <- paste("*",i_datels8,".*.*[.]tif$",sep = "")
        input.rasters <- lapply(list.files(pattern=search_str), raster)
        
         
        folder_merge <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm-2018/LS8-FMASK-MERGE/",collapse = "",sep = "")
        dir.create(folder_merge, showWarnings = FALSE)
        
        filename_result <- paste(folder_merge,"indramayu-",i_datels8,"_merge_fmask",sep="")
        filename_tif <- paste(folder_merge,"indramayu-",i_datels8,"_merge_fmask",".tif", sep="")
        print(format(Sys.time(), "%a %b %d %X %Y"))       
         print(filename_tif)
        if (file.exists(filename_tif)) {
          next
        }

        mosaic1 <- mosaic(input.rasters[[1]],input.rasters[[2]],fun=max)

        writeRaster(mosaic1, filename=filename_result, format="GTiff", overwrite=TRUE)  
     
 
}

