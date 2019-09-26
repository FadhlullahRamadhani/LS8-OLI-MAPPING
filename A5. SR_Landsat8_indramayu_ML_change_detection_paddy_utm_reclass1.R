
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

reclass_df <-function(df, assignvalue, list)
{
  for (i in list) {
    df[df==i] <- assignvalue
  }
  return(df)
}


for(i_datels8 in 1:1) {
  datels8_initial <- datels8[i_datels8]
  datels8_last <- datels8[i_datels8+1]
  datels8_combine <- paste(datels8_initial,"-",datels8_last,sep = "")

  model <- c('fda','nnet','rf','svmLinear','svmPoly','svmRadial')
  #model <- c('fda')
  folder <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm/CHANGE-DETECTION/",collapse = "",sep = "")
  
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
       folder_result_paddy <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm/CHANGE-DETECTION-RECLASSIFY/",model_ML,"/",collapse = "",sep = "")
       dir.create(folder_result_paddy, showWarnings = FALSE) 
    
        filename_result_paddy <- paste(folder_result_paddy,"paddy-",datels8_combine,"_reclassify_",model_ML, sep="")
        filename_tif_paddy <- paste(folder_result_paddy,"paddy-",datels8_combine,"_reclassify_",model_ML,".tif", sep="")
        print(format(Sys.time(), "%a %b %d %X %Y")) 
        print(filename_tif_paddy)
        if (file.exists(filename_tif_paddy)) {
          next
        }
        
        folder_change_detection <- paste("C:/MasseyOffice1/Research/R-Script/SR-LS8-indramayu-utm/CHANGE-DETECTION/",model_ML,"/",collapse = "",sep = "")
        setwd(folder_change_detection)
        search_str <- paste("*",datels8_initial,".*",model_ML,".*",".*[.]tif$",sep = "")
        input.rasters <- lapply(list.files(pattern=search_str), raster)
        raster_change_detection <- input.rasters[[1]]
        
        raster_reclassify <- raster_change_detection
        
        uniq_value <- unique(raster_change_detection)
        uniq_value_shadows <- uniq_value[!uniq_value %in% c(1,2,3,4,101,202,303,404,102,203,304,401,32401,32402,10324,20324,103,104,204,302,403,32404,402,32403,32404,201,301,30324,40324,3240324)]
        
        raster_reclassify <-reclass_df(raster_reclassify,NA, c(1,2,3,4))
        
        
        #No Change
        raster_reclassify <-reclass_df(raster_reclassify,1, c(101,202,303,404,3240324))
        #Change correctly
        raster_reclassify <-reclass_df(raster_reclassify,2, c(102,203,304,401,32401,32402,10324,20324))
        #Change incorrectly
        raster_reclassify <-reclass_df(raster_reclassify,3, c(103,104,204,302,403,32404,402,32403,32404,201,301,30324,40324))
        #Cloud and shadow
        raster_reclassify <-reclass_df(raster_reclassify,4, uniq_value_shadows)
        
    
        writeRaster(raster_reclassify, filename=filename_result_paddy, format="GTiff", overwrite=TRUE)  
        
        png(file = paste(filename_result_paddy, '.png', sep=""), width = 1200, height = 1200)
        plot(raster_reclassify)
        dev.off()
        
        filename_csv_paddy <- paste(folder_result_paddy,"paddy-",datels8_combine,"_reclassify_stats_",model_ML,"_utm.csv", sep="")
        
        paddy_raster_df <- as.data.frame(raster_reclassify)
        paddy_raster_df_count <- table(paddy_raster_df)
        area_ha <-paddy_raster_df_count * 30 * 30 / 10000
        write.csv(area_ha, file = filename_csv_paddy)
        
    }
  }
}

