# Import the Earth Engine Python Package
import ee
import time
import os
from datetime import datetime


path=range(15)
path[0] =117
path[1] =117
path[2] =118
path[3] =118
path[4] =119
path[5] =119
path[6] =120
path[7] =120
path[8] =120
path[9] =121
path[10] =121
path[11] =122
path[12] =122
path[13] =123
path[14] =123


row=range(15)
row[0] =65
row[1] =66
row[2] =65
row[3] =66
row[4] =65
row[5] =66
row[6] =64
row[7] =65
row[8] =66
row[9] =64
row[10] =65
row[11] =64
row[12] =65
row[13] =64
row[14] =65




# Initialize the Earth Engine object, using the authentication credentials.
ee.Initialize()

#12_LS8_SR_05Jul-21Jul-2017.tif

for i in range(1, 15):
	LS8_Collection = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR').filterDate('2017-05-01','2019-06-01')
	LS8_Collection = LS8_Collection.filter(ee.Filter.eq('WRS_PATH', path[i]))
	LS8_Collection = LS8_Collection.filter(ee.Filter.eq('WRS_ROW', row[i]))
	LS8_Collection = LS8_Collection.sort('DATE_ACQUIRED')
	colList = LS8_Collection.toList(1000)
	n = colList.size().getInfo();
	print  str(path[i]) +  str(row[i])
	print n
	for j in range(0, n):
		img = ee.Image(colList.get(j)).select(['B1','B2','B3','B4','B5','B6','B7']);
		id = img.id().getInfo();
		filename = 'LS8-SR-'+ id + '' 
		fullname_check = "F:\\RS_images\\LS8-SR-utm\\" + filename +  '.tif'
		if os.path.isfile(fullname_check):
			print datetime.now().strftime("%Y-%m-%d %H:%M:%S") + ' sudah didownload->  '  + str(j+1) + ' - ' + filename
		else:		
			task_config = {
			  'description': filename,
			  'scale': 30,
			  'crs' : 'EPSG:32749',
			  'driveFolder': 'LS8-SR-utm',
			}
			task = ee.batch.Export.image(img, filename, task_config)
			task.start()
			print datetime.now().strftime("%Y-%m-%d %H:%M:%S") + ' ->  '  + str(j+1) + ' - ' + filename


