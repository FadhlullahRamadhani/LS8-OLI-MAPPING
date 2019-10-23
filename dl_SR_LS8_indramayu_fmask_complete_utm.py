# Import the Earth Engine Python Package
import ee
import time
import os
from datetime import datetime

# Initialize the Earth Engine object, using the authentication credentials.
ee.Initialize()

indramayu_region = [ [ [ 107.817891, -6.802353193752624 ],[ 108.665771484375, -6.802353193752624 ],[ 108.665771484375, -6.14738179732885 ],[ 107.817891, -6.14738179732885 ], [ 107.817891, -6.802353193752624 ] ] ]


indramayu_poly = ee.Geometry.Polygon(indramayu_region)


LS8_Collection = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR').filterDate('2016-01-01','2017-01-01')
LS8_Collection = LS8_Collection.filterBounds(indramayu_poly)
LS8_Collection = LS8_Collection.sort('DATE_ACQUIRED')
colList = LS8_Collection.toList(1000);
n = colList.size().getInfo();
print n
path = "C:\\Users\\framadha\\Downloads\\SR-LS8-indramayu-FMASK-utm\\*.tif"
for j in range(0, n-1):
	img = ee.Image(colList.get(j)).select(['pixel_qa']);
	id = img.id().getInfo();
	filename = 'SR-indramayu-' + id + '_fmask'
	fullname_check = "C:\\Users\\framadha\\Downloads\\SR-LS8-indramayu-FMASK-utm\\" + 'SR-indramayu-' + id + '_fmask.tif'
	if os.path.isfile(fullname_check):
		print datetime.now().strftime("%Y-%m-%d %H:%M:%S") + ' sudah didownload->  '  + str(j+1) + ' - ' + filename
	else:		
		task_config = {
		  'description': filename,
		  'scale': 30,
		  'crs' : 'EPSG:32749',
		  'driveFolder': 'SR-LS8-indramayu-FMASK-utm',
		  'region': indramayu_region
		}
		task = ee.batch.Export.image(img, filename, task_config)
		task.start()
		print datetime.now().strftime("%Y-%m-%d %H:%M:%S") + ' ->  '  + str(j+1) + ' - ' + filename
