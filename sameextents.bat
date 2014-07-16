REM Make the resolution and extents of all the files to be the same 
cd C:\Users\dmb2\Documents\GitHub\randomForest\tifs
for /R %%f in (*.tif) do gdalwarp -t_srs "+proj=lcc +lat_1=41.7 +lat_2=40.43333333333333 +lat_0=39.66666666666666 +lon_0=-82.5 +x_0=600000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs" -te 2108845 551392.7 2277315 677922.7 -tr 10 10 -r lanczos %%f %%f.new
