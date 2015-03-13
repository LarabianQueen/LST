; Kelly G. Laraby
; PhD Candidate in Imaging Science
; Rochester Institute of Technology
; kga1099@rit.edu
;
; NAME:  
;    CALC_NEAREST_CLOUD
;
; PURPOSE:
;    IDL FUNCTION
;    Calculates the distance from the buoy to each cloud pixel in order to determine the distance to the nearest cloud pixel.
;    Also generates an array filled with all the calculated distances, and an array that is color coded based on distance
;    away from the buoy. These arrays are written out to tif files and may be useful for error analysis.
;    
; OUTLINE:
;    1) retreive upper left coordinates and zone for cloudmask
;    2) convert buoy lat/long to UTM
;    3) calculate difference between Landsat UTM and buoy UTM and divide by pixel size, which allows array indexing
;    4) index the cloudmask where there are clouds (value of 255), and obtain cols/rows
;    5) calculate distance from buoy col/row to each cloud pixel col/row
;    6) save distances in an array and output to tiff file
;    7) return smallest distance
;
; CALL SEQUENCE: 
;    CALC_NEAREST_CLOUD,   folderpath, $               ; path where output files sould go (include '\' after folder)
;                          imagebase, $                ; scene identifier (e.g. LE70160302008113EDC00)
;                          buoylat, $                  ; latitude of buoy
;                          buoylon, $                  ; longitude of buoy (negative if in western hemisphere)
;                          cloudmask, $                ; the cloud mask for the scene, modified by get_cloudmask.pro
;                          cmask_geotiff, $            ; the cloud mask geotiff, which has the upperleft coordinates and zone info
;
; RESTRICTIONS:
;    1) buoylat and buoylon must be in lat/lon form.
;
;
; REQUIRED PROGRAMS (in working directory):      
;    CONVERT_LL_UTM.PRO
;
; REQUIRED FILES (in folderpath directory):      
;    N/A
;
; MODIFICATIONS:
;    January, 2015        Original code
;    February, 2015       Added distarray and binsarray, which write to tiff files
;
;

FUNCTION CALC_NEAREST_CLOUD, folderpath, imagebase, buoylat, buoylon, cloudmask, cmask_geotiff
  
  cmask_ULeast= cmask_geotiff.modeltiepointtag[3]
  cmask_ULnorth= cmask_geotiff.modeltiepointtag[4]
  zone = STRMID(STRCOMPRESS(STRING(cmask_geotiff.PROJECTEDCSTYPEGEOKEY)),4,2)
  
  pixsizeX = cmask_geotiff.MODELPIXELSCALETAG[0]
  pixsizeY = cmask_geotiff.MODELPIXELSCALETAG[1]
  
  ;
  ; convert buoy lat/long to UTM
  ;
  buoyUTM= CONVERT_LL_UTM(buoylat, buoylon, zone)
  buoyEast= buoyUTM[1]
  buoyNorth= buoyUTM[2]
  
  ;
  ; calculate difference between Landsat UTM and buoy UTM
  ; for easting, do buoy-Landsat (b/c buoy has to be to the right of the Landsat easting)
  ; for northing, do landsat-buoy (b/c buoy has to be below the Landsat northing)
  ; put in terms of pixels for indexing
  ;
  buoyCol= DOUBLE(ROUND((buoyEast - cmask_ULeast)/pixsizeX))
  buoyRow= DOUBLE(ROUND((cmask_ULnorth - buoyNorth)/pixsizeY))
  
  ;
  ; find where cloud pixels are
  ;
  s = SIZE(cloudmask)
  ncol = s[1]
  cloudind = WHERE(cloudmask EQ 255)
  cloudCols = cloudind MOD ncol
  cloudRows = cloudind / ncol
  
  ;
  ; make distance array (wherever there is a cloud pixel the distance from that pixel to the buoy will be recorded)
  ; 
  ; make bins array ( 0-1 km away    =  red      )
  ;                 ( 1-4 km away    =  orange   )
  ;                 ( 4-7.5 km away  =  yellow   )
  ;                 ( 7.5-50 km away =  green    )
  ;                 ( 50-inf km away =  blue     )
  ;
  distances= DBLARR(1,N_ELEMENTS(cloudCols))
  distarray=FLTARR(s[1],s[2])
  binsarray=BYTARR(s[1],s[2],3)
  bin1= 1
  bin2= 4
  bin3= 7.5
  bin4= 50
  
  
  ;
  ; calculate distance to each cloud pixel and build up distance arrays
  ;
  PRINT, 'Calculating distance to all cloud pixels...'
  FOR i= 0, N_ELEMENTS(cloudCols)-1 DO BEGIN
    
    col_diff= ABS(buoyCol - cloudCols[i])
    row_diff= ABS(buoyRow - cloudRows[i])
    distances[i]= SQRT( col_diff^2 + row_diff^2 ) 
    
    dist_in_km=distances[i]*pixsizeX/1000
    distarray[cloudCols[i],cloudRows[i]]=dist_in_km
    
    IF dist_in_km GT 0 AND dist_in_km LE bin1 THEN BEGIN
      binsarray[cloudCols[i],cloudRows[i],*]= [255,0,0]
      
    ENDIF ELSE IF dist_in_km GT bin1 AND dist_in_km LE bin2 THEN BEGIN
      binsarray[cloudCols[i],cloudRows[i],*]= [255,97,3]
      
    ENDIF ELSE IF dist_in_km GT bin2 AND dist_in_km LE bin3 THEN BEGIN
      binsarray[cloudCols[i],cloudRows[i],*]= [255,255,0]
      
    ENDIF ELSE IF dist_in_km GT bin3 AND dist_in_km LE bin4 THEN BEGIN
      binsarray[cloudCols[i],cloudRows[i],*]= [0,255,0]
      
    ENDIF ELSE IF dist_in_km GT bin4 THEN BEGIN
      binsarray[cloudCols[i],cloudRows[i],*]= [0,0,255]
      
    ENDIF
      
  ENDFOR
  
  ; 
  ; write arrays to tiff files
  ;
  distarraypath= folderpath + imagebase + '_distarray.tif'
  binsarraypath= folderpath + imagebase + '_binsarray.tif'
  WRITE_TIFF, distarraypath, distarray
  WRITE_TIFF, binsarraypath, binsarray
  
  ;
  ; find smallest distance in kilometers
  ;
  minIndex= WHERE( distances EQ MIN(distances) )
  minDistance= distances(minIndex)*pixsizeX/1000
  RETURN, minDistance
  
END


