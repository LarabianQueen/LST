; Kelly G. Laraby
; 
; test making CFSR lat/lons
; 
;  
;

PRO make_cfsr_grid

  path='C:\Users\Kelly\Documents\RIT\RESEARCH\Code\'
  narr_name=path+'geopotlats_narr.txt'
  cfsr_name=path+'geopotlats_cfsr.txt'
  
  cfsrsize=720L*361L
  narrsize=349L*277L
  

  hgt1_narr=MAKE_ARRAY(narrsize,/DOUBLE)
  OPENR, unit,path+'HGT1_1000_NARR.txt',/GET_LUN
      hgt1_narr_all = MAKE_ARRAY(narrsize+2,/DOUBLE)
      READF, unit, hgt1_narr_all
  CLOSE, unit
  FREE_LUN, unit
  hgt1_narr=hgt1_narr_all[2:narrsize+1]
  
  tmp1_narr=MAKE_ARRAY(narrsize,/DOUBLE)
  OPENR, unit,path+'TMP1_1000_NARR.txt',/GET_LUN
      tmp1_narr_all = MAKE_ARRAY(narrsize+2,/DOUBLE)
      READF, unit, tmp1_narr_all
  CLOSE, unit
  FREE_LUN, unit
  tmp1_narr=tmp1_narr_all[2:narrsize+1]

  shum1_narr=MAKE_ARRAY(narrsize,/DOUBLE)
  OPENR, unit,path+'SHUM1_1000_NARR.txt',/GET_LUN
      shum1_narr_all = MAKE_ARRAY(narrsize+2,/DOUBLE)
      READF, unit, shum1_narr_all
  CLOSE, unit
  FREE_LUN, unit
  shum1_narr=shum1_narr_all[2:narrsize+1]
      
  hgt1_cfsr=MAKE_ARRAY(cfsrsize,/DOUBLE)
  OPENR, unit,path+'HGT1_1000_CFSR.txt',/GET_LUN
  hgt1_cfsr_all = MAKE_ARRAY(cfsrsize+2,/DOUBLE)
  READF, unit, hgt1_cfsr_all
  FREE_LUN, unit
  hgt1_cfsr=hgt1_cfsr_all[2:cfsrsize+1]

  tmp1_cfsr=MAKE_ARRAY(cfsrsize,/DOUBLE)
  OPENR, unit,path+'TMP1_1000_CFSR.txt',/GET_LUN
  tmp1_cfsr_all = MAKE_ARRAY(cfsrsize+2,/DOUBLE)
  READF, unit, tmp1_cfsr_all
  CLOSE, unit
  FREE_LUN, unit
  tmp1_cfsr=tmp1_cfsr_all[2:cfsrsize+1]

  shum1_cfsr=MAKE_ARRAY(cfsrsize,/DOUBLE)
  OPENR, unit,path+'SHUM1_1000_CFSR.txt',/GET_LUN
  shum1_cfsr_all = MAKE_ARRAY(cfsrsize+2,/DOUBLE)
  READF, unit, shum1_cfsr_all
  CLOSE, unit
  FREE_LUN, unit
  shum1_cfsr=shum1_cfsr_all[2:cfsrsize+1]
  
  hgt2_cfsr=MAKE_ARRAY(cfsrsize,/DOUBLE)
  OPENR, unit,path+'HGT2_1000_CFSR.txt',/GET_LUN
  hgt2_cfsr_all = MAKE_ARRAY(cfsrsize+2,/DOUBLE)
  READF, unit, hgt2_cfsr_all
  CLOSE, unit
  FREE_LUN, unit
  hgt2_cfsr=hgt2_cfsr_all[2:cfsrsize+1]

  tmp2_cfsr=MAKE_ARRAY(cfsrsize,/DOUBLE)
  OPENR, unit,path+'TMP2_1000_CFSR.txt',/GET_LUN
  tmp2_cfsr_all = MAKE_ARRAY(cfsrsize+2,/DOUBLE)
  READF, unit, tmp2_cfsr_all
  CLOSE, unit
  FREE_LUN, unit
  tmp2_cfsr=tmp2_cfsr_all[2:cfsrsize+1]

  shum2_cfsr=MAKE_ARRAY(cfsrsize,/DOUBLE)
  OPENR, unit,path+'SHUM2_1000_CFSR.txt',/GET_LUN
  shum2_cfsr_all = MAKE_ARRAY(cfsrsize+2,/DOUBLE)
  READF, unit, shum2_cfsr_all
  CLOSE, unit
  FREE_LUN, unit
  shum2_cfsr=shum2_cfsr_all[2:cfsrsize+1]
  
  ; using LE70160382008033EDC00
  UL_LAT= 32.69991
  UL_LON= -81.79589
  LR_LAT= 30.78547
  LR_LON= -79.27457
  
;  geoheight_narr=REFORM(geopot_narr[0,*],42,1)
;  lat_narr=REFORM(geopot_narr[1,*],42,1)
;  geoheight_cfsr=REFORM(geopot_cfsr[0,*],42,1)
;  lat_cfsr=REFORM(geopot_cfsr[1,*],42,1)
;  
;  geomet_narr=convert_geopot2geomet(geoheight_narr,lat_narr)
;  geomet_cfsr=convert_geopot2geomet(geoheight_cfsr,lat_cfsr)
  

;  ;;;;;; narr stuff ;;;;;;;
  ;read in file containing latitude and longitude grid information for the grib data
  OPENR, 20, path+'coordinates.txt'
  coordinates = MAKE_ARRAY(4, 96673, /DOUBLE)
  READF, 20, coordinates
  CLOSE, 20
  FREE_LUN, 20

  ;pull out latitude and reform to 349x277 grid
  narrLat = coordinates[2,*]
  lat = REFORM(narrLat, 349, 277)

  ;pull out longitude, manipulate range to [-180,180] and reform to 349x277 grid
  narrLon = coordinates[3,*]
  east = WHERE(narrLon GT 180.0)
  narrLon[east] = 360.0 - narrLon[east]
  west = WHERE(narrLon LE 180.0)
  narrLon[west] = (-1)*narrLon[west]
  lon = REFORM(narrLon, 349, 277)

  ;pull out i and j values and reform to 349 x 277 grid
  i = coordinates[0,*]
  eye = REFORM(i, 349, 277)
  j = coordinates[1,*]
  jay = REFORM(j, 349, 277)
  
  ;determine if landsat is in the northern or southern hemisphere. '6' = northern hemisphere, '7' = southern hermisphere.
  IF UL_LAT GE 0 THEN landsatHemi = 6 ELSE landsatHemi = 7

  ;expand range to include NARR points outside image for edge pixels
  UL_LATnarr = UL_LAT + 0.5
  UL_LONnarr = UL_LON - 0.5
  LR_LATnarr = LR_LAT - 0.5
  LR_LONnarr = LR_LON + 0.5

  ;determine what points in the NARR dataset fall within the Landsat image using logical operators
  ;lessThanLat and greaterThanLat are values where the NARR values are less than or greater than the edges of the Landsat
  ;corners values respectively
  ;pixels that are true in both fall within the Landsat scene
  ;the same thing is done with longitude values
  lessThanLat = DOUBLE(lat LT UL_LATnarr)
  greaterThanLat = DOUBLE(lat GT LR_LATnarr)
  keepLat = lessThanLat*greaterThanLat
  greaterThanLon = DOUBLE(lon GT UL_LONnarr)
  lessThanLon = DOUBLE(lon LT LR_LONnarr)
  keepLon = greaterThanLon*lessThanLon

  ;values that are true in both keepLat and keepLon fall within the Landsat image
  ;convert indices into (x,y) values in the NARR dataset
  ;Because the Landsat is in easting/northing originally and the NARR data is Lambert conformal, this is not a perfect square
  keep = keepLat * keepLon
  inLandsat_narr = WHERE(keep NE 0)
  
  ;determine indices to pull out rectangle of NARR points
  iindices = [MIN(eye[inLandsat_narr])-1,MAX(eye[inLandsat_narr])-1]
  jindices = [MIN(jay[inLandsat_narr])-1,MAX(jay[inLandsat_narr])-1]

  ;extract coordinates within this rectangle
  ivalues = eye[iindices[0]:iindices[1],jindices[0]:jindices[1]]
  jvalues = jay[iindices[0]:iindices[1],jindices[0]:jindices[1]]
  latvalues = lat[iindices[0]:iindices[1],jindices[0]:jindices[1]]
  lonvalues = lon[iindices[0]:iindices[1],jindices[0]:jindices[1]]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;;;;;;;
  ;;;;
  ;define sampling for latitude and longitude of merra grid
  deltalambda = 0.5
  deltaphi = 0.5

  ;define lon grid for CFSR data
  i = INDGEN(720)+1
  lon = -180 + deltalambda*(i-1.0)
  longrid = MAKE_ARRAY(720,361, /DOUBLE)
  eye = MAKE_ARRAY(720,361, /DOUBLE)
  FOR g = 0, 360 DO BEGIN
    longrid[*,g] = lon
    eye[*,g] = i
  ENDFOR

  ;define lat grid for CFSR data
  j = INDGEN(361)+1
  lat = -90 + deltaphi*(j-1.0)
  latgrid = MAKE_ARRAY(720,361, /DOUBLE)
  jay = MAKE_ARRAY(720,361, /DOUBLE)
  FOR g = 0, 719 DO BEGIN
    latgrid[g,*] = lat
    jay[g,*] = j
  ENDFOR

  ;expand range to include NARR points outside image for edge pixels
  UL_LATcfsr = UL_LAT + 0.6 ;1.3
  UL_LONcfsr = UL_LON - 0.6 ;1.3
  LR_LATcfsr = LR_LAT - 0.6 ;1.3
  LR_LONcfsr = LR_LON + 0.6 ;1.3
  
  ;determine what points in the CFSR dataset fall within the Landsat image using logical operators
  ;lessThanLat and greaterThanLat are values where the CFSR values are less than or greater than the edges of the Landsat
  ;corners values respectively
  ;pixels that are true in both fall within the Landsat scene
  ;the same thing is done with longitude values
  lessThanLat = DOUBLE(latgrid LT UL_LATcfsr)
  greaterThanLat = DOUBLE(latgrid GT LR_LATcfsr)
  keepLat = lessThanLat*greaterThanLat
  greaterThanLon = DOUBLE(longrid GT UL_LONcfsr)
  lessThanLon = DOUBLE(longrid LT LR_LONcfsr)
  keepLon = greaterThanLon*lessThanLon

  ;values that are true in both keepLat and keepLon fall within the Landsat image
  ;convert indices into (x,y) values in the CFSR dataset
  ;Because the Landsat is in easting/northing originally and the CFSR data is lat/lon, this is not a perfect square
  keep = keepLat * keepLon
  inLandsat_cfsr = WHERE(keep NE 0)

  ;determine indices to pull out rectangle of CFSR points
  iindices = [MIN(eye[inLandsat_cfsr])-1,MAX(eye[inLandsat_cfsr])-1]
  jindices = [MIN(jay[inLandsat_cfsr])-1,MAX(jay[inLandsat_cfsr])-1]

  ;extract coordinates within this rectangle
  ivalues = eye[iindices[0]:iindices[1],jindices[0]:jindices[1]]
  jvalues = jay[iindices[0]:iindices[1],jindices[0]:jindices[1]]

  ivector = REFORM(ivalues,1,N_ELEMENTS(ivalues))-1
  jvector = REFORM(jvalues,1,N_ELEMENTS(jvalues))-1

  inLandsatArray = [ivector,jvector]
  
  narrlatlons=MAKE_ARRAY(2,N_ELEMENTS(inLandsat_narr))
  cfsrlatlons=MAKE_ARRAY(2,N_ELEMENTS(inLandsat_cfsr))
  
  narrlatlons[0,*]=narrLat[inLandsat_narr]
  narrlatlons[1,*]=narrLon[inLandsat_narr]
  cfsrlatlons[0,*]=lat[inLandsatArray[1,*]]
  cfsrlatlons[1,*]=lon[inLandsatArray[0,*]]
  
  
  
  latdiff=MAKE_ARRAY(N_ELEMENTS(inLandsat_narr));
  londiff=MAKE_ARRAY(N_ELEMENTS(inLandsat_narr));
  totdiff=MAKE_ARRAY(N_ELEMENTS(inLandsat_narr));
  minindexes=MAKE_ARRAY(N_ELEMENTS(cfsrlatlons));
  for i=0,N_ELEMENTS(cfsrlatlons[0,*])-1 do begin
  for k=0,N_ELEMENTS(narrlatlons[0,*])-1 do begin
    latdiff[k]=abs(narrlatlons[0,k]-cfsrlatlons[0,i]);
    londiff[k]=abs(narrlatlons[1,k]-cfsrlatlons[1,i]);
    totdiff[k]= sqrt( latdiff[k]^2 + londiff[k]^2 );
end
smallest=min(totdiff);
indexes=where(totdiff eq min(totdiff));
minindexes[i]=indexes;
end

; minindexes says which narr latlons to use for each cfsr latlon
newnarrind=inLandsat_narr(minindexes);

narr1_hgt_subset=hgt1_narr[newnarrind];
narr1_tmp_subset=tmp1_narr[newnarrind];
narr1_shum_subset=shum1_narr[newnarrind];
cfsr1_hgt_subset=hgt1_cfsr[inLandsat_cfsr];
cfsr1_tmp_subset=tmp1_cfsr[inLandsat_cfsr];
cfsr1_shum_subset=shum1_cfsr[inLandsat_cfsr];
cfsr2_hgt_subset=hgt2_cfsr[inLandsat_cfsr];
cfsr2_tmp_subset=tmp2_cfsr[inLandsat_cfsr];
cfsr2_shum_subset=shum2_cfsr[inLandsat_cfsr];


cfsrmid_hgt= (cfsr1_hgt_subset + cfsr2_hgt_subset)/2
cfsrmid_tmp= (cfsr1_tmp_subset + cfsr2_tmp_subset)/2
cfsrmid_shum= (cfsr1_shum_subset + cfsr2_shum_subset)/2

hgt_error= cfsrmid_hgt - narr1_hgt_subset
tmp_error= cfsrmid_tmp - narr1_tmp_subset
shum_error=cfsrmid_shum- narr1_shum_subset

hgt_avgerr=MEAN(hgt_error)
tmp_avgerr=MEAN(tmp_error)
shum_avgerr=MEAN(shum_error)

hgt_rmse= SQRT( TOTAL(hgt_error^2)/N_ELEMENTS(hgt_error) )
tmp_rmse= SQRT( TOTAL(tmp_error^2)/N_ELEMENTS(tmp_error) )
shum_rmse= SQRT( TOTAL(shum_error^2)/N_ELEMENTS(shum_error) )
STOP
;rmse_errs=rms(narr1_subset - cfsrmid_subset)
;  ;
;  ; import data here
;  ; define 37 pressure levels in grib data
;  ;
;  p = ['1000', '0975', '0950', '0925', '0900', '0875', '0850', '0825', '0800', '0775', '0750', '0725', '0700', '0650', $
;    '0600', '0550', '0500', '0450', '0400', '0350', '0300', '0250', '0225', '0200', '0175', '0150', '0125', '0100', $
;    '0070', '0050', '0030', '0020', '0010', '0007', '0005', '0003', '0002', '0001']
;
;  gridsize= 720L*361L
;  hgt_1 = MAKE_ARRAY(gridsize, 1, /DOUBLE);N_ELEMENTS(p), /DOUBLE)
;  shum_1 = MAKE_ARRAY(gridsize, 1, /DOUBLE);N_ELEMENTS(p), /DOUBLE)
;  tmp_1 = MAKE_ARRAY(gridsize, 1, /DOUBLE);N_ELEMENTS(p), /DOUBLE)
;
;  ;
;  ; read in height, specific humidity, and temperature from text files for time before Landsat acquisition
;  ;
; ; FOR i = 0, N_ELEMENTS(p)-1 DO BEGIN
;
;    filepart = '0600mbar.txt'
;    hgtFile = path+'HGT1_'+filepart
;    shumFile = path+'SHUM1_'+filepart
;    tmpFile = path+'TMP1_'+filepart
;
;    OPENR, 20, hgtFile
;    tempHGT = MAKE_ARRAY(gridsize+2,/DOUBLE)
;    READF, 20, tempHGT
;    CLOSE, 20
;    FREE_LUN, 20
;    hgt_1[*,0] = tempHGT[2:gridsize+1]
;
;    OPENR, 20, shumFile
;    tempSHUM = MAKE_ARRAY(gridsize+2,/DOUBLE)
;    READF, 20, tempSHUM
;    CLOSE, 20
;    FREE_LUN, 20
;    shum_1[*,0] = tempSHUM[2:gridsize+1]
;
;    OPENR, 20, tmpFile
;    tempTMP = MAKE_ARRAY(gridsize+2,/DOUBLE)
;    READF, 20, tempTMP
;    CLOSE, 20
;    FREE_LUN, 20
;    tmp_1[*,0] = tempTMP[2:gridsize+1]
;
;    STOP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; new part
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    numPoints = N_ELEMENTS(ivector)
;
;    landsatHGT_1 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
;    landsatTMP_1 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
;    landsatSHUM_1 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
;    landsatHGT_2 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
;    landsatTMP_2 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
;    landsatSHUM_2 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
;
;
;    ;
;    ; select paramaters using indexes of which CFSR points are within the scene
;    ;
;    FOR g = 0, N_ELEMENTS(inLandsat)-1 DO BEGIN
;      landsatHGT_1[g,*] = hgt_1[inLandsatArray[0,g], inLandsatArray[1,g], *]
;      landsatTMP_1[g,*] = tmp_1[inLandsatArray[0,g], inLandsatArray[1,g], *]
;      landsatSHUM_1[g,*] = sh_1[inLandsatArray[0,g], inLandsatArray[1,g], *]
;      landsatHGT_2[g,*] = hgt_2[inLandsatArray[0,g], inLandsatArray[1,g], *]
;      landsatTMP_2[g,*] = tmp_2[inLandsatArray[0,g], inLandsatArray[1,g], *]
;      landsatSHUM_2[g,*] = sh_2[inLandsatArray[0,g], inLandsatArray[1,g], *]
;    ENDFOR
;
;    ;
;    ; wherever there are tiny numbers, replace with a 1
;    ;
;    fillHGT_1 = WHERE( ABS(landsatHGT_1 - 1.00000e15) LT 0.0001 )
;    fillheight_1 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
;    fillheight_1[fillHGT_1] = 1
;    fillHGT_2 = WHERE( ABS(landsatHGT_2 - 1.00000e15) LT 0.0001 )
;    fillheight_2 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
;    fillheight_2[fillHGT_2] = 1
;    fillHeight = fillheight_1 OR fillheight_2
;
;    fillTMP_1 = WHERE( ABS(landsatTMP_1 - 1.00000e15) LT 0.0001 )
;    filltemp_1 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
;    filltemp_1[fillTMP_1] = 1
;    fillTMP_2 = WHERE( ABS(landsatTMP_2 - 1.00000e15) LT 0.0001 )
;    filltemp_2 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
;    filltemp_2[fillTMP_2] = 1
;    fillTemp = filltemp_1 OR filltemp_2
;
;    fillSHUM_1 = WHERE( ABS(landsatSHUM_1 - 1.00000e15) LT 0.0001 )
;    fillsh_1 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
;    fillsh_1[fillSHUM_1] = 1
;    fillSHUM_2 = WHERE( ABS(landsatSHUM_2 - 1.00000e15) LT 0.0001 )
;    fillsh_2 = MAKE_ARRAY(numPOints, N_ELEMENTS(p))
;    fillsh_2[fillSHUM_2] = 1
;    fillSH = fillsh_1 OR fillsh_2
;
;    ;
;    ; create pressure array [numPoints x 37 pressure levels]
;    ;
;    pressure = MAKE_ARRAY(numPoints, 37, /DOUBLE)
;    FOR i = 0, numPoints-1 DO pressure[i,*] = DOUBLE(p)
;
;    ;
;    ; convert grib data to variables to be input to MODTRAN
;    ;
;    geometricHeight_1 = CONVERT_GEOPOT2GEOMET(landsatHGT_1, latgrid[inLandsat])
;    geometricHeight_1 = geometricHeight_1/1000D
;    geometricHeight_2 = CONVERT_GEOPOT2GEOMET(landsatHGT_2, latgrid[inLandsat])
;    geometricHeight_2 = geometricHeight_2/1000D
;    relativeHumidity_1 = CONVERT_SH_RH(landsatSHUM_1, landsatTMP_1, pressure)
;    relativeHumidity_2 = CONVERT_SH_RH(landsatSHUM_2, landsatTMP_2, pressure)
;    temperature_1 = landsatTMP_1
;    temperature_2 = landsatTMP_2
;  ;ENDFOR
;    STOP
;  
END