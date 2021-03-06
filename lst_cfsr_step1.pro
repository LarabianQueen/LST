; Kelly G. Laraby
; 06 March 2015
;
;
; NAME:  lst_cfsr_step1
;
; PURPOSE:  IDL PROCEDURE
;   Creates directories and writes tape5 file, caseList, and commandList
;
; CALL SEQUENCE: lst_cfsr_step1,  home, $            ;directory containing programs and supporting files
;                                 directory, $       ;directory containing landsat metadata and location for results
;                                 imageBase, $       ;basename of landsat files
;                                 year, $            ;year of aquisition of landsat scene
;                                 month, $           ;month of acquisition of landsat scene
;                                 day, $             ;day of acquisition of landsat scene
;                                 hour, $            ;hour of acquisition of landsat scene
;                                 min, $             ;minute of acquisition of landsat scene
;                                 sec, $             ;second of acquisition of landsat scene
;                                 landsatSamples, $  ;number of columns in thermal landsat scene
;                                 landsatLines, $    ;number of rows in thermal landsat scene
;                                 pixelSize, $       ;size of pixel in thermal landsat scene (in meters)
;                                 zone, $            ;utm zone of landsat scene (for utm corrdinates)
;                                 UL_EAST, $         ;easting coordinate of upper left corner
;                                 UL_NORTH, $        ;northing coordiante of upper left corner
;                                 UR_EAST, $         ;easting coordinate of upper right corner
;                                 UR_NORTH, $        ;northing coordinate of upper right corner
;                                 LL_EAST, $         ;easting coordinate of lower left corner
;                                 LL_NORTH, $        ;northing coordinate of lower left corner
;                                 LR_EAST, $         ;easting coordinate of lower right corner
;                                 LR_NORTH,$         ;northing coordinate of lower right corner
;                                 UL_LAT, $          ;latitude of upper left corner
;                                 UL_LON, $          ;longitude of upper left corner
;                                 UR_LAT, $          ;latitude of upper right corner
;                                 UR_LON, $          ;longitude of upper right corner
;                                 LL_LAT, $          ;latitude of lower left corner
;                                 LL_LON, $          ;longitude of lower left corner
;                                 LR_LAT, $          ;latitude of lower right corner
;                                 LR_LON             ;longitude of lower right corner
;
;  RESTRICTIONS:
;
;  REQUIRED PROGRAMS AND FILES (in home directory):   CONVERT_GEOPOT2GEOMET.pro
;                                                     CONVERT_SH_RH.pro
;                                                     stanAtm.txt
;                                                     head.txt
;                                                     tail.txt
;

PRO lst_cfsr_step1, home, $
                    directory, $
                    imageBase, $
                    year, $
                    month, $
                    day, $
                    hour, $
                    min, $
                    sec, $
                    landsatSamples, $
                    landsatLines, $
                    pixelSize, $
                    zone, $
                    UL_EAST, $
                    UL_NORTH, $
                    UR_EAST, $
                    UR_NORTH, $
                    LL_EAST, $
                    LL_NORTH, $
                    LR_EAST, $
                    LR_NORTH, $
                    UL_LAT, $
                    UL_LON, $
                    UR_LAT, $
                    UR_LON, $
                    LL_LAT, $
                    LL_LON, $
                    LR_LAT, $
                    LR_LON
                    

  ;convert inputs
  home = STRING(home)
  directory = STRING(directory)
  imageBase = STRING(imageBase)
  year = DOUBLE(year)
  month = DOUBLE(month)
  day = DOUBLE(day)
  hour = DOUBLE(hour)
  min = DOUBLE(min)
  sec = DOUBLE(sec)
  landsatSamples = DOUBLE(landsatSamples)
  landsatLines = DOUBLE(landsatLines)
  pixelSize = DOUBLE(pixelSize)
  zone = DOUBLE(zone)
  UL_EAST = DOUBLE(UL_EAST)
  UL_NORTH = DOUBLE(UL_NORTH)
  UR_EAST = DOUBLE(UR_EAST)
  UR_NORTH = DOUBLE(UR_NORTH)
  LL_EAST = DOUBLE(LL_EAST)
  LL_NORTH = DOUBLE(LL_NORTH)
  LR_EAST = DOUBLE(LR_EAST)
  LR_NORTH = DOUBLE(LR_NORTH)
  UL_LAT = DOUBLE(UL_LAT)
  UL_LON = DOUBLE(UL_LON)
  UR_LAT = DOUBLE(UR_LAT)
  UR_LON = DOUBLE(UR_LON)
  LL_LAT = DOUBLE(LL_LAT)
  LL_LON = DOUBLE(LL_LON)
  LR_LAT = DOUBLE(LR_LAT)
  LR_LON = DOUBLE(LR_LON)
  
  
  
  ;
  ; import data here
  ; define 37 pressure levels in grib data
  ;
  p = ['1000', '0975', '0950', '0925', '0900', '0875', '0850', '0825', '0800', '0775', '0750', '0725', '0700', '0650', $
       '0600', '0550', '0500', '0450', '0400', '0350', '0300', '0250', '0225', '0200', '0175', '0150', '0125', '0100', $
       '0070', '0050', '0030', '0020', '0010', '0007', '0005', '0003', '0002', '0001']
       
  gridsize= 720L*361L
  hgt_1 = MAKE_ARRAY(gridsize, N_ELEMENTS(p), /DOUBLE)
  shum_1 = MAKE_ARRAY(gridsize, N_ELEMENTS(p), /DOUBLE)
  tmp_1 = MAKE_ARRAY(gridsize, N_ELEMENTS(p), /DOUBLE)

  ;
  ; read in height, specific humidity, and temperature from text files for time before Landsat acquisition
  ;
  FOR i = 0, N_ELEMENTS(p)-1 DO BEGIN

    filepart = p[i]+'mbar.txt'
    hgtFile = directory+'HGT_1/HGT1_'+filepart
    shumFile = directory+'SHUM_1/SHUM1_'+filepart
    tmpFile = directory+'TMP_1/TMP1_'+filepart

    OPENR, 20, hgtFile
    tempHGT = MAKE_ARRAY(gridsize+2,/DOUBLE)
    READF, 20, tempHGT
    CLOSE, 20
    FREE_LUN, 20
    hgt_1[*,i] = tempHGT[2:gridsize+1]

    OPENR, 20, shumFile
    tempSHUM = MAKE_ARRAY(gridsize+2,/DOUBLE)
    READF, 20, tempSHUM
    CLOSE, 20
    FREE_LUN, 20
    shum_1[*,i] = tempSHUM[2:gridsize+1]

    OPENR, 20, tmpFile
    tempTMP = MAKE_ARRAY(gridsize+2,/DOUBLE)
    READF, 20, tempTMP
    CLOSE, 20
    FREE_LUN, 20
    tmp_1[*,i] = tempTMP[2:gridsize+1]
 
  ENDFOR
  
  
  ;
  ;determine if landsat is in the northern or southern hemisphere. '6' = northern hemisphere, '7' = southern hermisphere.
  ;
  IF UL_LAT GE 0 THEN landsatHemi = 6 ELSE landsatHemi = 7

  ;
  ;define sampling for latitude and longitude of CFSR grid
  ;
  deltalambda = 0.5
  deltaphi = 0.5

  ;
  ;define lon grid for CFSR data
  ;
  i = INDGEN(720)+1
  lon = -180 + deltalambda*(i-1.0)
  longrid = MAKE_ARRAY(720,361, /DOUBLE)
  eye = MAKE_ARRAY(720,361, /DOUBLE)
  FOR g = 0, 360 DO BEGIN
    longrid[*,g] = lon
    eye[*,g] = i
  ENDFOR

  ;
  ;define lat grid for CFSR data
  ;
  j = INDGEN(361)+1
  lat = -90 + deltaphi*(j-1.0)
  latgrid = MAKE_ARRAY(720,361, /DOUBLE)
  jay = MAKE_ARRAY(720,361, /DOUBLE)
  FOR g = 0, 719 DO BEGIN
    latgrid[g,*] = lat
    jay[g,*] = j
  ENDFOR

  ;
  ; expand range to include NARR points outside image for edge pixels
  ;
  UL_LAT = UL_LAT + 0.6 ;1.3
  UL_LON = UL_LON - 0.6 ;1.3
  LR_LAT = LR_LAT - 0.6 ;1.3
  LR_LON = LR_LON + 0.6 ;1.3

  ;
  ; determine what points in the CFSR dataset fall within the Landsat image using logical operators
  ; lessThanLat and greaterThanLat are values where the CFSR values are less than or greater than the edges of the Landsat
  ; corners values respectively. Pixels that are true in both fall within the Landsat scene
  ;
  lessThanLat = DOUBLE(latgrid LT UL_LAT)     
  greaterThanLat = DOUBLE(latgrid GT LR_LAT)   
  keepLat = lessThanLat*greaterThanLat        
  greaterThanLon = DOUBLE(longrid GT UL_LON)  
  lessThanLon = DOUBLE(longrid LT LR_LON)
  keepLon = greaterThanLon*lessThanLon

  ;
  ; values that are true in both keepLat and keepLon fall within the Landsat image
  ; convert indices into (x,y) values in the CFSR dataset
  ; Because the Landsat is in easting/northing originally and the CFSR data is lat/lon, this is not a perfect square
  ;
  keep = keepLat * keepLon
  inLandsat = WHERE(keep NE 0)

  ;
  ; determine indices to pull out rectangle of MERRA points
  ;
  iindices = [MIN(eye[inLandsat])-1,MAX(eye[inLandsat])-1]
  jindices = [MIN(jay[inLandsat])-1,MAX(jay[inLandsat])-1]

  ;
  ;extract coordinates within this rectangle
  ;
  ivalues = eye[iindices[0]:iindices[1],jindices[0]:jindices[1]]
  jvalues = jay[iindices[0]:iindices[1],jindices[0]:jindices[1]]
  ivector = REFORM(ivalues,1,N_ELEMENTS(ivalues))-1
  jvector = REFORM(jvalues,1,N_ELEMENTS(jvalues))-1

  inLandsatArray = [ivector,jvector]
  
; ====================================================================================================================== ;  
; |;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;| ;
; |;;;;;;;;;;;;;;;;;;;;;;;;;; CHANGES HAVE NOT ALL BEEN MADE BELOW THIS POINT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;| ;
; |;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;| ;
; ====================================================================================================================== ;
  
  numPoints = N_ELEMENTS(ivector)

  landsatHGT_1 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
  landsatTMP_1 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
  landsatSHUM_1 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
  landsatHGT_2 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
  landsatTMP_2 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
  landsatSHUM_2 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))

  
  ;
  ; select paramaters using indexes of which CFSR points are within the scene
  ;
  FOR g = 0, N_ELEMENTS(inLandsat)-1 DO BEGIN
    landsatHGT_1[g,*] = hgt_1[inLandsatArray[0,g], inLandsatArray[1,g], *]
    landsatTMP_1[g,*] = tmp_1[inLandsatArray[0,g], inLandsatArray[1,g], *]
    landsatSHUM_1[g,*] = sh_1[inLandsatArray[0,g], inLandsatArray[1,g], *]
    landsatHGT_2[g,*] = hgt_2[inLandsatArray[0,g], inLandsatArray[1,g], *]
    landsatTMP_2[g,*] = tmp_2[inLandsatArray[0,g], inLandsatArray[1,g], *]
    landsatSHUM_2[g,*] = sh_2[inLandsatArray[0,g], inLandsatArray[1,g], *]
  ENDFOR

  ;
  ; make "fill" array that says where the data has a missing value.
  ; will be used for interpolation
  ;
  fillHGT_1 = WHERE( ABS(landsatHGT_1 - 1.00000e15) LT 0.0001 )
  fillheight_1 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
  fillheight_1[fillHGT_1] = 1
  fillHGT_2 = WHERE( ABS(landsatHGT_2 - 1.00000e15) LT 0.0001 )
  fillheight_2 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
  fillheight_2[fillHGT_2] = 1
  fillHeight = fillheight_1 OR fillheight_2

  fillTMP_1 = WHERE( ABS(landsatTMP_1 - 1.00000e15) LT 0.0001 )
  filltemp_1 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
  filltemp_1[fillTMP_1] = 1
  fillTMP_2 = WHERE( ABS(landsatTMP_2 - 1.00000e15) LT 0.0001 )
  filltemp_2 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
  filltemp_2[fillTMP_2] = 1
  fillTemp = filltemp_1 OR filltemp_2

  fillSHUM_1 = WHERE( ABS(landsatSHUM_1 - 1.00000e15) LT 0.0001 )
  fillsh_1 = MAKE_ARRAY(numPoints, N_ELEMENTS(p))
  fillsh_1[fillSHUM_1] = 1
  fillSHUM_2 = WHERE( ABS(landsatSHUM_2 - 1.00000e15) LT 0.0001 )
  fillsh_2 = MAKE_ARRAY(numPOints, N_ELEMENTS(p))
  fillsh_2[fillSHUM_2] = 1
  fillSH = fillsh_1 OR fillsh_2

  ;
  ; create pressure array [numPoints x 37 pressure levels]
  ;
  pressure = MAKE_ARRAY(numPoints, 37, /DOUBLE)
  FOR i = 0, numPoints-1 DO pressure[i,*] = DOUBLE(p)

  ;
  ; convert grib data to variables to be input to MODTRAN
  ;
  geometricHeight_1 = CONVERT_GEOPOT2GEOMET(landsatHGT_1, latgrid[inLandsat])
  geometricHeight_1 = geometricHeight_1/1000D
  geometricHeight_2 = CONVERT_GEOPOT2GEOMET(landsatHGT_2, latgrid[inLandsat])
  geometricHeight_2 = geometricHeight_2/1000D
  relativeHumidity_1 = CONVERT_SH_RH(landsatSHUM_1, landsatTMP_1, pressure)
  relativeHumidity_2 = CONVERT_SH_RH(landsatSHUM_2, landsatTMP_2, pressure)
  temperature_1 = landsatTMP_1
  temperature_2 = landsatTMP_2


  ;========================;
  ; Temporal interpolation ;
  ;========================;
  
  hour1 = DOUBLE(hour1)
  hour2 = DOUBLE(hour2)

  ;
  ; round to nearest minute
  ; convert hour-min acquisition time to decimal time
  ;
  IF sec GE 30 THEN min = min + 1
  time = hour + (min/60D)

  ;
  ; linearly interpolate geometric height, relative humidity, and temperature for CFSR points within the scene
  ; this is the CFSR data corresponding to the acquisition time of the Landsat image converted to appropriated
  ; variable for MODTRAN input
  ;
  geometricHeight = geometricHeight_1 + (time-hour1)*((geometricHeight_2 - geometricHeight_1)/(hour2 - hour1))
  relativeHumidity = relativeHumidity_1 + (time-hour1)*((relativeHumidity_2 - relativeHumidity_1)/(hour2 - hour1))
  temperature = temperature_1 + (time-hour1)*((temperature_2 - temperature_1)/(hour2 - hour1))


  ;======================;
  ; Building tape5 files ;
  ;======================;

  ;
  ; read in file containing standard mid lat summer atmosphere information to be used for upper layers
  ;
  OPENR, 20, home+'stanAtm.txt'
  stanAtm = MAKE_ARRAY(4, 30, /DOUBLE)
  READF, 20, stanAtm
  CLOSE, 20
  FREE_LUN, 20

  ;
  ; separate variable in standard atmosphere
  ;
  stanGeoHeight = stanAtm[0,*]
  stanPress = stanAtm[1,*]
  stanTemp = stanAtm[2,*]
  stanRelHum = stanAtm[3,*]

  ;
  ; determine index of last layer in standard atmosphere
  ;
  stanNum = N_ELEMENTS(stanGeoHeight)
  stanLast = stanNum - 1

  ;
  ; define an array of ground altitudes at which to run modtran for each CFSR point
  ;
  gndalt = [0.0, 0.6, 1.1, 1.6, 2.1, 2.6, 3.1, 3.6, 4.05]
  numElevations = N_ELEMENTS(gndalt)

  ;
  ; determine number of MODTRAN runs and initialize arrays
  ;
  numCases = numPoints*numElevations*3
  caseList = MAKE_ARRAY(numCases, /STRING)
  commandList = MAKE_ARRAY(numCases, /STRING)
  entry = 0

  ;
  ; iterate through all CFSR points within the landsat scene
  ;
  FOR i = 0, numPoints-1 DO BEGIN
    PRINT, i

    ;create a directory for the current CFSR point
    latString = STRMID(STRCOMPRESS(STRING(latGrid[inLandsat[i]])),1,6)
    IF lonGrid[inLandsat[i]] LT 0 THEN BEGIN
      lonString = STRMID(STRCOMPRESS(STRING(lonGrid[inLandsat[i]])),2,6)
    ENDIF ELSE BEGIN
      tempLon = 360D - lonGrid[inLandsat[i]]
      lonString = STRMID(STRCOMPRESS(STRING(tempLon)),1,6)
    ENDELSE
    currentPoint = directory+latString+'_'+lonString
    command = "mkdir "+currentPoint
    SPAWN, command

    ;
    ; pull out arrays for current CFSR point
    ;
    p = REFORM(pressure[i,*])
    t = REFORM(temperature[i,*])
    hgt = REFORM(geometricHeight[i,*])
    rh = REFORM(relativeHumidity[i,*])
    fillt = fillTemp[i,*]
    fillhgt = fillHeight[i,*]
    fillrh = fillSH[i,*]

    ;
    ; check for missing values at each pressure level
    ; extrapolate or interpolate to missing values
    ;
    FOR g = 0, N_ELEMENTS(p)-1 DO BEGIN

      ; temperature
      ; ===========
      ; 
      ; if value is missing, begin interpolation/extrapolation
      ;
      IF fillt[g] EQ 1 THEN BEGIN
        ;if first value is missing, extrapolate using first three relevant values
        ;***assumes that once you find the first relevant value, the next two are also relevant
        IF g EQ 0 THEN BEGIN
          level = g+1
          WHILE fillt[level] EQ 1 DO level = level+1
          x = MAKE_ARRAY(2,3)
          x[0,*] = 1
          x[1,*] = p[level:level+2]
          y = MAKE_ARRAY(1, 3)
          y = t[level:level+2]
          a = INVERT(TRANSPOSE(x)##x)##TRANSPOSE(x)##y
          m = a[1]
          b = a[0]
          t[g] = m*p[g]+b
        ENDIF
        
        IF g NE 0 THEN BEGIN
          ;
          ; if missing value is not first value, determine relevant values above and below and interpolate
          ;
          above = g
          WHILE fillt[above] EQ 1 DO BEGIN
            above = above+1
            ;
            ; if there are no relevant values above, extrapolate using two relevant values below
            ; filling in relevant values from bottom to top - values below should always be relevant
            ;
            IF above EQ 42 THEN BEGIN
              x = MAKE_ARRAY(2,2)
              x[0,*] = 1
              x[1,*] = [p[g-2],p[g-1]]
              y = MAKE_ARRAY(1,2)
              y = [t[g-2],t[g-1]]
              a = INVERT(TRANSPOSE(x)##x)##TRANSPOSE(x)##y
              m = a[1]
              b = a[0]
              t[g] = m*p[g]+b
              BREAK
            ENDIF
          ENDWHILE
          ;
          ; if there are relevant values above, determine index above and interpolate
          ;
          IF above NE 42 THEN BEGIN
            below = g-1
            x = MAKE_ARRAY(2,2)
            x[0,*] = 1
            x[1,*] = [p[below],p[above]]
            y = MAKE_ARRAY(1,2)
            y = [t[below],t[above]]
            a = INVERT(TRANSPOSE(x)##x)##TRANSPOSE(x)##y
            m = a[1]
            b = a[0]
            t[g] = m*p[g]+b
          ENDIF
        ENDIF
      ENDIF

      ; height
      ; ======
      ; 
      ; if value is missing, begin interpolation/extrapolation
      ;
      IF fillhgt[g] EQ 1 THEN BEGIN
        ;if first value is missing, extrapolate using first three relevant values
        ;***assumes that once you find the first relevant value, the next two are also relevant
        IF g EQ 0 THEN BEGIN
          level = g+1
          WHILE fillhgt[level] EQ 1 DO level = level+1
          x = MAKE_ARRAY(2,3)
          x[0,*] = 1
          x[1,*] = p[level:level+2]
          y = MAKE_ARRAY(1, 3)
          y = hgt[level:level+2]
          a = INVERT(TRANSPOSE(x)##x)##TRANSPOSE(x)##y
          m = a[1]
          b = a[0]
          hgt[g] = m*p[g]+b
        ENDIF
        
        IF g NE 0 THEN BEGIN
          ;
          ; if missing value is not first value, determine relevant values above and below and interpolate
          ;
          above = g
          WHILE fillhgt[above] EQ 1 DO BEGIN
            above = above+1
            ;
            ; if there are no relevant values above, extrapolate using two relevant values below
            ; filling in relevant values from bottom to top - values below should always be relevant
            ;
            IF above EQ 42 THEN BEGIN
              x = MAKE_ARRAY(2,2)
              x[0,*] = 1
              x[1,*] = [p[g-2],p[g-1]]
              y = MAKE_ARRAY(1,2)
              y = [hgt[g-2],hgt[g-1]]
              a = INVERT(TRANSPOSE(x)##x)##TRANSPOSE(x)##y
              m = a[1]
              b = a[0]
              hgt[g] = m*p[g]+b
              BREAK
            ENDIF
          ENDWHILE
          ;
          ; if there are relevant values above, determine index above and interpolate
          ;
          IF above NE 42 THEN BEGIN
            below = g-1
            x = MAKE_ARRAY(2,2)
            x[0,*] = 1
            x[1,*] = [p[below],p[above]]
            y = MAKE_ARRAY(1,2)
            y = [hgt[below],hgt[above]]
            a = INVERT(TRANSPOSE(x)##x)##TRANSPOSE(x)##y
            m = a[1]
            b = a[0]
            hgt[g] = m*p[g]+b
          ENDIF
        ENDIF
      ENDIF

      ; relative humidity
      ; =================
      ; 
      ; if value is missing, begin interpolation/extrapolation
      ;
      IF fillrh[g] EQ 1 THEN BEGIN
        ;
        ; if first value is missing, extrapolate using first three relevant values
        ; assumes that once you find the first relevant value, the next two are also relevant
        ;
        IF g EQ 0 THEN BEGIN
          level = g+1
          WHILE fillrh[level] EQ 1 DO level = level+1
          x = MAKE_ARRAY(2,3)
          x[0,*] = 1
          x[1,*] = p[level:level+2]
          y = MAKE_ARRAY(1, 3)
          y = rh[level:level+2]
          a = INVERT(TRANSPOSE(x)##x)##TRANSPOSE(x)##y
          m = a[1]
          b = a[0]
          rh[g] = m*p[g]+b
        ENDIF
        IF g NE 0 THEN BEGIN
          ;
          ; if missing value is not first value, determine relevant values above and below and interpolate
          ;
          above = g
          WHILE fillrh[above] EQ 1 DO BEGIN
            above = above+1
            ;
            ; if there are no relevant values above, extrapolate using two relevant values below
            ; filling in relevant values from bottom to top - values below should always be relevant
            ;
            IF above EQ 42 THEN BEGIN
              x = MAKE_ARRAY(2,2)
              x[0,*] = 1
              x[1,*] = [p[g-2],p[g-1]]
              y = MAKE_ARRAY(1,2)
              y = [rh[g-2],rh[g-1]]
              a = INVERT(TRANSPOSE(x)##x)##TRANSPOSE(x)##y
              m = a[1]
              b = a[0]
              rh[g] = m*p[g]+b
              BREAK
            ENDIF
          ENDWHILE
          ;
          ; if there are relevant values above, determine index above and interpolate
          ;
          IF above NE 42 THEN BEGIN
            below = g-1
            x = MAKE_ARRAY(2,2)
            x[0,*] = 1
            x[1,*] = [p[below],p[above]]
            y = MAKE_ARRAY(1,2)
            y = [rh[below],rh[above]]
            a = INVERT(TRANSPOSE(x)##x)##TRANSPOSE(x)##y
            m = a[1]
            b = a[0]
            rh[g] = m*p[g]+b
          ENDIF
        ENDIF
      ENDIF

    ENDFOR

    ;
    ; set lowest altitude is the first geometric height at that CFSR point (if positive)
    ; (if negative set to zero)
    ;
    IF hgt[0] LT 0 THEN gndalt[0] = 0.000 ELSE gndalt[0] = hgt[0]

    ;
    ; define number of levels and index of maximum level in CFSR data
    ;
    numLevels = N_ELEMENTS(p)
    maxLevel = numLevels-1

    ;
    ; determine latitude and longitude of current CFSR point and insert into tail file
    ;
    command = "cat "+home+"tail.txt | sed 's/latitu/"+latString+"/' > "+home+directory+"newTail.txt"
    SPAWN, command
    command = "cat "+home+directory+"newTail.txt | sed 's/longit/"+lonString+"/' > "+home+directory+"newTail2.txt"
    SPAWN, command

    ;
    ; determine current julian day
    ; first determine if current year is a leap year
    ; A year is a leap year if it is divisible by 4 but not if its divisible by 100 except when its divisible by 400
    ;
    leap = 0
    IF (year MOD 4) EQ 0 THEN BEGIN
      leap = 1
      IF (year MOD 100) EQ 0 THEN BEGIN
        IF (year MOD 400) EQ 0 THEN leap = 1 ELSE leap = 0
      ENDIF
    ENDIF
    CASE month OF
      1: JDAY = (day)
      2: JDAY = 31 + (day)
      3: IF leap THEN JDAY = 31 + 29 + (day) ELSE JDAY = 31 + 28 + (day)
      4: IF leap THEN JDAY = 91 + (day) ELSE JDAY = 90 + (day)
      5: IF leap THEN JDAY = 121 + (day) ELSE JDAY = 120 + (day)
      6: IF leap THEN JDAY = 152 + (day) ELSE JDAY = 151 + (day)
      7: IF leap THEN JDAY = 182 + (day) ELSE JDAY = 181 + (day)
      8: IF leap THEN JDAY = 213 + (day) ELSE JDAY = 212 + (day)
      9: IF leap THEN JDAY = 244 + (day) ELSE JDAY = 243 + (day)
      10: IF leap THEN JDAY = 274 + (day) ELSE JDAY = 273 + (day)
      11: IF leap THEN JDAY = 305 + (day) ELSE JDAY = 304 + (day)
      12: IF leap THEN JDAY = 335 + (day) ELSE JDAY = 334 + (day)
    ENDCASE

    ;
    ; insert current julian day into tail file
    ;
    JDAY = FIX(JDAY)
    IF JDAY GE 100 THEN BEGIN
      jay = STRMID(STRCOMPRESS(STRING(JDAY)),1)
    ENDIF ELSE BEGIN
      jay = STRCOMPRESS(STRING(JDAY))
    ENDELSE
    command = "cat "+home+directory+"newTail2.txt | sed 's/jay/"+jay+"/' > "+home+directory+"newTail3.txt"
    SPAWN, command

    ;
    ; iterature through all ground altitudes at which modtran is run
    ;
    FOR j = 0, numElevations-1 DO BEGIN

      ;
      ; create a directory for the current height
      ;
      gdalt = STRING(gndalt[j], FORMAT = '(F5.3)')
      currentGdalt = currentPoint+'/'+gdalt
      command = "mkdir "+currentGdalt
      SPAWN, command

      ;
      ; print current location and height to command line
      ;
      PRINT, latString, ' ', lonString, ' ', gdalt

      ;
      ; determine layers below current gndalt and closest index above and below
      ;
      delete = WHERE(hgt LT gndalt[j])
      indexBelow = N_ELEMENTS(delete)-1
      indexAbove = N_ELEMENTS(delete)

      ;
      ; linearly interpolate pressure, temperature, and relative humidity to gndalt for lowest layer
      ;
      newPressure = p[indexBelow]+(gndalt[j]-hgt[indexBelow])*((p[indexAbove]-p[indexBelow])/(hgt[indexAbove]-hgt[indexBelow]))
      newTemperature = t[indexbelow]+(gndalt[j]-hgt[indexBelow])*((t[indexAbove]-t[indexBelow])/(hgt[indexAbove]-hgt[indexBelow]))
      newRelativeHumidity = rh[indexBelow]+(gndalt[j]-hgt[indexBelow])*((rh[indexAbove]-rh[indexBelow])/(hgt[indexAbove]-hgt[indexBelow]))

      ; 
      ; create arrays containing only layers to be included in current tape5 file
      ;
      tempGeoHeight = [gndalt[j], [hgt[indexAbove:maxLevel]]]
      tempPress = [newPressure, [p[indexAbove:maxLevel]]]
      tempTemp = [newTemperature, [t[indexAbove:maxLevel]]]
      tempRelHum = [newRelativeHumidity, [rh[indexAbove:maxLevel]]]

      ;
      ; modtran throws as error when there are two identical layers in the tape5 file
      ; if the current ground altitude and the next highest layer are close enough, eliminate interpolated layer
      ;
      IF ABS(gndalt[j]-hgt[indexAbove]) LT 0.001 THEN BEGIN
        tempGeoHeight = hgt[indexAbove:maxLevel]
        tempPress = p[indexAbove:maxLevel]
        tempTemp = t[indexAbove:maxLevel]
        tempRelHum = rh[indexAbove:maxLevel]
      ENDIF

      ;
      ; determine index of last layer
      ;
      last = N_ELEMENTS(tempGeoHeight)-1

      ;
      ; determine maximum height of CFSR layers and where the standard atmosphere is greater than this
      ;
      maxHeight = hgt[maxLevel]
      above = WHERE(stanGeoHeight GT maxHeight)

      ;
      ; if there are at least three layers above to highest CFSR layer, add standard atmosphere layers
      ;
      IF N_ELEMENTS(above) GE 3 THEN BEGIN
        ;
        ; interpolate to 2 layers above highest CFSR layer to create a smooth transition
        ;
        interpolateTo = above[2]
        
        ;
        ; linearly interpolate height, pressure, temp, and rel hum to create a smooth transition between
        ; the CFSR layers and the standard upper atmosphere
        ;
        newHeight = (stanGeoHeight[interpolateTo]+tempGeoHeight[last])/2D
        newPressure2 = tempPress[last]+(newHeight-tempGeoHeight[last])*((stanPress[interpolateTo]-tempPress[last])/(stanGeoHeight[interpolateTo]-tempGeoHeight[last]))
        newTemperature2 = tempTemp[last]+(newHeight-tempGeoHeight[last])*((stanTemp[interpolateTo]-tempTemp[last])/(stanGeoHeight[interpolateTo]-tempGeoHeight[last]))
        newRelativeHumidity2 = tempRelHum[last]+(newHeight-tempGeoHeight[last])*((stanRelHum[interpolateTo]-tempRelHum[last])/(stanGeoHeight[interpolateTo]-tempGeoHeight[last]))

        ;
        ; concatenate CFSR layers, new layer, and standard atmosphere layers
        ;
        tempGeoHeight = [tempGeoHeight, newHeight, stanGeoHeight[interpolateTo:stanLast]]
        tempPress = [tempPress, newPressure2, stanPress[interpolateTo:stanLast]]
        tempTemp = [tempTemp, newTemperature2, stanTemp[interpolateTo:stanLast]]
        tempRelHum = [tempRelHum, newRelativeHumidity2, stanRelHum[interpolateTo:stanLast]]

        ;
        ; determine index of last layer
        ;
        last = N_ELEMENTS(tempGeoHeight)-1

      ENDIF

      ; 
      ; write atmospheric layers to a text file in format proper for tape5 file
      ;
      OPENW, unit, home+directory+'tempLayers.txt', /GET_LUN
      FOR k = 0, last DO BEGIN
        PRINTF, unit, tempGeoHeight[k], tempPress[k], tempTemp[k], tempRelHum[k], 0, 0, 'AAH             ', $
          FORMAT = '(F10.3,E10.3,E10.3,E10.3,E10.3,E10.3,A16)'
      ENDFOR
      CLOSE, unit
      FREE_LUN, unit

      ;
      ; determine number of layers for current ground altitude and insert into head file
      ;
      numLayers = N_ELEMENTS(tempGeoHeight)
      IF numLayers GE 100 THEN BEGIN
        nml = STRMID(STRCOMPRESS(STRING(numLayers)),1)
      ENDIF ELSE BEGIN
        nml = STRCOMPRESS(STRING(numLayers))
      ENDELSE
      command = "cat "+home+"head.txt | sed 's/nml/"+nml+"/' > "+home+directory+"newHead.txt"
      SPAWN, command

      command = "cat "+home+directory+"newHead.txt | sed 's/gdalt/"+gdalt+"/' > "+home+directory+"newHead2.txt"
      SPAWN, command

      ;
      ; define arrays containing [temperature,albedo]  pairs at which to run modtran
      ;
      tmp = ['273','310','000']
      alb = ['0.0','0.0','0.1']

      ;
      ; iterate through pairs
      ;
      FOR k = 0, 2 DO BEGIN
        ;
        ; create directory for the current temperature
        ; insert current temperature into head file
        ;
        currentTemp = currentGdalt+'/'+tmp[k]
        command = "mkdir "+currentTemp
        SPAWN, command
        
        command = "cat "+home+directory+"newHead2.txt | sed 's/tmp/"+tmp[k]+"/' > "+home+directory+"newHead3.txt"
        SPAWN, command

        ;
        ; create directory for the current albedo
        ;
        currentAlb = currentTemp+'/'+alb[k]
        command = "mkdir "+currentAlb
        SPAWN, command

        ;
        ; insert current albedo into head file
        ;
        command = "cat "+home+directory+"newHead3.txt | sed 's/alb/"+alb[k]+"/' > "+home+directory+"newHead4.txt"
        SPAWN, command

        ;
        ; concatenate head file, atmospheric layers, and tail file to create a tape5 file for modtran
        ; specific to this location and ground altitude with variables for temperature and albedo
        ;
        headFile = home+directory+'newHead4.txt '
        tailFile = home+directory+'newTail3.txt'
        tempLayers = home+directory+'tempLayers.txt '
        newFile = currentAlb+'/tape5'
        command = 'cat '+headFile+tempLayers+tailFile+' > '+newFile
        SPAWN, command

        ;
        ; create string for case list containing location of current tape5 file
        ; create string for command list containing commands for modtran run
        ; iterate entry count
        ;
        currentCase = currentAlb
        caseList[entry] = home+currentCase
        commandList[entry] = 'pushd ' + home + currentCase + '; ln -s /dirs/pkg/Mod4v3r1/DATA; /dirs/pkg/Mod4v3r1/Mod4v3r1.exe; popd'
        entry = entry+1

        ; end temperatures loop
      ENDFOR
      ; end ground altitudes loop
    ENDFOR
    ; end CFSR points loop
  ENDFOR

  ;
  ; write caseList and commandList to a file
  ;
  OPENW, unit, home+directory+'caseList', /GET_LUN
  FOR k = 0, N_ELEMENTS(WHERE(caseList NE ''))-1 DO BEGIN
    PRINTF, unit, caseList[k]
  ENDFOR
  CLOSE, unit
  FREE_LUN, unit
  
  OPENW, unit, home+directory+'commandList', /GET_LUN
  FOR k = 0, N_ELEMENTS(WHERE(commandList NE ''))-1 DO BEGIN
    PRINTF, unit, commandList[k]
  ENDFOR
  CLOSE, unit
  FREE_LUN, unit

END


  
  