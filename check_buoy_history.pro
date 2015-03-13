; Kelly G. Laraby
; PhD Candidate in Imaging Science
; Rochester Institute of Technology
; kga1099@rit.edu
;
;
; NAME:  
;    CHECK_BUOY_HISTORY
;
; PURPOSE:  
;    IDL FUNCTION
;    This code determines if the buoy should be used based on its start and end dates, and provides the appropriate lat/lons 
;    if it should be used. This function is designed to be called by GET_PARAMS_MANY_SCENES.PRO
;
; OUTLINE:
;    1) check if buoy number is in history file
;    2) retrieve  buoy start/end dates from history file
;    3) change buoy start/end dates to day-of-year format
;    4) see if Landsat acquisition DOY falls within buoy start/end DOY
;    5) return a 0 if buoy isn't valid at acquisition time, return a 1 and lat/lon if buoy is valid
;
; CALL SEQUENCE: 
;    CHECK_BUOY_HISTORY,   buoyhistarr, $                 ; buoy history array from get_params_many_scenes.pro
;                          buoynumber, $                  ; buoy station numbe (e.g. 45012)
;                          yearday, $                     ; string containing year and day of scene (e.g. 2009167)
;
; RESTRICTIONS:
;    1) Doesn't account for leap days, but it shouldn't matter 
;    
;
; REQUIRED PROGRAMS (in working directory):   
;    N/A
;                                            
;  
; REQUIRED FILES (in folderpath directory):   
;    N/A
;
; MODIFICATIONS:
;    November, 2014      Original code
;
;
FUNCTION CHECK_BUOY_HISTORY, buoyhistarr, buoynumber, yearday

  ;
  ; check if buoy number is in history file
  ;
  usebuoy=FLTARR(3,1)
  index= WHERE(buoyhistarr[0,*] EQ buoynumber, count)
  IF count EQ 0 THEN BEGIN
    PRINT, STRJOIN(['ERROR: Buoy number',STRING(UINT(buoynumber), FORMAT = A5),' not found in buoy history file!'],' ')
    STOP
  ENDIF

  sceneyear=STRMID(yearday,0,4)
  sceneDOY=STRMID(yearday,4,3)

  ;
  ; figure out where the scene acquistion date falls to determine buoy lat/lon
  ;
  FOR i = 0, N_ELEMENTS(index)-1 DO BEGIN
    startdate=STRCOMPRESS(STRING(LONG(buoyhistarr[3,index[i]])))
    enddate=STRCOMPRESS(STRING(LONG(buoyhistarr[4,index[i]])))
    startyear= STRMID(startdate,1,4)
    startmonth=STRMID(startdate,5,2)
    startday=STRMID(startdate,7,2)
    endyear= STRMID(enddate,1,4)
    endmonth=STRMID(enddate,5,2)
    endday=STRMID(enddate,7,2)

    ;
    ; change format to day-of-year (DOY)
    ;
    CASE LONG(startmonth) OF
      1: startDOY= startday
      2: startDOY= 31 + startday
      3: startDOY= 31 + 28 + startday
      4: startDOY= 31 + 28 + 31 + startday
      5: startDOY= 31 + 28 + 31 + 30 + startday
      6: startDOY= 31 + 28 + 31 + 30 + 31 + startday
      7: startDOY= 31 + 28 + 31 + 30 + 31 + 30 + startday
      8: startDOY= 31 + 28 + 31 + 30 + 31 + 30 + 31 + startday
      9: startDOY= 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + startday
      10: startDOY= 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + startday
      11: startDOY= 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + startday
      12: startDOY= 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + startday
    ENDCASE
  
    CASE LONG(endmonth) OF
      1: endDOY= endday
      2: endDOY= 31 + endday
      3: endDOY= 31 + 28 + endday
      4: endDOY= 31 + 28 + 31 + endday
      5: endDOY= 31 + 28 + 31 + 30 + endday
      6: endDOY= 31 + 28 + 31 + 30 + 31 + endday
      7: endDOY= 31 + 28 + 31 + 30 + 31 + 30 + endday
      8: endDOY= 31 + 28 + 31 + 30 + 31 + 30 + 31 + endday
      9: endDOY= 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + endday
      10: endDOY= 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + endday
      11: endDOY= 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + endday
      12: endDOY= 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + endday
      99: endDOY=999
    ENDCASE
  
    ;  
    ; if sceneyear is between the start and end years of the buoy, assign lat/longs and break
    ;
    IF sceneyear GT startyear AND sceneyear LT endyear THEN BEGIN
      usebuoy[0]= 1
      usebuoy[1]= buoyhistarr[1,index[i]]
      usebuoy[2]=buoyhistarr[2,index[i]]
      BREAK
    ENDIF ELSE IF sceneyear EQ startyear AND sceneDOY GT startDOY THEN BEGIN
      usebuoy[0]= 1
      usebuoy[1]= buoyhistarr[1,index[i]]
      usebuoy[2]= buoyhistarr[2,index[i]]
      BREAK
    ENDIF ELSE IF sceneyear EQ endyear AND sceneDOY LT endDOY THEN BEGIN
      usebuoy[0]= 1
      usebuoy[1]= buoyhistarr[1,index[i]]
      usebuoy[2]= buoyhistarr[2,index[i]]
      BREAK
    ENDIF ELSE BEGIN
      usebuoy[0]= 0
      usebuoy[1]= 0
      usebuoy[2]= 0
    ENDELSE

    
ENDFOR

RETURN, usebuoy

END
