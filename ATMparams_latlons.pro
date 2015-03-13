; Kelly G. Laraby
; PhD Candidate in Imaging Science
; Rochester Institute of Technology
; kga1099@rit.edu
;
; NAME:  
;    ATMPARAMS_LATLONS
;
; PURPOSE:  
;    IDL PROCEDURE
;    This code is designed to calculate the atmospheric parameters for specific lat/lons within Landsat scenes. This allows a list of scenes to be
;    run automatically and writes a text file with the atmospheric parameters and other info for each scene in the list. This the alternative
;    to ATMparams_buoys.pro because non-buoy lat/lons are used.
;
; OUTLINE:
;    1) Read in scene_latlons.txt
;    2) For each scene in scene list, extract the Landsat number, path, row, and the acquisition date
;    3) Run get_atm_params for each lat/lon to obtain atmospheric parameters
;    4) Write .txt file that shows scene name, lat/long, the 5 atmo. params, and the predicted temperature 
;
; CALL SEQUENCE: 
;    ATMPARAMS_LATLONS,   folderpath, $               ; path where read-in files and result.tifs are (include \ after folder)
;                         results_filename, $         ; desired name of output .txt file (include .txt)
;                                        
;
; RESTRICTIONS:
;    1) currently, this program works for a maximum of 500 scenes
;    2) read-in files are currently hard-coded in, so their names should not be altered.
;    3) currently, this program is not equipped to get the "distance to nearest cloud" metric
;
; REQUIRED PROGRAMS (in working directory):      
;    GET_ATM_PARAMS.PRO
;    CONVERT_LL_UTM.PRO
;    CONVERT_RAD_TEMP.PRO
;    CHECK_BUOY_HISTORY.PRO
;    GET_CLOUD_MASK.PRO (optional)
;
; REQUIRED FILES (in folderpath directory):      
;    scene_latlons.txt
;    LUT7.txt
;                                    
; MODIFICATIONS:                                                
;    November, 2014       Original code            
;                                                  
;
PRO ATMparams_latlons, folderpath, results_filename

  ;
  ; import scene_latlons.txt
  ;
  latlonspath= folderpath + 'scene_latlons.txt'
  scene_latlons = MAKE_ARRAY(3,500, /STRING)
  line = ''
  count=-1
  OPENR, lun, latlonspath, /GET_LUN
  WHILE NOT EOF(lun) DO BEGIN & $
    count=count+1
    IF count EQ 0 THEN BEGIN
      CONTINUE
    ENDIF
    READF, lun, line & $
    scene_latlons[0,count]=STRSPLIT(line, STRING(9B), /EXTRACT)
  ENDWHILE
  scene_latlons=scene_latlons[*,0:count]
  CLOSE, lun
  FREE_LUN, lun
  
  ; 
  ; initiate variables that will be inputs for get_buoy_params.pro
  ;
  filename=scene_latlons[0,2:*]
  lats=FLOAT(scene_latlons[1,2:*])
  lons=FLOAT(scene_latlons[2,2:*])
  whichLandsat=INTARR(1,N_ELEMENTS(filename))
  path=INTARR(1,N_ELEMENTS(filename))
  row=INTARR(1,N_ELEMENTS(filename))


  FOR i= 0,N_ELEMENTS(filename)-1 DO BEGIN
    whichLandsat[i]=STRMID(filename[i],2,1)
    path[i]=STRMID(filename[i],4,2)
    row[i]=STRMID(filename[i],7,2)
  ENDFOR

  ;
  ; get parameters
  ;
  params=FLTARR(6,N_ELEMENTS(filename))
  FOR i=0,N_ELEMENTS(filename)-1 DO BEGIN
    PRINT, 'Obatining parameters for ',filename[i],'.....'
    params[*,i]= GET_BUOY_PARAMS(folderpath,filename[i],lats[i],lons[i],path[i],row[i],whichLandsat[i])
  ENDFOR

  ; 
  ; write results to file
  ;
  resultfile=folderpath + results_filename
  OPENW,unit,resultfile, /GET_LUN
  FOR i=-1,N_ELEMENTS(filename)-1 DO BEGIN
    IF i EQ -1 THEN BEGIN
      PRINTF, unit, FORMAT = '("Scene",22X,"Latitude",10X,"Longitude",10X,"Band6",10X,"Transmission",10X,"Upwelled",10X,"Downwelled",10X,"Obs Radiance",10X,"Temperature")'
      CONTINUE
    ENDIF
    PRINTF, unit, FORMAT = '(A21,3X,F10.4,9X,F10.4,9X,F10.6,5X,F10.6,12X,F10.6,8X,F10.6,10X,F10.6,14X,F10.6)', filename[i],lats[i],lons[i],params[0,i],params[1,i],params[2,i],params[3,i],params[4,i],params[5,i]
  ENDFOR
  CLOSE, unit
  FREE_LUN, unit

end