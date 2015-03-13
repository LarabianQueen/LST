; Kelly G. Laraby
; PhD Candidate in Imaging Science
; Rochester Institute of Technology
; kga1099@rit.edu
;
; NAME:  
;    CLOUDDIST_BATCH
;
; PURPOSE:
;    IDL PROCEDURE
;    For a list of Landsat scenes, the "distance to nearest cloud" is calculated for each buoy.  In other words, the euclidean distance
;    from the buoy to every cloud pixel in the scene is calculated, where the smallest distance corresponds to the closest cloud pixel.
;    
;    *** Use this program if you already have the LST bias errors. ***
;    *** If you do NOT have the errors, use ATMparams_buoy.pro to  ***
;    *** get all atmospheric parameters as well as the distance to ***
;    *** nearest cloud value.                                      ***
;
; OUTLINE:
;    1) Read in scenelist.txt, whichBuoyInScene.txt, and buoyHistory.txt
;    2) For each scene in scene list, extract the Landsat number, path, row, and the acquisition date
;    3) Find which buoy station(s) are in each scene and use check_buoy_history to obtain the appropriate lat/long based on date
;    4) Use get_cloud_mask.pro and run calc_nearest_cloud.pro to get distance to nearest cloud
;    5) Write .txt file that shows scene name, buoy #, buoy lat/long, and distance to nearest cloud
;
; CALL SEQUENCE: 
;    CLOUDDIST_BATCH,   folderpath, $               ; path where read-in files and result.tifs are (include \ after folder)
;                       results_filename, $         ; desired name of output .txt file (include .txt)
;                        
;
; RESTRICTIONS:
;    1) Currently, this program works for a maximum of 500 scenes
;    2) Code will only work for a maximum of 2 buoys per scene. Will be increased as necessary.
;    3) read-in files are currently hard-coded in, so their names should not be altered.
;
; REQUIRED PROGRAMS (in working directory):      
;    CALC_NEAREST_CLOUD.PRO
;    CONVERT_LL_UTM.PRO
;    CHECK_BUOY_HISTORY.PRO
;    GET_CLOUD_MASK.PRO
;
; REQUIRED FILES (in folderpath directory):      
;    scenelist.txt
;    whichBuoyInScene.txt
;    buoyHistory.txt
;    scene_results.tif
;    cloud mask files (optional. see create_cloud_mask.pro)
;
; MODIFICATIONS:
;    January, 2015      Original code
;


PRO cloudDist_batch, folderpath, results_filename

  ;
  ; import scenelist 
  ;                  
  scenelistpath= folderpath + 'scenelist.txt'
  scenelist = MAKE_ARRAY(1,500, /STRING)
  line = ''
  count=-1
  OPENR, lun, scenelistpath, /GET_LUN
  WHILE NOT EOF(lun) DO BEGIN & $
    count=count+1
  READF, lun, line & $
    scenelist[0,count]=line
  ENDWHILE

  ;
  ; truncate array 
  ;                
  scenelist=scenelist[*,0:count]
  CLOSE, lun
  FREE_LUN, lun


  ;
  ; import file that labels which buoy     
  ; station #'s correspond to which scenes  
  ;                                        
  whichbuoypath= folderpath + 'whichBuoyInScene.txt'
  buoysinscene = MAKE_ARRAY(5,500)
  line = ''
  count=-1
  OPENR, lun, whichbuoypath, /GET_LUN
  WHILE NOT EOF(lun) DO BEGIN & $
    count=count+1
    READF, lun, line & $
    buoysinscene[0,count]=FLOAT(STRSPLIT(line, string(9B), /EXTRACT))
  ENDWHILE
  buoysinscene=buoysinscene[*,0:count]
  CLOSE, lun
  FREE_LUN, lun 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;import buoyhist.txt
;buoyhistpath= folderpath + 'BuoyHistory.txt'
;buoyhistfull = STRARR(11,1000)
;line = ''
;count=-1
;OPENR, lun, buoyhistpath, /GET_LUN
;WHILE NOT EOF(lun) DO BEGIN & $
;  count=count+1
;READF, lun, line & $
;  ; 9B is the byte value for horizontal tab
;  buoyhistfull[0,count]=STRSPLIT(line, ',', /EXTRACT)
;ENDWHILE
;; truncate array
;buoyhistfull=buoyhistfull[*,0:count]
;CLOSE, lun
;FREE_LUN, lun

; import buoylatlons.txt
buoylatlonpath= folderpath + 'buoylatlon.txt'
buoylatlons = FLTARR(5,1000)
line = ''
count=-1
OPENR, lun, buoylatlonpath, /GET_LUN
WHILE NOT EOF(lun) DO BEGIN & $
  count=count+1
READF, lun, line & $
  ; 9B is the byte value for horizontal tab
  buoylatlons[0,count]=FLOAT(STRSPLIT(line, STRING(9B), /EXTRACT))
ENDWHILE
; truncate array
buoylatlons=buoylatlons[*,0:count]
CLOSE, lun
FREE_LUN, lun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; select only columns 0,1,2,6,7 from buoy history array
;s=SIZE(buoyhistfull)
;buoyhist=DBLARR(5,s[2]-1)
;buoyhist[0:2,*]=DOUBLE(buoyhistfull[0:2,1:*])
;buoyhist[3:4,*]=DOUBLE(buoyhistfull[6:7,1:*])
;buoylatlons=DOUBLE(buoylatlons)

; initiate variables that will be inputs for get_buoy_params.pro
filename=STRARR(1,500)
buoylat=DBLARR(1,500)
buoylon=DBLARR(1,500)
buoynum=INTARR(1,500)
;usebuoy=FLTARR(3,500)

k=0
FOR i= 0,N_ELEMENTS(scenelist)-1 DO BEGIN
  filename_temp=scenelist[i]
  path_temp=STRMID(scenelist[0,i],4,2)
  row_temp=STRMID(scenelist[0,i],7,2)
  yearday_temp=STRMID(scenelist[0,i],9,7)

  ; check which buoys are in the scene, and whether they should be used
  ind=WHERE(path_temp EQ FIX(buoysinscene[0,*]) AND row_temp EQ FIX(buoysinscene[1,*]))
  IF ind[0] EQ -1 THEN BEGIN
    PRINT, 'ERROR: BUOY FOR PATH/ROW NOT IN FILE!'
    STOP 
  ENDIF
  ; if only one buoy
  IF N_ELEMENTS(ind) EQ 1 THEN BEGIN
    buoy=buoysinscene[2,ind]
    ;usebuoy[*,k]= CHECK_BUOY_HISTORY(buoyhist,buoy[0],yearday_temp)
    buoynum[0,k]=buoy
;    buoylat[0,k]= usebuoy[1,k]
;    buoylon[0,k]=usebuoy[2,k]
    buoylat[0,k]=buoylatlons[3,ind]
    buoylon[0,k]=buoylatlons[4,ind]
    filename[0,k]=filename_temp

    k=k+1
    CONTINUE
  ENDIF ELSE BEGIN
    ; case where 2 buoys are present
    ; first buoy
    buoy=buoysinscene[2,ind[0]]
;    usebuoy[*,k]= CHECK_BUOY_HISTORY(buoyhist,buoy[0],yearday_temp)
    buoynum[0,k]=buoy
;    buoylat[0,k]= usebuoy[1,k]
;    buoylon[0,k]=usebuoy[2,k]
    buoylat[0,k]=buoylatlons[3,ind[0]]
    buoylon[0,k]=buoylatlons[4,ind[0]]
    filename[0,k]=filename_temp

    ; second buoy
    k=k+1
    buoy=buoysinscene[2,ind[1]]
;    usebuoy[*,k]= CHECK_BUOY_HISTORY(buoyhist,buoy[0],yearday_temp)
    buoynum[0,k]=buoy
;    buoylat[0,k]= usebuoy[1,k]
;    buoylon[0,k]=usebuoy[2,k]
    buoylat[0,k]=buoylatlons[3,ind[1]]
    buoylon[0,k]=buoylatlons[4,ind[1]]
    filename[0,k]=filename_temp

    k=k+1
    CONTINUE
  ENDELSE

ENDFOR

;truncate arrays
filename=filename[*,0:k-1]
buoynum=buoynum[*,0:k-1]
buoylat=buoylat[*,0:k-1]
buoylon=buoylon[*,0:k-1]
;usebuoy=usebuoy[*,0:k-1]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALCULATE DISTANCE TO NEAREST CLOUD  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

minDistances=DBLARR(1,N_ELEMENTS(buoylat))
FOR i=0,N_ELEMENTS(buoylat)-1 DO BEGIN
    PRINT, STRJOIN([filename[i],'.......'])
    
    ; read in cfmask to get geotiff
    cfmaskpath= folderpath + filename[i] +'_cfmask.tif'
    cfmask=READ_TIFF(cfmaskpath,GEOTIFF=cmask_geotiff)
      
    cloudmask= GET_CLOUDMASK(folderpath, filename[i])
    minDistances[i]= CALC_NEAREST_CLOUD(folderpath,buoylat[i],buoylon[i],cloudmask,cmask_geotiff)
ENDFOR

; write results to file
resultfile=folderpath + results_filename
OPENW,unit,resultfile, /GET_LUN
FOR i=-1,k-1 DO BEGIN
  IF i EQ -1 THEN BEGIN
    PRINTF, unit, FORMAT = '("Scene",T27,"Buoy #",T38,"Buoylat",T50,"Buoylon",T64,"Dist to Nearest Cloud")'
    CONTINUE
  ENDIF
  PRINTF, unit, FORMAT = '(A21,T27,I5,T35,F10.4,T48,F10.4,T62,F15.4)', filename[i],UINT(buoynum[i]),buoylat[i],buoylon[i],minDistances[i]
ENDFOR
CLOSE, unit
FREE_LUN, unit


END