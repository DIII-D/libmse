
function stark_multi_s,shot,time0,deltatime,bksub=bksub

IF N_PARAMS(0) LE 1 OR N_ELEMENTS(SHOT) EQ 0 $
	OR N_ELEMENTS(TIME0) EQ 0 OR N_ELEMENTS(DELTATIME) EQ 0 THEN BEGIN

  MESSAGE,/CONT,'Calling sequence: x=stark_multi_s(shot,time,dt,[bksub=bksub])'

  IF N_PARAMS(0) GE 1 THEN BEGIN
    MESSAGE,/CONT,'For parameter descriptions, run with no parameters.'
    MESSAGE,/CONT,' '
    RETURN, ''
  ENDIF
  PRINT,'where: '
  PRINT,'  SHOT     : input    shot number'
  PRINT,'  TIME     : input    time in mS at which to do average (or array).'
  PRINT,'  DT       : input    full width in mS of averaging window.'
  print,'   keywords:'
  PRINT,'     BKSUB : input    baseline subtract options (0,1,2)'
  RETURN,' '
ENDIF

SN=LONG(SHOT)
channel=indgen(35)+1
if shot lt 91300 then channel=indgen(16)+1
if shot lt 80540 then channel=indgen(8)+1


CH=[LONG(CHANNEL)]             ; make channels an array.
T0=[FLOAT(TIME0)]/1000.0       ; make an array and convert to seconds.
DTIME=FLOAT(DELTATIME)/1000.0  ; convert to seconds.

IF N_ELEMENTS(BKSUB) EQ 0 THEN BKSUB=0L ELSE BKSUB=LONG(BKSUB)

TGAMMA=0.0 & EGAMMA=0.0 & R=0.0
Z=0.0 & DELR=0.0 & DELZ=0.0 & A1=0.0 & A2=0.0 & A3=0.0 & A4=0.0 
A5=0.0 & A6=0.0
VPORT = 0L & NP=0L

;      SUBROUTINE STARK_MULTI(SN,TIME0,DELTATIME,TGAMMA,EGAMMA,
;     $     R,Z,A1,A2,A3,A4,A5,A6,VPORT,KERROR,CHNUM,BKSUB)

KERROR = -1L

IMAGE=MSE_SHARED_IMAGE()
IF IMAGE EQ '' THEN BEGIN
  MESSAGE,/CONT,'Cannot find mse_0 shared image.'
  RETURN, ''
ENDIF

ROUTINE='set_multibeam_bksub_0'
if bksub ne 0 then print,'Setting multi-beam baseline sub flag to ',bksub
ISTAT=CALL_EXTERNAL(IMAGE,ROUTINE,BKSUB)

ROUTINE='stark_multi_0'

NCH = N_ELEMENTS(CH)
TGAMMA=FLTARR(NCH)
EGAMMA=FLTARR(NCH)
R=FLTARR(NCH)
it=0			;time counter (only one time slice used here)

FOR IC=0,NCH-1 DO BEGIN

  IF IT NE 0 THEN ISTAT=CALL_EXTERNAL(IMAGE,'set_quiet_level_0',3L) $
  ELSE ISTAT=CALL_EXTERNAL(IMAGE,'set_quiet_level_0',0L)
  T=T0
  C=CH(IC)
  TMP_TGAMMA=0.0
  TMP_EGAMMA=0.0
  TMP_R=0.0

  ISTAT = $
     CALL_EXTERNAL(IMAGE,ROUTINE,SN,T,DTIME,TMP_TGAMMA,TMP_EGAMMA,TMP_R,$
            Z,A1,A2,A3,A4,A5,A6,VPORT,KERROR,C,BKSUB)

  IF KERROR NE 0 THEN BEGIN
    TGAMMA=0.0
    EGAMMA=0.0
    R=0.0
    MESSAGE,/CONT,'ERROR='+STRTRIM(KERROR,2)+' (TIME0='+strtrim(T*1000,2)+')'
    RETURN, ''
  ENDIF

  TGAMMA(IC) = TMP_TGAMMA
  EGAMMA(IC) = TMP_EGAMMA
  R(IC) = TMP_R

ENDFOR


   mse_geometry,ch,r,z,a1,a2,a3,a4,a5,a6,vport,sl,abmode,lr
   gam_deg=atan(tgamma)*180/!pi
   err_gam_deg=atan(egamma)*180/!pi

return, {shot:shot, time:time0, dt:deltatime, tgam:tgamma, err_tgam:egamma, $
         gam_deg:gam_deg, err_gam_deg:err_gam_deg, $
         r:r, z:z, a1:a1, a2:a2, a3:a3, a4:a4, a5:a5, a6:a6, vport:vport, $
         bksub:bksub, ierr:kerror}
end




