PRO STARK_MULTI,SHOT,CHANNEL,TIME0,DELTATIME,TGAMMA,EGAMMA,R, $
                 VP,KERROR,BKSUB=BKSUB, msefitfun=msefitfun

;ON_ERROR,2

IF N_PARAMS(0) LE 1 OR N_ELEMENTS(SHOT) EQ 0 OR N_ELEMENTS(CHANNEL) EQ 0 $
	OR N_ELEMENTS(TIME0) EQ 0 OR N_ELEMENTS(DELTATIME) EQ 0 THEN BEGIN

  MESSAGE,/CONT,'Calling sequence: STARK_MULTI,SHOT,CHANNEL,TIME0,DELTATIME,'
  MESSAGE,/CONT,'                     TGAMMA,EGAMMA,R,KERROR [,/BKSUB]'

  IF N_PARAMS(0) GE 1 THEN BEGIN
    MESSAGE,/CONT,'For parameter descriptions, run with no parameters.'
    MESSAGE,/CONT,' '
    RETURN
  ENDIF
  PRINT,'where: '
  PRINT,'  SHOT     : input    shot number'
  PRINT,'  CHANNEL  : input    channel number, 1 to 8'
  PRINT,'  TIME0    : input    time in mS at which to do average (or array).'
  PRINT,'  DELTATIME: input    width in mS of averaging window.'
  PRINT,'  (all output parameters are optional)'
  PRINT,'  TGAMMA   : output   Avg measured tangent of pitch angle' + $
		' at (each) TIME0 +/- DT'
  PRINT,'  EGAMMA   : output   Error of TGAMMA'
  PRINT,'  R        : output   R location of this channel, in meters.'
  PRINT,'  KERROR   : output   error return, 0 = OK'
  print,'  keywords:'
  PRINT,'    /BKSUB : input    turn on multi-beam baseline subtr'
  print,'    msefitfun =msefitfun : input   select fitting function'
  RETURN
ENDIF

  SN=LONG(SHOT)
  CH=[LONG(CHANNEL)]             ; make channels an array.
  T0=[FLOAT(TIME0)]/1000.0       ; make an array and convert to seconds.
  DTIME=FLOAT(DELTATIME)/1000.0  ; convert to seconds.

  IF N_ELEMENTS(BKSUB) EQ 0 THEN BKSUB=0L ELSE BKSUB=LONG(BKSUB)

  ; Default to the traditional fitting function
  ;     a1 * tan[ 2 * ( a2 * gamma + a3 ) ]
  ; if none is specified

  if ( n_elements(msefitfun) eq 0 ) then msefitfun = 1L

  TGAMMA = 0.0
  EGAMMA = 0.0
  R = 0.0
  Z = 0.0
  DELR = 0.0
  DELZ = 0.0
  A1 = 0.0
  A2 = 0.0
  A3 = 0.0
  A4 = 0.0
  A5 = 0.0
  A6 = 0.0
  VPORT = 0L
  NP = 0L


;      SUBROUTINE STARK_MULTI(SN,TIME0,DELTATIME,TGAMMA,EGAMMA,
;     $     R,Z,DELR,DELZ,A1,A2,A3,A4,KERROR,CHNUM)

KERROR = -1L

IMAGE=MSE_SHARED_IMAGE()
IF IMAGE EQ '' THEN BEGIN
  MESSAGE,/CONT,'Cannot find mse_0 shared image.'
  RETURN
ENDIF

;ROUTINE='set_multibeam_bksub_0'
;if bksub ne 0 then print,'Setting multi-beam baseline sub flag to ',bksub
;ISTAT=CALL_EXTERNAL(IMAGE,ROUTINE,BKSUB)

ROUTINE='stark_multi_0'

NTIME=N_ELEMENTS(T0)
NCH = N_ELEMENTS(CH)
TGAMMA=FLTARR(NTIME,NCH)
EGAMMA=FLTARR(NTIME,NCH)
R=FLTARR(NCH)
VP=FLTARR(NCH)

FOR IC=0,NCH-1 DO BEGIN
 FOR IT=0,NTIME-1 DO BEGIN

  IF IT NE 0 THEN ISTAT=CALL_EXTERNAL(IMAGE,'set_quiet_level_0',3L) $
  ELSE ISTAT=CALL_EXTERNAL(IMAGE,'set_quiet_level_0',0L)

  T=T0(IT)
  C=CH(IC)
  TMP_TGAMMA=0.0
  TMP_EGAMMA=0.0
  TMP_R=0.0
  ISTAT = $
     CALL_EXTERNAL(IMAGE,ROUTINE,SN,T,DTIME,msefitfun,TMP_TGAMMA,TMP_EGAMMA,  $
            TMP_R,Z,A1,A2,A3,A4,A5,A6,VPORT,KERROR,C,BKSUB)

  IF KERROR NE 0 THEN BEGIN
    IF ISASCALAR(NTIME) THEN BEGIN
      TGAMMA=0.0
      EGAMMA=0.0
      R=0.0
    ENDIF
    MESSAGE,/CONT,'ERROR='+STRTRIM(KERROR,2)+' (TIME0='+strtrim(T*1000,2)+')'
    RETURN
  ENDIF

  TGAMMA(IT,IC) = TMP_TGAMMA
  EGAMMA(IT,IC) = TMP_EGAMMA
  R(IC) = TMP_R
  VP(IC)=VPORT

 ENDFOR
ENDFOR

IF ISASCALAR(TIME0) AND ISASCALAR(CHANNEL) THEN BEGIN
  TGAMMA=TGAMMA(0)
  EGAMMA=EGAMMA(0)
  R = R(0)
ENDIF

RETURN
END
