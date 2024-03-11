;------------------------------------------------------------------------------
; History:
;
;   08/01/94  BR  Fixed gdata array so data=0 after edge MSE
;                 digitizer is off
;   01/23/95 mdb  Added multi-beam pulse baseline subtract flag.
;   13/01/99 mam  Modified code to handle alternative fitting functions by
;                 adding an optional parameter, msefitfun. In order to 
;                 maitain compatibility with exisiting codes, the traditional
;                 fitting function is used as the default
;
;------------------------------------------------------------------------------

PRO MSEP_MULTI, SHOT, CHANNEL, GTIME, GDATA, KERROR, LABEL = LABEL,           $
  BKSUB = BKSUB, msefitfun = msefitfun

  ON_ERROR, 2

  print, 'MSEP_MULTI.PRO: 45 channel version with fitting function option'

  IF ( ( N_PARAMS(0) LE 1 ) OR ( N_ELEMENTS(SHOT) EQ 0 )                      $
    OR ( N_ELEMENTS(CHANNEL) EQ 0 ) ) THEN BEGIN
    MESSAGE, /CONT, 'Calling sequence: MSEP_MULTI,SHOT,CHANNEL,GTIME,GDATA,'
    MESSAGE, /CONT, ' KERROR [,LABEL=LABEL] [,/BKSUB] [,MSEFITFUN=MSEFITFUN]'
    IF ( N_PARAMS(0) GE 1 ) THEN BEGIN
      MESSAGE, /CONT, 'For parameter descriptions, run with no parameters.'
      MESSAGE, /CONT, ' '
      RETURN
    ENDIF
    PRINT, 'where: '
    PRINT, '  SHOT     : input    shot number'
    PRINT, '  CHANNEL  : input    channel number, 1 to 8, or array of ' +     $
      'chan nums'
    PRINT, '  (all output parameters are optional)'
    PRINT, '  GTIME    : output   time array in mS, corresponding to ' +      $
      'GDATA array.'
    PRINT, '  GDATA    : output   data array of GDATA values.'
    PRINT, '  KERROR   : output   error return, 0 = OK'
    print, '  keywords:'
    PRINT, '    LABEL=LABEL  : output   return label like review'
    PRINT, '    /BKSUB       : input    turn on multi-beam baseline subtr'
    print, '    MSEFITFUN = MSEFITFUN : input  select fitting function'
    PRINT, ' '
    RETURN
  ENDIF

  SN = LONG(SHOT)
  CH = [ LONG(CHANNEL) ]
  TIME1 = -5000.0
  TIME2 = 0.0
  MXPTS = 32768L
  TIME = FLTARR(MXPTS)
  DATA = FLTARR(MXPTS)
  IF ( N_ELEMENTS(BKSUB) EQ 0 ) THEN BKSUB = 1L ELSE BKSUB = LONG(BKSUB)

  ; Default to the traditional fitting function
  ;     a1 * tan[ 2 * ( a2 * gamma + a3 ) ] 
  ; if none is specified

  if ( n_elements(msefitfun) eq 0 ) then msefitfun = 1L

  NP = 0L

  KERROR = -1L

  IMAGE = MSE_SHARED_IMAGE()
  print, image
  IF ( IMAGE NE '' ) THEN BEGIN

    ; Set error message, if any, to ON.

    ROUTINE = 'set_perr_0'
    ONE = 1L
    ISTAT = CALL_EXTERNAL(IMAGE,ROUTINE,ONE)

    ROUTINE = 'set_multibeam_bksub_0'
    if ( bksub ne 0 ) then                                                    $
      print, 'Setting multi-beam baseline sub flag to ', bksub
    ISTAT = CALL_EXTERNAL(IMAGE,ROUTINE,BKSUB)

  ENDIF

  ; Get the data

  NCH = N_ELEMENTS(CH)
  FOR C = 0, NCH - 1 DO BEGIN
    CH1 = CH[C]
    IF ( IMAGE NE '' ) THEN BEGIN
      ROUTINE = 'msep_multi_0'
      BLABEL = BYTENULLTERM('          ',12)
      ISTAT = CALL_EXTERNAL(IMAGE,ROUTINE,$
        SN,MSEFITFUN,BLABEL,TIME1,TIME2,TIME,DATA,NP,CH1,KERROR,MXPTS,BKSUB)
      LAB = STRTRIM( STRING(BLABEL(0:9)), 2 )
      print, 'MSEP_MULTI.PRO: kerror = ', kerror
    ENDIF ELSE BEGIN
      PRINT,'NO SHARED LIBRARY'
    ENDELSE

    IF ( KERROR NE 0 ) THEN BEGIN
      IF ( C EQ 0 ) THEN BEGIN
        GTIME = [0.0]
        GDATA = [0.0]
        return
      ENDIF ELSE BEGIN
        gdata[0,c] = fltarr( n_elements( gdata(*,0) ) )
      ENDELSE
      MESSAGE, /CONT, 'ERROR = ' + STRTRIM(KERROR,2)
    ENDIF ELSE BEGIN
      IF ( C EQ 0 ) THEN BEGIN
        GTIME = TIME[0:NP-1]
        GDATA = FLTARR(NP,NCH)
        LABEL = STRARR(NCH)
      ENDIF
      GDATA[0,C] = DATA[0:NP-1]
    ENDELSE
 
    LABEL[C] = LAB
  ENDFOR

  IF ( ISASCALAR(CHANNEL) ) THEN BEGIN
    GDATA = TEMPORARY( REFORM(GDATA) )
    LABEL = TEMPORARY( REFORM(LABEL) )
  ENDIF

  RETURN

END
