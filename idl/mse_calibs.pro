;-----------------------------------------------------------------------------
; History
;
; 99-12-30 mam - modified for 40 channels
; 02-05-14 mam - modified for 45 channels
;-----------------------------------------------------------------------------

PRO MSE_CALIBS, SHOT, NCHANS, ABMODE, GAIN, SLOPE, BSCALE, BOFFSET, R, Z, IERR

  ON_ERROR, 2
  IF ( ( N_PARAMS(0) LE 1 ) OR ( N_ELEMENTS(SHOT) NE 1 ) ) THEN BEGIN
    MESSAGE,/CONT,'Calling sequence: MSE_CALIBS,SHOT,NCHANS,
    MESSAGE,/CONT,'                    ABMODE,GAIN,SLOPE,BSCALE,' +          $
      ' BOFFSET,R,Z,IERR'
    RETURN
  ENDIF

  MXCH = 45L
  NCHANS = 0L
  OSHOT = LONG(SHOT)
  IERR = 0L
  ABMODE = LONARR(MXCH)
  GAIN = FLTARR(MXCH)
  SLOPE = GAIN
  BSCALE = GAIN
  BOFFSET = GAIN
  R = GAIN
  Z = GAIN

; SUBROUTINE READ_MSE_CALIBS(SHOT, NCHANS, ABMODE,
;     $     GAIN, SLOPE, BSCALE, BOFFSET, R, Z, IERR)

  IMAGE = MSE_SHARED_IMAGE()
  ROUTINE = 'read_mse_calibs_0'

  ISTAT = CALL_EXTERNAL( IMAGE, ROUTINE, OSHOT, NCHANS, ABMODE,              $
			 GAIN, SLOPE, BSCALE, BOFFSET, R, Z, IERR )

  IF ( IERR NE 0 ) THEN BEGIN
    MESSAGE, /CONT, ROUTINE + ' returned error = ' + strtrim( ierr, 2 )
  ENDIF ELSE BEGIN
    IF ( NCHANS LT MXCH ) THEN BEGIN
      ABMODE = TEMPORARY( ABMODE[0:NCHANS-1] )
      GAIN = TEMPORARY( GAIN[0:NCHANS-1] )
      SLOPE = TEMPORARY( SLOPE[0:NCHANS-1] )
      BSCALE = TEMPORARY( BSCALE[0:NCHANS-1] )
      BOFFSET = TEMPORARY( BOFFSET[0:NCHANS-1] )
      R = TEMPORARY( R[0:NCHANS-1] )
      Z = TEMPORARY( Z[0:NCHANS-1] )
    ENDIF
  ENDELSE

  RETURN

END
