PRO MSE_GEOMETRY,CHANNEL,R,Z,A1,A2,A3,A4,A5,A6,VPORT,SIGHTLINE,ABMODE,LRMODE,$
       OPTSHOT

   ON_ERROR, 2
   IF ( ( N_PARAMS(0) LE 1 ) OR ( N_ELEMENTS(CHANNEL) EQ 0 ) ) THEN BEGIN
      MESSAGE, /CONT, 'Calling sequence: MSE_GEOMETRY,CHANNEL,R,Z,A1,A2,A3,A4,'
      MESSAGE, /CONT, '              A5,A6,VPORT,SIGHTLINE,ABMODE,LRMODE,OPTSHOT'
      IF ( N_PARAMS(0) GE 1 ) THEN BEGIN
         MESSAGE, /CONT, 'For parameter descriptions, run with no parameters.'
         MESSAGE, /CONT, ' '
         RETURN
      ENDIF
      PRINT, 'where:'
      PRINT, '  CHANNEL    : input   channel number, 1 to 40, or array of chan nums'
      PRINT, '  (all output parameters are optional)'
      PRINT, '  R,Z        : output  radial and axial position, in METERS'
      PRINT, '  A1,A2,A3,A4: output  geometry coefficients'
      PRINT, '  A5, A6     : output  Er coefficients'
      PRINT, '  VPORT      : output  viewport (315, 45, 15)'
      PRINT, '  SIGHTLINE  : output  sightline #, 1 to 12'
      PRINT, '  ABMODE     : output  calc mode used, 1=A/B, 2=B/A'
      PRINT, '  LRMODE     : output  beam selections used, 1=L, 2=R'
      PRINT, '  OPTSHOT    : input   optional shot number of setup to read in.'
      PRINT, ' '
      RETURN
   ENDIF

   CH = [ LONG(CHANNEL) ]
   IF ( N_ELEMENTS(OPTSHOT) EQ 0 ) THEN OSHOT = 0L ELSE OSHOT = LONG(OPTSHOT)

   TMPR = 0.0
   TMPZ = 0.0
   TMPA1 = 0.0
   TMPA2 = 0.0
   TMPA3 = 0.0
   TMPA4 = 0.0
   TMPA5 = 0.0
   TMPA6 = 0.0
   TMPSIGHTLINE = 0L
   TMPABMODE = 0L
   TMPLRMODE = 0L
   TMPVPORT = 0L

;  SUBROUTINE MSE_GEOMETRY(CHANNEL, SIGHTLINE, ABMODE, LRMODE, VPORT
;                           R,Z,A1,A2,A3,A4,A5,A6,OPTSHOT)

   IMAGE = MSE_SHARED_IMAGE()
   ROUTINE = 'mse_geometry_0'

   NCH = N_ELEMENTS(CH)
   FOR C = 0, NCH - 1 DO BEGIN
      TMPCH = CH[C]
      ISTAT = CALL_EXTERNAL(IMAGE,ROUTINE,TMPCH,TMPSIGHTLINE,TMPABMODE, $
			    TMPLRMODE,TMPR,TMPZ,TMPA1,TMPA2,TMPA3,TMPA4, $
			    TMPA5,TMPA6,TMPVPORT,OSHOT)
;      IF TMPSIGHTLINE EQ 0 THEN BEGIN
;         MESSAGE,/CONT,'ERROR: SETUP FILES NOT READ IN YET OR CHANNEL OFF.'
;         RETURN
;      ENDIF

      IF ( C EQ 0 ) THEN BEGIN
         R = FLTARR(NCH)
         Z = R
         A1 = R
         A2 = R
         A3 = R
         A4 = R
         A5 = R
         A6 = R
         SIGHTLINE = R
         ABMODE = R
         LRMODE = R
         VPORT = R
      ENDIF

      R[C] = TMPR
      Z[C] = TMPZ
      A1[C] = TMPA1
      A2[C] = TMPA2
      A3[C] = TMPA3
      A4[C] = TMPA4
      A5[C] = TMPA5
      A6[C] = TMPA6
      VPORT[C] = TMPVPORT
      SIGHTLINE[C] = TMPSIGHTLINE
      ABMODE[C] = TMPABMODE
      LRMODE[C] = TMPLRMODE

   ENDFOR

   IF ( ISASCALAR(CHANNEL) ) THEN BEGIN
      R = R[0]
      Z = Z[0]
      A1 = A1[0]
      A2 = A2[0]
      A3 = A3[0]
      A4 = A4[0]
      A5 = A5[0]
      A6 = A6[0]
      VPORT = VPORT[0]
      SIGHTLINE = SIGHTLINE[0]
      ABMODE = ABMODE[0]
      LRMODE = LRMODE[0]
   ENDIF

   RETURN

END



