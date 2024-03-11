;-----------------------------------------------------------------------------
; Pro WRITE_MRZ
;
; Procedure reads rz_in.dat files, calculates the MSE A coeffs, then writes
; mrz_xx.dat file
;
; 3/27/97   B. Rice
; 99/12/7   M. Makowski - added some "/ 100.0" to convert from cm to m
;           Also added coding for new half energy channels (37 - 40). In 
;           order to make code back compatible with READC, the numerical
;           mapping
;                          1.0 -> full
;                          0.5 -> half
;
;           is used as the fifth column of data in the input file. This is 
;           a bit klugey, but it works.
; 03/12/9   M. Makowski - changed mse_geom_coeff ->  mse_geom_coeff2. The
;           new version of mse_geom_coeff incorporates changes to the 
;           geometry resulting from a careful measurement of the 30lt beam
;           centerline geometry.
;
;-----------------------------------------------------------------------------
; External Routines:
;
;  readc
;
;-----------------------------------------------------------------------------

PRO write_mrz, dum

   fname = ' '
   lf=1
   read, 'Enter LZ input filename: ', fname
   read, 'Enter 0 if file has R,Z data, 1 if file has L,Z data: ', lf
   readc, fname, rz

   chan = rz(0,*)
   if lf eq 0 then begin
     R = rz(1,*)
     z = rz(2,*)
     L = fltarr(n_elements(r))
     L(*) = -1
   endif else begin
     L = rz(1,*) / 100.0              ; Unit conversion cm -> m  (mam)
     z = rz(2,*) / 100.0              ; Unit conversion cm -> m  (mam)
     R = fltarr(n_elements(L))
     R(*) = -1
   endelse

   nch = n_elements(z)

   view = strtrim( string( fix(rz(3,*)) ), 2 )
   source = replicate( '30lt', nch )

; Flag energy measured by channel (mam)

   energy = replicate( 'full', nch )
   half_energy = where( rz(4,*) eq 0.5 )
   if ( half_energy[0] ne -1 ) then energy( half_energy ) = 'half'

   mg = mse_geom_coeff2( R, z, L, view, source, energy)

;   A='!fbr      (m)      (m)     coeff     coeff     coeff ' + $
;    '    coeff      (m)     (m)'
;   B='! #  nb   R        Z        A1        A2        A3  ' + $
;    '      A4      DelR    DelZ'
;   C='!--  -  ------  -------  --------  --------  --------' + $
;    '  --------  ------  ------'

   A='!fbr     (m)     (m)    coeff   coeff   coeff ' + $
    '  coeff    coeff     coeff'
   B='! #  nb   R       Z       A1      A2      A3  ' + $
    '    A4        A5        A6'
   C='!--  -  -----  ------  -------  ------  ------' + $
    '  ------  ---------  ---------'

   openw, unit, 'mrz_xx.dat',/get_lun
   printf, unit,A,B,C

   fm = '(1X,I2,2X,A1,1X,F6.3,1X,F7.3,1x,4(1X,F7.4),2(2x,E9.2))'

   FOR i=0,nch-1 DO BEGIN
;      printf,unit,format=fm,I+1,'L',R,Z,mg.A1,mg.A2,mg.A3,mg.A4,0.0,0.0
      printf,unit,format=fm,i+1,'L',R(i),Z(i),mg.A1(i),mg.A2(i),mg.A3(i),  $
       mg.A4(i),mg.A5(i),mg.a6(i)

   ENDFOR

close,unit

   return
END

