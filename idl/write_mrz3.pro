;----------------------------------------------------------------------------------------------
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
; 05/10/02  mam - Ellimiated call to readc - Replace with line by line read with calls to 
;           parse_line
;
; 13/02/12  CTH Changed call to mse_geom_coeff3 to a call to mse_geom_coeff4. The new version
;	    uses the 30LT beamline geometry Brian Grierson & Colin McCrystal say is conistent
;	    with their spectroscopic measurements
;
;----------------------------------------------------------------------------------------------

pro write_mrz3, dum

  a_line = ''
  fname = ''
  lf = 1
  read, 'Enter LZ input filename: ', fname
  read, 'Enter 0 if file has (R,Z) data, 1 if file has (L,Z) data: ', lf

  ; Read the lz or Rz data file ---------------------------------------------------------------
       
  openr, unit, fname, /get_lun, error = err
  if ( err ne 0 ) then begin
    print, !err_string
    print, 'ERROR: File (' + fname + ') not found'
  endif else begin
    print, ' Opening file ' + fname
  endelse

  R = fltarr(96)
  L = fltarr(96)
  z = fltarr(96)
  view = strarr(96)
  energy = strarr(96)

  while ( not eof(unit) ) do begin
    readf, unit, format = fmt1, a_line
    a_line = strcompress( a_line )
    print, a_line
    if ( strmid( a_line, 0, 1 ) ne ';' ) then begin
      print, a_line
      parse_line, a_line, fragments
      i_ch = fix( fragments[0] ) - 1
      if ( lf eq 0 ) then begin
        R[i_ch] = float( fragments[1] ) / 100.0       ; Unit conversion cm -> m
      endif else begin
        L[i_ch] = float( fragments[1] ) / 100.0       ; Unit conversion cm -> m
      endelse
      z[i_ch] = float( fragments[2] ) / 100.0         ; Unit conversion cm -> m
      view[i_ch] = fragments[3]
      if ( float( fragments[4] ) eq 1.0 ) then begin
        energy[i_ch] = 'full'
      endif else if ( float( fragments[4] ) eq 0.5 ) then begin
        energy[i_ch] = 'half'
      endif
    endif
  endwhile
  
  close, unit
  free_lun, unit

  ; Compute derivative quantities -------------------------------------------------------------

  n_chan = i_ch + 1

  if ( lf eq 0 ) then begin
    L[*] = -1
  endif else begin
    R[*] = -1
  endelse

  beam = replicate( '30lt', n_chan )
  rev_beam = where( ( view eq '195l' ) or ( view eq '195u' ) )
  if rev_beam[0] ne -1 then beam[rev_beam] = '210rt'

  ; Call mse_geom_coeff4, print a table of numbers for inspection, and write the mrz-file

  mg = mse_geom_coeff4( R[0:n_chan-1], z[0:n_chan-1], L[0:n_chan-1], view[0:n_chan-1],        $
    beam[0:n_chan-1], energy[0:n_chan-1] )

  A = '!fbr      (m)    (m)    coeff   coeff   coeff   coeff     coeff      coeff      coeff'
  B = '! #  nb    R      Z       A1      A2      A3      A4        A5         A6         A7'
  C = '!--- --  ----- ------  -------  ------  ------  ------  ---------  ---------  ---------'

  openw, unit, 'mrz_xx.dat', /get_lun
  printf, unit, A, B, C

  fm = '(1X,I2,2X,A1,1X,F6.3,1X,F7.3,1x,4(1X,F7.4),3(2x,E9.2))'

  for i = 0, n_chan - 1 do begin 
    printf, unit, format = fm, i + 1, 'L', mg.R[i], Z[i], mg.A1[i], mg.A2[i], mg.A3[i],       $
      mg.A4[i], mg.A5[i], mg.a6[i], mg.a7[i]
  endfor

  close, unit

  return

end

