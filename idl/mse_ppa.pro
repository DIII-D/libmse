; Procedure: mse_ppa
;
;   Routine to calculate the Plasma Pitch Angle (ppa) using the MSE
;   measured pitch angle
;
;----------------------------------------------------------------------------------------------------------------------
; Author: Mike Makowski
;
;----------------------------------------------------------------------------------------------------------------------
; History dd-mm-yy:
;
;   10-04-06 mm Wrote routine
;
;----------------------------------------------------------------------------------------------------------------------
; Inputs:
;
;   shot = shot number of interest
;   degrees = optional KEYWORD input to specify that the angles should be returned in degrees rather than radians 
;
;----------------------------------------------------------------------------------------------------------------------
; Output:
; 
;   ppa = structure containing the plasma pitch angles (in radians) versus time for each mse channel and a common 
;         timebase for all the waveforms
;
;----------------------------------------------------------------------------------------------------------------------

pro mse_ppa, shot, ppa, degree = degree

  get_mse_configuration, shot, config
  n_chans = n_elements( config.channel )

  gadat, tb, msep, 'msep1', shot
  ppa = { chan:replicate( { angle:fltarr( n_elements(msep) ) }, n_chans ), tb:fltarr( n_elements(msep) ) }
  ppa.tb = tb
  print, ppa.tb[0:100]

  for i_ch = 1, n_chans do begin
    pointname = 'msep' + strtrim( string(i_ch), 2 )
    gadat, tb, msep, pointname, shot
    tan_gamma_meas = tan( msep * !pi / 180.0 )
    plasma_pitch_angle = atan( config.channel[i_ch-1].a_coefs.a2 * tan_gamma_meas /                                   $
      ( config.channel[i_ch-1].a_coefs.a1 - config.channel[i_ch-1].a_coefs.a4 * tan_gamma_meas ) )
    if keyword_set(degree) then begin
      ppa.chan[i_ch-1].angle = plasma_pitch_angle * 180.0 / !pi
    endif else begin
      ppa.chan[i_ch-1].angle = plasma_pitch_angle
    endelse
  endfor

  return 

end
