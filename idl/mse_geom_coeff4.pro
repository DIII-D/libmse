;-----------------------------------------------------------------------------------------
; Procedure: mse_geom_coeff4
;
; Created: 96-10-4   B. Rice
;
; Modified (yy-mm-dd):
;
;   03/12/09 - mam - New version of mse_geom_coeff2 incorporating changes to
;                    30lt centerline geometry based on measurement by Ron
;                    Ellis in Oct/Nov 2003. For calibrations prior to 2003
;                    the earlier version of this code, mse_geom_coeff, 
;                    should be used.
;   05/10/01 - mam - Upgraded version incorporating the 210rt beamline and the new
;                    channels viewing it
;                    Changed beam voltage from 80 -> 81 kV
;                    Changed variable names to be consitent with MATHEMATICA notebook 
;                    MSE_geom_new4.nb
;                    Modified definition of omega 
; 
;   13/02/12 - CTH - mse_geom_coeff4: Change to the 30LT geometry used by the CER group. Brian Grierson
;		   - and Colin McCrystal say this is consistent with their 
;		     spectroscopic measurements
;
;-----------------------------------------------------------------------------------------
; Calling format:  
;
;  mg = mse_geom_coeff4( R, z, L, view, source, energy )
;
;-----------------------------------------------------------------------------------------
; Inputs:
;
;        R = major radius, scalar or vector (set to -1. if not used)
;        L = distance along beam from beam cross-over (set to -1. if not used)
;            (use R or L but not both)
;        z = z position of beam/view intersection (use zero if not known)
;     view = string '315', '45', or '15' for three mse views
;   source = beam source ('30lt' or '30rt' or '210rt')
;   energy = string 'full' or 'half'
;
;   Note: If R is a vector, then z, view, and source should be also
;
;-----------------------------------------------------------------------------------------
; Outputs:
;
;        mg = mse geometry structure with elements:
;     alpha = angle between beam and Bt
;     omega = angle between Bt and projection of view into r-phi plane
;   a1, ..., a6 = mse coefficients
;      R, L =  major radius and distance to cross-over point
;   x, y, z = x,y,z coordinates of beam-view intersection
;        dR = radial resolution
;
;   Notes: y is aligned with 0 degree port on tokamak
;	   z is in the vertical direction
; 	   All dimensions in meters
;
;-----------------------------------------------------------------------------------------
; Notes:
;
;   y is aligned with 0 degree port on tokamak
;   z is in the vertical direction
;   All dimensions in meters
;
;   Depricated 30lt beam parameters:
;
;     yb0[i] = 1.4039                    ; y-intercept of beam (Burrell lintar37)
;     yb0[i] = 1.3963                    ; y-intercept of beam (Wroblewski)
;     theta_b[i] = 37.437 * !pi / 180.0  ; Angle CCW from x-axis (Burrell)
;     theta_b[i] = 36.814 * !pi / 180.0  ; Angle CCW from x-axis (Wroblewski)
;
;-----------------------------------------------------------------------------------------

function mse_geom_coeff4, R, z, L, view, source, energy

  print_table = 'true'

  n = n_elements(R)
  if ( R[0] EQ -1 ) then n = n_elements(L)
  if ( n_elements(z) ne n ) then z = replicate( z, n )
  if ( n_elements(view) ne n ) then view = replicate( view, n )
  if ( n_elements(source) ne n ) then source = replicate( source, n )
  if ( n_elements(energy) ne n ) then energy = replicate( energy, n )
   
  ; Half and full energy deuterium beam velocities

  vb_full = sqrt( 2.0 * 81.0e3 * 1.6022e-19 / 2.0 / 1.6726e-27 ) 
  vb_half = vb_full / sqrt(2.0)

  wb = 0.14              ; beam full horizontal width half max

  yb0 = fltarr(n)
  theta_b = fltarr(n)
  x_lens = fltarr(n)
  y_lens = fltarr(n)
  z_lens = fltarr(n)
  spot_size = fltarr(n)
  xcp = fltarr(n)
  ycp = fltarr(n)
  cb = fltarr(n)
  sb = fltarr(n)
  sign = fltarr(n)

  for i = 0, n - 1 do begin

    ; Beam Parameters --------------------------------------------------------------------

    if ( source[i] eq '30lt' ) then  begin
     ; yb0[i] = 1.42305                           ; From Ellis 2003 beamline measurement 
     ; theta_b[i] = 35.8938 * !pi / 180.0         ; From Ellis 2003 beamline measurement
     ; xcp[i] = 1.31457                           ; x position of the beam cross-over point
     ; ycp[i] = 2.37442                           ; y position of the beam cross-over point
     ; sign[i] = -1.0
      yb0[i] = 1.40391                           ; From McCrystal/Grierson 2011 measurements 
      theta_b[i] = 37.4368 * !pi / 180.0         ; From McCrystal/Grierson 2011 measurements
      xcp[i] = 1.29260                           ; x position of the beam cross-over point
      ycp[i] = 2.39349                           ; y position of the beam cross-over point
      sign[i] = -1.0
    endif else if ( source[i] eq '30rt' ) then begin
      yb0[i] = 1.050                             ; y-intercept of beam
      theta_b[i] = 46.104 * !pi / 180.0          ; angle counterclockwise from x axis
      xcp[i] = 1.31457                           ; x position of the beam cross-over point
      ycp[i] = 2.37442                           ; y position of the beam cross-over point
      sign[i] = -1.0
    endif else if ( source[i] eq '210rt' ) then begin
      yb0[i] = 10.7242
      theta_b[i] = -96.1395 * !pi / 180.0
      xcp[i] = -1.40208                          ; Holcomb jan 06 in-vessel measurements
      ycp[i] = -2.31038                          ; Holcomb jan 06 in-vessel measurements
      sign[i] = 1.0
    endif

    cb[i] = cos( theta_b[i] )
    sb[i] = sin( theta_b[i] )

    ; MSE viewing lens positions ---------------------------------------------------------

    if ( view[i] eq '315' ) then  begin
      x_lens[i] = -1.8568
      y_lens[i] = 2.0429
      z_lens[i] = -0.13335
      spot_size[i] = 0.02
    endif else if ( view[i] eq '45' ) then begin
      x_lens[i] = 1.731
      y_lens[i] = 1.723
      z_lens[i] = -0.0213
      spot_size[i] = 0.012
    endif else if ( view[i] eq '15' ) then begin
      x_lens[i] = 0.667
      y_lens[i] = 2.505
      z_lens[i] = 0.0254
      spot_size[i] = 0.010
    endif else if ( view[i] eq '195l' ) then begin
      x_lens[i] = -0.6294
      y_lens[i] = -2.3491
      z_lens[i] = -0.127
      spot_size[i] = 0.012
    endif else if ( view[i] eq '195u' ) then begin
      x_lens[i] = -0.6294
      y_lens[i] = -2.3491
      z_lens[i] = 0.127
      spot_size[i] = 0.012
    endif

  endfor

  ; Calclulate R,L,x,y along beam line ---------------------------------------------------
  ; If R > 0 then major radii of sightline and beam centerline are provided, otherwise
  ; L = the distance from the beam cross-over point to point of intersection of the 
  ; sightline and beam centerline is provided

  if ( max(R) gt 0 ) then begin
    x = yb0 * cb * ( - sb - sign * sqrt( - cb^2 + R^2 / yb0^2 ) )
    y = sqrt( R^2 - x^2 )
    L = sqrt( ( xcp - x )^2 + ( ycp - y )^2 )
  endif else begin
    print, 'Compute R from L branch'
    x = xcp + sign * L / sqrt( 1.0 + tan( theta_b )^2 )
    y = tan(theta_b) * x + yb0
    R = sqrt( x^2 + y^2 ) 
  endelse

  if ( print_table eq 'true' ) then begin
    print, ''
    print, '    Channel        R             L           x           y'
    print, '--------------------------------------------------------------'
    for i = 0, n - 1 do begin
      print, i + 1, R[i], L[i], x[i], y[i]
    endfor
    print, ''
    print, ''
  endif

  ; Calculate unit direction vectors -----------------------------------------------------
  ; major radius (radial-vector)

  r_x = x / sqrt( x^2 + y^2 )
  r_y = y / sqrt( x^2 + y^2 )

  ; Toridal direction (phi-vector)

  phi_x = - r_y
  phi_y =   r_x

  ; Beam propagation direction (v-vector)

  v_x = - cb
  v_y = - sb

  ; Observation direction (s-vector)
 
  du = sqrt( ( x - x_lens )^2 + ( y - y_lens )^2 + ( z - z_lens )^2 )
  s_x = ( x_lens - x ) / du
  s_y = ( y_lens - y ) / du
  s_z = ( z_lens - z ) / du

  openw,1,'endpoints.dat'
  for i = 0,n_elements(x_lens)-1 do begin
  printf,1,x_lens[i]*100.0 ,y_lens[i]*100.0 ,z_lens[i]*100.0 ,x[i]*100.0 ,y[i]*100.0 ,z[i]*100.0
  endfor
  close,1
  ; t-vector

  t_x = - s_y / sqrt( s_x^2 + s_y^2 )
  t_y =   s_x / sqrt( s_x^2 + s_y^2 )

  ; Calculate MSE angles and coefficients ------------------------------------------------
  ; alpha = angle between beam and toroidal field

  sign_arg = phi_x * v_y - phi_y * v_x
  alpha = acos( phi_x * v_x + phi_y * v_y ) * sign_arg / abs( sign_arg )

  ; omega = Pi/2 + angle between t and toroidal field
  ; Error in Omega found by Lanctot corrected (mm). Omega_mm is from memo and incorrect
  ;   27-aug-2010

  sign_arg = t_x * phi_y - t_y * phi_x
  omega_mm = !pi / 2.0 + acos( phi_x * t_x + phi_y * t_y ) * sign_arg / abs( sign_arg )
  sign_arg2 = s_x * phi_Y - s_y * phi_x
  omega = acos( (s_x * phi_x + s_y * phi_y ) / sqrt( s_x^2 + s_y^2 ) ) * sign_arg2 /     $
    abs( sign_arg2 )

  print, '    Channel   Omega_mm      Omega     Difference
  print, '------------------------------------------------'
  for i = 0, n_elements(omega_mm) - 1 do begin
    print, i+1, 180.0 * omega_mm[i] / !pi, 180.0 * omega[i] / !pi,                       $
      180.0 * ( omega_mm[i] - omega[i] ) / !pi
  endfor
  print, ' '

  ; phi_d = Doppler angle

  sign_arg = v_x * s_y - v_y * s_x
  phi_d = - acos( - ( v_x * s_x + v_y * s_y ) ) * sign_arg / abs( sign_arg )

  ; theta = view angle relative to horizontal plane

  if ( n_elements(z) ne n_elements(R) ) then begin
    z = replicate( z[0], n_elements(R) )
  endif

  theta = atan( ( z_lens - z ) / sqrt( ( x_lens - x )^2 + ( y_lens - y )^2 ) )

  ; Calculate the A-ceofficients and Raial Resolution ------------------------------------

  a1 = - cos( alpha + omega )
  a2 = cos(theta) * sin(alpha)
  a3 = cos(theta) * cos(alpha)
  a4 = sin(theta) * sin( omega + alpha )

  ; Distinguish between full and half energy channels

  a5 = - cos(omega) / vb_full
  a6 = - cos(theta) / vb_full
  a7 = sin(theta) * sin(omega) / vb_full
  w_half = where( energy eq 'half' )
  if ( w_half[0] ne - 1 ) then begin
    a5[w_half] = - cos( omega[w_half] ) / vb_half
    a6[w_half] = - cos( theta[w_half] ) / vb_half
    a7 = sin(theta) * sin(omega) / vb_half
  endif

  ; Radial resolution

  dR = ( spot_size * abs( sin(alpha) ) + wb * abs( sin(omega) ) ) /                      $
    abs( sin( alpha + omega ) )

  ; Print table of results ---------------------------------------------------------------

  if ( print_table eq 'true' ) then begin
    heading = [ 'Ch', 'R ', 'alpha', 'omega', 'theta ', 'phi_d ', 'a1 ', 'a2 ', 'a3 ',   $
      'a4 ' ]
    print, heading, format = '(a4, 9a10)'
    n = n_elements(r)
    for i = 0, n - 1 do print, [ i + 1, r[i], alpha[i] * 180.0 / !pi, omega[i] * 180 /   $
      !pi, theta[i] * 180.0 / !pi, phi_d[i] * 180.0 / !pi, a1[i], a2[i], a3[i], a4[i] ], $
      format = '(i4, 9f10.4)' 
    print, ''
    print, ''
    heading = [ 'R ', 'alpha', 'omega', ' -cos(w)', 'a1 ', 'dR ' ]
    print, heading, format = '(6a9)'
    for i = 0, n - 1 do print, [ r[i], alpha[i] * 180.0 / !pi, omega[i] * 180.0 / !pi,   $
      - cos( omega[i] ), a1[i], dR[i] ], format = '(6f9.4)' 
  endif

  ; Create structure with geometric data and return --------------------------------------

  mg = { R: R, z: z, L: L, x: x, y: y, alpha: alpha, omega: omega, theta: theta,         $
    phi_d: phi_d, a1: a1, a2: a2, a3: a3, a4: a4, a5: a5, a6: a6, a7: a7, dR: dR }

  return, mg

end
