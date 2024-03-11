;------------------------------------------------------------------------------
; Procedure: mse_geom_coeff2
;
; Created: 10-4-96   B. Rice
;
; Modified:
;
;   03/12/09 - mam - New version of mse_geom_coeff2 incorporating changes to
;                    30lt centerline geometry based on measurement by Ron
;                    Ellis in Oct/Nov 2003. For calibrations prior to 2003
;                    the earlier version of this code, mse_geom_coeff, 
;                    should be used.
;
;------------------------------------------------------------------------------
; Calling format:  
;
;  mg = mse_geom_coeff( R, z, L, view, source, energy )
;
;------------------------------------------------------------------------------
; Inputs:
;
;        R = major radius, scalar or vector (set to -1. if not used)
;        L = distance along beam from beam cross-over (set to -1. if not used)
;            (use R or L but not both)
;        z = z position of beam/view intersection (use zero if not known)
;     view = string '315', '45', or '15' for three mse views
;   source = beam source ('30lt' or '30rt')
;   energy = string 'full' or 'half'
;
;   Note: If R is a vector, then z, view, and source should be also
;
;------------------------------------------------------------------------------
; Outputs:
;
;        mg = mse geometry structure with elements:
;     alpha = angle between beam and Bt
;     omega = angle between Bt and view
;   a1, ..., a6 = mse coefficients
;      R, L =  major radius and distance to cross-over point
;   x, y, z = x,y,z coordinates of beam-view intersection
;        dR = radial resolution
;
;   Notes: y is aligned with 0 degree port on tokamak
;	   z is in the vertical direction
; 	   All dimensions in meters
;
;------------------------------------------------------------------------------
  
function mse_geom_coeff2, R, z, L, view, source, energy

  print_table = 'true'

  n = n_elements(R)
  if ( R[0] EQ -1 ) then n = n_elements(L)
  if ( n_elements(z) ne n ) then z = replicate( z, n )
  if ( n_elements(view) ne n ) then view = replicate( view, n )
  if ( n_elements(source) ne n ) then source = replicate( source, n )
  if ( n_elements(energy) ne n ) then energy = replicate( energy, n )
   
  ; Half and full energy deuterium beam velocities

  vb_full = sqrt( 2.0 * 80.0e3 * 1.6022e-19 / 2.0 / 1.6726e-27 ) 
  vb_half = vb_full / sqrt(2.0)

  ; Beam geometry data

  xcp = 1.31457  ;1.2926 ; x position of right-left beam crossover point
  ycp = 2.37442  ;2.3935 ; y "     "
  wb = 0.14              ; beam full width half max

  yb0 = fltarr(n)
  theta_b = yb0
  xv = yb0
  yv = yb0
  zv = yb0
  dv = yb0
  cb = yb0
  sb = yb0

  for i = 0, n - 1 do begin

    case 1 of

      ( source[i] eq '30lt' ): begin

        yb0[i] = 1.42305    ; From Ellis 2003 measurement of beamline
;       yb0[i] = 1.4039     ; y-intercept of beam (Burrell lintar37)
;       yb0[i] = 1.3963     ; " " (Wroblewski)

         theta_b[i] = 35.8938*!pi/180.; From Ellis 2003 measurement of beamline
;        theta_b[i] = 37.437*!pi/180. ; angle ctrclockwise from x axis (Burrell)
;        theta_b[i] = 36.814*!pi/180. ; "   " (Wroblewski)

      end

      ( source[i] eq '30rt' ): begin

        yb0[i] = 1.050                     ; y-intercept of beam
        theta_b[i] = 46.104 * !pi / 180.0  ; angle counterclockwise from x axis

      end

    endcase

    cb[i] = cos( theta_b[i] )
    sb[i] = sin( theta_b[i] )

    ;************ MSE viewing lens positions **********

    case 1 of

      ( view[i] eq '315' ): begin     ; 315 degree port

        xv[i] = -1.8568               ; collection lens position
        yv[i] = 2.0429
        zv[i] = -0.13335
        dv[i] = 0.02                  ; spot size

      end

      ( view[i] eq '45' ): begin      ; 45 degree port

        xv[i] = 1.731                 ; mirror position
        yv[i] = 1.723
        zv[i] = -0.0213
        dv[i] = 0.012

      end

      ( view[i] eq '15' ): begin      ; 15 degree port

         xv[i] = 0.667                ; lens position
         yv[i] = 2.505
         zv[i] = +0.0254
         dv[i] = 0.010

      end

    endcase

  endfor

  ;********* calclulate R,L,x,y along beam line **********

  if ( max(R) gt 0 ) then begin
     x = yb0 * cb * ( -sb + sqrt( -1.0 * cb^2 + R^2 / yb0^2 ) )
     y = sqrt( R^2 - x^2 )
     L = sqrt( ( xcp - x )^2 + ( ycp - y )^2 )
  endif else begin
     b = -1.0 * cb^2 * 2.0 * ( xcp + ( -yb0 + ycp ) * tan(theta_b) )
     c = cb^2 * ( xcp^2 + ycp^2 + yb0^2 - 2.0 * ycp * yb0 - L^2 )
     x = ( -b - sqrt( b^2 - 4.0 * c ) ) / 2.0
     y = tan(theta_b) * x + yb0
;     x = xcp - L * cb
;     y = ycp - L * sb
     R = sqrt( x^2 + y^2 ) 
  endelse

  for i = 0, n - 1 do begin
    print, i+1, R[i], x[i], y[i]
  endfor

  ;************ calculate unit direction vectors **************

  ; major radius

  ux_r = x / sqrt( x^2 + y^2 )
  uy_r = y / x * ux_r

  ; Toridal direction

  ux_tor = -uy_r
  uy_tor =  ux_r

  ; Beam propagation direction

  ux_bm = -cb
  uy_bm = -sb

  ; Observation direction

  c = ( yv - y ) / ( xv - x )
  ux_v = ( xv - x ) / sqrt( ( x - xv )^2 + ( y - yv )^2 )
  uy_v = c * ux_v

  ;******************  calculate MSE angles and coefficients ***************

  ; alpha = angle between beam and toroidal field

  alpha=acos(ux_tor*ux_bm+uy_tor*uy_bm)

  ; omega = angle between observation direction and toroidal field

  c = ux_v * uy_tor - ux_tor * uy_v
  omega = c / abs(c) * acos( ux_tor * ux_v + uy_tor * uy_v )

;  phi = acos( ux_bm * ux_v + uy_bm * uy_v )
;  phi = phi * 180.0 / !pi
;  print, 'phi', phi * 180.0 / !pi
;  stop

  ; theta = view angle relative to horizontal plane

  if ( n_elements(z) ne n_elements(R) ) then                                  $
    z = replicate( z[0], n_elements(R) )
  theta = atan( ( zv - z ) / sqrt( ( xv - x )^2 + ( yv - y )^2 ) )

  a1 = -cos( alpha + omega )
  a2 = cos(theta) * sin(alpha)
  a3 = cos(theta) * cos(alpha)
  a4 = sin(theta) * sin( omega + alpha )

  ; Distinguish between full and half energy channels

  a5 = -cos(omega) / vb_full
  a6 = -cos(theta) / vb_full
  w_half = where( energy eq 'half' )
  if ( w_half[0] ne - 1 ) then begin
    a5[w_half] = -cos( omega[w_half] ) / vb_half
    a6[w_half] = -cos( theta[w_half] ) / vb_half
  endif

  dR = ( dv * abs( sin(alpha) ) + wb * abs( sin(omega) ) ) /                  $
    abs( sin( alpha + omega ) )

  ; Print table of results

  if ( print_table eq 'true' ) then begin
    heading = [ 'R ', 'alpha', 'omega', 'theta ', 'a1 ', 'a2 ', 'a3 ', 'a4 ' ]
    print, heading, format = '(8a9)'
    n = n_elements(r)
    for i = 0, n - 1 do print, [ r[i], alpha[i] * 180.0 / !pi,                $
      omega[i] * 180 / !pi, theta[i] * 180.0 / !pi, a1[i], a2[i], a3[i],      $
      a4[i] ], format = '(8f9.4)' 
    heading = [ 'R ', 'alpha', 'omega', '-cos(w) ', 'a1 ', 'dR ' ]
    print, heading, format = '(6a9)'
    n = n_elements(r)
    for i = 0, n - 1 do print, [ r[i], alpha[i] * 180.0 / !pi,                $
      omega[i] * 180.0 / !pi, -cos( omega[i] ), a1[i], dR[i] ],               $
      format = '(6f9.4)' 
  endif

  mg = { R: R, z: z, L: L, x: x, y: y, alpha: alpha, omega: omega,            $
    theta: theta, a1: a1, a2: a2, a3: a3, a4: a4, a5: a5, a6: a6, dR: dR }

  return, mg

end




