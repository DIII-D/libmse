;****************************************************************************
;
;  4dlib/DIAGS/MSE/get_mse_raw.pro
;
;  created:
;    08-30-94	B. Rice 
;
;  usage:       x=get_mse_raw(shot)
;		help,/str,mse -- to see listing of data arrays
;
;  modified:
;    07-02-08   wm  - rewrite for new library
;    07-10-06   mam - modifications to handle missing channels
;    10-01-04	cth - add explicit channel number tag
;    10-03-03   mam - gadat -> gadat2
;    01/27/00   mam - added msefitfun keyword
;    01/05/00   tbt - added check of envir variable before reading cache
;                   - changed file directory of cache to local directory.
;                   - took out redefinition of !path
;    01-04-00   mam -- upgraded for 40 channels
;    01-05-99   bwr -- upgraded for 36 channels
;    04-30-97   bwr -- added bksub, 35 chan upgrade, check for R=0 err
;    03-13-86	B. Rice -- changed MSE directory to /d/diags/mse/idl
;    05-09-85	K. Greene -- changed named structure (was MSECHAN) to
;		unnamed structure; attempting to change size of named
;		structure caused crash.
;
;***************************************************************************

function get_mse_raw, shot, cache = cache, bksub = bksub, msefitfun = msefitfun,dt_full=dt_full,times=times,quiet=quiet,debug=debug,ok_30rt=ok_30rt,ok_210lt=ok_210lt,strict=strict,max_beam_off=max_beam_off,notstrict=notstrict

;;fname= '/scratch/rice/msep/msep' + strtrim( string( shot, '(i6.6)' ), 2 )
  fname=                    'msep' + strtrim( string( shot, '(i6.6)' ), 2 )

  file = findfile(fname)
  image = mse_shared_image()

  max_mse_channels = 0L
  ierr = 0L
  lshot = long(shot)
  maxpts = long(393216)
  maxpts = long(480256)
  msep_rate = 2000.0
  mse_strict = 1L

  ; bksub=0 -- background subtract before and after 30lt beam
  ; bksub=1 -- background subtract before and after each beam modulation pulse
  ; bksub=2 -- background subtract before shot (t<0) only

  if ( isa(bksub) eq 0 ) then bksub = 0L

  if ( keyword_set(msefitfun) eq 0 ) then msefitfun = 1L
  
  if ( keyword_set(dt_full) eq 0 ) then dt_full = 0.01

  if ( isa(quiet) eq 0 ) then quiet = 0L
  if ( keyword_set(debug) eq 1 ) then quiet = 3L
  if ( keyword_set(ok_210lt) eq 0) then ok_210lt = 0L
  if ( keyword_set(ok_30rt) eq 0) then ok_30rt = 0L
  if ( keyword_set(strict) eq 1) then mse_strict = 1L
  if ( keyword_set(notstrict) eq 1) then mse_strict = 0L
  if ( keyword_set(max_beam_off) eq 0) then max_beam_off = 0.0



  routine = 'set_mse_quiet_0'
  istat = call_external( image, routine, long(quiet))
  routine = 'set_mse_bksub_0'
  istat = call_external( image, routine, long(bksub))
  routine = 'check_msefitfun_0'
  istat = call_external( image, routine, lshot,long(msefitfun))

  routine = 'get_mse_max_chans_0'
  istat = call_external( image, routine, max_mse_channels )

  if ( keyword_set(times) eq 0 ) then begin
       gadat,msetime,ms1a,'ms1a',lshot,/alldata
       i = where(msetime gt -50.0,count)
       if count gt 0 then begin
           msetime = msetime(i)
           ms1a = ms1a(i)
       endif
       datarange = (max(msetime) - 50.0)/1000.0
       ln_t = long(datarange * msep_rate)
       times = (findgen(ln_t) / msep_rate + 50.0)/1000.0
  endif

  nt = n_elements(times)
  ln_t = long(nt)
  lmsefitfun = long(msefitfun)
  lbksub = long(bksub)
  nch = max_mse_channels

  routine = 'set_mse_beam_logic_0'
  istat = call_external(image,routine,mse_strict,max_beam_off,ok_210lt,ok_30rt)
  routine = 'do_setup_and_beams_0'
  status = call_external(image,routine,lshot,ierr)


  get_mse_configuration2,lshot,c,quiet=long(quiet)
  nch = c.n_chans
  bz = fltarr(maxpts)
  ;gadat, bt_tm, bt, 'bt', lshot,/alldata
  gadat, bt_tm, bt, 'bcoil', lshot,/alldata
  bt_tm = bt_tm/1000.0
  bt_avg = fltarr(ln_t)
  n_bt = long(n_elements(bt))
  routine = 'avg_bt_0'
  status = call_external(image,routine,times,ln_t,0.05,bt,bt_tm,n_bt,bt_avg)

  R0 = 1.6955

  ms_a_avg = fltarr(ln_t)
  ms_b_avg = fltarr(ln_t)
  ms_a_std = fltarr(ln_t)
  ms_b_std = fltarr(ln_t)
  tan_gamma_avg = fltarr(ln_t)
  gamma_avg = fltarr(ln_t)
  tan_gamma_std = fltarr(ln_t)
  read_error = long(0)
  mse_sindata = fltarr(maxpts)
  mse_cosdata = fltarr(maxpts)
  msetime = fltarr(maxpts)
  raw_gamma = fltarr(maxpts)
  bksin = 0.0
  bkcos = 0.0
  bksdsin = 0.0
  bksdcos = 0.0
  beam_mask_aligned  = fltarr(maxpts)
  bt_aligned  = fltarr(maxpts)
  n_mse = 0L

  r_mse = fltarr(max_mse_channels)
  z_mse = fltarr(max_mse_channels)
  a1 = fltarr(max_mse_channels)
  a2 = fltarr(max_mse_channels)
  a3 = fltarr(max_mse_channels)
  a4 = fltarr(max_mse_channels)
  a5 = fltarr(max_mse_channels)
  a6 = fltarr(max_mse_channels)
  a7 = fltarr(max_mse_channels)

   kerror = 0
   dch = { time: fltarr(maxpts),gam: fltarr(maxpts), npts:maxpts, gam_avg: fltarr(nt), gam_err: fltarr(nt), bz:fltarr(maxpts),            $
           a1: float(0), a2: float(0), a3: float(0), a4: float(0),         $
           a5: float(0), a6: float(0), a7:float(0),r: float(0),            $
    z: float(0), vport: 0L, chanlabel:float(0),ierr: kerror }

   ch = replicate( dch, nch )			
   msep = { shot: shot, time_avg: times*1000.0, chan: ch, bksub: bksub,             $
            ierr: kerror, fit_func: msefitfun }

   for i = 0, nch - 1 do begin

      r_mse(i) = c.channel[i].geom.r
      z_mse(i) = c.channel[i].geom.z
      a1(i) = c.channel[i].a_coefs.a1
      a2(i) = c.channel[i].a_coefs.a2
      a3(i) = c.channel[i].a_coefs.a3
      a4(i) = c.channel[i].a_coefs.a4
      a5(i) = c.channel[i].a_coefs.a5
      a6(i) = c.channel[i].a_coefs.a6
      a7(i) = c.channel[i].a_coefs.a7




    if ( r_mse[i] ne 0 ) then begin           ; don't use bad channels 
        routine = 'average_mse_rch4_0'
	chan = long(i+1)
        status = call_external(image,routine,lshot,times,ln_t,chan,lmsefitfun, $
            dt_full,ms_a_avg, ms_b_avg, ms_a_std, ms_b_std, tan_gamma_avg, $
            tan_gamma_std, bt,bt_tm, n_bt,bt_avg,read_error, $
            mse_sindata,mse_cosdata,raw_gamma,msetime,n_mse, $
            bksin,bkcos,bksdsin,bksdcos,gamma_avg,beam_mask_aligned,bt_aligned)
       if( read_error ) then kerror = kerror + 1

       btr = bt_aligned / r_mse[i] * R0            ; Bt at mse locations
       bz = btr * a2[i] * raw_gamma / ( a1[i] - a4[i] * raw_gamma ) 
         ;approx bz
       j = where(beam_mask_aligned gt 0.8,count)
       msep.chan[i].time = msetime(j) * 1000.0
       msep.chan[i].gam = raw_gamma(j) * 180.0 / !pi
       msep.chan[i].bz = bz(j)
       msep.chan[i].npts = count
       msep.chan[i].gam_avg = gamma_avg
       msep.chan[i].gam_err = tan_gamma_std
     endif
   endfor
   msep.ierr = kerror


   msep.chan.r = r_mse[0:nch-1]
   msep.chan.z = z_mse[0:nch-1]
   msep.chan.a1 = a1[0:nch-1]
   msep.chan.a2 = a2[0:nch-1]
   msep.chan.a3 = a3[0:nch-1]
   msep.chan.a4 = a4[0:nch-1]
   msep.chan.a5 = a5[0:nch-1]
   msep.chan.a6 = a6[0:nch-1]
   msep.chan.a7 = a7[0:nch-1]
   msep.chan.vport = c.channel.view_port
   msep.chan.chanlabel = findgen(nch)


 if ( ( msep.ierr eq 0 ) and ( keyword_set(cache) ) ) then begin
   print, 'Get_mse: writing Cache file: ', fname
   save, filename = fname, msep
 endif

  return, msep 

end
