;****************************************************************************
;
;  4dlib/DIAGS/MSE/get_mse.pro
;
;  created:
;    08-30-94	B. Rice 
;
;  usage:       x=get_stark2(shot,certree=<n>,/use_cer330,/usr_cer210)
;              certree  = mdsplus tree to read cer data from
;                         0 - mdsplus tree CERQUICK (default)
;                         1 - CERAUTO
;                         2 - CERFIT
;                         3 - CERNEUR
;              /use_cer330 = use cer channals with viewing angles between 310-330 degrees
;              /use_cer210 = use cer channals with viewing angles between 210-230 degrees
;
;		help,/str,mse -- to see listing of data arrays
;
;  modified:
;    07-23-19   wm  - added stark2cer to get cer corrected data
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

function get_stark2, shot, cache = cache, bksub = bksub, msefitfun = msefitfun,dt_full=dt_full,quiet=quiet,certree=certree,use_cer330=use_cer330,use_cer210=use_cer210,times=ltimes


;;fname= '/scratch/rice/msep/msep' + strtrim( string( shot, '(i6.6)' ), 2 )
  fname=                    'msep' + strtrim( string( shot, '(i6.6)' ), 2 )

  file = findfile(fname)
  image = mse_shared_image()

  cer = 0
  if (keyword_set(use_cer330) eq 1 or keyword_set(use_cer210) eq 1) then cer = 1
  if keyword_set(certree) eq 0 then certree = 0L
  if keyword_set(use_cer330) eq 0 then use_cer330 = 0L
  if keyword_set(use_cer210) eq 0 then use_cer210 = 0L
  max_mse_channels = 0L
  if ( keyword_set(quiet) eq 0 ) then quiet = 0L
  routine = 'set_mse_quiet_0'
  istat = call_external( image, routine, long(quiet))
  routine = 'get_mse_max_chans_0'
  istat = call_external( image, routine, max_mse_channels )

  ; bksub=0 -- background subtract before and after 30lt beam
  ; bksub=1 -- background subtract before and after each beam modulation pulse
  ; bksub=2 -- background subtract before shot (t<0) only

  if ( isa(bksub) eq 0 ) then bksub = 0

  if ( keyword_set(msefitfun) eq 0 ) then msefitfun = 1L


  if ( keyword_set(dt_full) eq 0 ) then dt_full = 0.01


  lshot = long(shot)
  if isa(ltimes) eq 0 then begin
     gadat,msetime,ms1a,'ms1a',lshot
     msep_rate = 2000.0
     datarange = (max(msetime) - min(msetime))/1000.0
     n_t = datarange * msep_rate
     ln_t = long(n_t)
     times = findgen(ln_t) / msep_rate + msetime[0]/1000.0
  endif else begin
     n_t = n_elements(ltimes)
     ln_t = long(n_t)
     times = float(ltimes)
  endelse
  lmsefitfun = long(msefitfun)
  lbksub = long(bksub)
  nch = max_mse_channels
  tgamma = fltarr(ln_t,max_mse_channels)
  egamma = fltarr(ln_t,max_mse_channels)
  uncortgamma = fltarr(ln_t,max_mse_channels)
  r_mse = fltarr(max_mse_channels)
  z_mse = fltarr(max_mse_channels)
  a1 = fltarr(max_mse_channels)
  a2 = fltarr(max_mse_channels)
  a3 = fltarr(max_mse_channels)
  a4 = fltarr(max_mse_channels)
  a5 = fltarr(max_mse_channels)
  a6 = fltarr(max_mse_channels)
  a7 = fltarr(max_mse_channels)
  read_error = lonarr(max_mse_channels)
  mse_strict = 1L
  mse_max_beam_off = 0.0
  ok_210lt = 0L
  ok_30rt = 0L
  routine = 'set_mse_beam_logic_0'
  istat = call_external(image,routine,mse_strict,mse_max_beam_off,ok_210lt,ok_30rt)

  if ( cer eq 0 ) then begin
     routine = 'stark2_0'
     print,routine
     istat = call_external( image, routine,  lshot, times, ln_t, $
	    dt_full, lmsefitfun, tgamma, egamma, r_mse, z_mse, $
	    a1, a2, a3, a4, a5, a6, a7, read_error, lbksub, long(quiet) )
  endif else begin
     routine = 'set_cer_correction_0'
     print,routine
     istat = call_external(image,routine,1L,long(certree),long(use_cer330),long(use_cer210))
     routine = 'stark2cer_0'
     istat = call_external( image, routine,  lshot, times, ln_t, $
	    dt_full, lmsefitfun, tgamma, egamma, r_mse, z_mse, $
	    a1, a2, a3, a4, a5, a6, a7, read_error, lbksub, long(quiet), $
            uncortgamma )
  endelse
  index = where(read_error,kerror)
  if ( kerror ne max_mse_channels ) then begin
      get_mse_configuration2,lshot,c
      nt = ln_t
      nch = c.n_chans
      bz = fltarr(nt,nch)
      gadat, bt_tm, bt, 'bt', lshot
      w = where( ( bt_tm gt 0 ) and ( bt_tm lt 3000.0 ) )
      bt_ave = total( bt[w] ) / n_elements(w)
      R0 = 1.6955

      for i = 0, nch - 1 do begin
        if ( r_mse[i] ne 0 ) then begin           ; don't use bad channels 
          btr = bt_ave / r_mse[i] * R0            ; Bt at mse locations
          bz[*,i] = btr * a2[i] * tgamma[*,i] / ( a1[i] - a4[i] * tgamma[*,i] ) 
            ;approx bz
        endif
      endfor


      if ( keyword_set(cer) eq 0 ) then begin
          dch = { gam: fltarr(nt), tgamma: fltarr(nt),gam_err: fltarr(nt), bz:fltarr(nt),            $
              a1: float(0), a2: float(0), a3: float(0), a4: float(0),         $
              a5: float(0), a6: float(0), a7:float(0),r: float(0),            $
	      z: float(0), vport: 0L, chanlabel:float(0),ierr: kerror }

          ch = replicate( dch, nch )			
          msep = { shot: lshot, time: times*1000.0, chan: ch, bksub: bksub,             $
               ierr: kerror, fit_func: msefitfun }
          msep.chan.gam = atan(tgamma[*,0:nch-1]) * 180.0 / !pi
          msep.chan.tgamma = tgamma[*,0:nch-1]
          msep.chan[*].bz = bz
          msep.chan.gam_err = egamma[*,0:nch-1]
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
      endif else begin

          dch = { gam: fltarr(nt), tgamma: fltarr(nt),gam_err: fltarr(nt), bz:fltarr(nt),        $
              a1: float(0), a2: float(0), a3: float(0), a4: float(0),         $
              a5: float(0), a6: float(0), a7:float(0),r: float(0),            $
	      z: float(0), vport: 0L, chanlabel:float(0),ierr: kerror,   $
              uncorrectedgam:fltarr(nt) }

          ch = replicate( dch, nch )			
          msep = { shot: lshot, time: times*1000.0, chan: ch, bksub: bksub,             $
               ierr: kerror, fit_func: msefitfun }
          msep.chan.gam = atan(tgamma[*,0:nch-1]) * 180.0 / !pi
          msep.chan.tgamma = tgamma[*,0:nch-1]
          msep.chan.uncorrectedgam = uncortgamma
          msep.chan[*].bz = bz
          msep.chan.gam_err = egamma[*,0:nch-1]
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

      endelse

  endif else msep = { shot: lshot, ierr: kerror }

    if ( ( msep.ierr eq 0 ) and ( keyword_set(cache) ) ) then begin
      print, 'Get_mse: writing Cache file: ', fname
      save, filename = fname, msep
    endif

  return, msep 

end
