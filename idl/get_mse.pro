;****************************************************************************
;
;  4dlib/DIAGS/MSE/get_mse.pro
;
;  created:
;    08-30-94	B. Rice 
;
;  usage:       x=get_mse(shot,[/average | /raw],[option1],...)
;               Returns structure containing mse pitch angle data as is 
;               returned to efit (/average) or at full diagnostic rate (/raw). 
;               
;               universal options:
;               bksub=<n>    Controls the backgroud subtraction of both 
;                             average and raw mode.
;                    = 0 - up to 1 s before beam turn-on and up to 1 
;                          sec after beam turn-off
;                    = 1 - use between beam blip intervals to form 
;                          baseline average
;                    = 2 - use first 50 ms of data record only 
;                          (no post-beam-off data)
;                    = 3 - modified option 0 (eliminates junps in 
;                          averaging interval)
;                    = 4 - use (t0, t0 + up to 1 s) and (t1 - up to 1 s, t1)
;               msefitfun=<n>   Function to use in the fit. May be overridden 
;                               by logic in the library. New shots use 3.
;                    = 1 for tangent slop model
;                    = 3 for tangent offset model
;                    = 4 for tangent offset ressidual model
;               dt_full=<n>  <n> seconds used for time window in average.
;               quiet=<n>  increasing numbers increase verbosity 
;                          of library output
;
;               options for raw mode only:
;               times=[<t1>,<t2>,<t3>,...] times (s) to return averaged mse data
;               /ok_30rt      Ok to return data when 30rt beam is on
;               /ok_210lt     Ok to return data when 210lt beam is on
;               /strict       Beam must be on, no interpolation between on 
;                             intervals
;               /nostrict     Interpolation allowed between beam on times
;               max_beam_off=<f> Max time interval between beam off/beam on 
;                             time over which interpolation is allowed
;                             
;               options for average mode only:
;               certree  = mdsplus tree to read cer data from
;                          0 - mdsplus tree CERQUICK (default)
;                          1 - CERAUTO
;                          2 - CERFIT
;                          3 - CERNEUR
;               /use_cer330 = use cer channals with viewing angles between 310-330 degrees
;               /use_cer210 = use cer channals with viewing angles between 210-230 degrees
;
;               
;		help,/str,mse -- to see listing of data arrays
;
;  modified:
;    07-23-19   wm  - added the cer correction to averaged mode
;    03-19-13   wm  - added get_raw_mse and made default
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



pro get_mse_doc

print,""
print,"  usage:       x=get_mse(shot,[/average | /raw],[option1],...)"
print,"               Returns structure containing mse pitch angle data as is "
print,"               returned to efit (/average) or at full diagnostic rate (/raw). "
print,"               "
print,"           universal options:"
print,"               bksub=<n>    Controls the backgroud subtraction of both "
print,"                             average and raw mode."
print,"                    = 0 - up to 1 s before beam turn-on and up to 1 "
print,"                          sec after beam turn-off"
print,"                    = 1 - use between beam blip intervals to form "
print,"                          baseline average"
print,"                    = 2 - use first 50 ms of data record only "
print,"                          (no post-beam-off data)"
print,"                    = 3 - modified option 0 (eliminates junps in "
print,"                          averaging interval)"
print,"                    = 4 - use (t0, t0 + up to 1 s) and (t1 - up to 1 s, t1)"
print,"               msefitfun=<n>   Function to use in the fit. May be overridden "
print,"                               by logic in the library. New shots use 3."
print,"                    = 1 for tangent slop model"
print,"                    = 3 for tangent offset model"
print,"                    = 4 for tangent offset ressidual model"
print,"               dt_full=<n>  <n> seconds used for time window in average."
print,"               quiet=<n>  increasing numbers increase verbosity "
print,"                          of library output"
print,""
print,"          options for raw mode only:"
print,"                  times=[<t1>,<t2>,<t3>,...] times (s) to return averaged mse data"
print,"                  /ok_30rt      Ok to return data when 30rt beam is on"
print,"                  /ok_210lt     Ok to return data when 210lt beam is on"
print,"                  /strict       Beam must be on, no interpolation between on "
print,"                                intervals"
print,"                  /nostrict     Interpolation allowed between beam on times"
print,"                  max_beam_off=<f> Max time interval between beam off/beam on "
print,"                             time over which interpolation is allowed"
print,"                             "
print,"          options for average mode only:"
print,"                certree  = mdsplus tree to read cer data from"
print,"                           0 - mdsplus tree CERQUICK (default)"
print,"                           1 - CERAUTO"
print,"                           2 - CERFIT"
print,"                           3 - CERNEUR"
print,"                /use_cer330 = use cer channals with viewing angles between 310-330 degrees"
print,"                /use_cer210 = use cer channals with viewing angles between 210-230 degrees"

print,""
print,"               "
print,"                  help,/str,mse -- to see listing of data arrays"
end

function get_mse, shot, cache=cache, bksub=bksub, msefitfun=msefitfun,dt_full=dt_full,quiet=quiet,times=times,raw=raw,average=average,debug=debug,ok_30rt=ok_30rt,ok_210lt=ok_210lt,strict=strict,notstrict=notstrict,max_beam_off=max_beam_off,help=help,certree=certree,use_cer330=use_cer330,use_cer210=use_cer210


  mode = 0L
  if ( keyword_set(average) eq 1 ) then mode = 1L
  if ( keyword_set(raw) eq 1 ) then mode = 2L
  if ( keyword_set(help) eq 1 ) then begin
     get_mse_doc
     return,0
  end


  if ( mode eq 0 ) then begin
     if (keyword_set(use_cer330) eq 1 or keyword_set(use_cer210) eq 1) then $
         print,"CER Correction not available for raw data"
     msep = get_mse_raw( shot, cache = cache, bksub = bksub, msefitfun = msefitfun,dt_full=dt_full,times=times,quiet=quiet,debug=debug,ok_30rt=ok_30rt,ok_210lt=ok_210lt,strict=strict,notstrict=notstrict,max_beam_off=max_beam_off)
  endif

  if ( mode eq 1 ) then $
       msep = get_stark2(shot,cache=cache,bksub=bksub,msefitfun=msefitfun,dt_full=dt_full,quiet=quiet,certree=certree,use_cer330=use_cer330,use_cer210=use_cer210)

  if ( mode eq 2 ) then begin
     if (keyword_set(use_cer330) eq 1 or keyword_set(use_cer210) eq 1) then $
         print,"CER Correction not available for raw data"
     msep = get_mse_raw( shot, cache = cache, bksub = bksub, msefitfun = msefitfun,dt_full=dt_full,times=times,quiet=quiet,debug=debug,ok_30rt=ok_30rt,ok_210lt=ok_210lt,strict=strict,notstrict=notstrict,max_beam_off=max_beam_off)
   endif


       return, msep
end
