;****************************************************************************
;
;  4dlib/DIAGS/MSE/get_mse.pro
;
;  created:
;     8-30-94	B. Rice 
;
;  usage:       x=get_mse(shot)
;		help,/str,mse -- to see listing of data arrays
;
;  modified:
;    5-9-85	K. Greene -- changed named structure (was MSECHAN) to
;		unnamed structure; attempting to change size of named
;		structure caused crash.
;
;    3-13-86	B. Rice -- changed MSE directory to /d/diags/mse/idl
;
;    4-30-97    bwr  -- added bksub, 35 chan upgrade, check for R=0 err
;    1-5-99     bwr  -- upgraded for 36 channels
;***************************************************************************

function get_mse,shot,cache=cache,bksub=bksub

fname='/scratch/rice/msep/msep' + strtrim(string(shot,'(i6.6)'),2)
file = findfile(fname)

if file(0) ne '' then begin
  print,'reading ',fname
  restore,fname
endif else begin 
  if !version.os eq 'vms' then begin
    result = strpos(!path,'llnl_res')
    if result lt 0 then  llnl_set
  endif else begin
    result = strpos(!path,'/d/diags/mse/idl')
    if result lt 0 then !path=!path+':/d/diags/mse/idl'
  endelse

  nch=36
  if shot lt 97400 then nch = 35
  if shot lt 91300 then nch=16
  if shot lt 80540 then nch=8
  chan=indgen(nch)+1

  ;bksub=0  ;-- background subtract before and after 30lt beam
  ;bksub=1 ;-- background subtract before and after each beam modulation pulse
  ;bksub=2 ;-- background subtract before shot (t<0) only

  if (keyword_set(bksub) eq 0) then bksub=0

;  print,'Reading msep data, background subtract flag = '+string(bksub)
  msep_multi,shot,chan,msep_tm,mdata,kerror,bksub=bksub
  if kerror eq 0 then begin
    mse_geometry,chan,mse_rad,Z,A1,A2,A3,A4,A5,A6,vport	;get mse geometry
    nt=n_elements(msep_tm)
    bz=fltarr(nt,nch)
    gadat,bt_tm,bt,'bt',shot
    w=where((bt_tm gt 0) and (bt_tm lt 3000.))
    bt_ave=total(bt(w))/n_elements(w)
    R0=1.6955
    tg=tan(!pi/180*mdata)

    for i=0,nch-1 do BEGIN
      IF mse_rad(i) NE 0 THEN BEGIN              ;don't use bad channels 
         btr=bt_ave/mse_rad(i)*R0                 ;Bt at mse locations
         bz(*,i)=btr*a2(i)*tg(*,i)/(a1(i)-a4(i)*tg(*,i)) ;approx bz
      ENDIF
    ENDFOR

    dch = {gam: fltarr(nt), gam_err: fltarr(nt), bz:fltarr(nt), $
			a1: float(0),a2:float(0),a3:float(0), $
			a4:float(0),a5:float(0),a6:float(0),r:float(0), $
                        z:float(0), vport:0L, ierr: kerror}
    ch = replicate(dch,nch)			
    msep={shot: shot, time: msep_tm, chan: ch,bksub:bksub, ierr: kerror}
    msep.chan.gam=mdata
    msep.chan.bz=bz
    msep.chan(*).gam_err(*)=.35
    msep.chan.r=mse_rad
    msep.chan.z=z
    msep.chan.a1=a1
    msep.chan.a2=a2
    msep.chan.a3=a3
    msep.chan.a4=a4
    msep.chan.a5 = a5
    msep.chan.a6 = a6
    msep.chan.vport = vport

  endif else msep = {shot: shot, ierr:kerror}
  if (msep.ierr eq 0) and keyword_set(cache) then begin
    print,'writing ',fname
    save,filename = fname,msep
  endif
endelse

return,msep
end
