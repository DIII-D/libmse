function read_msep,shot

;	B. Rice 8/30/94
;	usage:  msep=read_msep(shot)
;		help,/str,msep -- to see listing of data arrays

nch=16
if shot lt 80540 then nch=8

chan=indgen(nch)+1

print,'Reading msep data'
msep_multi,shot,chan,msep_tm,mdata,kerror,bksub=1
mse_geometry,chan,mse_rad,Z,A1,A2,A3,A4
nt=n_elements(msep_tm)

msep={dat:fltarr(nt,nch),tm:fltarr(nt),r:fltarr(nch),z:fltarr(nch),$
        a1:fltarr(nch),a2:fltarr(nch),a3:fltarr(nch),a4:fltarr(nch)}

msep.dat=mdata
msep.tm=msep_tm
msep.r=mse_rad
msep.z=z
msep.a1=a1
msep.a2=a2
msep.a3=a3
msep.a4=a4

return,msep
end

