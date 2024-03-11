set_plot,'tek'
device,/tek4100
s=84278

if n_elements(mm) eq 0 then begin
  msep_multi,s,12,t12,m12 & msep_multi,s,13,a,b& msep_multi,s,12,tm,mm,mu=2
endif

if n_elements(ms12a) eq 0 then ms12a=getdata('ms12a',s,t12a)
if n_elements(ms12b) eq 0 then ms12b=getdata('ms12b',s,t12b)
if n_elements(lb) eq 0 then lb=getdata('nbvac30lt',s,tb)

if n_elements(xr) eq 0 then xr=[2600,3000]

!p.multi=[0,1,3]

plot,xr=xr,t12,m12 & plot,xr=xr,tm,mm

yr=[-4,6]
plot,xr=xr,yr=yr,tb,lb,psym=3
oplot,t12a,ms12a,psym=3,color=2
oplot,t12b,ms12b,psym=3,color=3

nmse=n_elements(ms12a) & nnew=0L & kerror=0L
image=shared_image('mse_0')
msesin=ms12a & msecos=ms12b & msetime=t12a
print,' '
MSEP_MULTI,S,13,A,B,MULTI=0
MSEP_MULTI,S,12,A,B,MULTI=0
istat = call_external(image,'read_extr_bksub_0',$
	s,12L,1L,nmse,msesin,msecos,msetime,nnew,kerror)

IF NNEW GT 0 THEN BEGIN
  MSESIN=TEMPORARY(MSESIN(0:NNEW-1))
  MSECOS=TEMPORARY(MSECOS(0:NNEW-1))
  MSETIME=TEMPORARY(MSETIME(0:NNEW-1))
  !P.MULTI=[0,1,2]
  PLOT,TITLE='SIN',T12A,MS12A,XR=[0,5000],PSYM=3 & OPLOT,MSETIME*1000,MSESIN
  PLOT,TITLE='COS',T12B,MS12B,XR=[0,5000],PSYM=3 & OPLOT,MSETIME*1000,MSECOS
ENDIF

end
