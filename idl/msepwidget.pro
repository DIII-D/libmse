pro defmsepwidgetcom
common msepwidgetcom,shottextw,readbutw,nchantextw
end

pro msepwidget_event,ev
common msepwidgetcom

if ev.id eq readbutw then begin
  widget_control,nchantextw,get_value=anchan
  i=execute('nchan=['+anchan(0)+']')
  nchan=fix(nchan)
  widget_control,shottextw,get_value=shot & shot=long(shot(0))
  if shot le 0 or shot gt 99999 then shot=77676
  print,'Reading msep data for shot',shot
  msep_multi,shot,nchan,time,mse,ierr
  if ierr ne 0 then begin
   plot,findgen(100),/nodata,xstyle=4,ystyle=4
   xyouts,20,50,charsize=2,'MSEP_MULTI ERROR # '+strtrim(ierr,2),color=4
  endif else begin
    mmin = min(mse)
    mmax = max(mse)
    title = 'MSEP, shot '+strtrim(shot,2)+', channel(s) '+anchan(0)
    plot,time,mse(*,0),yrange=[mmin,mmax],xtitle='time (ms)',title=title
    for i=0,n_elements(nchan)-1 do oplot,time,mse(*,i),color=(i mod 8)+1
  endelse
  return
endif

if ev.id eq shottextw then begin
  return
endif

if ev.id eq nchantextw then begin
  return
endif

help,ev,/stru
return
end

pro msepwidget
common msepwidgetcom
base=widget_base(column=1)
  col1 = widget_base(base,row=1)
  readbutw = widget_button(col1,value='READ AND PLOT MSE DATA')
  sh1 = widget_base(col1,column=1)
    dummy = widget_label(sh1,value='shot number')
    shottextw = widget_text(sh1,value='77676',ysize=1,/edit)
  nc1 = widget_base(col1,column=1)
    dummy = widget_label(nc1,value='chans (eg. 1 or [1,2,3]')
    nchantextw = widget_text(nc1,value='1',ysize=1,/edit)
  draw = widget_draw(base,xsize=400,ysize=200,color=-100)
widget_control,base,/realize
primarycolors
xmanager,'msepwidget',base
print,'xmanager exit'
return
end
