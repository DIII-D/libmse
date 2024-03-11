; 
; msehv.pro
;
; jm  12/20/01	msep(3)
; jm  10/17/01	channels 1-45, msevolts3, max = 1100 
; jm  10/17/01	access database by pointname instead of experiment 
; jm   8/ 4/00	view menu redraw 
; jm   8/ 4/00	plot position, font, charsize keywords
; jm   6/27/00	tmin, tmax widgets
; jm   6/12/00	default = current shot number
; jm   6/ 7/00	msehv_dbread, dbwrite retry on error
; jm   6/ 5/00	short integer dbdata
; jm   6/ 5/00	save file improved format, read .msep, .ms2p files
; jm   6/ 5/00	draw, db button set slider value
; jm   6/ 5/00	forget copy/paste/undo
; jm   6/ 2/00	save/restore file
; jm   6/ 2/00	undo, shot settings, retain=1
; jm   6/ 1/00	showexp, copy/paste
; jm   5/31/00	read, write database
; jm   5/30/00	read shot data
; jm   5/17/00	mse hv settings editor
;
pro msehv_event, ev

help, ev, /structure
type = tag_names(ev, /structure)

widget_control, ev.top, get_uvalue=msehv
help,type

wh = where(([msehv.shot, msehv.tmin, msehv.tmax] eq ev.id), nwh)
if nwh eq 1 then begin
  if ev.update ne 0 then begin 
    widget_control, msehv.shot, get_value=shot
    help,shot 
    if ((shot gt 0) and (shot lt 10000)) or (shot gt 500000) then begin 
      print,string(7b)
    endif else begin
      drawshot, msehv
    endelse
  endif
endif

if type eq 'WIDGET_DRAW' then begin
;
; set corresponding slider to requested hv for given shot
;
  wh = where((msehv.draw eq ev.id), nwh)
  if nwh ne 1 then begin
    help, nwh
  endif else begin
    ich = wh(0)
;
; draw widget uvalue = requested hv 
;
    widget_control, msehv.draw(ich), get_uvalue=value
    widget_control, msehv.slider(ich), set_value=value
  endelse
endif

if type eq 'WIDGET_SLIDER' then begin

  msehv.changes = 1
  widget_control, msehv.discard, sensitive=msehv.changes
  widget_control, msehv.save,    sensitive=msehv.changes
endif

if type eq 'WIDGET_BUTTON' then begin
  widget_control, ev.id, get_value=value
  help, value
  case ev.id of 

msehv.settings:	$
    begin
      widget_control, /hourglass

      for ich=0,msehv.nch-1 do begin
	value = msehv.vreq(ich)
	widget_control, msehv.slider(ich), set_value=value
      endfor

      msehv.changes = 1
      widget_control, msehv.discard, sensitive=msehv.changes
      widget_control, msehv.save,    sensitive=msehv.changes
    end

msehv.putfile:	$
    begin
      suffix = '.msehv'
      filename = dialog_pickfile(dialog_parent=msehv.top,	$
		filter='*'+suffix, get_path=path, /write) 
      help, path, filename

      len = strlen(path) 
      pos = strpos(filename,suffix,len)
      if pos lt 0 then begin
	filename = filename + suffix
      endif

      widget_control, /hourglass
      openw, lun, filename, /get_lun

      for ich=0,msehv.nch-1 do begin
	widget_control, msehv.slider(ich), get_value=value
	chan = msehv.chan(ich)
	printf, lun, chan, value
      endfor

      close, lun
      free_lun, lun
    end

msehv.getfile:	$
    begin
      suffix = '.msehv'
      filename = dialog_pickfile(dialog_parent=msehv.top,	$
		filter='*'+suffix, get_path=path, /read)
      help, path, filename

      widget_control, /hourglass
      openr, lun, filename, /get_lun
      on_ioerror, getout

      pos = strpos(filename,suffix)
      if pos gt 0 then begin			; our file format

	chan = 0
	value = 0

	while 1 do begin
	  readf, lun, chan, value
	  wh = where((msehv.chan eq chan), nwh)
	  if nwh ne 1 then begin
	    help, nwh 
	  endif else begin
	    ich = wh(0)
	    widget_control, msehv.slider(ich), set_value=value
	  endelse
	endwhile
      endif else begin 			; not our format

	len = strlen(filename)
	suffix = strmid(filename,len-4,4)	; last 4 characters

	msep = ['msep','ms2p','ms3p']
	wh = where((msep eq suffix), nwh)
	if nwh eq 1 then begin		; old format

	  isuffix = wh(0)
	  dbdata = intarr(32)
	  readu, lun, dbdata

	  for ich=0,msehv.nch-1 do begin
	    if msehv.msep(ich) eq isuffix then begin
	      jch = msehv.msch(ich)
	      value = dbdata(jch)
	      widget_control, msehv.slider(ich), set_value=value
	    endif
	  endfor
	endif
      endelse
getout: 
      close, lun
      free_lun, lun

      msehv.changes = 1
      widget_control, msehv.discard, sensitive=msehv.changes
      widget_control, msehv.save,    sensitive=msehv.changes
    end

msehv.quit:	$
    begin
      if msehv.changes ne 0 then begin
	response = dialog_message('Save changes?',title='Quit',	$
		/question, /cancel, dialog_parent=ev.top )
	help,response

	if response eq 'Yes' then begin
	  savedb, msehv
	endif 
      endif else response = 'No'

      if response ne 'Cancel' then begin
	widget_control, ev.top, /destroy
	return
      endif
    end

msehv.redraw:	$
    begin
      drawshot, msehv
    end

msehv.reverse:	$
    begin
      widget_control, msehv.reverse, get_uvalue=reverse
      reverse = 1 - reverse
      widget_control, msehv.reverse, set_uvalue=reverse

      temp = !p.color
      !p.color = !p.background
      !p.background = temp
      drawshot, msehv
    end

msehv.showexp:	$
    begin
      widget_control, msehv.showexp, get_uvalue=showexp
      showexp = 1 - showexp
      widget_control, msehv.showexp, set_uvalue=showexp

      showdb, msehv
      drawdb, msehv
    end

msehv.reread:	$
    begin
      readdb, msehv
      drawdb, msehv
      compare, msehv
    end

msehv.discard:	$
    begin
      discard, msehv
    end

msehv.save:	$
    begin
      savedb, msehv
      readdb, msehv
      drawdb, msehv
      compare, msehv
    end

else:		$
    begin
      wh = where((msehv.label eq ev.id), nwh)
      if nwh ne 1 then begin
	help, nwh
      endif else begin
	ich = wh(0)
	widget_control, msehv.slider(ich), set_value=msehv.vdb(ich)
      endelse
    end
  endcase
endif

widget_control, ev.top, set_uvalue=msehv

return
end

pro populate, msehv

for icol=0,msehv.ncols-1 do begin
  msehv.col(icol) = widget_base(msehv.rbase, /col)
endfor

xplot = msehv.xsize / msehv.ncols - 10
yplot = msehv.ysize / msehv.ncols - 66

for ich=0,msehv.nch-1 do begin
icol = ich mod msehv.ncols

  msehv.cbase(ich) = widget_base(msehv.col(icol), /col, $
	/frame, xpad=0, ypad=0, space=0)

  msehv.draw(ich) = widget_draw   (msehv.cbase(ich),	$
	xsize=xplot, ysize=yplot, /button_events)
;
; slow, scrollbar bug
;
;	xsize=xplot, ysize=yplot, /button_events, retain=2)
;
  msehv.slider(ich) = widget_slider(msehv.cbase(ich),	$
	xsize=xplot, min=0, max=1100, uvalue=0)

  msehv.label(ich) = widget_button (msehv.cbase(ich),	$
	value=string(replicate(byte(' '),48)))
endfor

return
end

pro depopulate, msehv

for icol=0,msehv.ncols-1 do begin
  widget_control, msehv.col(icol), /destroy
  msehv.col(icol) = 0
endfor

for ich=0,msehv.nch-1 do begin
  msehv.draw(ich)  = 0
  msehv.slider(ich) = 0
endfor

return
end

pro drawshot, msehv

print,'begin drawshot'
widget_control, /hourglass

widget_control, msehv.tmin, get_value=tmin
widget_control, msehv.tmax, get_value=tmax

tminmax  = [tmin,tmax]
!x.range = [min(tminmax),max(tminmax)]

xout = mean(!x.range) 

!y.range = [-1,6]
!y.style = 1
!y.ticks = 7
!y.minor = 0
!y.tickname = [' ','0',' ','2',' ','4',' ','6']

widget_control, msehv.shot, get_value=shot

if shot le 0 then begin
  shot0 = shotno(shoterr)
  if shoterr ne 0 then begin
    help, shoterr
    goto, shotout
  endif
  shot = shot + shot0
  help,shot 
endif

!p.title = 'Shot ' + strtrim(string(shot),2)

gadat, t, volts,  'msevolts',  shot, /nomds, err=errv
gadat, t, volts2, 'msevolts2', shot, /nomds, err=errv2
gadat, t, volts3, 'msevolts3', shot, /nomds, err=errv3

if errv ne 0 then begin
  help,errv
  volts = fltarr(64)
endif

if errv2 ne 0 then begin
  help,errv2
  volts2 = fltarr(64)
endif

if errv3 ne 0 then begin
  help,errv3
  volts3 = fltarr(64)
endif

voltsx = [volts,volts2,volts3]
voltsx = float(voltsx,0,32,2,3)
voltsx = -fix(voltsx)

for ich=0,msehv.nch-1 do begin
  msehv.vreq(ich) = voltsx(msehv.msch(ich),0,msehv.msep(ich))
  msehv.vact(ich) = voltsx(msehv.msch(ich),1,msehv.msep(ich))
endfor

for ich=0,msehv.nch-1 do begin
;
; hsm optical disk widget clears hourglass, so reset.
; must be done here - timing dependent?
;
  widget_control, /hourglass

  widget_control, msehv.draw(ich), get_value=dwin
  wset,dwin

  chan = msehv.chan(ich) 
  pn = 'ms' + strtrim(string(chan),2)
  pna = pn + 'a'
  pnb = pn + 'b'

  gadat, tmsa, va, pna, shot, /nomds, err=erra
  gadat, tmsb, vb, pnb, shot, /nomds, err=errb

  if erra eq 0 then begin
    ta = tmsa * .001		; time, seconds
    plot, ta, va, position=[.06,.13,.96,.86], font=-1, charsize=.9
  endif else begin
    help, pna, erra
  endelse

  if errb eq 0 then begin
    tb = tmsb * .001
    if erra eq 0 then begin
      oplot, tb, vb
    endif else begin 
      plot , tb, vb
    endelse
  endif else begin
    help, pnb, errb
    if erra ne 0 then begin			; no a, no b
      response = dialog_message(title='gadat',			$
	!p.title +', gadat error = '+ strtrim(string(erra),2),	$
	/cancel, dialog_parent=msehv.top )
      help,response
      if response eq 'Cancel' then goto, shotout
    endif
  endelse
  
  xyouts,xout,5,'req = ' + strtrim(string(msehv.vreq(ich)),2), font=-1
  xyouts,xout,4,'act = ' + strtrim(string(msehv.vact(ich)),2), font=-1
;
; draw widget uvalue = given shot hv request 
;
  value = msehv.vreq(ich)
  widget_control, msehv.draw(ich), set_uvalue=value
endfor
shotout:

print,'end drawshot'

return
end

pro showdb, msehv

print,'begin showdb'

widget_control, msehv.showexp, get_uvalue=showexp

if showexp eq 0 then begin
  value = 'Show msep channels'
endif else begin
  value = 'Hide msep channels'
endelse

widget_control, msehv.showexp, set_value=value 

print,'end showdb'

return
end

pro drawdb, msehv

print,'begin drawdb'
widget_control, /hourglass

widget_control, msehv.showexp, get_uvalue=showexp
msep = ['msep','ms2p','ms3p']

for ich=0,msehv.nch-1 do begin
  value = 'mse'+strtrim(string(msehv.chan(ich)),2)	$
	+ '  ' +strtrim(string(msehv.vdb(ich)),2)
  
  if showexp ne 0 then begin
;
; msehv.msep = 0 for msep, 1 for ms2p
; msehv.msch = channel, 0-up in db array
;
; number channels 1-up on the label, to match hvs
;
    value = msep(msehv.msep(ich)) 			$
	+ ' '  +strtrim(string(msehv.msch(ich)+1),2)	$
	+ ' = '+value
  endif
  widget_control, msehv.label(ich), set_value=value
;
; label uvalue = current hv setting
;
  value = msehv.vdb(ich)
  widget_control, msehv.label(ich), set_uvalue=value
endfor

print,'end drawdb'

return
end

pro discard, msehv

print,'begin discard'
widget_control, /hourglass

for ich=0,msehv.nch-1 do begin
  value = msehv.vdb(ich)
  widget_control, msehv.slider(ich), set_value=value
endfor

msehv.changes = 0
widget_control, msehv.discard, sensitive=msehv.changes
widget_control, msehv.save,    sensitive=msehv.changes

print,'end discard'

return
end

pro compare, msehv

print,'begin compare'
widget_control, /hourglass

msehv.changes = 0

for ich=0,msehv.nch-1 do begin
  widget_control, msehv.slider(ich), get_value=vdb
  if vdb ne msehv.vdb(ich) then begin
    msehv.changes = 1
  endif
endfor

widget_control, msehv.discard, sensitive=msehv.changes
widget_control, msehv.save,    sensitive=msehv.changes

print,'end compare'

return
end

function msehv_dbread,expname

print,'begin msehv_dbread'

image = '/d/diags/mse/lib/msehv.sl'
entry = 'msehv_dbread_idl'

dbdata = intarr(32)

for try = 1,3 do begin
    err = call_external(image,entry,expname,dbdata,value=[1b,0b])
    if err eq 0 then goto,tryout
    help, err, try
endfor

tryout:
print,'end msehv_dbread'

return, dbdata
end

pro msehv_dbwrite,expname,dbdata

print,'begin msehv_dbwrite'

image = '/d/diags/mse/lib/msehv.sl'
entry = 'msehv_dbwrite_idl'

for try=1,3 do begin
    err = call_external(image,entry,expname,dbdata,value=[1b,0b])
    if err eq 0 then goto,tryout
    help, err, try
endfor

tryout:
print,'end msehv_dbwrite'

return
end

pro readdb, msehv

print,'begin readdb'
widget_control, /hourglass

dbdata = msehv_dbread('msevolts')
dbdata2 = msehv_dbread('msevolts2')
dbdata3 = msehv_dbread('msevolts3')

dbdatax = [dbdata,dbdata2,dbdata3]
dbdatax = fix(dbdatax,0,32,3)

for ich=0,msehv.nch-1 do begin
  msehv.vdb(ich) = dbdatax(msehv.msch(ich),msehv.msep(ich))
endfor

print,'end readdb'

return
end

pro savedb, msehv

print,'begin savedb'
widget_control, /hourglass

for ich=0,msehv.nch-1 do begin
  widget_control, msehv.slider(ich), get_value=vdb
  msehv.vdb(ich) = vdb
endfor

; reread current db values

dbdata = msehv_dbread('msevolts')
dbdata2 = msehv_dbread('msevolts2')
dbdata3 = msehv_dbread('msevolts3')

dbdatax = [dbdata,dbdata2,dbdata3]
dbdatax = fix(dbdatax,0,32,3)

; overwrite with slider values

for ich=0,msehv.nch-1 do begin
  widget_control, msehv.slider(ich), get_value=vdb
  dbdatax(msehv.msch(ich),msehv.msep(ich)) = vdb
endfor

dbdata  = dbdatax(*,0)
dbdata2 = dbdatax(*,1)
dbdata3 = dbdatax(*,2)

msehv_dbwrite,'msevolts',dbdata
msehv_dbwrite,'msevolts2',dbdata2
msehv_dbwrite,'msevolts3',dbdata3

print,'end savedb'

return
end

pro msehv, shot, xsize=xsize, ysize=ysize, ncols=ncols, 	$
		reverse=reverse, showexp=showexp

if n_elements(xsize)	ne 1 then xsize = 800
if n_elements(ysize)	ne 1 then ysize = 600
if n_elements(ncols)	ne 1 then ncols = 3
if n_elements(reverse)	ne 1 then reverse = 0
if n_elements(showexp)	ne 1 then showexp = 0
;
; earliest shot with present configuration
;
if n_params() 	        lt 1 then shot = 0L	; current shot


; nch = 39L					; channels 
; chan = 2L + lindgen(nch)			; mse  channel, 2-40
; msch = [1L + lindgen(19),lindgen(20)]		; msep channel, 0-up
; msep = [replicate(0L,19),replicate(1L,20)]	; 0=msep, 1=ms2p

nch = 45L					; channels
chan = 1L + lindgen(nch)			; mse  channel, 1-45
msch = [lindgen(20),lindgen(16),lindgen(9)]	; msep channel, 0-up
msep = [replicate(0L,20),replicate(1L,16),	$
			 replicate(2L,9)]	; 0=msevolts, 1=msevolts2, etc.

vreq = replicate(0L,nch)			; shot hv - requested
vact = replicate(0L,nch)			; shot hv - actual
vdb  = replicate(0L,nch)			; db   hv

msehv = {			$
	xsize:long(xsize),	$
	ysize:long(ysize),	$
	ncols:long(ncols),	$
	nch:nch,		$
	changes:0L,		$
	top:0L,			$
	mbar:0L,		$
	file:0L,		$
	edit:0L,		$
	view:0L,		$
	database:0L,		$
	settings:0L,		$
	putfile:0L,		$
	getfile:0L,		$
	quit:0L,		$
	redraw:0L,		$
	reverse:0L,		$
	showexp:0L,		$
	reread:0L,		$
	discard:0L,		$
	save:0L,		$
	dbbase:0L,		$
	rbase:0L,		$
	xbase:0L,		$
	shot:0L,		$
	tmin:0L,		$
	tmax:0L,		$
	chan:chan,		$
	msch:msch,		$
	msep:msep,		$
	vreq:vreq,		$
	vact:vact,		$
	vdb:vdb,		$
	vrq:lonarr(nch),	$
	col:lonarr(nch),	$
	cbase:lonarr(nch),	$
	draw:lonarr(nch),	$
	slider:lonarr(nch),	$
	label:lonarr(nch),	$
	done:0L			}
;
; top widget
;
msehv.top	= widget_base	(title='msehv',	/col,		$
		xpad=0, ypad=0, space=0, mbar=mbar)

msehv.mbar	= mbar
msehv.file	= widget_button	(mbar,	/menu,	value='File')
msehv.edit	= widget_button	(mbar,	/menu,	value='Edit')
msehv.view	= widget_button	(mbar,	/menu,	value='View')
msehv.database	= widget_button	(mbar,	/menu,	value='Database')

msehv.settings	= widget_button	(msehv.file,	value='Shot Settings')
msehv.putfile	= widget_button	(msehv.file,	value='Save to File')
msehv.getfile	= widget_button	(msehv.file,	value='Read from File')
msehv.quit	= widget_button	(msehv.file,	value='Quit')

msehv.redraw	= widget_button (msehv.view,	value='Redraw')
msehv.reverse	= widget_button (msehv.view,	value='Reverse b/w')
msehv.showexp	= widget_button (msehv.view,	value='Show msep channels')

msehv.reread	= widget_button	(msehv.database,value='Reread Database')
msehv.discard	= widget_button	(msehv.database,value='Discard Changes')
msehv.save	= widget_button	(msehv.database,value='Save Changes')

scrollbase	= widget_base	(msehv.top,	/col,	/scroll,	$
		 x_scroll_size=xsize, y_scroll_size=ysize,		$
		xpad=0, ypad=0, space=0)

msehv.rbase	= widget_base	(scrollbase,	/row,		$
		xpad=0, ypad=0, space=0)

msehv.xbase	= widget_base	(msehv.top,	/row)
msehv.shot	= cw_field	(msehv.xbase,	/long,		$
		title = 'Shot',	xsize = 6,	value = shot,	$
		/return_events)

msehv.tmin	= cw_field	(msehv.xbase,	/float,		$
		title = 'Tmin',	xsize = 4,	value = 0.,	$
		/return_events)

msehv.tmax	= cw_field	(msehv.xbase,	/float,		$
		title = 'Tmax',	xsize = 4,	value = 6.,	$
		/return_events)

populate,msehv

widget_control, msehv.top, /realize
set_plot, 'x'

if reverse ne 0 then begin
	!p.background = 255
	!p.color = 0
endif

widget_control, msehv.reverse, set_uvalue=reverse
widget_control, msehv.showexp, set_uvalue=showexp

showdb, msehv
readdb, msehv
drawdb, msehv
discard, msehv
drawshot, msehv

widget_control, msehv.top, set_uvalue=msehv
xmanager, 'msehv', msehv.top	

return
end
