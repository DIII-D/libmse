; Find mse shared image, and return it.

function mse_shared_image

  image = ''
  ostype = getenv('OSTYPE')

  help,names='mse_shared_image',/source,/functions,output=s

  if(strmid(ostype,0,5) eq 'linux') then begin
    if(!version.arch eq 'x86_64')then begin
       dir=getenv('MSE_DIR')
       j = strsplit(s[-1],/extract)
       k = file_dirname(j[-1])
       m = strsplit(k,'/',/extract)
       ver = m[-1]
       image = dir+'/lib/'+ver+'/libmseidl.so'
    end else begin
       image = '/d/diags/mse/newlib/libmseidllinuxlf95.so
    endelse
  end else begin
    if(!version.arch eq 'x86_64')then begin
       dir=getenv('MSE_DIR')
       j = strsplit(s[-1],/extract)
       k = file_dirname(j[-1])
       m = strsplit(k,'/',/extract)
       ver = m[-1]
       image = dir+'/lib/'+ver+'/libmseidl.so'
    endif else begin
       image = '/d/diags/mse/newlib/libmseidl.sl
    endelse
  endelse

  return, image

end
