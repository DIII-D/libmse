
; Find mse shared image, and return it.

function mse_shared_image
;@whereami

image = ''

;if using_unix then begin
  f = findfile('/d/diags/mse/source/libmse.sl',count=nf)   ;use for testing
;  f = findfile('/d/diags/mse/lib/libmse.sl',count=nf)
  if nf gt 0 then image=f(0)
;endif

return,image
end