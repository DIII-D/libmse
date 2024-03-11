;************************************************************************
;
;  beam_geom.pro
;
;  created:  10-4-96   B. Rice
;
;  modified:
;
;
;  calling format:  
;	beam_geom,R,L,source,x,y
;  input:  
;	R = major radius (single or multi-point)
;	L = distance along beam from beam cross-over
;	    (use R or L but not both)
;	source = beam source (30lt or 30rt)
;  output:
;	x = x position of beam at R (if R >0) or L (if L > 0)
;	y = y position of beam at  "   "
;
;******************************************************************
  
pro beam_geom,R,L,source,x,y

xcp=1.2926		;x position of right-left beam crossover opint
ycp=2.3935		;y "     "

CASE 1 of

(source eq '30lt'):BEGIN

  yb0=1.4039			;y-intercept of beam
  theta_b=37.437*!pi/180.	;angle counterclockwise from x axis

END

(source eq '30rt'):BEGIN

  yb0=1.050			;y-intercept of beam
  theta_b=46.104*!pi/180.	;angle counterclockwise from x axis

END
ENDCASE

cb=cos(theta_b)
sb=sin(theta_b)

if max(R) gt 0 then begin
  x=yb0*cb*(-sb+sqrt(-1.*cb^2+R^2/yb0^2))
  y=sqrt(R^2-x^2)
  L=sqrt((xcp-x)^2+(ycp-y)^2)
endif else begin
  b=-1.*cb^2*2.*(xcp+(-yb0+ycp)*tan(theta_b))
  c=cb^2*(xcp^2+ycp^2+yb0^2-2.*ycp*yb0-L^2)
  print,b,c,b^2,4.*c
stop
  x=(-b-sqrt(b^2-4.*c))/2.
  y=tan(theta_b)*x+yb0
  R=sqrt(x^2+y^2) 
endelse

print,'r,l,x,y'
print,r,l,x,y

return
end