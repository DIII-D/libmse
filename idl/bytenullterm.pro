function bytenullterm,astring,maxlen
; Given a string, return an equivalent null-termintated byte array.
;  If maxlen is supplied then a byte array of maxlen is returned, with
;  either the byte array padded with 0's, or the string truncated and
;  null-terminated.
IF NOT isastring(astring) THEN thestring=' ' ELSE thestring=astring+' '
slen=STRLEN(thestring)
b=BYTE(astring+' ')
b(strlen(astring))=0B
IF NOT undefined(maxlen) THEN BEGIN
  IF maxlen GT slen THEN b=[b,BYTARR(maxlen-slen)] ELSE $
  IF maxlen LE 1 THEN b=[0B] ELSE $
  IF maxlen LT slen THEN b=[b(0:maxlen-2),0B]
ENDIF
return,b
end
