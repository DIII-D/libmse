FUNCTION isastring,astring
; returns 1 if a scaler string, 0 if not.
s=size(astring)
IF s(0) EQ 0 AND s(1) EQ 7  THEN RETURN,1 ELSE RETURN,0
END
