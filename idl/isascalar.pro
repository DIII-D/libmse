FUNCTION isascalar,ascalar
; returns 1 if a scalar, 0 if not.
s=size(ascalar)
IF s(0) EQ 0 THEN RETURN,1 ELSE RETURN,0
END
