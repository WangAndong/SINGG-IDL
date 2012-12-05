PRO check_open_units
; Runs through the unit numbers, and says which file units are being
; used, and by what files.  Generally speaking, GET_LUN will range
; from 100 to 127, in order.

  FOR ii = 0,127 DO BEGIN
    str = fstat(ii)
    IF str.open THEN BEGIN
      print,str.unit," ",str.name
    ENDIF
  ENDFOR

  RETURN

END
