FUNCTION ew_imf,singgEW,KROUPA=krB,SCALO=scB,SILENT=silent,REVERSE=reverse

;  IF NOT KEYWORD_SET(singgEW) THEN singgEW = 11.7

  B = [0.00045,0.034,0.12,0.22,0.50,1.0,2.0,3.0,5.0,10.0]

  krEW=[0.78,3.41,10.6,16.8,29.0,42.5,58.1,68.4,82.4,106]
  salEW=[0.08,5.24,16.2,25.6,44.0,63.8,86.0,100,119,151]
  scEW=[0.02,0.91,2.95,4.90,9.48,15.3,23.6,30.3,39.6,57.2]

  n_ews = N_ELEMENTS(singgEW)

  krB = FLTARR(n_ews)
  salB = FLTARR(n_ews)
  scB = FLTARR(n_ews)

  FOR ii = 0,n_ews-1 DO BEGIN
    IF NOT KEYWORD_SET(reverse) THEN BEGIN
      krB[ii] = spline(krEW,B,singgEW[ii])
      salB[ii] = spline(salEW,B,singgEW[ii])
      scB[ii] = spline(scEW,B,singgEW[ii])
    ENDIF ELSE BEGIN
; in this case, singgEW is actually a birthrate, and we're trying to
; find an EW.  I'm not going to rename the variables.
      krB[ii] = spline(B,krEW,singgEW[ii])
      salB[ii] = spline(B,salEW,singgEW[ii])
      scB[ii] = spline(B,scEW,singgEW[ii])
    ENDELSE
  ENDFOR

  IF NOT KEYWORD_SET(silent) THEN PRINT,"kroupa ",krB," salpeter ",salB," scalo ",scB

  RETURN,salB

END
