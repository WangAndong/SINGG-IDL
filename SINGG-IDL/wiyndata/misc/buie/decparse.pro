;+
; NAME:
;  decparse
; PURPOSE:
;  Convert Declination string to radians.
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  dec=decparse(str)
; INPUTS:
;  str - String (or array) to parse as a declination
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  return value is scalar or vector value of Dec in radians
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  1997/06/04, Written by Marc W. Buie, Lowell Observatory
;  2000/11/9, MWB, removed Str_sep call.
;-
FUNCTION decparse,str

   dec=dblarr(n_elements(str))

   FOR i=0,n_elements(str)-1 DO BEGIN
      ; Make copy of string
      wstr = strtrim(str[i],2)

      ; convert separator tokens to blank
      wstr = repchar(wstr,':',' ')
      wstr = repchar(wstr,',',' ')

      ; check for leading sign character, if '-' found, strip and save
      decsign=strmid(wstr,0,1)
      IF decsign eq '-' THEN BEGIN
         wstr=strmid(wstr,1,99)
         decsign=-1
      ENDIF ELSE BEGIN
         decsign=1
      ENDELSE

      ; split into fields by blanks
      decf=double(strsplit(wstr,/extract))
      IF n_elements(decf) eq 1 THEN BEGIN
         decf = [decf,0.,0.]
      ENDIF ELSE IF n_elements(decf) EQ 2 THEN BEGIN
         decf = [decf[*],0.]
      ENDIF
      dmstorad,decsign,decf[0],decf[1],decf[2],dec0
      dec[i] = dec0
   ENDFOR

   IF n_elements(dec) eq 1 THEN return,dec[0] ELSE return,dec

END
