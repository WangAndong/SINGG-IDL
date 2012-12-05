;+
; NAME:
;  haparse
; PURPOSE:
;  Convert Hour Angle (HA) string to radians.
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  ha=haparse(str)
; INPUTS:
;  str - String (or array) to parse as an hour angle
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  return value is scalar or vector value of HA in radians
;    Note - W,w,+ prefixes are all west hour angles (positive)
;           E,e,- prefixes are all east hour angles (negative)
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
;  2002/03/14, Written by Marc W. Buie, Lowell Observatory
;-
function haparse,str

   ha=dblarr(n_elements(str))

   for i=0,n_elements(str)-1 do begin
      wstr = strtrim(str[i],2)

      token = strmid(wstr,0,1)
      if token eq 'W' or token eq 'w' or token eq '+' then begin
         sign = 1.0
         wstr = strmid(wstr,1,99)
      endif else if token eq 'E' or token eq 'e' or token eq '-' then begin
         sign = -1.0
         wstr = strmid(wstr,1,99)
      endif else begin
         sign = 1.0
      endelse
         ha = sign * raparse(wstr)
   endfor

   if n_elements(ha) eq 1 then return,ha[0] else return,ha

end
