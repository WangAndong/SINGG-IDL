;+
; NAME:
;  raparse
; PURPOSE:
;  Convert Right Ascension (RA) string to radians.
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  ra=raparse(str)
; INPUTS:
;  str - String (or array) to parse as a right ascension
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  return value is scalar or vector value of RA in radians
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
function raparse,str

   ra=dblarr(n_elements(str))

   for i=0L,n_elements(str)-1 do begin
      ; Make copy of string
      wstr = str[i]

      ; convert separator tokens to blank
      wstr = repchar(wstr,':',' ')
      wstr = repchar(wstr,',',' ')

      ; split into fields by blanks
      raf=double(strsplit(wstr,/extract))
      if n_elements(raf) eq 1 then begin
         raf = [raf,0.,0.]
      endif else if n_elements(raf) eq 2 then begin
         raf = [raf[*],0.]
      endif
      hmstorad,raf[0],raf[1],raf[2],ra0
      ra[i] = ra0
   endfor

   if n_elements(ra) eq 1 then return,ra[0] else return,ra

end
