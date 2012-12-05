;+
; NAME:
;  naifname
; PURPOSE:
;  Convert an ephem standard name to a common name (NAIF name scheme)
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  propername = naifname(standardname)
; INPUTS:
;  standardname = string, standard name code (see EPHEM)
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  return is the proper string name, or, the input if standard name isn't
;    recognized.
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
;  97/02/12 - Written by Marc W. Buie, Lowell Observatory
;-
FUNCTION naifname,stdname
   
   if badpar(stdname,7,0,caller='NAIFNAME: (stdname) ') then return,''

   name=strlowcase(stdname)

   code = strmid(name,0,1)

   IF code eq 'p' THEN BEGIN
      CASE strmid(name,1,99) OF
         '1': BEGIN
            name = 'Mercury'
            END
         '2': BEGIN
            name = 'Venus'
            END
         '3': BEGIN
            name = 'Earth'
            END
         '4': BEGIN
            name = 'Mars'
            END
         '5': BEGIN
            name = 'Jupiter'
            END
         '6': BEGIN
            name = 'Saturn'
            END
         '7': BEGIN
            name = 'Uranus'
            END
         '8': BEGIN
            name = 'Neptune'
            END
         '801': BEGIN
            name = 'Triton'
            END
         '9': BEGIN
            name = 'Pluto'
            END
         '901': BEGIN
            name = 'Charon'
            END
         ELSE: BEGIN
            name = stdname
            ENDELSE
      ENDCASE
   ENDIF ELSE BEGIN
      name=stdname
   ENDELSE

   RETURN, name
END
