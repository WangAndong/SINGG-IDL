;+
; NAME:
;  windstr
; PURPOSE:   (one line only)
;  Convert a wind direction angle to a string name.
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  wname = windstr(wdir)
; INPUTS:
;  wdir - Wind direction in degrees, scalar or vector
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value is the name of the wind direction (string)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2005/03/26
;-
function windstr,wdir

   self='WINDSTR: '
   if badpar(wdir,[2,3,4,5],[0,1],caller=self+'(wdir) ') then return,''

   direction=[ $
      "N","NNE","NE","ENE", $
      "E","ESE","SE","SSE", $
      "S","SSW","SW","WSW", $
      "W","WNW","NW","NNW"  $
      ]

   z = (wdir + 11.25) mod 360.0
   zz = where(z lt 0.,count)
   if count ne 0 then z[zz] += 360.0
   z = fix(z/22.5)

   return,direction[z]

end
