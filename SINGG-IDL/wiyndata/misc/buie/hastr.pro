;+
; NAME:
;  hastr
; PURPOSE:
;  Convert an Hour Angle to a string.
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  hastr,ha,places,str
; INPUTS:
;  ha     - scalar or vector, hour angle in radians
;  places - output format and precision (see rastr).
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  str    - formatted string
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2001/10/09
;-
pro hastr,ha,places,str
   ha_sign = replicate('W',n_elements(ha))
   z=where(ha lt 0.0,count)
   if count ne 0 then ha_sign[z] = 'E'
   rastr,abs(ha),places,str
   str = ha_sign+str
   if n_elements(str) eq 1 then str=str[0]
end
