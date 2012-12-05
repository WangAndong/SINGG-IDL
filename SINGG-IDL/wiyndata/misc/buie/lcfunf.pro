;+
; NAME: 
;  lcfunf
; PURPOSE: 
;  Compute a lightcurve function (Fourier series plus phase coefficient).
; DESCRIPTION:
;  This is to be used in cases where you already know the fundamental period.
;  The input independent variable is assumed to be reduced to phase of a
;  fundamental period already.  The integer part of the number is not used
;  by the function.
; CATEGORY:
;  Mathematical
; CALLING SEQUENCE:
; INPUTS:
;  x - independent variable (longitude, between 0 and 360)
;  c - fourier series coefficients
;        0 - beta    -> linear phase coefficient
;        1 - a(0)    -> constant term
;        2 - a(1)    -> cos(x2pi)
;        3 - b(1)    -> sin(x2pi)
;        4 - a(2)    -> cos(2x2pi)
;        5 - b(2)    -> sin(2x2pi)
;     and so on for as many elements as in c
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value - Evaluated function value
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  94/10/10, Written by Marc W. Buie, Lowell Observatory
;  95/08/01, cloned from LCFUN by MWB
;-
function lcfunf,x,m,PHANG=in_phang
   common lc_com,phang

   if n_elements(in_phang) eq 1 or $
      n_elements(x) eq n_elements(in_phang) then phang = in_phang

   f = fltarr(n_elements(x),m)

   f[*,0] = phang
   f[*,1] = 1.0
   j = 0
   i = 2
   while (i lt m) do begin
      f[*,i] = cos(2.0*!pi*(j+1)*x/360.0)
      i = i + 2
      j = j + 1
   endwhile
   j = 0
   i = 3
   while (i lt m) do begin
      f[*,i] = sin(2.0*!pi*(j+1)*x/360.0)
      i = i + 2
      j = j + 1
   endwhile

   return,f
end
