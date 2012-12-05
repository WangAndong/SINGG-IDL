;+
; NAME:
;  asteval
; PURPOSE:
;  Evaulate an astrometric polynomial function.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  val=asteval,ind,coeff,terms
; INPUTS:
;  x     - X - coordinate (scalar or vector)
;  y     - Y - coordinate (scalar or vector)
;  coeff - Coefficients of the transformation, this must be a vector that has
;              the terms to be used, the length must be equal to total(terms)
;  terms - Which terms are to be used
;           This must be a 10 element vector, a 1 means use the term, 0 means don't
;              0 - const (always use this)
;              1 - x     (always use this)
;              2 - y     (always use this)
;              3 - r
;              4 - x^2
;              5 - y^2
;              6 - x^3
;              7 - y^3
;              8 - xy^2
;              9 - yx^2
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  return value - Dependent value(s), if x,y was 1-d then this will be scalar.
;
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
;  97/06/17, Written by Marc W. Buie, Lowell Observatory
;
;-
FUNCTION asteval,x,y,coeff,terms

   ind = astterms(x,y,terms)
   sz=size(ind)
   IF sz[0] eq 1 THEN BEGIN
      val = total(ind*coeff,/double)
   ENDIF ELSE BEGIN
      val = ind##coeff
   ENDELSE

   return,val[*]

END
