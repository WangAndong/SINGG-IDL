;+
; NAME:
;  astterms
; PURPOSE:
;  Evaulate the independent vectors to match an astrometric polynomial function.
; DESCRIPTION:
;
;  This function is a support routine to ASTROM and handles part of the
;  transformation between pixel (x,y) coordinates and the tangent plane
;  coordinates (xi,eta).  The transformation from (ra,dec) to (xi,eta)
;  is not handled in this routine.  The premise is that the transformation
;  from the tangent plane to pixel coordinates can be done with a polynominal.
;  I have implemented all of the common terms found in typical astrometric
;  solutions.  In practice, the high order terms are probably not needed
;  except for very large fields or for highly distorted fields caused by
;  excessive optics.  Most CCD fields can be accurately modeled using just
;  the linear terms.
;
;  This function does NOT actually evaluate the transformation.  Instead,
;  the indepedent values for the polynominal are computed.  The result is
;  an array with (nterms,nvals) elements where nterms is the number of
;  non-zero terms and nvals is the number of input x and y values (which must
;  be of the same length.  The table below lists the contents of the i^th
;  column in the output array.
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  val=astterms(x,y,terms)
; INPUTS:
;  x     - X - coordinate (scalar or vector)
;  y     - Y - coordinate (scalar or vector)
;  terms - Which terms are to be built
;           This must be a 10 element vector, a 1 means use the term,
;                   0 means don't use it.
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
FUNCTION astterms,x,y,terms

   nterms=fix(total(terms))
   nvals =n_elements(x)

   ind = dblarr(nterms,nvals,/nozero)

   tpos=0

   IF terms[0] eq 1 THEN BEGIN
      ind[tpos,*] = 1.0
      tpos=tpos+1
   ENDIF

   IF terms[1] eq 1 THEN BEGIN
      ind[tpos,*] = x
      tpos=tpos+1
   ENDIF

   IF terms[2] eq 1 THEN BEGIN
      ind[tpos,*] = y
      tpos=tpos+1
   ENDIF

   IF terms[3] eq 1 THEN BEGIN
      ind[tpos,*] = sqrt(x^2+y^2)
      tpos=tpos+1
   ENDIF

   IF terms[4] eq 1 THEN BEGIN
      ind[tpos,*] = x^2
      tpos=tpos+1
   ENDIF

   IF terms[5] eq 1 THEN BEGIN
      ind[tpos,*] = y^2
      tpos=tpos+1
   ENDIF

   IF terms[6] eq 1 THEN BEGIN
      ind[tpos,*] = x^3
      tpos=tpos+1
   ENDIF

   IF terms[7] eq 1 THEN BEGIN
      ind[tpos,*] = y^3
      tpos=tpos+1
   ENDIF

   IF terms[8] eq 1 THEN BEGIN
      ind[tpos,*] = x*y^2
      tpos=tpos+1
   ENDIF

   IF terms[9] eq 1 THEN BEGIN
      ind[tpos,*] = x^2*y
      tpos=tpos+1
   ENDIF

   IF nvals eq 1 THEN BEGIN
      return,ind[*]
   ENDIF ELSE BEGIN
      return,ind
   ENDELSE

END
