;+
; NAME:
;  astinvrt
; PURPOSE:
;  Invert an astrometric solution to get x,y given ($\xi$,$\eta$).
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  astinvrt,xi,cxi,xiterms,eta,ceta,etaterms,x,y
; INPUTS:
;  xi       - Coordinate in tangent plane.
;  cxi      - Coefficients of xi astrometric fit, this must be a vector that has
;              the terms to be used, the length must be equal to total(xiterms)
;  xiterms  - Flag array, which terms to use, see ASTEVAL.PRO for description.
;  eta      - Coordinate in tangent plane.
;  ceta     - Coefficients of eta astrometric fit, this must be a vector that has
;              the terms to be used, the length must be equal to total(etaterms)
;  etaterms - Flag array, which terms to use, see ASTEVAL.PRO for description.
;  x        - Initial guess of x coordinate
;  y        - Initial guess of y coordinate
;  
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  x        - x coordinate
;  y        - y coordinate
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
;  2000/9/5, Written by Marc W. Buie, Lowell Observatory
;
;-
pro astinvrt,xi,cxi,xiterms,eta,ceta,etaterms,x,y

   cxia  = rebin(cxi,total(xiterms),n_elements(xi))
   cetaa = rebin(ceta,total(etaterms),n_elements(eta))

   tol = 1.0d-10
   iterstop = 20

   ; Work in the principle axis of each coordinate
   if abs(cxi[1]) gt abs(cxi[2]) then begin

      niter=0
      repeat begin
         ; Compute "independent" vectors
         xiind  = astterms(x,y,xiterms)
         etaind = astterms(x,y,etaterms)
         xival  = cxia*xiind
         etaval = cetaa*etaind

         ; Save old values
         oldx = x
         oldy = y

         ; Compute new x
         x = xi - xival[0,*] - xival[2,*]
         for i=3,total(xiterms)-1 do x = x - xival[i,*]
         x = x / (xival[1,*]/oldx)

         ; Compute new y
         y = eta - etaval[0,*] - etaval[1,*]
         for i=3,total(etaterms)-1 do y = y - etaval[i,*]
         y = y / (etaval[2,*]/oldy)

         err=max([abs(x-oldx),abs(y-oldy)])
         niter=niter+1

      endrep until err lt tol or niter gt iterstop

   endif else begin

      niter=0
      repeat begin
         ; Compute "independent" vectors
         xiind  = astterms(x,y,xiterms)
         etaind = astterms(x,y,etaterms)
         xival  = cxia*xiind
         etaval = cetaa*etaind

         ; Save old values
         oldx = x
         oldy = y

         ; Compute new x
         x = eta - etaval[0,*] - etaval[2,*]
         for i=3,total(etaterms)-1 do x = x - etaval[i,*]
         x = x / cetaa[1,*]

         ; Compute new y
         y = xi - xival[0,*] - xival[1,*]
         for i=3,total(xiterms)-1 do y = y - xival[i,*]
         y = y / cxia[2,*]

         err=max([abs(x-oldx),abs(y-oldy)])
         niter=niter+1

      endrep until err lt tol or niter gt iterstop

   endelse

   if niter gt iterstop then $
      print,'ASTINVRT:  Warning, maximum iteration count exceeded.'

end
