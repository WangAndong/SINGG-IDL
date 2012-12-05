;+
; NAME:
;  colcob
; PURPOSE:
;  Compute the offset from the center-of-light to center-of-body.
; DESCRIPTION:
;
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
;  colcob,map,npix,pole,selat,selon,sslat,sslon,dx,dy
; INPUTS:
;  map - Array containing a full map of surface
;  npix - Diameter of sphere in pixels for computation.
;  pole   - Position angle of pole, east from north (degrees)
;  selat  - Sub-earth latitude (degrees)
;  selon  - Sub-earth longitude (degrees)
;  sslat  - Sub-solar latitude (degrees)  Only used for Hapke functions. 
;  sslon  - Sub-solar longitude (degrees)  Only used for Hapke functions.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
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
;  98/02/17, Written by Marc W. Buie, Lowell Observatory
;
;-
PRO colcob,map,npix,pole,selat,selon,sslat,sslon,dx,dy, $
           NODISPLAY=nodisplay,_EXTRA=e

   scale = 1.0
   radius = float(npix)/2.0
   sz=fix(npix+2)
   render,map,radius,scale,pole,selat,selon,sslat,sslon,sz,sz,image, $
      xarr=x,yarr=y,nodisplay=nodisplay,/silent,_EXTRA=e

   dx = total(x*image)/total(image)
   dy = total(y*image)/total(image)

   if not keyword_set(nodisplay) then begin
      print,dx,dy

      setwin,0,xsize=sz,ysize=sz
      tvscl,image
   endif

END
