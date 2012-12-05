;+
; NAME:
;  astrd2xy
; PURPOSE:
;  Astrometry conversion from ($\alpha$,$\delta$) to image (x,y)
;
; DESCRIPTION:
;  This transformation is based on a simple linear transformation with
;    rotation from the celestial sphere to linear CCD chip coordinates.
;    This is only an approximate treatment and will not work for very large
;    fields.
;
; CATEGORY:
;  Astrometry
;
; CALLING SEQUENCE:
;  astrd2xy,ra,dec,info,x,y
;
; INPUTS:
;  ra     - Right ascension (radians)
;  dec    - Declination (radians)
;  info   - Transformation information held in an anonymous structure with
;              the following tags:
;                 raref  - Right ascension of center of tangent plane.
;                 decref - Declination of center of tangent plane.
;                 pscale - Plate scale (arcsec/pixel).
;                 rang   - Rotation angle of image (radians).
;                 xflip  - -1 if image flipped in X, 1 if not.
;                 yflip  - -1 if image flipped in Y, 1 if not.
;                 xcref  - X center of image.
;                 ycref  - Y center of image.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  x      - X coordinate in image
;  y      - Y coordinate in image
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
;  97/04/05 - Written by Marc W. Buie, Lowell Observatory
;
;-
pro astrd2xy,ra,dec,info,x,y,XI=xi,ETA=eta

   astrd2sn,ra,dec,info.raref,info.decref,xi,eta
   xi  = xi*180.0d0/!dpi*3600.0d0  ; convert to arcsec
   eta = eta*180.0d0/!dpi*3600.0d0
   x = xi/info.pscale*cos(info.rang) - eta/info.pscale*sin(info.rang)
   y = xi/info.pscale*sin(info.rang) + eta/info.pscale*cos(info.rang)
   x = info.xflip*x + info.xcref
   y = info.yflip*y + info.ycref

end

