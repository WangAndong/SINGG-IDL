;+
; NAME:
;  astxy2rd
; PURPOSE:
;  Astrometry conversion from image (x,y) to ($\alpha$,$\delta$)
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
;  astxy2rd,x,y,info,ra,dec
;
; INPUTS:
;  x      - X coordinate in image
;  y      - Y coordinate in image
;  info   - Transformation information held in an anonymous structure with
;              the following tags:
;                 racen  - Right ascension of center of image.
;                 deccen - Declination of center of image.
;                 pscale - Plate scale (arcsec/pixel).
;                 rang   - Rotation angle of image (radians).
;                 xflip  - -1 if image flipped in X, 1 if not.
;                 yflip  - -1 if image flipped in Y, 1 if not.
;                 xc     - X center of image.
;                 yc     - Y center of image.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  ra     - Right ascension (radians)
;  dec    - Declination (radians)
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
pro astxy2rd,x,y,info,ra,dec
   sx = float(info.xflip)/info.pscale
   sy = float(info.yflip)/info.pscale
   x0= (x-info.xcref)*cos(info.rang) - (y-info.ycref)*sin(info.rang)
   y0= (x-info.xcref)*sin(info.rang) + (y-info.ycref)*cos(info.rang)
   ra = x0/!radeg/3600.0/cos(info.decref)/sx + info.raref
   dec = y0/!radeg/3600.0/sy + info.decref
end

