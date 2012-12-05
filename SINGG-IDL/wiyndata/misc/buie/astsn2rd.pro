;+
; NAME:
;  astsn2rd
; PURPOSE:
;  Astrometry conversion from ($\xi$,$\eta$) to ($\alpha$,$\delta$)
; DESCRIPTION:
;  Standard coordinate conversion (see Smart, p283)
;
; CATEGORY:
;  Astrometry
;
; CALLING SEQUENCE:
;  astsn2rd,xi,eta,racen,deccen,ra,dec
;
; INPUTS:
;  xi     - Coordinate in tangent plane (radians).
;  eta    - Coordinate in tangent plane (radians).
;  racen  - Right ascension of tangent point between plane and celestial sphere.
;  deccen - Declination of tangent point between plane and celestial sphere.
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
PRO astsn2rd,xi,eta,racen,deccen,ra,dec
   cosdeccen = cos(deccen)
   sindeccen = sin(deccen)
   ra  = atan(xi/(cosdeccen-eta*sindeccen)) + racen
   dec = atan((sin(ra-racen)*(sindeccen+eta*cosdeccen))/xi)
END
