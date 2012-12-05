;+
; NAME:
;  lcltoeq
; PURPOSE:
;  Convert from local to equatorial coordinates.
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  lcltoeq,alt,az,lat,ha,dec
; INPUTS:
;  alt - altitude above horizon (radians)
;  az  - azimuth (radians), 180 is due south, 90 is due east
;  lat - latitude of observatory (radians)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  ha  - hour angle (radians)
;  dec - declination (radians)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2001/11/22, ported
;    from a C library routine.
;-
pro lcltoeq,alt,az,lat,ha,dec

   calt = cos(alt)
   salt = sin(alt)
   caz  = cos(az)
   saz  = sin(az)
   clat = cos(lat)
   slat = sin(lat)

   arg = slat*salt + clat*calt*caz

   z = where(arg ge -1.0 and arg le 1.0, count)
   dec = arg*0.0
   if count ne 0 then dec[z] = asin(arg)

   ha = atan(-saz,salt*clat/calt - caz*clat)

end
