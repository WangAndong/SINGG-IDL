;+
; NAME: 
;  disphase
; PURPOSE: 
;  Apply distance and phase angle correction to observed magnitudes.
; DESCRIPTION:
;  Apply IAU standard asteroidal-law corrections to observed magnitudes
;     given the distance, phase angle, and the G coefficient.  Magnitudes
;     are corrected to 1 AU from Sun and Earth and to 0 degrees phase angle.
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  disphase,mag,r,d,phang,g,hmag
; INPUTS:
;     mag   - Observed magnitude.
;     r     - Sun-object distance in AU.
;     d     - Earth-object distance in AU.
;     phang - Phase angle of observation in degrees.
;     g     - IAU standard G value (phase angle coefficient).
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;     hmag  - Magnitude corrected for distance and phase angle.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;-
pro disphase,mag,r,d,phang,g,hmag

;   phi1 = exp( -3.33 * ( tan(abs(phang)/!radeg/2.0)^0.63 ) )
;   phi2 = exp( -1.87 * ( tan(abs(phang)/!radeg/2.0)^1.22 ) )

   phangr = abs(phang)/!radeg
   sphang = sin(phangr)
   tphang2= tan( phangr * 0.5 )

   w = exp( -90.56 * tphang2 * tphang2 )
   t = sphang / (0.119 + 1.341*sphang - 0.754*sphang*sphang )
   phi1s = 1.0 - 0.986 * t
   phi2s = 1.0 - 0.238 * t
   phi1l = exp( -3.332*tphang2^0.631 )
   phi2l = exp( -1.862*tphang2^1.218 )

   phi1 = w*phi1s + (1.0-w)*phi1l
   phi2 = w*phi2s + (1.0-w)*phi2l

   hmag = mag - 5.0*alog10(r*d) + 2.5*alog10( (1-g)*phi1 + g*phi2 )

end

