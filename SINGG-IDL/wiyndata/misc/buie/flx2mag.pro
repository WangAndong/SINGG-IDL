;+
; NAME:
;     flx2mag
; PURPOSE: (one line)
;     Convert from flux units to magnitudes with errors.
; DESCRIPTION:
; CATEGORY:
;     Photometry
; CALLING SEQUENCE:
;     flx2mag,flux,fluxerr,mag,magerr
; INPUTS:
;     flux    - Flux values for the magnitudes.
;     fluxerr - Uncertainties on the fluxes.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;     ZEROPT  - Magnitude that corresponds to a flux of 1. (default=0)
; OUTPUTS:
;     mag    - Magnitudes.
;     magerr - Uncertainties on the magnitudes.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;-
pro flx2mag,flux,fluxerr,mag,magerr, ZEROPT = zeropt

if badpar(flux,[2,3,4,5],[0,1,2,3,4,5,6,7,8],caller='FLX2MAG: (flux) ') then return
if badpar(fluxerr,[2,3,4,5],[0,1,2,3,4,5,6,7,8],caller='FLX2MAG: (fluxerr) ') then return
if badpar(zeropt,[0,2,3,4,5],0,caller='FLX2MAG: (zeropt) ',default=0.0) then return

mag = zeropt - 2.5 * alog10(flux)
magerr = fluxerr/flux*2.5*alog10(exp(1.0))

end
