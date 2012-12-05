PRO grism_taperspec, flux, eflx, lam, lrange, ntaper, fluxo, eflxo, power=power
   ;
   ; taper spectra to 0.0 at edges beyond lrange
   ; flux   -> spectrum in flux units
   ; eflx   -> error spectrum
   ; lam    -> wavelength of each pixel
   ; lrange -> wavelength range of 
   ; ntaper -> number of pixels in taper
   ; fluxo  <- tapered spectrum
   ; eflxo  <- tapered error spectrum
   ; power  -> if passed, the taper is raised to this 
   ;           power before applying to the spectrum.  
   ;           if not set or <= 1.0 then power is set to 1.0
   ;
   ; G. Meurer 01/2007
   ;
   ;
   ; setup default reurned values: spectrum with no tapering
   fluxo  = flux
   eflxo  = eflx
   ns     = n_elements(flux)
   ;
   ; get power to apply
   pow    = 1.0
   IF keyword_set(power) THEN pow = max([power, 1.0])
   ;
   kk     = where(lam GE min(lrange) AND lam LE max(lrange), nkk)
   IF nkk LE 0 THEN BEGIN 
     print, 'GRISM_TAPERSPEC: WARNING no pixels within lambda range. Tapered spec = input spec.' 
     return
   ENDIF 
   medf   = median(flux[kk])
   ;
   k1  = max([min(kk) - ntaper,0])
   k2  = min(kk) - 1
   k3  = max(kk) + 1
   k4  = min([max(kk) + ntaper,ns-1])
   IF NOT (k1 LT k2 AND k2 LT k3 AND k3 LT k4) THEN BEGIN 
      print, 'GRISM_TAPERSPEC: WARNING spectrum too short to taper. Tapered spec = input spec.' 
      return
   ENDIF 
   ;
   ; make (linear) taper function which will be multiplied by spectrum
   taper        = make_array(ns, /float, value=1.0)
   xx           = findgen(ns)
   taper[k1:k2] = 1.0 - (float(k2) - xx[k1:k2])/float(ntaper)
   taper[k3:k4] = 1.0 - (xx[k3:k4] - float(k3))/float(ntaper)
   IF k1-1 GE 0    THEN taper[0:k1-1]    = 0.0
   IF k4+1 LE ns-1 THEN taper[k4+1:ns-1] = 0.0
   taper        = taper^pow
   taperc       = 1.0 - taper
   ;
   fluxo        = flux*taper + taperc*medf
   eflxo        = eflxo*taper
END 
