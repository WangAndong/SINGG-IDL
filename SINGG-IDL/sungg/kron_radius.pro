pro kron_radius, prof, errprof, rad, magzpt, skysigbx, kron_radius, err_kr, kron_mag, err_mag
  ;
  ; Calculates 2D Kron radii and magnitudes (like Source Extractor).
  ; 
  ; prof        -> surface brightness profile sigma(r)
  ; errprof     -> error in above
  ; rad         -> corresponding radii at which sigma(r) is sampled (same units as prof)
  ; magzpt      -> magnitude zero point
  ; skysigbx    -> Sky background. R_max = where profile < k*skysigbx 
  ; kron_radius <- the Kron radius $r_k(Rmax) = \frac{ \int_0^Rmax I(r) r^2 dr} {\int_0^Rmax I(r) r dr}
  ; err_kr      <- error in above
  ; kron_mag    <- the flux enclosed by 2.5 r_k where 2.5 r_k is the Kron aperture. Cuts off at 
  ;                maximum radius if the surface brightness profile doesn't go that far out.
  ; err_mag     <- error in above
  ;
  ; S. Andrews (ICRAR/UWA) - 01/2013
  ;
  
  inner_rad = [0.0d, rad]
  
  ; calculate rmax, then the Kron radius
  rmax = min(where(prof lt 1.0d*skysigbx and prof ne 0 and finite(prof), count))
  if count lt 1 then rmax = n_elements(rad)-1
  temp    = (rad[0:rmax]^2 - inner_rad[0:rmax]^2) * rad[0:rmax] * prof[0:rmax]
  temperr = (rad[0:rmax]^2 - inner_rad[0:rmax]^2) * rad[0:rmax] * errprof[0:rmax]
  x = total(temp * rad[0:rmax], /nan)
  dx = sqrt(total( (temperr*rad[0:rmax])^2 , /nan))
  y = total(temp, /nan)
  dy = sqrt(total(temperr^2, /nan))
  kron_radius = x/y
  err_kr = (x/y)*sqrt( (dx/x)^2 + (dy/y)^2 )
  
  ; now integrate I(r) out to 2.5 r_k, or as far as we can
  aperture = min(where(rad gt 2.5d*kron_radius, count), /nan)
  if count lt 1 then aperture = n_elements(rad)-1
  temp = !pi * (rad[0:aperture]^2 - inner_rad[0:aperture]^2)
  kron_flux = total(temp * prof[0:aperture], /nan)
  err_flux = sqrt( total( (temp*errprof[0:aperture])^2 , /nan ))
  kron_mag = flux2mag(kron_flux, magzpt)
  err_mag = alog10(1 + err_flux/kron_flux)
end