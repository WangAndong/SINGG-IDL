pro grm_sky, image, skylev, eskylev, circlerad=circlerad, mask=mask, silent=silent, poisson=poisson
  ;
  ;  Determine the sky level in a manner that is robust to whether 
  ;  the data is at the poisson limit or not.  
  ;
  ;  Basically this code is a wrapper.  
  ;  * it calls mysky.pro (from Dan Hanish) if poisson is not set
  ;  * it calls poisson_sky.pro if poisson is set
  ;
  ;  G. Meurer 02/2011  (ICRAR/UWA)
  ;
  if keyword_set(poisson) then begin 
     poisson_sky, image, skylev, mask=mask, silent=silent
  endif else begin 
     mysky, image, skylev, eskylev, circlerad=circlerad, mask=mask, silent=silent
  endelse 
end 
