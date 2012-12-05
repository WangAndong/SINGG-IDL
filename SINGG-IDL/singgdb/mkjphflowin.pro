PRO mkjphflowin, name, ra, dec, vhel, h0=h0
   ;
   ; Make the input file (flow.in) for the program JPHFLOW (this is
   ; John Huchra's multipole attractor distance stimator).
   ;
   ; name -> object name.
   ; ra   -> right ascension (J2000) in decimal degrees.
   ; dec  -> declination (J2000) in decimal degrees.
   ; vhel -> heliocentric radial velocity in km/s 
   ; h0   -> Hubble constant (default is h0 = 70 km/s/Mpc.
   ;
   ; G. Meurer (JHU) 3/04
   invrad = 3.141592654/180.
   IF keyword_set(h0) THEN h_0 = h0 ELSE h_0 = 70.0
   filo   = 'flow.in'
   dumsig = 50.0
   dum    = 0.1
   fmt    = '(a11,f9.5,f10.5,f10.1,f5.1,f10.1,f5.1,f6.1)'
   ;
   ; get galactic coords
   glactc, ra, dec, 2000.0, glong, glat, 1, /degree
   glat  = invrad*glat
   glong = invrad*glong
   ;
   ; determine local group corrected velocity
   ;vlg   = vhel 
   vlg   = vhel - 79.0*cos(glong)*cos(glat) + 296.0*sin(glong)*cos(glat) - 36.0*sin(glat)
   ;
   ; precess coords to 1950.
   bprecess, ra, dec, ra_1950, dec_1950
   ;
   ; estimate distance, distance modulus
   d     = vlg / h_0
   k     = where(d LT 1, nk)
   IF nk GT 0 THEN d[k] = 1.0
   dm    = 25.0 + 5.0*alog10(d)
   ;
   ; write output
   openw,lu,filo,/get_lun
   n = n_elements(name)
   FOR i = 0, n-1 DO printf,lu,ljust(name[i],11),ra_1950[i]/15.0,dec_1950[i],vhel[i],$
                               dumsig,dm[i],dum,d[i],format=fmt
   free_lun,lu
END 
