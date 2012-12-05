PRO mk_fly99_matchin
   ; 
   ; Make input catalog of Fernandez-Soto et al (1999) 
   ; for matching with our detection Image.
   ;
   ; G. Meurer 08/2004
   ;
   ; setup
   detim   = 'detectionImage.fits'                           ; detection image
   ddir    = '/home/meurer/ACS/Grism/HDFN_jun04/Apsis/Work/' ; where detim is
   wd      = ddir + 'Match'                                  ; where to put results
   fili    = 'table4_fly99.dat'                              ; input data
   fmti    = '(i4,15x,a12,1x,a12,1x,a13,1x,f6.2,6x,f6.3,i2)'
   imax    = 24.0 ; max mag for matching
   xmin    = -500.0
   xmax    = 5000.0
   ymin    = -500.0
   ymax    = 5000.0
   film    = 'fly99_matchin.dat'
   ;
   cd, wd, current=cwd
   ;
   ; read input file
   readfmt, fili, fmti, idfly, rastr, decstr, idhdfn, m814, zfly, tfly
   nh      = n_elements(idfly)
   indh    = lindgen(nh)+1
   ;
   ; convert positions to decimal degrees
   ra      = 15.0d0*sexideg(rastr, delim=' ')
   dec     = sexideg('+'+strtrim(decstr,2), delim=' ')
   ;
   ; read in header of detection image
   fits_read, ddir+detim, data, hdr, /header_only
   ; 
   ; determine pixel positions in detection Image
   adxy, hdr, ra, dec, xidt, yidt
   xidt    = xidt + 1.0
   yidt    = yidt + 1.0
   ;
   ; save objects that are fairly bright and within IDT 
   ; pixel range for matching
   k = where(m814 LE imax AND xidt GE xmin AND xidt LE xmax AND yidt GE ymin AND yidt LE ymax, nk)
   print, nk
   openw, lu, film, /get_lun
   FOR i = 0, nk-1 DO printf, lu, indh[k[i]], xidt[k[i]], yidt[k[i]], m814[k[i]]
   free_lun, lu
   ;
   cd, cwd
END 
