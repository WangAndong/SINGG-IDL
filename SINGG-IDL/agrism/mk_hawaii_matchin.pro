PRO mk_hawaii_matchin
   ; 
   ; Make input catalog of hawaii spectro-z catalog
   ; for matching with our detection Image.
   ;
   ; G. Meurer 08/2004
   ;
   ; setup
   detim   = 'detectionImage.fits'                           ; detection image
   ddir    = '/home/meurer/ACS/Grism/HDFN_jun04/Apsis/Work/' ; where detim is
   wd      = ddir + 'Match'                                  ; where to put results
   imax    = 24.0                                            ; max mag for matching
   xmin    = -500.0
   xmax    = 5000.0
   ymin    = -500.0
   ymax    = 5000.0
   film    = 'hawaii_matchin.dat'
   ;
   cd, wd, current=cwd
   ;
   ; read in Hawaii spectro-z catalog
   read_cbhcs04, ra, dec, hkmag, zmag, imag, rmag, vmag, bmag, umag, $
                 zsp, flag_zsp, src_zsp, zph, flag_zph, zphodds   
   nh   = n_elements(ra)
   indh = lindgen(nh)+1
   ;
   ; read in header of detection image
   fits_read, ddir+detim, data, hdr, /header_only
   ; 
   ; determine pixel positions in detection Image
   adxy, hdr, ra, dec, xidt, yidt
   xidt    = xidt + 1.0
   yidt    = yidt + 1.0
   ;
   ; save small objects that are fairly bright for matching
   k = where(imag LE imax AND xidt GE xmin AND xidt LE xmax AND yidt GE ymin AND yidt LE ymax, nk)
   print, nk
   openw, lu, film, /get_lun
   FOR i = 0, nk-1 DO printf, lu, indh[k[i]], xidt[k[i]], yidt[k[i]], imag[k[i]]
   free_lun, lu
   ;
   cd, cwd
END 
