PRO mk_capak_matchin
   ;
   ; prepare Capak catalog for matching with Apsis ACS team detection
   ; image catalog.
   ;
   ; G. Meurer (9/2004)
   ;
   detim   = 'detectionImage.fits'                           ; detection image
   ddir    = '/home/meurer/ACS/Grism/HDFN_jun04/Apsis/Work/' ; where detim is
   wd      = ddir + 'Match'                                  ; where to put results
   fili    = 'hdf.Rcnt.photz.cat'                            ; input data
   fmti    = '(l,d,d,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f)'
   imax    = 24.0 ; max mag for matching
   xmin    = -500.0
   xmax    = 5000.0
   ymin    = -500.0
   ymax    = 5000.0
   film    = 'capak_matchin.dat'
   ;
   cd, wd, current=cwd
   ;
   ; read input file
   readcol, fili, idcap, ra, dec, ucap, bcap, vcap, rcap, icap, zcap, hkcap, $
            zbcap, z0cap, z1cap, tbcap, ocap, zmlcap, tmlcap, chicap, format=fmti
   nh      = n_elements(idcap)
   indh    = lindgen(nh)+1
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
   k = where(icap LE imax AND xidt GE xmin AND xidt LE xmax AND yidt GE ymin AND yidt LE ymax, nk)
   print, nk
   openw, lu, film, /get_lun
   FOR i = 0, nk-1 DO printf, lu, indh[k[i]], xidt[k[i]], yidt[k[i]], icap[k[i]]
   free_lun, lu
   ;
   cd, cwd
   
END 
