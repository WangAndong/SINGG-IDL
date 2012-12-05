PRO hdfn_fly_reg
   ;
   ; Make regions file for Capak photo-z catalog.
   ;
   ; G. Meurer 03/2006
   ;
   detim    = 'detectionImage.fits'
   filif    = '/home/meurer/ACS/Grism/HDFN_jun04/Apsis/Work/Match/table4_fly99.dat'
   filrf    = '/home/meurer/ACS/Grism/HDFN_jun04/Apsis/Work/fly99.reg'
   fmtf     = '(i4,15x,a12,1x,a12,1x,a13,1x,f6.2,6x,f6.3,i2)'
   ;
   ; trans structure for FLY99 catalog
   af        = -2.030026187
   bf        =  0.999551219
   cf        =  0.002221419
   df        =  7.322390192
   ef        = -0.002895114
   ff        =  1.000134379
   ;
   ; read in header of detection image
   fits_read, detim, data, hdr, /header_only
   naxis1    = sxpar(hdr, 'naxis1')
   naxis2    = sxpar(hdr, 'naxis1')
   ;
   ; read fly99
   readfmt, filif, fmtf, idf, rastr, decstr, idhdfnf, imagf, zfly, tfly
   nf        = n_elements(idf)
   indf      = make_array(nf,/long,value=-1l)
   ;
   ; compress name
   idhdfnf   = strtrim(idhdfnf,2)
   ;
   ; convert positions to decimal degrees
   raf       = 15.0d0*sexideg(rastr, delim=' ')
   decf      = sexideg('+'+strtrim(decstr,2), delim=' ')
   ;
   ; calculate position in detection image
   adxy, hdr, raf, decf, xidt, yidt
   xidt      = xidt + 1.0
   yidt      = yidt + 1.0
   xidtf     = fix(af + bf*xidt + cf*yidt + 0.5)
   yidtf     = fix(df + ef*xidt + ff*yidt + 0.5)
   ;
   ; write regions file
   openw, lu, filrf, /get_lun
   printf,lu, 'global color=cyan'
   FOR ii = 0l, nf-1l DO BEGIN 
      printf,lu,'box('+strtrim(string(xidtf[ii]),2)+','+strtrim(string(yidtf[ii]),2)+',7,7,45)'
   ENDFOR 
   free_lun, lu
END 
