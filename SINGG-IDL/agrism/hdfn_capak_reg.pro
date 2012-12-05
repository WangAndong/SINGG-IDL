PRO hdfn_capak_reg
   ;
   ; Make regions file for Capak photo-z catalog.
   ;
   ; G. Meurer 03/2006
   ;
   detim    = 'detectionImage.fits'
   filic    = '/home/meurer/ACS/Grism/HDFN_jun04/Apsis/Work/hdf.Rcnt.photz.cat'
   filrc    = '/home/meurer/ACS/Grism/HDFN_jun04/Apsis/Work/capak.reg'
   fmtc     = '(l,d,d,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f)'
   ;
   ; trans structure for capak catalog
   ac        =  -5.767623626    
   bc        =   1.000759887     
   cc        =   0.001336394     
   dc        =   5.397664976     
   ec        =  -0.002130544    
   fc        =   1.000337764 
   ;
   ; read in header of detection image
   fits_read, detim, data, hdr, /header_only
   naxis1    = sxpar(hdr, 'naxis1')
   naxis2    = sxpar(hdr, 'naxis1')
   ;
   ; read in Capak photometry & spectro-z catalog
   readcol, filic, idc, rac, decc, umagc, bmagc, vmagc, rmagc, imagc, zmagc, hkmagc, $
            zbc, z0c, z1c, tbc, oc, zmlc, tmlc, chic, format=fmtc
   nc        = n_elements(rac)
   indc      = make_array(nc,/long,value=-1l)
   ;
   ; calculate positions in detection image
   adxy, hdr, rac, decc, xidt, yidt
   xidt      = xidt + 1.0
   yidt      = yidt + 1.0
   xidtc     = fix(ac + bc*xidt + cc*yidt + 0.5)
   yidtc     = fix(dc + ec*xidt + fc*yidt + 0.5)
   ;
   qq        = where(xidtc GT 0 AND xidtc LE naxis1 AND yidtc GT 0 AND yidtc LE naxis2, nqq)
   ;
   ; write regions file
   openw, lu, filrc, /get_lun
   printf,lu, 'global color=red'
   FOR ii = 0l, n11-1l DO BEGIN 
      printf,lu,'box('+strtrim(string(xidtc[qq[ii]]),2)+','+strtrim(string(yidtc[qq[ii]]),2)+',5,5,0)'
   ENDFOR 
   free_lun, lu
END 
