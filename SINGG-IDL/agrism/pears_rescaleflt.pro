PRO pears_rescaleflt, fili, filo
   ;
   ; reverse the exposure time scaling and sky subtraction 
   ; done to PEARS FLT images.
   ;
   ; G. Meurer
   ;
   ; break out input fits file
   fits_open, fili, fcb
   fits_read, fcb, dum0, hdr0, exten_no=0
   fits_read, fcb, sci1, hdrs1, extname='SCI', extver=1
   fits_read, fcb, err1, hdre1, extname='ERR', extver=1
   fits_read, fcb, dq1, hdrq1,  extname='DQ',  extver=1
   fits_read, fcb, sci2, hdrs2, extname='SCI', extver=2
   fits_read, fcb, err2, hdre2, extname='ERR', extver=2
   fits_read, fcb, dq2, hdrq2,  extname='DQ',  extver=2
   fits_close, fcb
   ;
   ; get appropriate keywords from primary header
   sky  = float(sxpar(hdr0, 'MDRIZSKY'))
   texp = float(sxpar(hdr0, 'EXPTIME'))
   ;
   ; rescale science and error arrays
   sci1  = texp*sci1 + sky
   sci2  = texp*sci2 + sky
   err1  = texp*err1
   err2  = texp*err2
   ;
   ; recreate file as a multi-extension fits file
   fits_open, filo, fcb, /write
   fits_write, fcb, dum0, hdr0
   fits_write, fcb, sci1, hdrs1, extname='SCI', extver=1
   fits_write, fcb, err1, hdre1, extname='ERR', extver=1
   fits_write, fcb, dq1, hdrq1,  extname='DQ',  extver=1
   fits_write, fcb, sci2, hdrs2, extname='SCI', extver=2
   fits_write, fcb, err2, hdre2, extname='ERR', extver=2
   fits_write, fcb, dq2, hdrq2,  extname='DQ',  extver=2
   fits_close, fcb
END 
