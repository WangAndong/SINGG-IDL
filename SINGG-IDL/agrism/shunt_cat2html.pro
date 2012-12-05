PRO shunt_cat2html, fcat, fhtml, keep_class, pfxo, title=title
   ;
   ; translate an ascii catalog from shunt to an html web page
   ; sorting the sources by spectral class and magnitude in the
   ; process
   ;
   ; G. Meurer 09/2005
   ;
   pfx_plots  = pfxo+'_shunt'
   pfx_dstamp = pfxo+'_shunt_direct_stamp'
   IF NOT keyword_set(title) THEN title = 'SHUNT classifications of field : '+pfxo
   ;
   ; read appropriate quantities from the ascii catalog
   readcol, fcat, id, xsq, ysq, mag, spec_class, ra, dec, format='(l,f,f,f,a,x,x,x,x,x,x,x,x,f,f)'
   spec_class = strtrim(spec_class,2)
   ;
   ; sort sources by keep_class and then mag
   nclass = strlen(keep_class)
   nobj   = n_elements(id)
   nkeep  = 0
   FOR ii = 0, nclass-1 DO BEGIN 
      cl     = strmid(keep_class,ii,1)
      kk     = where(strpos(spec_class,cl) GE 0, nkk)
      IF nkk GT 0 THEN BEGIN 
         jj = sort(mag[kk])
         qq = kk[jj]
         IF nkeep EQ 0 THEN ptr = qq ELSE ptr = [ptr, qq]
         nkeep    = nkeep + nkk
      ENDIF 
   ENDFOR 
   ;
   ; save only the good entries and in the proper sort order
   id         = id[ptr]
   xsq        = xsq[ptr]
   ysq        = ysq[ptr]
   mag        = mag[ptr]
   spec_class = spec_class[ptr]
   ra         = ra[ptr]
   dec        = dec[ptr]
   ;
   ; make web page
   shunt_html_out, fhtml, fcat, title, id, xsq, ysq, spec_class, mag, ra, dec, pfx_plots, pfx_dstamp=pfx_dstamp
END 
