PRO shunt_readcat, fcat, nobj, id, xim, yim, mag, krad, backg, aim, bim, thim, w50, class, $
                   verbose=verbose
   ;
   ; read SE catalog produced by shunt
   ;
   ; G. Meurer 09/2005
   ;
   fmt = '(l,f,f,f,x,x,x,x,x,f,f,x,f,f,f,x,f,x,f)'
   IF keyword_set(verbose) THEN print, 'SHUNT_READCAT: reading SE catalog - '+fcat
   readcol, fcat, id, xim, yim, mag, krad, backg, aim, bim, thim, w50, class, format=fmt
   nobj = n_elements(id)
   IF keyword_set(verbose) THEN print, 'SHUNT_READCAT: number of objects read from catalog : ',nobj
END 
