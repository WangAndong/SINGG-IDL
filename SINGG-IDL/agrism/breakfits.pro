PRO breakfits, fili, prefixo
   ;
   ; Break out a multi-extension fits file into extensions.
   ; 
   ; G. Meurer  08/ 2005
   ;
   ; open fits image, getting information
   fits_open, fili, fcb
   ;
   ; loop through extensions: create output name, read, write output
   FOR jj = 1, fcb.nextend DO BEGIN 
      filo = prefixo + '_' + fcb.extname[jj] + '_' + strtrim(fcb.extver[jj],2) + '.fits'
      fits_read, fcb, img, hdr, extname=fcb.extname[jj], extver=fcb.extver[jj]
      ; 
      ; make dummy header  **** this is a kludge.  Should make useful
      ; header
      mkhdr, hdro, img
      grm_cphdr, hdr, hdro
      mwrfits, img, filo, hdro, /create
   ENDFOR 
   fits_close, fcb
END 
