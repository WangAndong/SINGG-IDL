PRO grm_cphdr, hdri, hdro
   ;
   ; copy "essential" portions of one header to an output
   ; header.
   ;
   ; G. Meurer
   kwd_skip = ['bitpix', 'naxis', 'naxis1', 'naxis2', 'pcount', 'gcount', $
               'nextend', 'filename', 'extname', 'extver', 'xtension', 'history', $
               'origin', 'iraf-tlm', 'date', 'comment']
   kwd_skip = strupcase(kwd_skip)
   ;
   ; loop through the input header until the end statement 
   kwd = ''
   ii  = 0
   nhdri = n_elements(hdri)
   REPEAT BEGIN 
     ch    = strmid(hdri[ii],8,1)
     IF ch EQ '=' THEN BEGIN 
        kwd = strupcase(strtrim(strmid(hdri[ii],0,8),2))
        kk  = where(kwd EQ kwd_skip, nkk)
        IF nkk EQ 0 THEN BEGIN 
           value = sxpar(hdri, kwd, count=count, comment=comment)
           sxaddpar, hdro, kwd, value, comment
        ENDIF 
     ENDIF 
     ii    =  ii + 1
   ENDREP UNTIL (kwd EQ 'END' OR ii GE nhdri)
END 
