PRO shunt_regout, freg, keep_class, keep_color, id, spec_class, bx_ra, bx_dec
   ;
   ; Make a regions file for ds9 to display direct image error boxes
   ;
   ; G. Meurer 09/2005
   hdr = 'global color=green font="helvetica 10 normal" wcs=wcs'
   nn  = n_elements(id)
   openw, lu, freg, /get_lun
   printf, lu, hdr
   FOR ii = 0, nn-1 DO BEGIN 
      jj = strpos(keep_class, spec_class[ii])
      str = 'wcs ; polygon '
      FOR kk = 0, 3 DO str = str + string(bx_ra[ii,kk],bx_dec[ii,kk], format='(f11.6,f11.6)')
      str = str + '# color='+keep_color[jj]+' text={'+strtrim(string(id[ii]),2)+'}'
      printf, lu, str, format='(a)'
   ENDFOR 
   free_lun, lu
END 
