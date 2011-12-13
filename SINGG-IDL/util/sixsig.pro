PRO sixsig, xx, yy, aa, bb, sigxx, sigyy
   ;
   ; Calculate dispersions about sixlin fits resulting
   ; in six dispersions in each of xx and yy
   ;
   ; G. Meurer 04/2005
   cc    = -1.0*aa/bb
   dd    =  1.0/bb
   sigxx =  make_array(6.0, /float, value=0.0)
   sigyy =  make_array(6.0, /float, value=0.0)
   np    =  n_elements(xx)
   ;
   FOR ii = 0, 5 DO BEGIN 
      residxx   = xx - (cc[ii] + dd[ii]*yy)
      residyy   = yy - (aa[ii] + bb[ii]*xx)
      sigxx[ii] = sqrt(total(residxx^2)/float(np-2))
      sigyy[ii] = sqrt(total(residyy^2)/float(np-2))
   ENDFOR 
END 
