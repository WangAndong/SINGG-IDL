PRO mk_blem_1stqa, qastat, filcat, awkgrcat, fili, filo
   ;
   ; Set first QA status for a list of objects
   ;
   ; qastat   -> status code to set for first QA check through 
   ;             blind_emfind
   ; filcat   -> name of sextractor catalog file containing 
   ;             source IDs and positions
   ; awkgrcat -> awk file to read the grism direct catalog
   ; fili     -> file containing id numbers to set QA values
   ; filo     -> name of output file to write.  It will containg
   ;             four columns
   ;             ID  X  Y QASTAT
   ;
   ; G. Meurer 08/2005
   ;
   ; read catalog file
   blem_rdsex, filcat, idg, xg, yg, magg, ag, bg, thetag, fwhmg, flagg, class_star, $
    awkfil=awkgrcat
   ;
   ; read file of object ids
   readcol, fili, idflg, format='(l)'
   nn = n_elements(idflg)
   ;
   ido = make_array(nn, /long, value=0)
   xxo = make_array(nn, /float, value=0.0)
   yyo = make_array(nn, /float, value=0.0)
   use = make_array(nn, /byte, value=0b)
   FOR ii = 0, nn-1 DO BEGIN 
      jj = where(idg EQ idflg[ii], njj)
      IF njj GT 0 THEN BEGIN 
         xxo[ii] = xg[jj[0]]
         yyo[ii] = yg[jj[0]]
         use[ii] = 1b
      ENDIF 
   ENDFOR 
   ;
   jj  = where(use EQ 1b, njj)
   openw, lu, filo, /get_lun
   FOR ii = 0, njj-1 DO BEGIN 
      kk = jj[ii]
      printf, lu, idflg[kk], xxo[kk], yyo[kk], qastat, format='(i,f,f,i)'
   ENDFOR 
   free_lun, lu
END 
