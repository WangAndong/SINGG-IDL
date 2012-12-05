FUNCTION blem_assign_sexid, shift, icen, nid, id, imin, imax
   ;
   ; assign SExtractor ID
   nelm   = n_elements(shift)
   outstr = make_array(nelm, /string, value='-1')
   IF nid GT 0 THEN BEGIN 
      FOR k = 0, nelm-1 DO BEGIN 
         ii = float(icen) + shift[k]
         mm = where(ii LE imax AND ii GE imin, nm)
         IF nm GT 0 THEN BEGIN 
            outstr[k] = ''
            FOR j = 0, nm-1 DO BEGIN
               IF j GT 0 THEN delim = ',' ELSE delim = ' '
               outstr[k] = outstr[k]+delim+strtrim(string(id[mm[j]]),2)
            ENDFOR
         ENDIF 
      ENDFOR 
   ENDIF
   return,outstr
END 

