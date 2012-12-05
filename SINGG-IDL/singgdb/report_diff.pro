PRO report_diff, ll, tol, oldarr, newarr, quant=quant, name=name, detail=detail
   ;
   ; Report where two arrays differ by more than some tolerance
   ;
   ;  ll     -> logical unit for reporting results
   ;  tol    -> tolerance - difference larger than this are reported
   ;  oldarr -> old array
   ;  newarr -> new array
   ;  name   -> string array with object names
   ;  quant  -> Nmae of quantity in arrays
   ;  detail -> if set, then details of all sources that differ are 
   ;            printed.
   ;
   ; G. Meurer 12/2004
   ;
   diff = abs(oldarr - newarr)
   IF keyword_set(quant) THEN qstr = quant ELSE qstr = 'arrays'
   kk = where(diff GT abs(tol), nkk)
   jj = reverse(sort(diff))
   printf, ll, 'REPORT_DIFF: check of '+qstr+' to tolerance = '+strtrim(string(tol),2)
   printf, ll, 'REPORT_DIFF: number of elements in '+qstr+' that differ by tolerance = '+strtrim(string(nkk),2)
   IF nkk GT 0 THEN BEGIN 
      printf, ll, 'REPORT_DIFF: max difference is at array element = '+strtrim(string(jj[0]),2)
      IF keyword_set(name) THEN printf, ll, '    Corresponds to name = '+name[jj[0]]
      IF keyword_set(detail) THEN BEGIN 
         IF keyword_set(name) THEN str = '    name = '+ljust(name[kk],15)+' ' ELSE str=' '+strtrim(string(kk),2)
         str = str + ' | old = '+string(oldarr[kk])+' | new = '+string(newarr[kk])
         FOR ii = 0, nkk-1 DO printf, ll, str[ii]
      ENDIF ELSE printf, ll, '    | old = '+string(oldarr[jj[0]])+' | new = '+string(newarr[jj[0]])

   ENDIF 
END 
