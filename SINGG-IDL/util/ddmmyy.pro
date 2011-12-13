FUNCTION ddmmyy, day, month, year
   ;
   ; return date (day, month, year) as a string in ddmmyy format.
   ;
   ; Make sure all input arrays are the same size
   nd = n_elements(day)
   nm = n_elements(month)
   ny = n_elements(year)
   IF nd NE ny AND nd NE nm THEN BEGIN 
      print, '**** Error in ddmmyy: day month and year must all have same dimensions'
      stop
   ENDIF 
   ;
   ; make sure all input values are positive
   k   = where(day LT 0, n1)
   k   = where(month LT 0, n2)
   k   = where(year LT 0, n3)
   IF (n1 + n2 + n3) GT 0 THEN BEGIN 
      print, '**** WARNING in ddmmyy: day month and year must all be positive'
      print, '**** expect garbage'
      stop
   ENDIF 
   ;
   mn  = fix(abs(month)) - 100*fix(abs(month)/100)
   yr  = fix(abs(year)) - 100*fix(abs(year)/100)
   dy  = fix(abs(day)) - 100*fix(abs(day)/100)
   str = make_array(nd,/string)
   FOR i = 0, nd-1 DO str[i] = string(dy[i],mn[i],yr[i],format='(i2,i2,i2)')
   k   = where(dy LT 10,nk)
   IF nk GT 0 THEN BEGIN 
      FOR i = 0,nk-1 DO BEGIN 
         ss = str[k]
         strput,ss,'0',0
         str[k] = ss
      ENDFOR 
   ENDIF 
   k   = where(mn LT 10,nk)
   IF nk GT 0 THEN BEGIN 
      FOR i = 0,nk-1 DO BEGIN 
         ss = str[k]
         strput,ss,'0',2
         str[k] = ss
      ENDFOR 
   ENDIF 
   k   = where(yr LT 10,nk)
   IF nk GT 0 THEN BEGIN 
      FOR i = 0,nk-1 DO BEGIN 
         ss = str[k]
         strput,ss,'0',4
         str[k] = ss
      ENDFOR 
   ENDIF 
   ;
   IF nd EQ 1 THEN str = temporary(str[0])
   return,str
END 
