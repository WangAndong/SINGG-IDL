FUNCTION ljust, str, len
   ;
   ; left justify a string - remove all blanks at beginning of 
   ; input string, pad right side with blanks
   ;
   ; str   -> inpt string (array)
   ; len   -> number of characters in output string
   ;
   ; ljust <- left justified string (array)
   ; 
   ; G. Meurer (JHU) - 02/2004
   ;                 - 11/2007 make loop index a long integer
   ;
   ; generate array of blanks
   ns  = n_elements(str)
   s   = ''
   FOR i =0l, len-1l DO s = s + ' '
   res = make_array(ns, /string, value=s)
   ;
   ; do the work one element at a time.
   FOR i = 0l, ns-1l DO BEGIN 
      s      =  res[i]
      strput, s, strtrim(str[i],1), 0
      res[i] = s
   ENDFOR 
   ;
   ; convert to scalar if only one element
   IF ns EQ 1 THEN res = temporary(res[0])
   return, res
END 
