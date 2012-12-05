FUNCTION pfplt_kwdread, keyword, kwdarr, valuearr, default, usetype=usetype
   ;
   ; Return the value of  KEYWORD from the keyword and value arrays
   ;
   ; keyword  -> The keyword value to array
   ; kwdarr   -> array of keywords from header.  
   ; valuearr -> array of values from header in string format
   ; default  -> default return value
   ;
   ; The keyword matching is done by an exact string match.  
   ; if there is more than one match then an array is returned.
   ;
   ; G.R. Meurer 10/2004
   ;
   IF keyword_set(usetype) THEN uset = strupcase(strtrim(usetype,2)) ELSE uset = 'FLOAT'
   kk = where(kwdarr EQ keyword, nkk)
   IF nkk EQ 0 THEN value = default ELSE BEGIN 
      CASE uset OF 
         'STRING' : value = valuearr[kk]
         'BYTE'   : value = byte(fix(valuearr[kk]))
         'INT'    : value = fix(valuearr[kk])
         'FLOAT'  : value = float(valuearr[kk])
         ELSE     : value = float(valuearr[kk])
      ENDCASE  
   ENDELSE 
   IF nkk EQ 1 THEN value= value[0]
   return, value
END  

