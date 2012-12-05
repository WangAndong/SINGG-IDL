FUNCTION blem_addgauss
   ;
   ; Query quality of fit from user
   yestr = '1YJT'
   nostr = '0NF'
   ans   = ' '
   qual  = 0b
   goodchoice = 0b
   REPEAT BEGIN 
      read,ans,prompt='Add another Gaussian ? ',format='(a)'
      ans = strupcase(strmid(strtrim(ans,2),0,1))
      sy = strpos(yestr,ans)
      sn = strpos(nostr,ans)
      IF sy LT 0 AND sn LT 0 THEN BEGIN 
         print, '**** '
         print, '**** Your selection : ', ans, ' is not a valid choice, try again ****'
         print, '**** '
         goodchoice = 0b
      ENDIF ELSE BEGIN 
         goodchoice = 1b
         IF sy GE 0 THEN qual = 1b ELSE qual = 0b
      ENDELSE 
   ENDREP UNTIL goodchoice 
   return, qual
END 

