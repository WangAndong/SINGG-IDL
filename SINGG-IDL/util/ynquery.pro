FUNCTION ynquery, prompt=prompt
   ;
   ; Query user to get a yes or no response.  Returns the 
   ; response
   ;
   ;  prompt -> string used to prompt user for a response
   ;            default is "yes or no?"
   ;
   ;  result <- 0b => no
   ;            1b => yes
   ;
   ; addapted from blem_addgaus.
   ;
   ; G. Meurer 12/2010 UWA/ICRAR
   yestr  = '1YJT'
   nostr  = '0NF'
   ans    = ' '
   result = 0b
   goodchoice = 0b
   if keyword_set(prompt) then pmpt = prompt else pmpt = "yes or no? "
   REPEAT BEGIN 
      read,ans,prompt=pmpt,format='(a)'
      ans = strupcase(strmid(strtrim(ans,2),0,1))
      sy = strpos(yestr,ans)
      sn = strpos(nostr,ans)
      IF sy LT 0 AND sn LT 0 THEN BEGIN 
         print, '**** Your selection : ', ans, ' is not a valid choice (yes="'+yestr+'";no="'+nostr+'"), try again ****'
         goodchoice = 0b
      ENDIF ELSE BEGIN 
         goodchoice = 1b
         IF sy GE 0 THEN result = 1b ELSE result = 0b
      ENDELSE 
   ENDREP UNTIL goodchoice 
   return, result
END 

