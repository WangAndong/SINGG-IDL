FUNCTION  grcl_disposition, k 
   ;
   ; Query disposition of spectrum from user
   ; if k > 0 then can only do G or I
   ;
   ; G.R. Meurer - late 2002: original program written
   ; G.R. Meurer - 01/2007: allow option L = reset lambda range
   goodchoice = 0b
   ans        = ' '
   IF k LE 0 THEN BEGIN 
      choices = 'GMIL' 
      prmpt   = 'Enter your choice (G, M, I, or L) : '
   ENDIF ELSE BEGIN 
      choices = 'GIL'
      prmpt   = 'Enter your choice (G, I, or L) : '
   ENDELSE 

   REPEAT BEGIN 
      print,'What do you want to do with this spectrum? Options are: '
      print,'  G  : fit highest S/N peak with a Gaussian '
      IF k LE 0 THEN print,'  M  : classify as late type star or SN '
      print,'  I  : ignore - emission feature probably not real '
      print,'  L  : reset lambda fit range '
      read,ans,prompt=prmpt,format='(a)' 
      ans = strupcase(strmid(strtrim(ans,2),0,1))
      s = strpos(choices,ans)
      IF s GE 0 THEN BEGIN
         goodchoice = 1b
      ENDIF ELSE BEGIN 
         print, '**** '
         print, '**** Your selection : ', ans, ' is not a valid choice, try again ****'
         print, '**** '
         goodchoice = 0b
      ENDELSE 
   ENDREP UNTIL goodchoice
   return, ans
END 

