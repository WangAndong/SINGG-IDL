FUNCTION shunt_classify
   ;
   ; Get classification of spectrum from the user
   ;
   option  = ['S : this really looks like a Supernova (to me)!', $
              'M : obvious M star ', $
              'K : obvious K star ', $
              'A : Absorption spectrum, neither (obviously) K nor M star ', $
              'E : Emission line spectrum ', $
              'B : Break spectrum', $
              'U : Unclear, flat or featureless spectrum', $
              'O : Other order (not first order)', $
              'N : no classification ', $
              'Q : quit classification loop (remaining objects marked "N")']
   nop        = n_elements(option)
   ostr       = ''
   goodchoice = 0b
   ans        = ''
   FOR ii = 0, nop-1 DO ostr = ostr + strmid(option[ii],0,1)
   REPEAT BEGIN 
      print, 'Spectrum classification options: '
      FOR ii = 0, nop-1 DO print, '  '+option[ii]
      ;
      read, ans, prompt='Classify spectrum using above options ('+ostr+') : ', format='(a)'
      ans = strupcase(strmid(strtrim(ans,2),0,1))
      kk  = strpos(ostr, strmid(ans,0,1))
      IF kk GE 0 THEN goodchoice = 1b
   ENDREP UNTIL goodchoice
   return, ans
END 
