PRO reform_tabsep, fili, filo, charsp=charsp
   ;
   ; reformat tab separated file to space separated
   ;
   ; fili   -> input file name
   ; filo   -> output file name
   ; charsp -> if set character to replace blanks in file
   ;
   ; G. Meurer 1/2005
   tab    = string(9b)
   ;
   ; get number of lines in input file
   nlines = NUMLINES( fili )
   if nlines LT 0 then return
   ;
   ; open input and output lines
   openr, lui, fili, /get_lun
   openw, luo, filo, /get_lun
   ;
   stri = ' '
   FOR ii = 0, nlines-1 DO BEGIN
      readf, lui, stri
      IF keyword_set(charsp) THEN BEGIN 
         tmp  = stri
         stri = repchr(tmp, ' ', charsp)
      ENDIF 
      stro = repchr(stri, tab, ' ')
      printf, luo, stro
   ENDFOR 
   free_lun, lui
   free_lun, luo
END 
