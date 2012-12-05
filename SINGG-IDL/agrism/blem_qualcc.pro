FUNCTION blem_qualcc, np, killcc=killcc
   ;
   ; query quality of cross correlation results
   ;
   ; np          -> number of peaks
   ; killcc      -> returned as 1b if the cross-correlation
   ;                results are to be deleted
   ;
   ; blem_qualcc <- returned quality code
   ;
   ; G. Meurer 12/2002 - written
   ;           06/2004 - revised (codes changed, killcc added) and documented
   ;
   qual_cc = ' '
   REPEAT BEGIN 
      goodqual = 0b
      print,'  '
      print,'Val  Meaning                                     '
      print,'----------------------------------------------------------------'
      print,' 0 : (or blank) OK                                              '
      IF np GT 2 THEN print,' 1 : CC peak selection ambiguous                          '
      print,' 2 : no valid peak in direct image (CC results will be deleted) '
      print,' 3 : poor vertical alignment direct - grism                     '
      print,' 4 : Other problem with CC                                      '
      print,' R : redo this source (starting at cut plots)                   '
      print,'  '
      read,'Enter quality code : ', qual_cc
      killcc  = 0b
      qual_cc = strupcase(strtrim(qual_cc,2))
      IF strlen(qual_cc) EQ 0 THEN qual_cc = '0'
      CASE qual_cc OF 
         '0'  : goodqual = 1b
         '1'  : IF np GT 1 THEN goodqual = 1b $
                 ELSE print,'**** The value "'+qual_cc+'" is not valid.'
         '2'  : BEGIN 
                   goodqual = 1b
                   killcc   = 1b
                END 
         '3'  : goodqual = 1b
         '4'  : goodqual = 1b
         'R'  : goodqual = 1b
         ELSE : print,'**** The value "'+qual_cc+'" is not valid.'
      ENDCASE 
    ENDREP UNTIL goodqual 
    return, qual_cc
END 

