PRO blem_qualcut, qual_cut, lstep, go_on, qset=qset
   ;
   ; query quality of cuts and 2d extractions
   ;
   ; qual_cut <- Output quality assesment of cutouts
   ; lstep    <- Amount to increment loop in blind_emfind
   ; go_on    <- set to true if the next stage of processing is 
   ;             to be done for this source
   ; qset     -> if set then nothing is printed and Quality is set
   ;             to this value
   ;
   ; G. Meurer 12/2002 - originally written
   ;           06/2004 - options reduced, lstep added
   ;           08/2005 - qset added
   go_on    = 3b
   qdef     = '0'
   IF keyword_set(qset) THEN quse = strupcase(strtrim(string(qset),2)) ELSE quse = qdef
   REPEAT BEGIN 
      goodqual = 0b
      IF NOT keyword_set(qset) THEN BEGIN 
         print,'  '
         print,'Val  Meaning                            Do CRCOR?'
         print,'-------------------------------------------------'
         print,' 0 : (or blank) OK                          Y    '
         print,' 1 : Grism blemish (not real detection)     N    '
         print,' 2 : Detect blemish (not real detection)    N    '
         print,' 3 : Star or continuum ripple               N    '
         print,' - : Redo previous source                   N    '
         print,' R : Redo this source                       N    '
         print,' S : Stop here, write output                N    '
         print,'  '
         read,'Enter quality code : ', qual_cut
         IF strlen(qual_cut) EQ 0 THEN qual_cut = '0'
         qual_cut = strupcase(strtrim(qual_cut,2))
      ENDIF ELSE BEGIN 
         qual_cut = quse
         print, 'BLEM_QUALCUT: setting user passed quality code : '+qual_cut
      ENDELSE 
      CASE qual_cut OF 
         '0'  : BEGIN 
                   go_on = 1b
                   lstep = 1
                END 
         '1'  : BEGIN 
                   go_on = 0b
                   lstep = 1
                END
         '2'  : BEGIN 
                   go_on = 0b
                   lstep = 1
                END
         '3'  : BEGIN 
                   go_on = 0b
                   lstep = 1
                END
         '-'  : BEGIN 
                   go_on = 0b
                   lstep = -1
                END
         'R'  : BEGIN 
                   go_on = 0b
                   lstep = 0
                END
         'S'  : BEGIN 
                   go_on = 0b
                   lstep = -99
                END
         ELSE : BEGIN 
                   IF NOT keyword_set(qset) THEN BEGIN 
                      print,'**** The value "'+qual_cut+'" is not valid.' 
                   ENDIF ELSE BEGIN 
                      print,'**** The value "'+qual_cut+'" is not valid, using default : '+qdef
                      quse = qdef
                   ENDELSE 
                END 
         ENDCASE 
       goodqual = (go_on LT 2b)
    ENDREP UNTIL goodqual 
END 

