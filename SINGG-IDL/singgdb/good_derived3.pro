FUNCTION good_derived3, ng, snlimit=snlimit, cmdin=cmdin
   ;
   ; determine good entries in singg_derived using 
   ; quantities in database.  
   ;
   ; returns the good entries in the database
   ;
   ; The singg_derived database must be open for this to work
   ;
   ; ng       <- number of good entries
   ; snlimit  -> if set the minimum Halpha S/N, defaults to 2.0
   ; cmdin    -> other search commands, these will be included
   ;             in the search
   ;
   ;  G. Meurer 02/2008
   ;snl     = 2.0
   IF keyword_set(snlimit) THEN BEGIN 
      ; 
      ; transform S/N limit to an error limit and generate 
      ; first part of search command
      elfhalim  = alog10(1.0 + 1.0/snlimit) 
      cmd       = 'err_logf_ha_t < '+strtrim(string(elfhalim),2)+', '
   ENDIF ELSE BEGIN 
      cmd       = ''
   ENDELSE 
   ;
   ; append search input search command if need be
   IF keyword_set(cmdin) THEN cmd = cmd + cmdin + ', '
   ;
   ; find good entries.
   goodha  = dbfind(cmd+'mult_priority = 1', /silent)
   goodha2 = dbfind(cmd+'mult_priority = 901', /silent)
   IF goodha2[0] GT 0 THEN goodha  = [goodha, goodha2] 
   ng      = n_elements(goodha)
   ;
   ; just in case there are no good entries
   IF ng EQ 1 AND goodha[0] EQ -1 THEN BEGIN 
      ng     = 0
      goodha = -1
   ENDIF 
   return, goodha
END 
