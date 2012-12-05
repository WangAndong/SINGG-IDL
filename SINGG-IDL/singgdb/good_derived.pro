FUNCTION good_derived, exclopt=exclopt, outopt=outopt, loud=loud
   ;
   ; flag entries in database as good or bad
   ; This is done by finding and flagging all 
   ; known bad entries, and picking the best of 
   ; multiple datasets for same target.
   ;
   ;  This can work on the singg_derived or sr1 databases.
   ;
   ;  J1321-31  : not detected
   ;  J0409-56  : do not use the Rsub image - saturated
   ;  J0410-61  : is an HVC, shouldn't really be there.
   ;  J0507-37  : do not use the Csub image - truncated
   ;  J0943-05b : do not use 6600 NB image: 6619 has better PSF
   ;  J2022-31  : should not use for tgas
   ;  J0403-01  : should not use for tgas (pretty shady all around)
   ;              strong diffuse background.
   ;
   ; exclopt -> Tells function what to exclude (only relevant
   ;            if outopt = 0)
   ;            : -1 - don't exclude anything
   ;            :  0 - just exclude the sources that should not be used
   ;                   at all
   ;            :  1 - exclude sources that should not be used at all
   ;                   and those with bad tgas
   ;            :  2 - also exclude sources with bad r_e(R) or 
   ;                   r_e(Halpha)
   ;
   ; outopt  -> Tells function what to return
   ;            : 0 - default.  Returns entries of good sources
   ;            : 1 - returns quality code: 
   ;                  0b:  good
   ;                  4b:  don't use for H-alpha surface brightnes, r_e
   ;                  8b:  don't use for R surface brightness, r_e
   ;                  16b: bad tgas:
   ;                       + J0403-01  (too much diffuse Halpha)
   ;                       + J2022-31  
   ;                  32b: not in SR1 but detected in Halpha
   ;                       + J2022-31 
   ;                       + J0008-34 
   ;                  64b: don't use at all (not in SR1)
   ;                       + J0009-34
   ;                       + J2022-31 
   ;                       + J1321-31 
   ;                       + J0410-61 
   ;                       + J0409-56 R band
   ;                       + J0507-37 6850/95
   ;                       + J0943-05 6600
   ;
   ; G. Meurer (06/2005)
   IF keyword_set(exclopt) THEN exop = exclopt ELSE exop = 0
   IF keyword_set(outopt) THEN ouop = outopt ELSE ouop = 0
   ;
   ; extract the needed quantities
   ;dbext,-1,'entry,name,filter_r,filter_n,mapp_r_f,mapp_r_t,logfha_f,logfha_t',$
   ;          entry,name,filtr,filtn,app_mag_rf,app_mag_r,logfhaf,logfha
   dbext,-1,'entry,name,filter_r,filter_n,mapp_r_f,mapp_r_t,logf_ha_f,logf_ha_t',$
             entry,name,filtr,filtn,app_mag_rf,app_mag_r,logfhaf,logfha
   ;
   nn       = n_elements(entry)
   qacode   = make_array(nn, /byte, value=0b)
   name     = strtrim(name,2)
   filtr    = strtrim(filtr,2)
   filtn    = strtrim(filtn,2)
   IF exop GE 0 THEN BEGIN 
      ;
      ; determine fraction of total flux outside of elliptical apertures
      foutha = 1.0 - 10.0^(logfhaf - logfha)
      foutr  = 1.0 - 10.0^(0.4*(app_mag_r - app_mag_rf))
      ;
      ; check off bad sources one by one
      kk     = where(name EQ 'J1321-31', nkk)
      IF nkk GE 1 THEN qacode[kk] = qacode[kk] + 64b
      IF keyword_set(loud) THEN print, 'Marking entries for J1321-31, N = ', nkk, ' QA = +64b'
      ;
      kk       = where(name EQ 'J0410-61', nkk)
      IF nkk GE 1 THEN qacode[kk] = qacode[kk] + 64b
      IF keyword_set(loud) THEN print, 'Marking entries for J0410-61, N = ', nkk, ' QA = +64b'
      ;
      kk       = where(name EQ 'J2022-31', nkk)
      IF nkk GE 1 THEN qacode[kk] = qacode[kk] + 64b + 32b
      IF keyword_set(loud) THEN print, 'Marking entries for J2022-31, N = ', nkk, ' QA = +96b'
      ;
      kk       = where(name EQ 'J0009-34', nkk)
      IF nkk GE 1 THEN qacode[kk] = qacode[kk] + 64b
      IF keyword_set(loud) THEN print, 'Marking entries for J0009-34, N = ', nkk, ' QA = +64b'
      ;
      kk       = where(name EQ 'J0008-34', nkk)
      IF nkk GE 1 THEN qacode[kk] = qacode[kk] + 64b + 32b
      IF keyword_set(loud) THEN print, 'Marking entries for J0008-34, N = ', nkk, ' QA = +96b'
      ;
      kk       = where(name EQ 'J0409-56' AND filtr EQ 'R_Harris', nkk)
      IF nkk GE 1 THEN qacode[kk] = qacode[kk] + 64b
      IF keyword_set(loud) THEN print, 'Marking entries for J0409-56, N = ', nkk, ' QA = +64b'
      ;
      kk       = where(name EQ 'J0507-37' AND filtr EQ '6850/95', nkk)
      IF nkk GE 1 THEN qacode[kk] = qacode[kk] + 64b
      IF keyword_set(loud) THEN print, 'Marking entries for J0507-37, N = ', nkk, ' QA = +64b'
      ;
      kk       = where(name EQ 'J0943-05b' AND filtn EQ '6600/75', nkk)
      IF nkk GE 1 THEN qacode[kk] = qacode[kk] + 64b
      IF keyword_set(loud) THEN print, 'Marking entries for J0943-05, N = ', nkk, ' QA = +64b'
      ;
      ;kk       = where(name EQ 'J0403-01', nkk)
      ;IF nkk GE 1 THEN qacode[kk] = qacode[kk] + 32b
      ;IF keyword_set(loud) THEN print, 'Marking entries for J0403-01, N = ', nkk
      ;
      ; find entries with >50% of flux outside of apertures and mark QA
      kk       = where(foutha GE 0.5 OR name EQ 'J0403-01' OR name EQ 'J0409-56', nkk)
      IF nkk GE 1 THEN qacode[kk] = qacode[kk] + 4b
      IF keyword_set(loud) THEN print, 'Marking entries with undetermined r_e(Halpha), N = ', nkk, ' QA = +4b'
      kk       = where(foutr GE 0.5 OR name EQ 'J0403-01', nkk)
      IF nkk GE 1 THEN qacode[kk] = qacode[kk] + 8b
      IF keyword_set(loud) THEN print, 'Marking entries with undetermined R_e(R) N = ', nkk, ' QA = +8b'
      kk     = where(name EQ 'J0403-01' OR name EQ 'J2022-31', nkk)
      IF nkk GE 1 THEN qacode[kk] = qacode[kk] + 16b
      IF keyword_set(loud) THEN print, 'Marking entries with undetermined t_gas N = ', nkk, ' QA = +16b'
   ENDIF 
   ;
   jj       = indgen(nn)
   kk       = jj
   nkk      = nn
   ;
   ; determine which are good
   CASE exop OF 
      -1 : kk = jj
       0 : kk = where(qacode LT 64b, nkk)
       1 : kk = where(qacode LT 16b, nkk)
       2 : kk = where(qacode LT 4b, nkk)
    ENDCASE 
    ;
    ; set output
    IF ouop EQ 0 THEN BEGIN 
       out = entry[kk] 
       IF keyword_set(loud) THEN print, 'Returning '+strtrim(string(nkk),2)+' good entries'
    ENDIF ELSE BEGIN 
       out = qacode
       IF keyword_set(loud) THEN print, 'Returning '+strtrim(string(n_elements(out)),2)+' quality codes'
    ENDELSE 
   return, out
END
