PRO singg_make_observed, sampfil, nhd_samp, colobs, $
                         obslist, obsfil, update_fil, logfile=logfile
; PRO singg_make_observed, sampfil, nhd_samp, colobs, extrafil, nhd_extra, $
;                          obslist, obsfil, update_fil
;
; From a list of observed galaxies & the sample file, make
; observed.dat file containing all the info in the sample file for the
; observed galaxies.  Also update the sample file to indicate which
; galaxies were observed.
; 
   IF keyword_set(logfile) THEN openw, lulog, logfile, /get_lun
   ;
   ; read input file, first the header, then the data
   readfmt,sampfil,'A150',head,numline=nhd_samp
   readfmt,sampfil,'A150',datline,skipline=nhd_samp
   samp_matched  = make_array(n_elements(datline), /INT, VALUE=0)
   goodlin       = make_array(n_elements(datline), /BYTE, VALUE=1b)
   FOR i = 0, n_elements(datline)-1 DO IF (strmid(datline[i],0,1) EQ '#') THEN goodlin[i] = 0b
   ;
   ; reset all observations to N
   FOR i = 0, n_elements(datline)-1 DO IF (goodlin[i]) THEN $
    datline[i]   = strtrim(strmid(datline[i],0,colobs-1)+'N'+strmid(datline[i],colobs))
   FOR i = 0, n_elements(datline)-1 DO IF (strmid(datline[i],0,1) EQ '#') THEN goodlin[i] = 0b
   goodind       = where(goodlin,ngoodlin)
   namsamp       = strtrim(strmid(datline,0,12),2)
   ; read observed galaxies list
   ; readfmt,obslist,'A14,A3',namobs,statin
   readfmt,obslist,'A14,A3',namobs,statin
   goodnam       = make_array(n_elements(namobs), /BYTE, VALUE=1b)
   ; strip out comments
   FOR i = 0, n_elements(namobs)-1 DO IF (strmid(namobs[i],0,1) EQ '#') THEN goodnam[i] = 0b
   ;
   ; I probably should have some condition for no good names...
   namobs        = strtrim(namobs[where(goodnam)],2)
   stat          = statin[where(goodnam)]
   obs_matched   = make_array(n_elements(namobs), /INT, VALUE=0)
   ;
   ; compare names; loop over sample
   openw,luno,obsfil,/get_lun
   FOR i = n_elements(head)-2, n_elements(head)-1 DO printf,luno,strtrim(head[i])
   nobserved = 0
   FOR i = 0, ngoodlin-1 DO BEGIN 
      j               = goodind[i]
      matches         = STRCMP(namsamp[j],namobs,/FOLD_CASE)
      samp_matched[j] = total(matches)
      IF (samp_matched[j] GT 0 ) THEN BEGIN
         im           = where(matches GT 0,ncount)
         lasto        = im[ncount-1]
         ;datline[j]   = strtrim(strmid(datline[j],0,colobs-1)+'Y'+strmid(datline[j],colobs))
         ;
         ; Make sure that if any of the observations are with stat = 'Y'
         ; that the output status also equals Y.  Otherwise use last 
         ; last stat.
         ;
         wherey       = where(strupcase(strtrim(stat[im],2)) EQ 'Y', ny)
         IF ny GE 1 THEN usestat = 'Y  ' ELSE usestat = stat[im[ncount-1]]
         ;
         datline[j]   = strtrim(strmid(datline[j],0,colobs-1)+usestat+strmid(datline[j],colobs+2))
         ; datline[j]   = strtrim(strmid(datline[j],0,colobs-1)+stat[lasto]+strmid(datline[j],colobs+2))
         ; print,datline[j]
         printf,luno,datline[j]
         IF (samp_matched(goodind[j]) GT 1) THEN BEGIN 
            print,'**** Warning: ',namsamp[j],' matched ',samp_matched[j],' times'
            IF keyword_set(logfile) THEN printf, lulog, '**** Warning: ',namsamp[j],' matched ',samp_matched[j],' times'
         ENDIF 
         nobserved = nobserved + 1
      ENDIF 
      obs_matched = obs_matched + matches
   ENDFOR 
   ;
   ; note objects in observed.lis without matches
   notmatched = where(obs_matched EQ 0,count)
   IF (count GT 0) THEN BEGIN
      print,'**** Warning - observed sources not found in sample:'
      IF keyword_set(logfile) THEN printf,lulog,'**** Warning - observed sources not found in sample:'
      FOR i = 0,count-1 DO BEGIN 
         print,'  ',namobs[notmatched[i]]
         IF keyword_set(logfile) THEN printf,lulog,'  ',namobs[notmatched[i]]
      ENDFOR 
   ENDIF 
   ;
   ; write update file
   openw,lunu,update_fil,/get_lun
   FOR i = 0, n_elements(head)-1 DO printf,lunu,strtrim(head[i])
   FOR i = 0, n_elements(datline)-1 DO printf,lunu,datline[i]
   close,lunu
   close,luno
   print, 'From singg_make_observed : galaxies observed = ', nobserved
   IF keyword_set(logfile) THEN printf, lulog, 'From singg_make_observed : galaxies observed = ', nobserved

   ; FOR i = 0, n_elements(namobs)-1 DO print,namobs[i],'  ',obs_matched[i]
END 

;singg_make_observed,'sample_ravsort.dat',5,89,'extras.dat',4,'observed.lis', $
; 'observed.dat','sample_ravsort.update'

