FUNCTION update_name,inname
; Temporary function that patches an old-format name to a new one, and
; trims from the J(I6)(+/-)(I4) down to the usual J(I4)(+/-)(I2)

  n_names = N_ELEMENTS(inname)
  outname = inname

  oldname = ["J2336-37","J0216-11","J0943-05","J0009-34", $
             "J0943-09","J0456-42","J1002-05", $
             "J0039-14","J2023-31","J2216-42","J2334-46", $
             "J0209-10a","J0042-22","J1303-17"]

  newname = ["J2336-37a","J0216-11c","J0943-05b","J0009-34x", $
             "J0943-09b","J0457-42","J1002-06", $
             "J0039-14a","J2022-31","J2217-42","J2334-45b", $
             "J0209-10","J0043-22","J1303-17b"]

;; In the long run, change it to a 2D array, with each line being [old,new]

  FOR ii = 0,n_names-1 DO BEGIN
; First, trim the name if necessary.  Cut off anything after the space.
    space = STRPOS(outname[ii],' ')
    IF space GE 0 THEN outname[ii] = STRMID(outname[ii],0,space)

    chunk1 = STRMID(outname[ii],0,1)
    IF STRUPCASE(chunk1) EQ 'J' AND STRLEN(outname[ii]) GE 8 THEN BEGIN
      pos = STRPOS(outname[ii],'+')
      neg = STRPOS(outname[ii],'-')
      IF pos EQ 7 OR neg EQ 7 THEN BEGIN
; It's the 6/4 designation, cut off two digits on each part
        chunk2 = STRMID(outname[ii],1,4)        
        chunk3 = STRMID(outname[ii],7,1)        
        chunk4 = STRMID(outname[ii],8,2)        
        IF STRLEN(outname[ii]) GT 12 THEN chunk5 = STRMID(outname[ii],12,1) $
                                     ELSE chunk5 = ''
        outname[ii] = 'J'+STRTRIM(chunk2,2)+STRTRIM(chunk3,2)+STRTRIM(chunk4,2)+STRTRIM(chunk5,2)
      ENDIF ELSE BEGIN
; It's a shorter designation, OR a standard starting with a J.  We
; don't really NEED to do anything else, except make sure it starts
; with a capital J.
        IF STRMID(outname[ii],0,1) EQ 'j' THEN outname[ii] = 'J'+STRMID(outname[ii],1,STRLEN(outname[ii])-1)
      ENDELSE
    ENDIF
    
; Now, patch the name.
    index = WHERE(STRTRIM(oldname,2) EQ STRTRIM(outname[ii],2),count)
    IF count GT 0 THEN outname[ii] = STRTRIM(newname[index[0]],2)
  ENDFOR

; Special fix for J1339-31.
  test1 = WHERE(STRTRIM(outname,2) EQ "J1339-31b" OR STRTRIM(outname,2) EQ "J1339-31A",count1)
  test2 = WHERE(STRTRIM(outname,2) EQ "J1339-31",count2)

  IF (count1 GT 0) THEN outname[test1] = "J1339-31A"
; If there's no J1339-31 at all, call it all A
;  IF (count1 EQ 0 AND count2 GT 0) THEN outname[test2] = "J1339-31A"

  RETURN,outname
END
