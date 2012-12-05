FUNCTION sexideg, str, delim=delim
   ;
   ; convert a sexigessimal string (array) to degrees
   ; invalid values set to -999.99999
   ;
   ; G. Meurer ??/20?? (JHU) originally written
   ; G. Meurer 10/2012 (ICRAR/UWA) fix bug with leading sign
   invalid = -999.99999d0
   ;
   ; set default delimiter
   IF NOT keyword_set(delim) THEN delim = ':'
   ;
   ; define output array
   n = n_elements(str)
   deg = make_array(n, /double)
   ;
   FOR i = 0l, n-1l DO BEGIN 
      ss = strtrim(str[i],2)     ; remove leading, trailing blanks
      ll = strlen(ss)
      p1 = strpos(ss, delim)     ; find first delimiter
      IF p1 GT -1 THEN BEGIN 
         ;
         ; first delimiter found, extract degrees and read
         s  = strmid(ss, 0, p1)
         s0 = strmid(s, 0, 1)                             ; bug fix
         if s0 eq '-' or s0 eq '+' then s = strmid(s, 1)  ; to strip leading sign
         IF s0 EQ '-' THEN sgn = -1.0d0 ELSE sgn = 1.0d0
         reads, s, val, format='(f)'
         deg[i] = double(abs(val))
         ll = ll - p1 - 1
         ss = strmid(ss, p1+1, ll)
         ;
         p1 = strpos(ss, delim)  ; find 2nd delimiter
         IF p1 GT -1 THEN BEGIN 
            ;
            ; second delimiter found, extract minutes
            s  = strmid(ss, 0, p1)
            reads, s, val, format='(f)'
            deg[i] = deg[i]+double(val)/60.0d0
            ;
            ; extract seconds from remainder of string
            ll = ll - p1 - 1
            ss = strmid(ss, p1+1, ll)
            reads, ss, val, format='(f)'
            deg[i] = deg[i]+double(val)/3600.0d0
         ENDIF ELSE BEGIN
            ;
            ; second delimiter not found read rest of string as min.
            reads, ss, val, format='(f)'
            deg[i] = deg[i] + val/60.0d0
         ENDELSE 
         deg[i] = sgn*deg[i]
      ENDIF ELSE BEGIN 
         ;
         ; first delimiter not found, assume the whole thing is degrees
         reads, ss, val, format='(f)'
         deg[i] = val
      ENDELSE 
   ENDFOR
   ;
   ; do this so a simple variable is returned if only one element in deg
   IF n EQ 1 THEN val = deg[0] ELSE val = deg
   return, val
END 
