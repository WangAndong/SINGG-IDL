FUNCTION percentile, arr, perclev
   ;
   ; Determine percentiles of a distribution.
   ;
   ; arr     -> array of values for which the percentiles are 
   ;            determined.  There must be at least 2 elements
   ;            in array.
   ; perclev -> percentage levels (in percent) at which the
   ;            percentiles are calculated.  Perclev can be an
   ;            array or variable, but must be between 0 & 100.
   ;
   ; G. Meurer 09/2004
   na   = n_elements(arr)
   np   = n_elements(perclev)
   perc = make_array(np, /float, value=-99.99)
   IF na LE 1 THEN BEGIN
      print, '**** ERROR in PERCENTILE - array must have at least 2 elements'
   ENDIF ELSE BEGIN
      IF min(perclev) LT 0.0 OR max(perclev) GT 100.0 THEN BEGIN 
         print, print, '**** ERROR in PERCENTILE - perlev values must be between 0 & 100'
      ENDIF ELSE BEGIN 
         kk   = sort(arr)
         ii0  = fix(0.01*perclev*float(na-1))
         ii1  = ii0 + 1 MOD na
         w0   = 1.0 - (0.01*perclev*float(na-1) - float(ii0))
         w1   = 1.0 - w0
         perc = w0*arr[kk[ii0]] + w1*arr[kk[ii1]]
      ENDELSE 
   ENDELSE 
   return, perc
END 
