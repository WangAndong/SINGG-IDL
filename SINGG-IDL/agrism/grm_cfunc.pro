FUNCTION grm_cfunc, arr1, arr2, pad=pad
   ;
   ; Cross-correlate two one dimensional arrays in the Fourier domain.
   ;
   ; arr1 -> first array.  
   ; arr2 -> second array.
   ;         ARR1,2 should have zero mean and no sharp 
   ;         discontinuities at edges.
   ; pad  -> if set then the arrays are padded
   ;
   ; G.R. Meurer 12/2002
   ;
   IF keyword_set(pad) THEN BEGIN 
      f1    = fft(zero_pad(arr1))
      f2    = fft(zero_pad(arr2))
   ENDIF ELSE BEGIN 
      f1    = fft(arr1)
      f2    = fft(arr2)
   ENDELSE 
   cfunc    = float(fft(f1*conj(f2),/inverse))
   return, cfunc
END 

