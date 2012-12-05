PRO errspec, bintab, espec, update=update
   ; Extracts error spectrum.  
   ; For now we calculate an error spectrum from a model of 
   ; the mean variance per pixel times the number of input 
   ; pixels summed up. 
   COMMON errmod, varpix
   espec = sqrt(bintab.count + bintab.weight*varpix)
   IF keyword_set(update) THEN bintab.error = espec
END 
