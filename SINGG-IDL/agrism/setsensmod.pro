PRO setsensmod, file, ext, fudge=fudge
   COMMON sensmod, lams, sens, esens, hsens
   ;
   ; Read in sensitivity curve, save in a common block.
   ; file : file name to read
   ; ext  : extension number containing sensitivity table.
                    ; fudge : multiplicative fudge facator to apply to
                    ;         sensitivity curve from file.
   IF keyword_set(fudge) THEN fuj = fudge ELSE fuj = 1.0
   s     = mrdfits(file,1,hsens)
   lams  = s.wavelength
   sens  = fuj*s.sensitivity
   esens = fuj*s.error
END
