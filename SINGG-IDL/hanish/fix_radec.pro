PRO fix_radec,Rhd,Chd,Shd
; Fix the RA and DEC in the header to match the results of WCStan.pl
; (CRVAL1 and CRVAL2).  This requires a conversion from decimal to h:m:s format

  radec,SXPAR(Rhd,"CRVAL1"),SXPAR(Rhd,"CRVAL2"), $
              ihr,imin,xsec,ideg,imn,xsc
; Fix formatting; RA should be 2:2:2.2, DEC should be (+/-)2:2:2.2, with
; zeroes padding the digits.  Easy way to do that is add 100 and take the last
; two digits of everything.  Specifying FORMAT for STRING isn't enough.
  Rra = STRMID(STRTRIM(STRING(100+ihr),2),1,2)+":"+ $
        STRMID(STRTRIM(STRING(100+imin),2),1,2)+":"+ $
        STRMID(STRTRIM(STRING(100.0+xsec),2),1,5)
  SXADDPAR,Rhd,"RA",Rra,' right ascension (telescope)'
  IF ideg GE 0 THEN sign = "+" ELSE sign = "-"
  Rdec = sign+STRMID(STRTRIM(STRING(100+ABS(ideg)),2),1,2)+":"+ $
         STRMID(STRTRIM(STRING(100+imn),2),1,2)+":"+ $
         STRMID(STRTRIM(STRING(100.0+xsc),2),1,5)
  SXADDPAR,Rhd,"DEC",Rdec,' declination (telescope)'

; We used to manage the two RA/DECs separately, but they shouldn't be
; different, and the R-band version is probably more correct.
;  radec,SXPAR(Chd,"CRVAL1"),SXPAR(Chd,"CRVAL2"), $
;              ihr,imin,xsec,ideg,imn,xsc
; Fix formatting; RA should be 2:2:2.2, DEC should be (+/-)2:2:2.2, with
; zeroes padding the digits.  Easy way to do that is add 100 and take the last
; two digits of everything.  Specifying FORMAT for STRING isn't enough.
;  Cra = STRMID(STRTRIM(STRING(100+ihr),2),1,2)+":"+ $
;        STRMID(STRTRIM(STRING(100+imin),2),1,2)+":"+ $
;        STRMID(STRTRIM(STRING(100.0+xsec),2),1,5)
;  IF ideg GE 0 THEN sign = "+" ELSE sign = "-"
;  Cdec = sign+STRMID(STRTRIM(STRING(100+ABS(ideg)),2),1,2)+":"+ $
;         STRMID(STRTRIM(STRING(100+imn),2),1,2)+":"+ $
;         STRMID(STRTRIM(STRING(100.0+xsc),2),1,5)
  Cra = Rra
  Cdec = Rdec
  SXADDPAR,Chd,"RA",Cra,' right ascension (telescope)'
  SXADDPAR,Chd,"DEC",Cdec,' declination (telescope)'

; Add to the Rsub, too
  SXADDPAR,Shd,"RA",Cra,' right ascension (telescope)'
  SXADDPAR,Shd,"DEC",Cdec,' declination (telescope)'

  RETURN
END
