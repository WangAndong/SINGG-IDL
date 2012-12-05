PRO make_smask,indir,outdir
; Remakes the subtracted-image masks without remaking the other masks.

  spawn,'pwd',cdir

  idir = STRTRIM(indir,2)
  IF STRMID(idir,0,1,/reverse_offset) NE '/' THEN idir = idir+'/'
; If it's a relative path (either './' or nothing), add the pwd.
  IF STRMID(idir,0,2) EQ './' THEN idir = STRTRIM(cdir[0],2)+STRMID(idir,1,STRLEN(idir)-1)
  IF STRMID(idir,0,1) NE '/' THEN idir = STRTRIM(cdir[0],2)+'/'+idir

  spawn,'ls '+idir+'J*_?sub.fits',Slist
  IF NOT FILE_TEST(Slist[0]) THEN BEGIN
    PRINT,"ERROR in make_smask: No net images found"
    RETURN
  ENDIF

  FOR ii = 0,N_ELEMENTS(Slist)-1 DO BEGIN
    dotpos = STRPOS(Slist[ii],'.fits')
    contchr = STRMID(Slist[ii],dotpos-4,1)

;    Smasko = proc4_smask(
  ENDFOR

  RETURN

END
