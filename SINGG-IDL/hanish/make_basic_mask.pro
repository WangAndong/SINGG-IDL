PRO make_basic_mask,indir=indir,outdir=outdir
; Create a basic bad-column/pixel mask based solely on the SDflat and
; Zero images for each run.
; OPTIONAL INPUTS:
;   indir     Input directory, where the Zero and SDflat files are.
;                Default is the local directory.
;   outdir    Output directory, where the basic_mask file will be placed.
;                Default is the local directory.
; Must be run BEFORE housekeeper.
;   D. Hanish, 6/2006.

  spawn,"pwd",cdir

; indir = 'RunXX/Raw/'
  IF KEYWORD_SET(indir) THEN BEGIN
    idir = STRTRIM(indir,2)
; If it doesn't end in a slash, add one.
    IF STRMID(idir,0,1,/reverse_offset) NE '/' THEN idir = idir+'/'
; If it's a relative path (either './' or nothing), add the pwd.
    IF STRMID(idir,0,2) EQ './' THEN idir = STRTRIM(cdir[0],2)+STRMID(idir,1,STRLEN(idir)-1)
    IF STRMID(idir,0,1) NE '/' THEN idir = STRTRIM(cdir[0],2)+'/'+idir
  ENDIF ELSE BEGIN
    idir = STRTRIM(cdir[0],2)+'/'
  ENDELSE

  IF KEYWORD_SET(outdir) THEN BEGIN
    odir = STRTRIM(outdir,2)
; If it doesn't end in a slash, add one.
    IF STRMID(odir,0,1,/reverse_offset) NE '/' THEN odir = odir+'/'
; If it's a relative path (either './' or nothing), add the pwd.
    IF STRMID(odir,0,2) EQ './' THEN odir = STRTRIM(cdir[0],2)+STRMID(odir,1,STRLEN(odir)-1)
    IF STRMID(odir,0,1) NE '/' THEN odir = STRTRIM(cdir[0],2)+'/'+odir
  ENDIF ELSE BEGIN
    odir = STRTRIM(cdir[0],2)+'/'
  ENDELSE

  spawn,"ls "+idir+"Zero_r*.fits",zerolist
  IF FILE_TEST(zerolist[0]) THEN BEGIN
    fits_read,zerolist[0],zimg,bhd
  ENDIF ELSE BEGIN
; Check to see if the gzipped version exists.
    spawn,"ls "+idir+"Zero_r*.fits.gz",zerolist
    IF FILE_TEST(zerolist[0]) THEN BEGIN
      spawn,"cp -f "+STRTRIM(zerolist[0],2)+" ./Temporary.fits.gz"
      spawn,"gunzip ./Temporary.fits.gz"
      fits_read,"./Temporary.fits",zimg,bhd
      spawn,"/bin/rm -f ./Temporary.fits"
    ENDIF ELSE BEGIN
      PRINT,"ERROR in make_basic_mask: can't find necessary Zero image: ",idir
      RETURN
    ENDELSE
  ENDELSE

; Get sky values for the four quadrants.  (1.5m images might only have
; two halves, but let's just be safe.)

  sz = SIZE(zimg)

; Just to be absolute sure, pad the edges.
  basic = BYTARR(sz[1],sz[2]) + 1b
  edge = 5
  basic[edge:sz[1]-edge-1,edge:sz[2]-edge-1] = 0b
  sigarr = FLTARR(sz[1],sz[2])

  offset = 20
  mysky,zimg[LONG(sz[1]/4):LONG(sz[1]/2)-offset-1,LONG(sz[2]/4):LONG(sz[2]/2)-offset-1],skytemp,sigtemp,/SILENT
  sigarr[0:LONG(sz[1]/2)-1,0:LONG(sz[2]/2)-1] = (zimg[0:LONG(sz[1]/2)-1,0:LONG(sz[2]/2)-1] - skytemp) / sigtemp

  mysky,zimg[LONG(sz[1]/4):LONG(sz[1]/2)-offset-1,LONG(sz[2]/2)+offset:LONG(3*sz[2]/4)],skytemp,sigtemp,/SILENT
  sigarr[0:LONG(sz[1]/2)-1,LONG(sz[2]/2):sz[2]-1] = (zimg[0:LONG(sz[1]/2)-1,LONG(sz[2]/2):sz[2]-1] - skytemp) / sigtemp

  mysky,zimg[LONG(sz[1]/2)+offset:LONG(3*sz[1]/4),LONG(sz[2]/4):LONG(sz[2]/2)-offset-1],skytemp,sigtemp,/SILENT
  sigarr[LONG(sz[1]/2):sz[1]-1,0:LONG(sz[2]/2)-1] = (zimg[LONG(sz[1]/2):sz[1]-1,0:LONG(sz[2]/2)-1] - skytemp) / sigtemp

  mysky,zimg[LONG(sz[1]/2)+offset:LONG(3*sz[1]/4),LONG(sz[2]/2)+offset:LONG(3*sz[2]/4)],skytemp,sigtemp,/SILENT
  sigarr[LONG(sz[1]/2):sz[1]-1,LONG(sz[2]/2):sz[2]-1] = (zimg[LONG(sz[1]/2):sz[1]-1,LONG(sz[2]/2):sz[2]-1] - skytemp) / sigtemp

  basic = (basic OR (ABS(sigarr) GT 10.0))

  frac = 0.1 ; Fill out any columns that are more than 10% masked
  xdiv = LONG(sz[1]/2)
  ydiv = LONG(sz[2]/2)
  basico = basic
  FOR ii = 0,xdiv-1 DO BEGIN
    imin = (ii-1) > 0
    imax = (ii+1)
; Two things have changed from the commented-out variables.
; First, any error in either quadrant gets reflected into the one
; above/below.  Second, the 1-pixel padding that had been done in the
; grow_mask is done here.  This'll only grow lines, not individual
; pixels (or the horizontal edges).

; Need to use temporary array, or else it'll feed itself when it grows
    junk = WHERE(basic[ii,0:ydiv-1],count)
    IF count GT frac*FLOAT(ydiv) THEN $
;                basico[ii,0:ydiv-1] = 1b
                basico[imin:imax,0:sz[2]-1] = 1b

    junk = WHERE(basic[ii,ydiv:sz[2]-1],count)
    IF count GT frac*FLOAT(ydiv) THEN $
;                basico[ii,ydiv:sz[2]-1] = 1b
                basico[imin:imax,0:sz[2]-1] = 1b

    imin2 = (ii+xdiv-1)
    imax2 = (ii+xdiv+1) < (sz[1]-1)
    junk = WHERE(basic[ii+xdiv,0:ydiv-1],count)
    IF count GT frac*FLOAT(ydiv) THEN $
;                basico[ii+xdiv,0:ydiv-1] = 1b
                basico[imin2:imax2,0:sz[2]-1] = 1b

    junk = WHERE(basic[ii+xdiv,ydiv:sz[2]-1],count)
    IF count GT frac*FLOAT(ydiv) THEN $
;                basico[ii+xdiv,ydiv:sz[2]-1] = 1b
                basico[imin2:imax2,0:sz[2]-1] = 1b
  ENDFOR

;  grow_mask,basic,basico,1.0,goodval=1b,badval=0b

;show_mask,basico
;stop

; Flats?
  ftot = INTARR(sz[1],sz[2])
  spawn,"ls "+idir+"SDflat_n*_R.fits",flatlist1
  FOR ii = 0,N_ELEMENTS(flatlist1)-1 DO BEGIN
    IF FILE_TEST(flatlist1[ii]) THEN BEGIN
      fits_read,flatlist1[ii],fimg,fhd
      ftot = ftot + LONG(fimg LT 0.1 OR fimg GT 10.0)
    ENDIF
  ENDFOR

  spawn,"ls "+idir+"SDflat_n*_R.fits.gz",flatlist2
  FOR ii = 0,N_ELEMENTS(flatlist2)-1 DO BEGIN
    IF FILE_TEST(flatlist2[ii]) THEN BEGIN
      spawn,"cp -f "+STRTRIM(flatlist2[ii],2)+" ./Temporary.fits.gz"
      spawn,"gunzip ./Temporary.fits.gz"
      fits_read,"./Temporary.fits",fimg,fhd
      ftot = ftot + LONG(fimg LT 0.1 OR fimg GT 10.0)
      spawn,"/bin/rm -f ./Temporary.fits"
    ENDIF
  ENDFOR

  fmask = (ftot GE 2)
  fmasko = fmask

  FOR ii = 0,xdiv-1 DO BEGIN
    junk = WHERE(fmask[ii,0:ydiv-1],count)
    IF count GT frac*FLOAT(ydiv) THEN $
;                fmasko[ii,0:ydiv-1] = 1b
                fmasko[ii,0:sz[2]-1] = 1b

    junk = WHERE(fmask[ii,ydiv:sz[2]-1],count)
    IF count GT frac*FLOAT(ydiv) THEN $
;                fmasko[ii,ydiv:sz[2]-1] = 1b
                fmasko[ii,0:sz[2]-1] = 1b

    junk = WHERE(fmask[ii+xdiv,0:ydiv-1],count)
    IF count GT frac*FLOAT(ydiv) THEN $
;                fmasko[ii+xdiv,0:ydiv-1] = 1b
                fmasko[ii+xdiv,0:sz[2]-1] = 1b

    junk = WHERE(fmask[ii+xdiv,ydiv:sz[2]-1],count)
    IF count GT frac*FLOAT(ydiv) THEN $
;                fmasko[ii+xdiv,ydiv:sz[2]-1] = 1b
                fmasko[ii+xdiv,0:sz[2]-1] = 1b
            ENDFOR

; The basic mask needs to be INVERTED.  That is, 1b represents the
; places where the pixels are usable.
  basic = 1b-(basico OR fmasko)

; Finally, the image needs to be re-oriented, just like the object
; images will be.

  hk_orientimg,-1,bhd,basic,success

  fits_write,odir+"basic_mask.fits",basic,bhd

  RETURN
END
