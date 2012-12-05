PRO proc4_write,masklist,Timg,Rid,initmask,odir,grow,mask, $
                segval,segfile,listfile,maskfile,Imaskfile,Smaskfile,ellipse, $
                Nhd,incmask,plmask,ecntrat,Rsexfile,Ssexfile

; Writes the various output files.  I put it in a separate routine to
; save on swap space, since the arrays can be dumped when we're done.
  goodval = 0b
  badval = 1b

  sz = SIZE(Timg)

  segmask = INTARR(sz[1],sz[2])

  FOR xx = 0,sz[1]-1 DO BEGIN
    FOR yy = 0,sz[2]-1 DO BEGIN
      IF Timg[xx,yy] GT 0 THEN BEGIN
        k = WHERE(Rid EQ Timg[xx,yy],nk)
        IF nk GT 0 THEN BEGIN
          IF masklist[k[0]] GE 0 THEN segmask[xx,yy] = segval[k[0]] $
                                 ELSE segmask[xx,yy] = -1*segval[k[0]]
        ENDIF
      ENDIF
    ENDFOR
  ENDFOR
  fits_write,odir+segfile,segmask,Nhd

  OPENW,listunit,odir+listfile,/GET_LUN
  PRINTF,listunit,"# ID   Mask state   Initial state"
  FOR ii = 0,N_ELEMENTS(Rid)-1 DO BEGIN
    IF Rid[ii] GT 0 THEN BEGIN
      PRINTF,listunit,Rid[ii],masklist[ii],initmask[ii],FORMAT='(I," ",I," ",I)'
    ENDIF ELSE BEGIN
      PRINTF,listunit,Rid[ii],masklist[ii],2,FORMAT='(I," ",I," ",I)'
    ENDELSE
  ENDFOR
  CLOSE,listunit
  FREE_LUN,listunit

  PRINT,"Creating final masks"

; grow the "exclude" mask
  PRINT,"Growing exclusion mask by "+STRTRIM(STRING(grow),2)+" pixels
  badmask = mask AND (Timg GT 0)
  userbadmask = mask AND (Timg LT 0)
  grow_mask, badmask, masko, grow, goodval=badval, badval=goodval
;; Change to 2 grows, one for big objects (npix > thresh) and one for small.

  IF MAX(incmask) EQ MIN(incmask) THEN BEGIN
    PRINT,"No inclusion mask defined."
    incmasko = incmask
  ENDIF ELSE BEGIN
; grow the "include" mask by twice as much.
    incmask1 = incmask AND (Timg GT 0)
    PRINT,"Growing inclusion mask by "+STRTRIM(STRING(2*grow),2)+" pixels
    grow_mask, incmask1, incmasko, 2.0*grow, goodval=badval, badval=goodval
; Don't grow user-created objects
    incmasko = TEMPORARY(incmasko) OR (incmask AND (Timg LT 0))
  ENDELSE

; We don't want to "grow" on top of a user-defined object, and if
; grows overlap, side with the inclusion.
  masko = (TEMPORARY(masko) AND NOT incmasko) OR userbadmask
  incmasko = TEMPORARY(incmasko) AND NOT userbadmask

  maskout = masko OR plmask

  fits_write,odir+maskfile,maskout,Nhd
  fits_write,odir+Imaskfile,incmasko,Nhd

  Smasko = proc4_smask(odir+Rsexfile,odir+Ssexfile,mask,odir+ellipse,ecntrat)

  Smasko = (TEMPORARY(Smasko) AND NOT incmasko) OR userbadmask OR plmask
  fits_write,odir+Smaskfile,Smasko,Shd

  RETURN

END
