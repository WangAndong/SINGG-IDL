FUNCTION find_middle,imlist
  num_files = N_ELEMENTS(imlist)

; Old way
;  middle = (num_files+1)/2

; New way
  imra = FLTARR(num_files)
  imdec = FLTARR(num_files)
  FOR ii = 0,(num_files-1) DO BEGIN
    IF STRPOS(imlist[ii],'sh.fits') GT 0 THEN BEGIN
; This should come BEFORE the sh.fits files are created, but the
; old-format list files used the sh names, so just pull those two
; letters out.
      objfile = STRMID(imlist[ii],0,STRLEN(imlist[ii])-7)+".fits"
    ENDIF ELSE BEGIN
      objfile = STRTRIM(imlist[ii],2)
    ENDELSE
    IF NOT FILE_TEST(objfile) THEN BEGIN
      objfile = imlist[ii]
      IF NOT FILE_TEST(objfile) THEN BEGIN
        PRINT,"ERROR in find_middle: file does not exist ",objfile
        RETURN,objfile
      ENDIF
    ENDIF
    fits_read,objfile,img,hd,/header_only

    xpos = SXPAR(hd,'NAXIS1')/2
    ypos = SXPAR(hd,'NAXIS2')/2
    xyad,hd,xpos,ypos,ra,dec
    imra[ii] = ra
    imdec[ii] = dec
 
; Old version, which made the mistake of not considering which pixel
; the RA and DEC corresponded to.
;    get_coords,coords,instring=(SXPAR(hd,"RA")+" "+SXPAR(hd,"DEC"))
;    imra[ii] = coords[0]*15.0 ; we want it in degrees
;    imdec[ii] = coords[1]
  ENDFOR

  totdist = FLTARR(num_files)
  FOR ii = 0,num_files-1 DO BEGIN
    distang = sphdist(imra,imdec,imra[ii],imdec[ii],/degrees)
    totdist[ii] = TOTAL(distang^2)
  ENDFOR

; Originally, we had just used a mean RA,Dec, and took spherical
; distances from that point.  Unfortunately, if the target overlapped
; RA=0, this could cause big math problems.

  junk = MIN(totdist,middle)

  RETURN,imlist[middle]
END
