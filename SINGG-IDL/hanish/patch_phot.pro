PRO patch_phot,dir
; Takes an existing object, reads the .profile files, and replaces the
; photometry line with a new value extracted from the appropriate
; image header.

  spawn,'ls '+dir+'*.profile',proflist
  temp = ''

  IF NOT FILE_TEST(proflist[0]) THEN BEGIN
    PRINT,'ERROR in patch_phot: no profile files exist ',dir
    RETURN
  ENDIF

  FOR ii = 0,N_ELEMENTS(proflist)-1 DO BEGIN
    doneflag = 0b
    fscale = 0.0
    filename = ''
    outfile = STRTRIM(proflist[ii],2)+'_bak'

    nlines = NUMLINES(proflist[ii])
    OPENR,lin,proflist[ii],/GET_LUN
    OPENW,lout,outfile,/GET_LUN
    FOR jj = 0,nlines-1 DO BEGIN
      READF,lin,temp
      IF STRPOS(temp,'FILENAME') GT 0 THEN BEGIN
        eqpos = STRPOS(temp,'=')
        fitspos = STRPOS(temp,'.fits')
        filename = STRMID(temp,eqpos+2,(fitspos-eqpos+3))
      ENDIF

      IF STRPOS(temp,'FLUX_SCALE') GT 0 THEN BEGIN
        IF STRLEN(filename) LT 1 THEN BEGIN
          PRINT,'ERROR in patch_phot: missing file name ',proflist[ii]
        ENDIF

        fits_read,STRTRIM(dir,2)+STRTRIM(filename,2),img,hd,/header_only
        imtype = SXPAR(hd,'IMTYPE')
        IF STRTRIM(imtype,2) EQ 'net' THEN fscale = SXPAR(hd,'PHOTFLUX') $
                                      ELSE fscale = SXPAR(hd,'PHOTFLAM')
        PRINTF,lout,'# FLUX_SCALE = '+STRTRIM(STRING(fscale),2)+' / Flux scale [above units / (DN/sec)]',FORMAT='(A)'
      ENDIF ELSE BEGIN
        PRINTF,lout,temp,FORMAT='(A)'
      ENDELSE

    ENDFOR
    CLOSE,lin
    CLOSE,lout
    FREE_LUN,lin
    FREE_LUN,lout
    spawn,'mv -f '+outfile+' '+STRTRIM(proflist[ii],2)
  ENDFOR

  RETURN
END
