PRO patch_cntrat,RFILE=Rfile,NFILE=Nfile,SFILE=Sfile, $
                 FILTER=filter,IMSIZE=imsize,BUFFER=buffer
; For a given run, goes through and recalculates cntrat2 and ecntrat2

  edge = 50
  band = 200
  IF NOT KEYWORD_SET(buffer) THEN buffer = 150
  IF NOT KEYWORD_SET(imsize) THEN imsize = [2048,2048]
  imsize = imsize+(2*buffer)

  IF KEYWORD_SET(Rfile) THEN BEGIN $
    IF NOT FILE_TEST(Rfile) THEN BEGIN
      PRINT,"ERROR in sky_calibration: R file not found. ",Rfile
      RETURN
    ENDIF
  ENDIF ELSE BEGIN
; Rfile wasn't specified, so we need to find it.
; Assume Rfile will be of the form *_R.fits.
    spawn,"ls J*_R.fits",Rlist
    IF NOT FILE_TEST(Rlist[0]) THEN spawn,"ls J*_C.fits",Rlist

    IF N_ELEMENTS(Rlist) GT 1 THEN BEGIN
      PRINT,"ERROR in patch_cntrat: Multiple combined R-band images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the RFILE keyword explicitly."
      RETURN
    ENDIF

    IF NOT FILE_TEST(Rlist[0]) THEN BEGIN
      PRINT,"ERROR in patch_cntrat: No combined R-band images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the RFILE keyword explicitly."
      RETURN
    ENDIF

    Rfile = Rlist[0]
    PRINT,"R image used: ",Rfile
  ENDELSE
  object = "j"+STRTRIM(STRMID(Rfile,1,STRLEN(Rfile)-8),2)

; For narrow-band, it's a bit more complex since there could be multiple
; narrow-band filters used for this object.
  IF KEYWORD_SET(Nfile) THEN BEGIN $
    IF NOT FILE_TEST(Nfile) THEN BEGIN
      PRINT,"ERROR in patch_cntrat: Narrow-band file not found. ",Nfile
      RETURN
    ENDIF
  ENDIF ELSE BEGIN
; Nfile wasn't specified, so we need to find it.
; Assume Nfile will be of the form *_6XXX.fits.
    spawn,"ls J*_6???.fits",Nlist

    IF N_ELEMENTS(Nlist) GT 1 THEN BEGIN
      PRINT,"ERROR in patch_cntrat: Multiple combined narrow-band images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the CFILE keyword explicitly."
      RETURN
    ENDIF

    IF NOT FILE_TEST(Nlist[0]) THEN BEGIN
      PRINT,"ERROR in patch_cntrat: No combined narrow-band images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the CFILE keyword explicitly."
      RETURN
    ENDIF

    Nfile = Nlist[0]
    PRINT,"Narrow-band image used: ",Nfile
  ENDELSE
  IF NOT KEYWORD_SET(filter) THEN filter = STRMID(Nfile,STRLEN(Nfile)-9,4)

  IF KEYWORD_SET(Sfile) THEN BEGIN $
    IF NOT FILE_TEST(Sfile) THEN BEGIN
      PRINT,"ERROR in patch_cntrat: Subtracted file not found. ",Sfile
      RETURN
    ENDIF
  ENDIF ELSE BEGIN
; Sfile wasn't specified, so we need to find it.
; Assume Sfile will be of the form *_Rsub.fits.
    spawn,"ls J*_Rsub.fits",Slist

    IF N_ELEMENTS(Slist) GT 1 THEN BEGIN
      PRINT,"ERROR in patch_cntrat: Multiple combined subtracted images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the SFILE keyword explicitly."
      RETURN
    ENDIF

    IF NOT FILE_TEST(Slist[0]) THEN BEGIN
      PRINT,"ERROR in patch_cntrat: No combined subtracted images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the SFILE keyword explicitly."
      RETURN
    ENDIF

    Sfile = Slist[0]
    PRINT,"Subtracted image used: ",Sfile
  ENDELSE

; Now that we've got the three files straightened out, patch them
  fits_read,Rfile,Rimg,Rhd
  fits_read,Nfile,Nimg,Nhd
  fits_read,Sfile,Simg,Shd

;  temp = SXPAR(Rhd,"FLUXREF")
;  Rreffile = STRMID(temp,0,STRLEN(temp)-7)+"sh.fits"
;  spawn,"gunzip "+STRTRIM(Rreffile,2)+".gz"
;  fits_read,Rreffile,tempimg,rfluxhd,/header_only
;  spawn,"gzip -f "+STRTRIM(Rreffile)
;  Rexptime = SXPAR(rfluxhd,"EXPTIME")
Rexptime = 120.0

;  temp = SXPAR(Nhd,"FLUXREF")
;  Nreffile = STRMID(temp,0,STRLEN(temp)-7)+"sh.fits"
;  spawn,"gunzip "+STRTRIM(Nreffile,2)+".gz"
;  fits_read,Nreffile,tempimg,cfluxhd,/header_only
;  spawn,"gzip -f "+STRTRIM(Nreffile)
;  Nexptime = SXPAR(cfluxhd,"EXPTIME")
Nexptime = 600.0

  Rssfile = STRMID(Rfile,0,STRLEN(Rfile)-5)+"_ss.fits"
  Nssfile = STRMID(Nfile,0,STRLEN(Nfile)-5)+"_ss.fits"
  Sssfile = STRMID(Sfile,0,STRLEN(Sfile)-5)+"_ss.fits"

  fits_read,Rssfile,junk,Rshd,/header_only
  fits_read,Nssfile,junk,Nshd,/header_only
  fits_read,Sssfile,Sssimg,Ssshd

  RWskysig = SXPAR(Rshd,"SKYSIG")*Rexptime
  NWskysig = SXPAR(Nshd,"SKYSIG")*Nexptime

  tempRfile = "Temporary_IDL_file_R.fits"
  tempNfile = "Temporary_IDL_file_"+filter+".fits"
  Rstarfile = tempRfile+'.stars'
  Nstarfile = tempNfile+'.stars'

  fits_write,tempRfile,Rimg*Rexptime,Rhd
  fits_write,tempNfile,Nimg*Nexptime,Nhd

  command = "runalard.pl "+tempRfile+" "+tempNfile
  spawn,command

; Since the R image is "brighter" than the narrow-band, use it as the reference
  del_M = calc_mdiff(Nstarfile,Rstarfile,NWskysig,RWskysig, $
                     imsize,(edge+buffer),sigma,nmatch)

; This gave us a magnitude difference and uncertainty; convert to a flux ratio 
; Since del_M should be positive, this'll make cntrat2<1
  cntrat2 = 10.0^(-0.4*del_M)
  ecntrat2 = cntrat2*ALOG(10.0)*0.4*sigma

; Add these to the Sfile header

  SXADDPAR,Shd,"CNTRAT2",cntrat2,' Continuum ratio'
  SXADDPAR,Shd,"ECNTRAT2",ecntrat2,' Continuum ratio RMS'

  SXADDPAR,Ssshd,"CNTRAT2",cntrat2,' Continuum ratio'
  SXADDPAR,Ssshd,"ECNTRAT2",ecntrat2,' Continuum ratio RMS'

  PRINT,"Object ",STRTRIM(object,2)," is now cntrat= "+STRTRIM(STRING(cntrat2),2)+ $
                " +/- "+STRTRIM(STRING(ecntrat2),2)

  fits_write,Sfile,Simg,Shd
  fits_write,Sssfile,Sssimg,Ssshd

  spawn,"/bin/rm -f Temporary_IDL_file*"

  RETURN

END
