PRO patch_phot
; Copies the photometry information from the backup database to the
; current files

  photdb = "singg_phot"

  dbopen,photdb,0
  dbext,-1,"RUNNAME,OBJECT,FILENAME,FILTNAME,IMTYPE,PHOTPLAM,PHOTGW,PHOTRW,PHOTMTRC,", $
            runname,object,filename,filtname,imtype,photplam,photgw,photrw,photmtrc
  dbext,-1,"PHOTFLAM,PHOTFLUX,AQFILT,NIIFRAC,NIIRAT0,PROFMOD,MAGZPT1,MAGSYS1",$
            photflam,photflux,aqfilt,niifrac,niirat0,profmod,magzpt1,magsys1
  dbclose

;;  object = update_name(object)

  filt_rddbfnames,"filter",fnamarr

  FOR ii = 0,N_ELEMENTS(object)-1 DO BEGIN
;; Fix: many directories no longer have the same name as the DB name
    obj = "j"+STRMID(object[ii],1,STRLEN(object[ii])-1)
    file = "/home/hanish/data/"+STRTRIM(runname[ii],2)+ $
               "/"+STRTRIM(obj,2)+"/"+STRTRIM(filename[ii],2)
    PRINT,file
    fits_read,file,img,hd

    filt = singg_filtnam(fnamarr,filtname[ii],pos)
    SXADDPAR,hd,"FILTNAME",STRTRIM(filt[0],2),"Filter name"
    SXADDPAR,hd,"PHOTPLAM",photplam[ii],"Filter pivot wavelength [Angstrom]"
    SXADDPAR,hd,"PHOTGW",photgw[ii],"Filter Gauss. equiv. bandpass FWHM [Angstrom]"
    SXADDPAR,hd,"PHOTRW",photrw[ii],"Filter rectangular equiv. bandpass [Angstrom]"
    SXADDPAR,hd,"PHOTMTRC",photmtrc[ii],"Observed in photometric conditions?"

    IF STRTRIM(imtype[ii],2) NE "net" THEN BEGIN
      SXADDPAR,hd,"PHOTFLAM",photflam[ii],"Unit response [erg/cm^2/Angstrom/DN]"
    ENDIF ELSE BEGIN
      SXADDPAR,hd,"PHOTFLUX",photflux[ii],"Unit response [erg/cm^2/DN]"
    ENDELSE

    SXADDPAR,hd,"AQFILT",aqfilt[ii],"A_file * Q_(filt,gal) [Angstrom]"
    SXADDPAR,hd,"NIIFRAC",niifrac[ii],"Estimated [NII]6548+6583/total count ratio"
    SXADDPAR,hd,"NIIRAT0",niirat0[ii],"Assumed intrinsic [NII]6583/Halpha flux ratio"
    SXADDPAR,hd,"PROFMOD",profmod[ii],"Adopted Halpha profile model"
    SXADDPAR,hd,"MAGZPT1",magzpt1[ii],"Zero-point magnitude"
    SXADDPAR,hd,"MAGSYS1",magsys1[ii],"Magnitude system"

    fits_write,file,img,hd
    CLOSE,/ALL
  ENDFOR

  RETURN
END
