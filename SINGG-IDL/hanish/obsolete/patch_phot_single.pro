PRO patch_phot_single
; patches the photometry keywords for all images in the directory

  photdb = "singg_phot"

  dbopen,photdb,0
  dbext,-1,"RUNNAME,OBJECT,FILENAME,FILTNAME,IMTYPE,PHOTPLAM,PHOTGW,PHOTRW,PHOTMTRC,", $
            runname,object,filename,FILTNAME,imtype,photplam,photgw,photrw,photmtrc
  dbext,-1,"PHOTFLAM,PHOTFLUX,AQFILT,NIIFRAC,NIIRAT0,PROFMOD,MAGZPT1,MAGSYS1",$
            photflam,photflux,aqfilt,niifrac,niirat0,profmod,magzpt1,magsys1
  dbclose

  object = update_name(object)
  
  filt_rddbfnames,"filter",fnamarr

  spawn,"ls *ss.fits",imlist
  FOR ii = 0,N_ELEMENTS(imlist)-1 DO BEGIN
    index = WHERE(STRTRIM(filename,2) EQ STRTRIM(imlist[ii],2),count)
    IF count EQ 1 THEN BEGIN
      fits_read,imlist[ii],img,hd

      filt = singg_filtnam(fnamarr,filtname[index[0]],pos)
      SXADDPAR,hd,"FILTNAME",filt[0],"Filter name"
      SXADDPAR,hd,"PHOTPLAM",photplam[index[0]],"Filter pivot wavelength [Angstrom]"
      SXADDPAR,hd,"PHOTGW",photgw[index[0]],"Filter Gauss. equiv. bandpass FWHM [Angstrom]"
      SXADDPAR,hd,"PHOTRW",photrw[index[0]],"Filter rectangular equiv. bandpass [Angstrom]"
      SXADDPAR,hd,"PHOTMTRC",photmtrc[index[0]],"Observed in photometric conditions?"

      IF STRTRIM(imtype[index[0]],2) NE "net" THEN BEGIN
        SXADDPAR,hd,"PHOTFLAM",photflam[index[0]],"Unit response [erg/cm^2/Angstrom/DN]"
      ENDIF ELSE BEGIN
        SXADDPAR,hd,"PHOTFLUX",photflux[index[0]],"Unit response [erg/cm^2/DN]"
      ENDELSE

      SXADDPAR,hd,"AQFILT",aqfilt[index[0]],"A_file * Q_(filt,gal) [Angstrom]"
      SXADDPAR,hd,"NIIFRAC",niifrac[index[0]],"Estimated [NII]6548+6583/total count ratio"
      SXADDPAR,hd,"NIIRAT0",niirat0[index[0]],"Assumed intrinsic [NII]6583/Halpha flux ratio"
      SXADDPAR,hd,"PROFMOD",profmod[index[0]],"Adopted Halpha profile model"
      SXADDPAR,hd,"MAGZPT1",magzpt1[index[0]],"Zero-point magnitude"
      SXADDPAR,hd,"MAGSYS1",magsys1[index[0]],"Magnitude system"

      fits_write,imlist[ii],img,hd

    ENDIF ELSE BEGIN
      PRINT,"ERROR: wrong number of matches ",imlist[ii],count
    ENDELSE
  ENDFOR
END
