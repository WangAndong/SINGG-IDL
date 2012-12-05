PRO run_flux,DIR=dir,CATALOG=catalog,SKIP=skip,NOEXTRA=noextra,FAST=fast
; OPTIONAL INPUTS
; dir          Run directory to execute scripts off of; input will be
;               Proc4/Jwhatever, output will be Proc4/Jwhatever/.
; catalog      Catalog file to use; if not set, there can only be one located
;              in this directory.  This file must be of a specific format:
;                1> Name of the directory (see above)
;                2> Name of the narrow-band filter used
;                3-5> Names of the three combined images, usually J*_R.fits,
;                     J*_6???.fits, and J*_Rsub.fits
;                6> Name of the ellipse file, containing endpoints for the
;                   major and minor axes as well as the name of the reference
;                   image used.
;                7> Object number of the reference image
;                8-9> Names of the two mask images (R and narrow), usually
;                     basic_mask.fits for both
; /skip         If the profile files exist, don't redo this one.  Check Rsub
; /noextra      Don't measure flux from objects outside the main aperture.
; /fast         Bypass interactive mode

  IF NOT KEYWORD_SET(dir) THEN BEGIN
    spawn,'pwd',cdir
    dir = cdir[0]
  ENDIF
  IF STRMID(dir,0,1,/REVERSE_OFFSET) NE '/' THEN dir = dir+'/'

; Run from where the catalog files are.
  IF KEYWORD_SET(catalog) THEN BEGIN
    IF NOT FILE_TEST(catalog) THEN BEGIN
      catalog = dir+'Proc3/'+catalog
      IF NOT FILE_TEST(catalog) THEN BEGIN
        PRINT,"ERROR in run_flux: specified catalog file does not exist"
        RETURN
      ENDIF
    ENDIF
  ENDIF ELSE BEGIN
    spawn,"ls "+dir+"Proc3/Run*.catalog",catlist

    IF N_ELEMENTS(catlist) NE 1 THEN BEGIN
      PRINT,"ERROR in run_flux: Need one and only one catalog file in the directory",N_ELEMENTS(catlist)
      RETURN
    ENDIF
    catalog = catlist[0]
  ENDELSE

  flag = 0
  IF KEYWORD_SET(noextra) THEN flag = flag + 10
  IF KEYWORD_SET(fast) THEN flag = flag + 1

  read_catalog,catalog,run_struct,object,filter,Rfile,Nfile,Sfile, $
                       ellipse,refnum,Rmask,Nmask,nsig,/SILENT

  FOR ii = 0,N_ELEMENTS(object)-1 DO BEGIN
    PRINT,'Totalling flux for object: '+STRTRIM(object[ii],2)+ $
       ' ('+STRTRIM(STRING(ii+1),2)+'/'+STRTRIM(STRING(N_ELEMENTS(object)),2)+')'

    Rssfile = STRMID(Rfile[ii],0,STRLEN(Rfile[ii])-5)+"_ss.fits"
    Nssfile = STRMID(Nfile[ii],0,STRLEN(Nfile[ii])-5)+"_ss.fits"
    Sssfile = STRMID(Sfile[ii],0,STRLEN(Sfile[ii])-5)+"_ss.fits"

    indir = dir+'/Proc4/'+object[ii]+'/'
    outdir = dir+'/Proc4/'+object[ii]+'/'

    Sisofile = STRMID(Sfile[ii],0,STRLEN(Sfile[ii])-5)+"_ss_isophote.profile"
    Sbrtfile = STRMID(Sfile[ii],0,STRLEN(Sfile[ii])-5)+"_ss_brightness.profile"
    IF KEYWORD_SET(skip) $
       AND FILE_TEST(outdir+Sisofile) AND FILE_TEST(outdir+Sbrtfile) THEN BEGIN
       PRINT,"Object "+object[ii]+" already processed, bypassing"
    ENDIF ELSE BEGIN
      CASE flag OF
      0: ssflux,Rfile=Rssfile,Nfile=Nssfile,Sfile=Sssfile,/units, $
               ellipse=ellipse[ii],indir=indir,outdir=outdir
      1: ssflux,Rfile=Rssfile,Nfile=Nssfile,Sfile=Sssfile,/units, $
               ellipse=ellipse[ii],indir=indir,outdir=outdir,/fast
      10: ssflux,Rfile=Rssfile,Nfile=Nssfile,Sfile=Sssfile,/units, $
               ellipse=ellipse[ii],indir=indir,outdir=outdir,/noextra
      11: ssflux,Rfile=Rssfile,Nfile=Nssfile,Sfile=Sssfile,/units, $
               ellipse=ellipse[ii],indir=indir,outdir=outdir,/noextra,/fast
      ENDCASE
    ENDELSE
  ENDFOR

  CLOSE,/ALL

  RETURN

END
