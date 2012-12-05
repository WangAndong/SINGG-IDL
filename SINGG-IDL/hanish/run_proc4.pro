PRO run_proc4,DIR=dir,CATALOG=catalog,SKIP=skip,FAST=fast
; OPTIONAL INPUTS
; dir          Run directory to execute scripts off of; input will be
;               Proc3/Jwhatever, output will be Proc4/Jwhatever/.
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
; /skip         If the ellipse and mask files exist, don't redo this one.
; /fast         Bypass interactive mode

  IF NOT KEYWORD_SET(dir) THEN BEGIN
    spawn,'pwd',cdir
    dir = cdir[0]
  ENDIF
  IF STRMID(dir,0,1,/REVERSE_OFFSET) NE '/' THEN dir = dir+'/'

  IF KEYWORD_SET(catalog) THEN BEGIN
    IF NOT FILE_TEST(catalog) THEN BEGIN
      catalog = dir+catalog
      IF NOT FILE_TEST(catalog) THEN BEGIN
        PRINT,"ERROR in run_proc4: specified catalog file does not exist"
        RETURN
      ENDIF
    ENDIF
  ENDIF ELSE BEGIN
    spawn,"ls "+dir+"Run*.catalog",catlist

    IF N_ELEMENTS(catlist) NE 1 THEN BEGIN
      PRINT,"ERROR in run_proc4: Need one and only one catalog file in the directory",N_ELEMENTS(catlist)
      RETURN
    ENDIF
    catalog = catlist[0]
  ENDELSE

  read_catalog,catalog,run_struct,object,filter,Rfile,Nfile,Sfile, $
                       ellipse,refnum,Rmask,Nmask,nsig,/SILENT

  FOR ii = 0,N_ELEMENTS(object)-1 DO BEGIN
    PRINT,'Setting up masks and ellipses for object: '+STRTRIM(object[ii],2)+ $
       ' ('+STRTRIM(STRING(ii+1),2)+'/'+STRTRIM(STRING(N_ELEMENTS(object)),2)+')'

    spawn,'ls '+dir+'/Proc3/'+object[ii]+'/J*_?sub.fits',Slist
    IF N_ELEMENTS(Slist) GT 1 THEN Rbase=STRMID(Rfile[ii],0,STRLEN(Rfile[ii])-5) $
                              ELSE Rbase=STRMID(Rfile[ii],0,STRLEN(Rfile[ii])-7)

    maskfile = Rbase+'_mask.fits'
    Smaskfile = STRMID(Sfile[ii],0,STRLEN(Sfile[ii])-5)+'_mask.fits'
    Imaskfile = Rbase+'_inc_mask.fits'

    indir = dir+'/Proc3/'+object[ii]+'/'
    outdir = dir+'/Proc4/'+object[ii]+'/'

    IF KEYWORD_SET(skip) AND FILE_TEST(outdir+maskfile) AND FILE_TEST(outdir+Imaskfile) AND $
          FILE_TEST(outdir+Smaskfile) AND FILE_TEST(outdir+ellipse[ii]) THEN BEGIN
       PRINT," Object "+object[ii]+" already processed, bypassing"
    ENDIF ELSE BEGIN
      IF KEYWORD_SET(fast) THEN BEGIN
        proc4,RFILE=Rfile[ii],NFILE=Nfile[ii],SFILE=Sfile[ii], $
              ELLIPSE=ellipse[ii],INDIR=indir,OUTDIR=outdir,/SILENT,/FORCE
      ENDIF ELSE BEGIN
        proc4,RFILE=Rfile[ii],NFILE=Nfile[ii],SFILE=Sfile[ii], $
              ELLIPSE=ellipse[ii],INDIR=indir,OUTDIR=outdir
      ENDELSE
    ENDELSE

  ENDFOR

  CLOSE,/ALL

  RETURN

END
