PRO run_combine,DIR=dir,CATALOG=catalog,OUTPUT=output, $
                FORCE=force,BUFFER=buffer,SKIP=skip
; OPTIONAL INPUTS
; dir          Run directory to execute scripts off of; input will be
;               Proc2/, output will be Proc3/Jwhatever/.
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
; /output      If set, it'll dump J???????_combine.dat into each directory,
;                containing weighting factors, rough sky levels, and so on.
; /force       Pass /force on to singg_combine, which causes it to rerun the
;               WCStan and imregister scripts, whether they need it or not.
; buffer       Amount to pad each edge of the images by.  In general, each 
;               image is shifted by ~140 pixels, so buffer=150 is a good start.
; /skip        If the combined files exist, don't overwrite, and move on.

  IF NOT KEYWORD_SET(dir) THEN BEGIN
    spawn,'pwd',cdir
    dir = cdir[0]
  ENDIF
  IF STRMID(dir,0,1,/REVERSE_OFFSET) NE '/' THEN dir = dir+'/'

; Run from the run's root directory, where the catalog files are.
  IF KEYWORD_SET(catalog) THEN BEGIN
    IF NOT FILE_TEST(catalog) THEN BEGIN
      catalog = dir+'Proc3/'+catalog
      IF NOT FILE_TEST(catalog) THEN BEGIN
        PRINT,"ERROR in run_combine: specified catalog file does not exist"
        RETURN
      ENDIF
    ENDIF
  ENDIF ELSE BEGIN
    spawn,'ls '+dir+'Run*.catalog',catlist

    IF N_ELEMENTS(catlist) NE 1 THEN BEGIN
      PRINT,"ERROR in run_combine: Need one and only one catalog file in the directory",N_ELEMENTS(catlist)
      RETURN
    ENDIF
    catalog = catlist[0]
  ENDELSE

  read_catalog,catalog,run_struct,object,filter,Rfile,Nfile,Sfile, $
                       ellipse,refnum,Rmask,Nmask,nsig,/SILENT

; Let's face it, we're pretty much ALWAYS going to be using buffer=150.
  IF NOT KEYWORD_SET(buffer) THEN buffer=150

  FOR ii = 0,N_ELEMENTS(object)-1 DO BEGIN
    PRINT,'Combining images for object: '+STRTRIM(object[ii],2)+ $
       ' ('+STRTRIM(STRING(ii+1),2)+'/'+STRTRIM(STRING(N_ELEMENTS(object)),2)+')'

    flag = 0
    IF STRMID(Rfile[ii],STRLEN(Rfile[ii])-6,1) EQ "C" THEN flag = flag + 100
    IF KEYWORD_SET(output) THEN flag = flag + 10
    IF KEYWORD_SET(force) THEN flag = flag + 1

    odir = dir+'Proc3/'+STRTRIM(object[ii],2)+'/'
    IF KEYWORD_SET(skip) AND FILE_TEST(odir+Rfile[ii]) AND $
       FILE_TEST(odir+Nfile[ii]) AND FILE_TEST(odir+Sfile[ii]) THEN BEGIN
      PRINT,"Object ",object[ii]," has already been combined, skipping"
    ENDIF ELSE BEGIN
      refimage = "obj"+refnum[ii]+".fits"

      CASE flag OF
      0: singg_combine,filter=filter[ii],refimage=refimage,buffer=buffer, $
                       rfile=Rfile[ii],nfile=Nfile[ii],sfile=Sfile[ii], $
                       indir=dir+'Proc2/',outdir=odir,scale=run_struct.pixsize, $
                       rmask=Rmask[ii],nmask=Nmask[ii], $
                       nsig=nsig[ii]
      1: singg_combine,filter=filter[ii],refimage=refimage,buffer=buffer, $
                       rfile=Rfile[ii],nfile=Nfile[ii],sfile=Sfile[ii], $
                       indir=dir+'Proc2/',outdir=odir,scale=run_struct.pixsize, $
                       rmask=Rmask[ii],nmask=Nmask[ii], $
                       nsig=nsig[ii],/force
      10: singg_combine,filter=filter[ii],refimage=refimage,buffer=buffer, $
                       rfile=Rfile[ii],nfile=Nfile[ii],sfile=Sfile[ii], $
                       indir=dir+'Proc2/',outdir=odir,scale=run_struct.pixsize, $
                       rmask=Rmask[ii],nmask=Nmask[ii], $
                       nsig=nsig[ii],/output
      11: singg_combine,filter=filter[ii],refimage=refimage,buffer=buffer, $
                       rfile=Rfile[ii],nfile=Nfile[ii],sfile=Sfile[ii], $
                       indir=dir+'Proc2/',outdir=odir,scale=run_struct.pixsize, $
                       rmask=Rmask[ii],nmask=Nmask[ii], $
                       nsig=nsig[ii],/force,/output
      100: singg_combine,filter=filter[ii],refimage=refimage,buffer=buffer, $
                       rfile=Rfile[ii],nfile=Nfile[ii],sfile=Sfile[ii], $
                       indir=dir+'Proc2/',outdir=odir,scale=run_struct.pixsize, $
                       rmask=Rmask[ii],nmask=Nmask[ii], $
                       nsig=nsig[ii],/cont
      101: singg_combine,filter=filter[ii],refimage=refimage,buffer=buffer, $
                       rfile=Rfile[ii],nfile=Nfile[ii],sfile=Sfile[ii], $
                       indir=dir+'Proc2/',outdir=odir,scale=run_struct.pixsize, $
                       rmask=Rmask[ii],nmask=Nmask[ii], $
                       nsig=nsig[ii],/force,/cont
      110: singg_combine,filter=filter[ii],refimage=refimage,buffer=buffer, $
                       rfile=Rfile[ii],nfile=Nfile[ii],sfile=Sfile[ii], $
                       indir=dir+'Proc2/',outdir=odir,scale=run_struct.pixsize, $
                       rmask=Rmask[ii],nmask=Nmask[ii], $
                       nsig=nsig[ii],/output,/cont
      111: singg_combine,filter=filter[ii],refimage=refimage,buffer=buffer, $
                       rfile=Rfile[ii],nfile=Nfile[ii],sfile=Sfile[ii], $
                       indir=dir+'Proc2/',outdir=odir,scale=run_struct.pixsize, $
                       rmask=Rmask[ii],nmask=Nmask[ii], $
                       nsig=nsig[ii],/force,/output,/cont
      ENDCASE
    ENDELSE

  ENDFOR

  CLOSE,/ALL

  RETURN

END
