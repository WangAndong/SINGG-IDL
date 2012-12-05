PRO run_sky,DIR=dir,CATALOG=catalog,OUTPUT=output,FAST=fast, $
            BOXDUMP=boxdump,SKIP=skip,BW=bw
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
; output       The name of the run's output file, to which the final sky
;              levels and uncertainties are dumped.  This is in the same 
;              directory as the catalog.  If not set, there is no output other
;              than the sky-subtracted images.
; /fast        If set, skips interactive mode.
; /boxdump     If set, three data files containing the sky levels in each 35x35
;              box are dumped to each directory.  They will be named, by
;              default, J*_R.box, J*_6XXX.box, and J*_Rsub.box.
;              This also turns on the /nofits flag, so it won't create the
;              .fits files.
; /skip        If the _ss files already exist, don't overwrite, and move on.

  IF NOT KEYWORD_SET(dir) THEN BEGIN
    spawn,'pwd',cdir
    dir = cdir[0]
  ENDIF
  IF STRMID(dir,0,1,/REVERSE_OFFSET) NE '/' THEN dir = dir+'/'
  p3dir = dir+'Proc3/'
  p4dir = dir+'Proc4/'

  IF KEYWORD_SET(catalog) THEN BEGIN
    IF NOT FILE_TEST(catalog) THEN BEGIN
      catalog = p3dir+catalog
      IF NOT FILE_TEST(catalog) THEN BEGIN
        PRINT,"ERROR in run_sky: specified catalog file does not exist"
        RETURN
      ENDIF
    ENDIF
  ENDIF ELSE BEGIN
    spawn,'ls '+p3dir+'Run*.catalog',catlist

    IF N_ELEMENTS(catlist) NE 1 THEN BEGIN
      PRINT,"ERROR in run_sky: Need one and only one catalog file in the directory",N_ELEMENTS(catlist)
      RETURN
    ENDIF
    catalog = catlist[0]
  ENDELSE

  read_catalog,catalog,run_struct,object,filter,Rfile,Nfile,Sfile, $
                       ellipse,refnum,Rmask,Nmask,nsig,/SILENT

; We need a call to sky_calibration, but it'll depend on which of the
; flags you picked.  Technically, any of the them can be combined, although
; there are a few redundant combinations.

; So, we'll do this in pseudo-binary.  "BW" is the 1000s place, "Output" is 
; the 100s place, "fast" is the 10s, and "boxdump" is the 1s.  It's messy, but
; it's easier than writing a massive IF-ELSE tree.

  flag = 0
  IF KEYWORD_SET(bw) THEN flag = flag + 1000
  IF KEYWORD_SET(output) THEN flag = flag + 100
  IF KEYWORD_SET(fast) THEN flag = flag + 10
  IF KEYWORD_SET(boxdump) THEN flag = flag + 1

; Note that "output" is the only one requiring an actual argument, the others
; are toggles. 
; We also turn /noplot on if /fast is on.  On your first pass
; through, you should always have /fast turned off, but any later time (where
; you're not actually in interactive mode), it's annoying to have those things
; pop up when you're trying to do other work.

  IF KEYWORD_SET(output) THEN tempout = "../"+STRTRIM(output,2)

  FOR ii = 0,N_ELEMENTS(object)-1 DO BEGIN
    PRINT,'Processing sky for object: '+STRTRIM(object[ii],2)+ $
       ' ('+STRTRIM(STRING(ii+1),2)+'/'+STRTRIM(STRING(N_ELEMENTS(object)),2)+')'

    Rssfile = STRMID(Rfile[ii],0,STRLEN(Rfile[ii])-5)+"_ss.fits"
    Nssfile = STRMID(Nfile[ii],0,STRLEN(Nfile[ii])-5)+"_ss.fits"
    Sssfile = STRMID(Sfile[ii],0,STRLEN(Sfile[ii])-5)+"_ss.fits"

    indir = p3dir+object[ii]+'/'
    outdir = p4dir+object[ii]+'/'

    IF KEYWORD_SET(skip) AND FILE_TEST(outdir+Rssfile) AND $
       FILE_TEST(outdir+Nssfile) AND FILE_TEST(outdir+Sssfile) THEN BEGIN
      PRINT," Object ",object[ii]," has already been sky-subtracted, skipping"
    ENDIF ELSE BEGIN
      CASE flag OF
      0: sky_calibration,indir=indir,outdir=outdir, $
         Rfile=Rfile[ii],Nfile=Nfile[ii],Sfile=Sfile[ii],ellipse=ellipse[ii]
      1: sky_calibration,indir=indir,outdir=outdir, $
         Rfile=Rfile[ii],Nfile=Nfile[ii],Sfile=Sfile[ii],ellipse=ellipse[ii],$
         /boxdump,/nofits
      10: sky_calibration,indir=indir,outdir=outdir,/fast, $
         Rfile=Rfile[ii],Nfile=Nfile[ii],Sfile=Sfile[ii],ellipse=ellipse[ii]
      11: sky_calibration,indir=indir,outdir=outdir,/fast, $
         Rfile=Rfile[ii],Nfile=Nfile[ii],Sfile=Sfile[ii],ellipse=ellipse[ii],$
         /boxdump,/nofits
      100: sky_calibration,indir=indir,outdir=outdir, $
         Rfile=Rfile[ii],Nfile=Nfile[ii],Sfile=Sfile[ii],ellipse=ellipse[ii],$
         output=tempout
      101: sky_calibration,indir=indir,outdir=outdir, $
         Rfile=Rfile[ii],Nfile=Nfile[ii],Sfile=Sfile[ii],ellipse=ellipse[ii],$
         output=tempout,/boxdump,/nofits
      110: sky_calibration,indir=indir,outdir=outdir,/fast, $
         Rfile=Rfile[ii],Nfile=Nfile[ii],Sfile=Sfile[ii],ellipse=ellipse[ii],$
         output=tempout
      111: sky_calibration,indir=indir,outdir=outdir,/fast, $
         Rfile=Rfile[ii],Nfile=Nfile[ii],Sfile=Sfile[ii],ellipse=ellipse[ii],$
         output=tempout,/boxdump,/nofits
      1000: sky_calibration,indir=indir,outdir=outdir,/bw, $
         Rfile=Rfile[ii],Nfile=Nfile[ii],Sfile=Sfile[ii],ellipse=ellipse[ii]
      1001: sky_calibration,indir=indir,outdir=outdir,/bw, $
         Rfile=Rfile[ii],Nfile=Nfile[ii],Sfile=Sfile[ii],ellipse=ellipse[ii],$
         /boxdump,/nofits
      1010: sky_calibration,indir=indir,outdir=outdir,/bw,/fast, $
         Rfile=Rfile[ii],Nfile=Nfile[ii],Sfile=Sfile[ii],ellipse=ellipse[ii]
      1011: sky_calibration,indir=indir,outdir=outdir,/bw,/fast, $
         Rfile=Rfile[ii],Nfile=Nfile[ii],Sfile=Sfile[ii],ellipse=ellipse[ii],$
         /boxdump,/nofits
      1100: sky_calibration,indir=indir,outdir=outdir,/bw, $
         Rfile=Rfile[ii],Nfile=Nfile[ii],Sfile=Sfile[ii],ellipse=ellipse[ii],$
         output=tempout
      1101: sky_calibration,indir=indir,outdir=outdir,/bw, $
         Rfile=Rfile[ii],Nfile=Nfile[ii],Sfile=Sfile[ii],ellipse=ellipse[ii],$
         output=tempout,/boxdump,/nofits
      1110: sky_calibration,indir=indir,outdir=outdir,/bw,/fast, $
         Rfile=Rfile[ii],Nfile=Nfile[ii],Sfile=Sfile[ii],ellipse=ellipse[ii],$
         output=tempout
      1111: sky_calibration,indir=indir,outdir=outdir,/bw,/fast, $
         Rfile=Rfile[ii],Nfile=Nfile[ii],Sfile=Sfile[ii],ellipse=ellipse[ii],$
         output=tempout,/boxdump,/nofits
      ENDCASE
    ENDELSE
  ENDFOR

  CLOSE,/ALL

  RETURN

END
