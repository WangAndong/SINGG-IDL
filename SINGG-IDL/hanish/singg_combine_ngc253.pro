PRO singg_combine_ngc253,FILTER=filter,REFIMAGE=refimage, $
                  RFILE=Rfile,NFILE=Nfile,SFILE=Sfile, $
                  OUTPUT=output,FORCE=force, $
                  WELLSATUR=wellsatur,NSIG=nsig,NSTARS=nstars, $
                  HEADER_TEMPLATE=header_template, $
                  RMASK=Rmask,NMASK=Nmask,TRIM=trim,CONT=cont,SCALE=scale
; The script can be run as is, no arguments specified.  In general, though, 
; this should only be done if the imregister step has already been completed 
; for this directory.  Why?  Because to do imregister it's better to specify 
; which image will be used as the positional reference; the
; find_middle logic's not bad, but it IS an automated process.

; OPTIONAL INPUTS:
; filter           Name of the narrow-band filter, usually "6568" or similar, 
;                    and it must match an entry in the filter database.  If 
;                    not set, all non-R images used must have the same filter
;                    entry.  If set, narrow-band images without this filter 
;                    will be ignored.
; refimage         Which image to calibrate position with (imregister_mine.pl).
;                    If not set, the middle (numerically) R image will be used.
; Rfile            name of combined R image(if not set, J???????_R.fits)
; Nfile            name of narrow-band image(if not set, J???????_6???.fits)
; Sfile            name of the subtracted image(if not set, J???????_Rsub.fits)
;                  The only time these should be different is when multiple 
;                    filters are used on the same object, in which case this
;                    script must be run multiple times, which usually requires
;                    multiple lines in the catalog file.  While Rfile and Nfile
;                    can stay the same for the different filters, the Sfile
;                    needs clarification.
; /output          If set, the script will dump data to J???????_combine.dat
; /force           Normally the script will not perform the imregister step if
;                    the output files (sh.fits, sh.mask.fits, .match,
;                    .fits.stars) already exist.  This flag overrides that,
;                    forcing the script to reprocess all objects in the
;                    directory.  Note that if you decide to change the 
;                    reference image, you NEED to do this.
; wellsatur        Well saturation threshold to use.  If not set, it uses the 
;                    CCD saturation.  Note that bias correction will subtract
;                    a small amount from any image but SDflat correction can
;                    add or subtract a substantial amount thanks to the jump
;                    between the two sides, so it's easily possible for an
;                    image to actually saturate at 69-70k, or as low as 59k.
; header_template  Template for header reduction.  If not set, it uses a 
;                    hard-coded machine-specific default.
; Rmask            Basic exclusion masks made from SDflat files.  One for R, 
; Nmask              one for narrow-band images.
; /trim            When doing the runalard step, only explicitly use those 
;                    stars within the central (overlap) area.
; /cont            This is a 6850 ("cont") galaxy instead of an R one.  For the
;                    most part this doesn't change anything.
; scale            Override for WCStan's scaling.

; NOTE: "!singgdir" is an environmental variable that must be set as part of
; your ~/.idlstartup script.  Set it to a directory containing the template
; file, filter file, and so on.  It'll also check there for masks if the one
; you listed isn't in the run's /mask/ subdirectory.

; Make sure the memory is clear first, we'll need it.
  CLOSE,/ALL

  Rlist = INTARR(10) ; Index of R-band images
  Nlist = INTARR(10) ; Index of narrow-band images
  impos = STRARR(20)
  Image = STRARR(20) ; Name of the source images (all extensions removed)
  shfile = STRARR(20) ; obj*sh.fits
  maskfile = STRARR(20) ; obj*sh.mask.fits
  infile = STRARR(20) ; obj*.fits in Obj directory
  Rflag = BYTARR(20) ; Whether the image is an R image (1b)
                     ; or a narrow-band image (0b)
  num_R = 0
  num_narrow = 0
  object = ""
  edge = 50
  band = 200

  buffer = 500 ; 850 for full

  IF NOT KEYWORD_SET(nsig) THEN nsig = 30.0
  IF NOT KEYWORD_SET(nstars) THEN nstars = 50 ELSE nstars = LONG(nstars)

  ccdsatur = 59000   ; The CCD actually saturates at 65535, but above this
                     ; number don't use the pixel for image-to-image matching

  IF NOT KEYWORD_SET(wellsatur) THEN wellsatur = ccdsatur

  spawn,"pwd",cdir

  idir = '/data1/acs22/hanish/Run15/Proc2/'
  objdir = idir+'Obj/'
  refdir = idir+'Ref/'

  odir = '/data1/acs22/hanish/Run15/Proc3/J0047-25/'

; The following files shouldn't be hard-coded, but it's just so much easier.
; If you want to do this on any machine other than mine, change them.
  IF NOT KEYWORD_SET(header_template) THEN $
       header_template = !singgdir+"/hdr_template2.dat"
  IF NOT FILE_TEST(header_template) THEN BEGIN
    PRINT,"ERROR in singg_combine: header template not found ",header_template
    RETURN
  ENDIF

; When looking for masks, use the following order:
; 1> Present directory (or specified directory)
; 2> run's Proc2/Ref directory
; 3> !singgdir
; and if the keyword isn't set, use the run's default

  IF NOT KEYWORD_SET(Rmask) THEN Rmask = 'basic_mask.fits'

  IF NOT FILE_TEST(Rmask) THEN BEGIN
; The mask file wasn't found in the current directory (or whatever directory
; was specified)
    IF FILE_TEST(refdir+Rmask) THEN BEGIN
      Rmask = refdir+Rmask
    ENDIF ELSE BEGIN
; Mask file wasn't in the Ref directory either.
      IF FILE_TEST(!singgdir+"/"+Rmask) THEN BEGIN
        Rmask = !singgdir+"/"+Rmask
      ENDIF ELSE BEGIN
        PRINT,"ERROR in singg_combine: can't find R mask: ",Rmask
        RETURN
      ENDELSE
    ENDELSE
  ENDIF

  IF NOT KEYWORD_SET(Nmask) THEN Nmask = 'basic_mask.fits'

Nmask = '/data1/acs22/hanish/Run15/Proc2/Ref/mask_NGC253.fits'

  IF NOT FILE_TEST(Nmask) THEN BEGIN
; The mask file wasn't found in the current directory (or whatever directory
; was specified)
    IF FILE_TEST(refdir+Nmask) THEN BEGIN
      Nmask = refdir+Nmask
    ENDIF ELSE BEGIN
; Mask file wasn't in the Ref directory either.
      IF FILE_TEST(!singgdir+Nmask) THEN BEGIN
        Nmask = !singgdir+Nmask
      ENDIF ELSE BEGIN
        PRINT,"ERROR in singg_combine: can't find narrow-band mask: ",Nmask
        RETURN
      ENDELSE
    ENDELSE
  ENDIF

; Load filter table:
  filt_rddbfnames,"filter",fnamarr

  IF KEYWORD_SET(filter) THEN BEGIN
    filtername = singg_filtnam(fnamarr,filter,pos,/SILENT)
  ENDIF ELSE BEGIN
    filter = ""
    filtername = ""
  ENDELSE

; STEP 1: Figure out which object images to use for each filter.  

; The usual situation, where we use the .lis files to assemble the lists.
    Rlistfile = 'obj_R.lis'
    readcol_new,odir+Rlistfile,rtemp,rpos,FORMAT="A,A",COMMENT='#',/SILENT
; rtemp will be all the "obj???????.fits" names.
    num_R = N_ELEMENTS(rtemp)
    Rlist = INDGEN(num_R)

    impos[Rlist] = STRTRIM(rpos,2)
    FOR ii = 0,num_R-1 DO BEGIN
      Image[ii] = STRMID(rtemp[ii],0,STRLEN(rtemp[ii])-5)
      shfile[ii] = odir+Image[ii]+"sh.fits"
      infile[ii] = objdir+Image[ii]+".fits"
      maskfile[ii] = odir+Image[ii]+"sh.mask.fits"

      Rflag[ii]=1b
    ENDFOR
    fits_read,infile[0],img,hd,/header_only
    object = STRMID(SXPAR(hd,'TARGET'),0,8)

    Nlistfile = 'obj_6563.lis'
    readcol_new,odir+Nlistfile,ntemp,npos,format="A,A",comment='#',/silent
; ntemp will be all the "obj???????.fits" names.
    num_narrow = N_ELEMENTS(ntemp)
    Nlist = INDGEN(num_narrow)+num_R

    impos[Nlist] = STRTRIM(npos,2)
    FOR jj = 0,num_narrow-1 DO BEGIN
      ii = jj + num_R
      Image[ii] = STRMID(ntemp[jj],0,STRLEN(ntemp[jj])-5)
      shfile[ii] = odir+Image[ii]+"sh.fits"
      infile[ii] = objdir+Image[ii]+".fits"
      maskfile[ii] = odir+Image[ii]+"sh.mask.fits"
    ENDFOR
    n_files = num_R + num_narrow

  shgzfile = shfile+'.gz'
  maskgzfile = maskfile+'.gz'

  filter = '6563'

  PRINT,"Number of R images: ",num_R
  PRINT,"Number of narrow-band images: ",num_narrow
  PRINT,"Narrow-band filter: ",filter

  IF num_R EQ 0 THEN BEGIN
    PRINT,"ERROR in singg_combine: no R-band images found"
    RETURN
  ENDIF

  IF num_narrow EQ 0 THEN BEGIN
    PRINT,"ERROR in singg_combine: no narrow-band images match input filter"
    RETURN
  ENDIF

; In general we COULD read these from headers, but the rdnoise is just a 
; rough guess there anyway, and the gain will be incorrect thanks to our
; overscan correction algorithms.
  Rrdnoise = REPLICATE(3.1,num_R)
  Nrdnoise = REPLICATE(3.1,num_narrow)
  Rgain = REPLICATE(1.0,num_R)
  Ngain = REPLICATE(1.0,num_narrow)

; STEP 2: Run WCStan.pl on all input .fits files

; We have three reference images:
  refimage = 'obj1501098.fits'
  refim_NE = 'obj1501102.fits'
  refim_SW = 'obj1501108.fits'

  ind = WHERE(infile EQ objdir+refimage,count)
  IF count EQ 1 THEN posref_C = ind[0] ELSE PRINT,'ERROR!!!'
  ind = WHERE(infile EQ objdir+refim_NE,count)
  IF count EQ 1 THEN posref_NE = ind[0] ELSE PRINT,'ERROR!!!'
  ind = WHERE(infile EQ objdir+refim_SW,count)
  IF count EQ 1 THEN posref_SW = ind[0] ELSE PRINT,'ERROR!!!'

  PRINT,"Position matching image = ",refimage
  IF FILE_TEST(objdir+refimage) THEN fits_read,objdir+refimage,refimg,refhd $
                                ELSE refimg = readfits(objdir+refimage+'.gz',refhd,/SILENT)
  buffer2 = buffer + SXPAR(refhd,'BUFFER')
  sz = SIZE(refimg)
  imsize = [sz[1],sz[2]]+(2*buffer)

  IF NOT KEYWORD_SET(scale) THEN BEGIN
    telescope = SXPAR(refhd,'TELESCOP')
    CASE STRTRIM(telescope,2) OF
      'CTIO 0.9 meter telescope': scale = 0.396
      'CTIO 1.5 meter telescope': scale = 0.432
      'TEST': scale = 4.5655 ; the 2.3m has a completely different header.
      'CTIO/Michigan Curtis Schmidt': scale = 2.3184
      ELSE: BEGIN
            PRINT,"ERROR in singg_combine: invalid telescope name ",telescope
            RETURN
            END
    ENDCASE
  ENDIF

  FOR ii=0,n_files-1 DO BEGIN
; First, make sure the image has a WCS.

    starfile = infile[ii]+'.stars'
    IF FILE_TEST(infile[ii]) AND $
       (NOT FILE_TEST(starfile) OR KEYWORD_SET(force)) THEN BEGIN
      spawn,"WCStan.pl "+infile[ii]+" -force -scale "+STRTRIM(STRING(scale),2)

; Clean up the WCStan byproducts
      IF FILE_TEST(infile[ii]+'.catalog') THEN spawn,'/bin/rm -f '+infile[ii]+'.catalog'
      IF FILE_TEST(infile[ii]+'.wcs') THEN spawn,'/bin/rm -f '+infile[ii]+'.wcs'
      IF FILE_TEST(infile[ii]+'.wcsmatch') THEN spawn,'/bin/rm -f '+infile[ii]+'.wcsmatch'
      IF FILE_TEST(infile[ii]+'.xieta') THEN spawn,'/bin/rm -f '+infile[ii]+'.xieta'
    ENDIF
  ENDFOR

; STEP 3: Run imregister_mine.pl on each file

  PRINT,"Registering images"

; Some general cleanup
  IF KEYWORD_SET(force) THEN BEGIN
    spawn,'/bin/rm -f '+objdir+'*map*'
    spawn,'/bin/rm -f '+objdir+'*sh.fits'
    spawn,'/bin/rm -f '+objdir+'*.mask.fits'
    spawn,'/bin/rm -f '+objdir+'*.fits.stars'
  ENDIF

; Cleanup in aisle three.  That is, the Temporary_whatever files will
; all be handled in the local directory, NOT in the output one.
  spawn,'/bin/rm -f Temporary_*'+STRTRIM(object,2)+'*'

; Before we imregister, we need to pad both basic mask files, as well as the
; reference image(s)
  Rbuffmask = 'Temporary_mask_'+STRTRIM(object,2)+'_R.buff.fits'
  Nbuffmask = 'Temporary_mask_'+STRTRIM(object,2)+'_'+filter+'.buff.fits'

; Buffer the two masks.
  Rmaskimg = FLTARR(imsize[0],imsize[1])

  fits_read,Rmask,img,hd
  sz = SIZE(img)
  FOR jj = 0,sz[1]-1 DO BEGIN
    xshift = UINT((imsize[0]-sz[1])/2)
    FOR kk = 0,sz[2]-1 DO BEGIN
      yshift = UINT((imsize[1]-sz[2])/2)
      Rmaskimg[jj+xshift,kk+yshift] = img[jj,kk]
    ENDFOR
  ENDFOR
  SXADDPAR,hd,"CRPIX1",FLOAT(SXPAR(hd,"CRPIX1")+buffer),''
  SXADDPAR,hd,"CRPIX2",FLOAT(SXPAR(hd,"CRPIX2")+buffer),''
  fits_write,Rbuffmask,Rmaskimg,hd

  IF Rmask EQ Nmask THEN BEGIN
    Nmaskimg = Rmaskimg
  ENDIF ELSE BEGIN
    Nmaskimg = FLTARR(imsize[0],imsize[1])
    fits_read,Nmask,img,hd
    sz = SIZE(img)
    FOR jj = 0,sz[1]-1 DO BEGIN
      xshift = UINT((imsize[0]-sz[1])/2)
      FOR kk = 0,sz[2]-1 DO BEGIN
        yshift = UINT((imsize[1]-sz[2])/2)
        Nmaskimg[jj+xshift,kk+yshift] = img[jj,kk]
      ENDFOR
    ENDFOR
    SXADDPAR,hd,"CRPIX1",FLOAT(SXPAR(hd,"CRPIX1")+buffer),''
    SXADDPAR,hd,"CRPIX2",FLOAT(SXPAR(hd,"CRPIX2")+buffer),''
  ENDELSE
  fits_write,Nbuffmask,Nmaskimg,hd

  xsize = (imsize[0]-2*buffer)
  ysize = (imsize[1]-2*buffer)
  edgemask = BYTARR(xsize,ysize)
  edgemask[10:xsize-11,10:ysize-11] = 1b
  edgemask[100:xsize-101,100:ysize-101] = 0b
  
  diagmask = BYTARR(xsize,ysize)
  diagmask[0:(xsize/2)-1,0:(ysize/2)-1] = 1b
  diagmask[(xsize/2):xsize-1,(ysize/2):ysize-1] = 1b
  diagmask[(xsize/4):(3*xsize/4)-1,(ysize/4):(3*ysize/4)-1] = 0b

  bufffile = odir+Image+".buff.fits"
  buffedge = 10 ;; NGC253 only!!!
  FOR ii = 0,n_files-1 DO BEGIN
    IF NOT FILE_TEST(bufffile[ii]) THEN BEGIN
      fits_read,infile[ii],img,hdr
      IF Rflag[ii] THEN BEGIN
        edgeind = WHERE(edgemask EQ 1b AND Rmaskimg EQ 1b AND diagmask EQ 1b)
      ENDIF ELSE BEGIN
        edgeind = WHERE(edgemask EQ 1b AND Nmaskimg EQ 1b AND diagmask EQ 1b)
      ENDELSE
      grm_avsigclip,img[edgeind],3.0,100,newsky,newskysig,ngood,nbad,niter
; newsky = SXPAR(hdr,'SKYLEV')
      tempimg = FLTARR(imsize[0],imsize[1]) + newsky
      FOR jj = buffedge,sz[1]-1-buffedge DO BEGIN
        xshift = UINT((imsize[0]-sz[1])/2)
        FOR kk = buffedge,sz[2]-1-buffedge DO BEGIN
          yshift = UINT((imsize[1]-sz[2])/2)
          tempimg[jj+xshift,kk+yshift] = img[jj,kk]
        ENDFOR
      ENDFOR

      ref_crpix1 = SXPAR(hdr,"CRPIX1")+buffer
      SXADDPAR,hdr,"CRPIX1",ref_crpix1,''
      ref_crpix2 = SXPAR(hdr,"CRPIX2")+buffer
      SXADDPAR,hdr,"CRPIX2",ref_crpix2,''
      SXADDPAR,hdr,'SKYLEV',newsky,' box2box sky level'
      SXADDPAR,hdr,'SKYSIG',newskysig,' pixel-to-pixel sky RMS'
      fits_write,bufffile[ii],tempimg,hdr
    ENDIF
  ENDFOR

  starfile = odir+Image+".buff.fits.stars"
  matchfile = odir+Image+".buff.match"

  ref_NE = 'Temporary_ref_NE.fits' ; was shfile[posref_NE]
  ref_SW = 'Temporary_ref_SW.fits' ; was shfile[posref_SW]
  tempmask = 'Temporary_ref_mask.fits'

; Before anything else, shift the two outside posrefs.
  IF NOT FILE_TEST(ref_NE) THEN BEGIN
;    spawn,'rm -f '+tempmask
    command="imregister_mine.pl "+bufffile[posref_C]+" "+bufffile[posref_NE]+$
            " -useiraf -sat "+STRTRIM(STRING(ccdsatur),2)+$
;;            " -thresh "+STRTRIM(STRING(nsig),2)+" -out "+shfile[posref_NE]+" -maskin "+$
;;            Rbuffmask+" -maskout "+maskfile[posref_NE]+" -force -n "+STRTRIM(STRING(nstars),2)
            " -thresh "+STRTRIM(STRING(nsig),2)+" -out "+ref_NE+" -maskin "+Rbuffmask+" -maskout "+tempmask+" -force -n "+STRTRIM(STRING(nstars),2)
    PRINT,command
    spawn,command

    fits_read,ref_NE,refimg,refhdr
    fits_read,tempmask,refmask,junk,/data_only

    skylev = SXPAR(refhdr,'SKYLEV')
    skysig = SXPAR(refhdr,'SKYSIG')
    refimg = patch_sky(refimg,(refmask LT 0.5),MODE=1,SKY=[skylev,skysig])
    fits_write,ref_NE,refimg,refhdr
;;    patch_shfile,shfile[posref_NE]
  ENDIF
  IF NOT FILE_TEST(ref_SW) THEN BEGIN
    spawn,'rm -f '+tempmask
    command="imregister_mine.pl "+bufffile[posref_C]+" "+bufffile[posref_SW]+$
            " -useiraf -sat "+STRTRIM(STRING(ccdsatur),2)+$
;;            " -thresh "+STRTRIM(STRING(nsig),2)+" -out "+shfile[posref_SW]+" -maskin "+$
;;            Rbuffmask+" -maskout "+maskfile[posref_SW]+" -force -n "+STRTRIM(STRING(nstars),2)
            " -thresh "+STRTRIM(STRING(nsig),2)+" -out "+ref_SW+" -maskin "+Rbuffmask+" -maskout "+tempmask+" -force -n "+STRTRIM(STRING(nstars),2)
    PRINT,command
    spawn,command

    fits_read,ref_SW,refimg,refhdr
    fits_read,tempmask,refmask,junk,/data_only

    skylev = SXPAR(refhdr,'SKYLEV')
    skysig = SXPAR(refhdr,'SKYSIG')
    refimg = patch_sky(refimg,(refmask LT 0.5),MODE=1,SKY=[skylev,skysig])
    fits_write,ref_SW,refimg,refhdr
;;    patch_shfile,shfile[posref_SW]        
  ENDIF

  PRINT,'Subreference images shifted.'

  FOR ii=0,n_files-1 DO BEGIN
; Check to see if the files already exist.  If any of the needed files are
; found missing, OR if you set /force, do imregister again. 

    PRINT,'  Processing image ',infile[ii]

    IF NOT (FILE_TEST(shfile[ii]) OR FILE_TEST(shgzfile[ii])) OR $
       NOT (FILE_TEST(maskfile[ii]) OR FILE_TEST(maskgzfile[ii])) OR $
       NOT FILE_TEST(starfile[ii]) OR NOT FILE_TEST(matchfile[ii]) OR $
       KEYWORD_SET(force) THEN BEGIN

;      spawn,"/bin/rm -f *map*"
;      spawn,"/bin/rm -f *match*"
      IF FILE_TEST(shfile[ii]) THEN spawn,"/bin/rm -f "+shfile[ii]
      IF FILE_TEST(shgzfile[ii]) THEN spawn,"/bin/rm -f "+shgzfile[ii]
      IF FILE_TEST(maskfile[ii]) THEN spawn,"/bin/rm -f "+maskfile[ii]
      IF FILE_TEST(maskgzfile[ii]) THEN spawn,"/bin/rm -f "+maskgzfile[ii]
      IF FILE_TEST(starfile[ii]) THEN spawn,"/bin/rm -f "+starfile[ii]
      IF FILE_TEST(matchfile[ii]) THEN spawn,"/bin/rm -f "+matchfile[ii]

; imregister_mine.pl is a lot like imregister_new.pl, except I've added a few
; minor things.  One of these is the ability to pass in a mask, have it
; transformed in the same way as the image, and pass out a new mask
; file.  For NGC253, I've also added the ability to pass a second
; image/mask combo in and have THEM tranformed the same way, for cases
; where I know imregister will fail otherwise.

      IF Rflag[ii] THEN inmask = Rbuffmask ELSE inmask = Nbuffmask
; Since buffer is greater than zero, pad the image out by making a temporary
; image to feed into imregister.

; If this is one of the outer-position images, shift to THAT refimage,
; which'll already be shifted.
      CASE impos[ii] OF
        'C': reffile = bufffile[posref_C]
        'NE': reffile = ref_NE
        'SW': reffile = ref_SW
        ELSE: BEGIN
              PRINT,'ERROR in singg_combine_NGC253: invalid position code ',impos[ii]
              RETURN
              END
      ENDCASE

      command="imregister_mine.pl "+reffile+" "+bufffile[ii]+$
              " -useiraf -sat "+STRTRIM(STRING(ccdsatur),2)+$
              " -thresh "+STRTRIM(STRING(nsig),2)+" -out "+shfile[ii]+" -maskin "+$
              inmask+" -maskout "+maskfile[ii]+" -force -n "+STRTRIM(STRING(nstars),2)
      PRINT,command
      spawn,command

      IF NOT FILE_TEST(shfile[ii]) OR NOT FILE_TEST(maskfile[ii]) THEN BEGIN
        PRINT,'ERROR in singg_combine: imregister failed for file ',shfile[ii]
;; better yet, remove this image from the list and keep going.
        RETURN
      ENDIF

; Patch the WCS back into the image, using what we extracted from the
; reference image file.
      fits_read,shfile[ii],shimg,shhd
; Use extast to check header.
      extast,shhd,astr,noparams
      IF noparams LT 0 THEN BEGIN
        PRINT,'ERROR in singg_combine: extast reports missing astrometry'
        RETURN
      ENDIF

      singg_copy_wcs,refhd,shhd
      fits_write,shfile[ii],shimg,shhd
; Don't really need to patch the header on the mask image.

; Cleanup everything we don't need.  That is, toss the .geomap and .map files.
; We need the .match files for refflux, and the sh.fits and sh.mask.fits files
; for future steps also.
      spawn,"/bin/rm -f "+objdir+Image[ii]+".*map"
    ENDIF
  ENDFOR

  PRINT,"All images aligned"

; STEP 4: Set up a simple mask.  We only want to cancel out edge effects and 
; the galaxy.  For this mask, 1b means BAD pixels.
; We could do this inside singg_cr, but only if we passed in buffer, edge, band
  PRINT,"Creating sky mask"
  sky_mask = MAKE_ARRAY(imsize[0],imsize[1],/BYTE,VALUE=1b)
  sky_mask[buffer2+edge:(imsize[0]-1)-(buffer2+edge),$
           buffer2+edge:(imsize[1]-1)-(buffer2+edge)] = 0b
  sky_mask[(buffer2+edge+band):(imsize[0]-1)-(buffer2+edge+band),$
           (buffer2+edge+band):(imsize[1]-1)-(buffer2+edge+band)] = 1b

; STEP 5: Now, set up the data for cr_reject, and call the function that
;         combines the data into two images.
  PRINT,"Now beginning combine process."

  PRINT,"Combining R images:"
  ImageR = singg_cr(Image[Rlist],odir,shfile[Rlist],maskfile[Rlist],$
                    Rrdnoise,Rgain,$
                    wellsatur,sky_mask,imsize,edge,buffer2,$
                    Rnoise,Rnpix,Rexptime,RWexptime,Rdate,$
                    Rsky,Rskysig,RWsky,RWskysig,$
                    Rscale,Rerrscale,Rref,/header_sky)

  IF TOTAL(FINITE(Rscale)) NE num_R THEN BEGIN
    RETURN
  ENDIF

  PRINT,"Combining narrow-band images:"
  ImageN = singg_cr(Image[Nlist],odir,shfile[Nlist],maskfile[Nlist],$
                    Nrdnoise,Ngain,$
                    wellsatur,sky_mask,imsize,edge,buffer2,$
                    Nnoise,Nnpix,Nexptime,NWexptime,Ndate,$
                    Nsky,Nskysig,NWsky,NWskysig,$
                    Nscale,Nerrscale,Nref,/header_sky)

  IF TOTAL(FINITE(Nscale)) NE num_narrow THEN BEGIN
    RETURN
  ENDIF

; STEP 6: Set up the new headers

  PRINT,"Images combined, now preparing output"

; Just make sure the J is capitalized for the output images.  Don't
; use STRUPCASE because the trailing letter might be lowercase.
  id = "J"+STRMID(object,1,7)

  IF NOT KEYWORD_SET(Rfile) THEN Rfile=STRTRIM(id,2)+"_R.fits"
  IF NOT KEYWORD_SET(Nfile) THEN $
         Nfile=STRTRIM(id,2)+"_"+STRTRIM(filter,2)+".fits"
  IF NOT KEYWORD_SET(Sfile) THEN Sfile=STRTRIM(id,2)+"_Rsub.fits"

  Rnoisefile = STRMID(Rfile,0,STRLEN(Rfile)-5)+".sig.fits"
  Rnpixfile = STRMID(Rfile,0,STRLEN(Rfile)-5)+".pl.fits"
  Nnoisefile = STRMID(Nfile,0,STRLEN(Nfile)-5)+".sig.fits"
  Nnpixfile = STRMID(Nfile,0,STRLEN(Nfile)-5)+".pl.fits"
  Snoisefile = STRMID(Sfile,0,STRLEN(Sfile)-5)+".sig.fits"
  Snpixfile = STRMID(Sfile,0,STRLEN(Sfile)-5)+".pl.fits"

; Write Snpixfile as 2/1/0 (in both, in only 1, in neither)
; Note that for our purposes, only having 1 image contributing to that pixel
; isn't useful unless there's only one image involved.  It makes runalard give
; screwy results.  So, 0 or 1 is "no", 2 or higher is "yes".
  Rthresh = 0.5 + FLOAT(num_R GT 1)
  Nthresh = 0.5 + FLOAT(num_narrow GT 1)
; Rthresh and Nthresh will be 1.5 if 2 or more images were used, 0.5 otherwise
  Snpix = LONG(Rnpix GT Rthresh) + LONG(Nnpix GT Nthresh)

; Run the headers through our template, using the best image as reference
  IF FILE_TEST(shfile[Rlist[Rref]]) THEN Rimg = readfits(shfile[Rlist[Rref]],Rhd,/SILENT) $
                                    ELSE Rimg = readfits(shgzfile[Rlist[Rref]],Rhd,/SILENT)
  IF FILE_TEST(shfile[Nlist[Nref]]) THEN Nimg = readfits(shfile[Nlist[Nref]],Nhd,/SILENT) $
                                    ELSE Nimg = readfits(shgzfile[Nlist[Nref]],Nhd,/SILENT)

  hdr_template,Rhd,Rimg,header_template,Rhd_out,/silent
  hdr_template,Nhd,Nimg,header_template,Nhd_out,/silent

; Merge the TARGLIST values, and pick a single TARGET/TARGTYPE
  targets = STRARR(10)
  n_targs = 0
  targlist = ''
  FOR ii = 0,n_files-1 DO BEGIN
    IF FILE_TEST(shfile[ii]) THEN fits_read,shfile[ii],junk,hd,/header_only $
                             ELSE fits_read,shgzfile[ii],junk,hd
    targtemp = STRTRIM(SXPAR(hd,'TARGLIST'),2)
; parse the targlist
    compos = 999
    WHILE compos GE 0 DO BEGIN
      compos = STRPOS(targtemp,',')
      IF compos LT 0 THEN BEGIN
        targ = targtemp
        targtemp = ''
      ENDIF ELSE BEGIN
        targ = STRMID(targtemp,0,compos)
        targtemp = STRMID(targtemp,compos+1,STRLEN(targtemp)-compos-1)
      ENDELSE

      junk = WHERE(STRTRIM(targets,2) EQ targ,count)
      IF count EQ 0 AND targ NE 'UNKNOWN' THEN BEGIN
        targets[n_targs] = targ
        IF n_targs GT 0 THEN targlist = targlist+','
        targlist = targlist+targ
        n_targs = n_targs+1
      ENDIF
    ENDWHILE
  ENDFOR
  IF n_targs EQ 0 THEN BEGIN
    PRINT,'ERROR in singg_combine: no valid target lists found'
    RETURN
  ENDIF

  SXADDPAR,Rhd,'TARGLIST',targlist,'Target list'
  SXADDPAR,Nhd,'TARGLIST',targlist,'Target list'

; If the fluxref images have screwy targets, just copy the refimage one.
  IF STRTRIM(SXPAR(Rhd,'TARGET'),2) EQ 'UNKNOWN' OR $
     STRTRIM(SXPAR(Nhd,'TARGET'),2) EQ 'UNKNOWN' THEN BEGIN
    target = STRTRIM(SXPAR(refhd,'TARGET'),2)
    targtype = STRTRIM(SXPAR(refhd,'TARGTYPE'),2)
    IF target NE 'UNKNOWN' THEN BEGIN
      SXADDPAR,Rhd,'TARGET',target,'Target name'
      SXADDPAR,Nhd,'TARGET',target,'Target name'
      SXADDPAR,Rhd,'TARGTYPE',targtype,'Target name'
      SXADDPAR,Nhd,'TARGTYPE',targtype,'Target name'
    ENDIF ELSE BEGIN
      PRINT,'ERROR in singg_combine: posref has invalid target'
      RETURN
    ENDELSE
  ENDIF

  Shd_out = Nhd_out
; Change a few essentials
  SXADDPAR,Shd_out,'IMTYPE','net',' Image type (cont/onband/net)'
  Rpixsize = SXPAR(Rhd_out,'XPIXSIZE',count=matches)
  IF matches LT 1 THEN Rpixsize = scale 
  Npixsize = SXPAR(Nhd_out,'XPIXSIZE',count=matches)
  IF matches LT 1 THEN Npixsize = scale

; STEP 7: Create the _Rsub file using runalard

; First, write the total count images (not count rate).  These will be used for
; runalard.pl, which needs a total-count image for statistic purposes.
  tempRfile = 'Temporary_IDL_file_'+STRTRIM(object,2)+'_R.fits'
  tempNfile = 'Temporary_IDL_file_'+STRTRIM(object,2)+'_'+filter+'.fits'
  tempkernel = 'Temporary_IDL_file_'+STRTRIM(object,2)+'_'+filter+'.sum_kernel'
  Rstarfile = tempRfile+'.stars'
  Nstarfile = tempNfile+'.stars'

  ImageR = patch_sky(ImageR,(Rnpix LT 0.5),MODE=1,EDGE=buffer2+50)
  ImageR2 = ImageR*FLOAT(Rnpix GT 0.5)
  ImageN = patch_sky(ImageN,(Nnpix LT 0.5),MODE=1,EDGE=buffer2+50)
  ImageN2 = ImageN*FLOAT(Nnpix GT 0.5)
;ImageR2 = patch_sky(ImageR,(Rnpix LT 0.5),MODE=0,EDGE=buffer2+50)
;ImageN2 = patch_sky(ImageN,(Nnpix LT 0.5),MODE=0,EDGE=buffer2+50)

  fits_write,tempRfile,ImageR2*Rexptime[Rref],Rhd_out
  fits_write,tempNfile,ImageN2*Nexptime[Nref],Nhd_out

  fits_write,odir+Rnoisefile,Rnoise,Rhd_out
  fits_write,odir+Rnpixfile,Rnpix,Rhd_out
  fits_write,odir+Nnoisefile,Nnoise,Nhd_out
  fits_write,odir+Nnpixfile,Nnpix,Nhd_out
; We haven't set Snoise
;  fits_write,odir+Snoisefile,Snoise,Shd_out
  fits_write,odir+Snpixfile,Snpix,Shd_out

  spawn,'gzip -f '+odir+'*.sig.fits'
  spawn,'gzip -f '+odir+'*.pl.fits'

  spawn,"runalard.pl "+tempRfile+" "+tempNfile

  IF NOT FILE_TEST(tempkernel) THEN BEGIN
    PRINT,'ERROR in singg_combine: runalard failed'
    RETURN
  ENDIF

; If you absolutely need to override the normal runalard algorithm, do it
; with this command.  But, you still have to run the normal runalard FIRST.
  IF KEYWORD_SET(trim) THEN BEGIN
    trim_stars,Rstarfile,(buffer2+edge),imsize,npix=Snpix
    spawn,'runalard.pl '+tempRfile+' '+tempNfile+' -starlist Temporary_IDL_file_'+STRTRIM(object,2)+'_short.stars -FWHMi 2.0 -FWHMt 2.0'
  ENDIF

; Now this has created a file named Temporary_IDL_file_6XXX.sub.fits; read it,
; correct for exposure time, and write again to the correct file name.
  tempSfile = 'Temporary_IDL_file_'+STRTRIM(object,2)+'_'+filter+'.sub.fits'
  fits_read,tempSfile,Simg,Shd,/data_only
  Simg = Simg / Nexptime[Nref]

  Simg = patch_sky(Simg,(Snpix LT 1.5),MODE=0,EDGE=buffer2+50,SKY=0.0)

; Calculate seeing, in arcsec
  Rseeing = calc_seeing(Rstarfile,imsize,(edge+buffer2))*Rpixsize
  Nseeing = calc_seeing(Nstarfile,imsize,(edge+buffer2))*Npixsize
  Sseeing = MAX([Rseeing,Nseeing])

; Figure out date stuff
  Rend = find_end_time(Rdate,Rexptime)
  Nend = find_end_time(Ndate,Nexptime)

; Find min and max values for each image
  Rmin = MIN(ImageR[WHERE(Rnpix GT 0)])
  Nmin = MIN(ImageN[WHERE(Nnpix GT 0)])
  Smin = MIN(Simg[WHERE(Snpix GT 1)])

  Rmax = MAX(ImageR[WHERE(Rnpix GT 0)])
  Nmax = MAX(ImageN[WHERE(Nnpix GT 0)])
  Smax = MAX(Simg[WHERE(Snpix GT 1)])

; Get continuum scaling ratio through two different methods.
; First, the long way, using the star files from the images.

  IF num_R GT 1 THEN Rphotqual = STDDEV(Rscale*Rexptime)/Rexptime[Rref] $
                ELSE Rphotqual = 0.0
  IF num_narrow GT 1 THEN Nphotqual = STDDEV(Nscale*Nexptime)/Nexptime[Nref] $
                ELSE Nphotqual = 0.0

; Since the R image is "brighter" than the narrow-band, use it as the reference
  del_M = calc_mdiff(tempNfile,tempRfile,(edge+buffer2),sigma,nmatch, $
                     IMSKY=[NWsky,NWskysig],REFSKY=[RWsky,RWskysig])
; Was Nstarfile,Rstarfile and sky was just skysig.

  IF del_M GT 900.0 THEN BEGIN
    PRINT,'ERROR in singg_combine: calc_mdiff reported error. '
    RETURN
  ENDIF

; This gave us a magnitude difference and uncertainty; convert to a flux ratio 
; Since del_M should be positive, this'll make cntrat2<1
  cntrat2 = 10.0^(-0.4*del_M) * Rexptime[Rref]/Nexptime[Nref]
  ecntrat2 = cntrat2*ALOG(10.0)*0.4*sigma

; Now, the short way, just reading the outputs of runalard.pl
  kernelfile = STRMID(tempNfile,0,STRLEN(tempNfile)-5)+".sum_kernel"
  IF FILE_TEST(kernelfile) THEN BEGIN
    readcol_new,kernelfile,junk,kernrat,format="A,F",comment='#',/silent
; The file will have one line of the format "sum_kernel:    0.1076"
; Thanks to an occasional bug it sometimes inverts the ratio.  It'll be less
; than 1, because we always scale down the R to match the narrow-band.
    IF kernrat[0] GT 1.0 THEN cntrat1 = 1.0/FLOAT(kernrat[0]) * Rexptime[Rref]/Nexptime[Nref] $
                         ELSE cntrat1 = FLOAT(kernrat[0]) * Rexptime[Rref]/Nexptime[Nref]
    ecntrat1 = 0.0 ; until we find a value
  ENDIF ELSE BEGIN
    PRINT,"WARNING: cannot open kernel file: ",kernelfile
    cntrat1 = cntrat2
    ecntrat1 = 0.0
  ENDELSE

; and add the cntrat stuff to the header
  SXADDPAR,Shd_out,"CNTRAT1",cntrat1,' Continuum ratio (kernel)'
  IF ecntrat1 GT 0 THEN SXADDPAR,Shd_out,"ECNTRAT1",ecntrat1, $
                                ' Continuum ratio RMS (kernel)'
  SXADDPAR,Shd_out,"CNTRAT2",cntrat2,' Continuum ratio'
  SXADDPAR,Shd_out,"ECNTRAT2",ecntrat2,' Continuum ratio RMS'

  singg_header,Rhd_out, $
               Rfile,num_R,TOTAL(Rexptime),RWexptime,buffer2, $
               refimage,Image[Rlist[Rref]]+'.fits',Image[Rlist]+'sh.fits', $
               Rscale,Rphotqual,MIN(Rdate),Rend,Rnpixfile,Rseeing,Rmin,Rmax

  singg_header,Nhd_out, $
               Nfile,num_narrow,TOTAL(Nexptime),NWexptime,buffer2, $
               refimage,Image[Nlist[Nref]]+'.fits',Image[Nlist]+'sh.fits', $
               Nscale,Nphotqual,MIN(Ndate),Nend,Nnpixfile,Nseeing,Nmin,Nmax

  singg_header,Shd_out, $
               Sfile,2,TOTAL(Nexptime),NWexptime,buffer2, $
               refimage,Image[Nlist[Nref]]+'.fits',[Rfile,Nfile], $
               [-cntrat1,1.0],Nphotqual,MIN([Rdate,Ndate]),MAX([Rend,Nend]), $
               Snpixfile,Sseeing,Smin,Smax

; Our previous IRAF algorithms combined to an average exposure time, while
; cr_reject combines to the TOTAL exposure time.  We're going to output a count
; rate image, though, which cr_weight outputs.

  hdr_template,Rhd_out,Rimg,header_template,Rhd_out2,/silent
; Clear the WCS a bit.
  SXADDPAR,Rhd_out2,'LTM1_1',1.0,''
  SXADDPAR,Rhd_out2,'LTM2_2',1.0,''
  SXADDPAR,Rhd_out2,'LTV1',0,''
  SXADDPAR,Rhd_out2,'LTV2',0,''
  SXADDPAR,Rhd_out2,'LTM1_2',0,''
  SXADDPAR,Rhd_out2,'LTM2_1',0,''
  fits_write,odir+Rfile,ImageR,Rhd_out2

  hdr_template,Nhd_out,Nimg,header_template,Nhd_out2,/silent
  fits_write,odir+Nfile,ImageN,Nhd_out2

; STEP 8: Yes, WCStan.pl again.  Explicitly do Rfile, then copy to
; Nfile and Sfile.
; We need -force since we're using a header that already had WCS in it.
;;command = "WCStan.pl "+odir+Rfile+" -force -scale "+STRTRIM(STRING(Rpixsize),2)
  spawn,"WCStan.pl "+odir+Rfile+" -force -scale "+STRTRIM(STRING(Rpixsize),2)

; Clean up the WCStan byproducts
  IF FILE_TEST(odir+Rfile+'.catalog') THEN spawn,'/bin/rm -f '+odir+Rfile+'.catalog'
  IF FILE_TEST(odir+Rfile+'.wcs') THEN spawn,'/bin/rm -f '+odir+Rfile+'.wcs'
  IF FILE_TEST(odir+Rfile+'.wcsmatch') THEN spawn,'/bin/rm -f '+odir+Rfile+'.wcsmatch'
  IF FILE_TEST(odir+Rfile+'.xieta') THEN spawn,'/bin/rm -f '+odir+Rfile+'.xieta'

; Re-read the Rfile to get WCS information, which should then be pasted into
; Nhd_out and Shd_out
  fits_read,odir+Rfile,img,Rhd_temp,/header_only
  singg_copy_wcs,Rhd_temp,Nhd_out2
  singg_copy_wcs,Rhd_temp,Shd_out

; While we're at it, correct the headers' RA and DEC to match the output of
; the R-band WCStan.pl.
  fix_radec,Rhd_out2,Nhd_out2,Shd_out

; We fixed Rhd and Nhd, but we should rewrite to Rfile and Nfile, which 
; requires rerunning hdr_template...?  We're skipping this for now.

; Now, we can write the final _Rsub.fits file.
  hdr_template,Shd_out,Simg,header_template,Shd_out2,/silent
  fits_write,odir+Sfile,Simg,Shd_out2

; Cleanup in aisle seven.  As in, make sure they're ALL gone now.
  spawn,'/bin/rm -f Temporary_*'+STRTRIM(object,2)+'*'
  spawn,'/bin/rm -f Temporary_ref_*'

  spawn,'gzip -f '+odir+'obj*sh*.fits'

; STEP 9: If you wanted an output file, write it too.
  IF KEYWORD_SET(output) THEN BEGIN

    outfile = odir+STRTRIM(object,2)+"_combine.dat"

    OPENW,unit,outfile,/GET_LUN

    PRINT,"Now writing the output file ",outfile

    PRINTF,unit,"# Format is (filename) (filter) (sky) (err_sky) (scale) (err_scale) (weight)"
    PRINTF,unit,"# Positional reference image was "+refimage

    FOR ii=0,(num_R-1) DO BEGIN
      PRINTF,unit,STRTRIM(infile[Rlist[ii]],2),Rsky[ii],Rskysig[ii],$
             Rscale[ii],Rerrscale[ii],(1.0/Rscale[ii]),$
             FORMAT='(A,"   R  ",F," ",F," ",F," ",F," ",F)'
    ENDFOR

    FOR ii=0,(num_narrow-1) DO BEGIN
      PRINTF,unit,STRTRIM(infile[Nlist[ii]],2),STRTRIM(filtername,2),$
             Nsky[ii],Nskysig[ii],$
             Nscale[ii],Nerrscale[ii],(1.0/Nscale[ii]),$
             FORMAT='(A," ",A," ",F," ",F," ",F," ",F," ",F)'
    ENDFOR

    CLOSE,unit
    FREE_LUN,unit

  ENDIF

  PRINT,"  "
  PRINT,"Object "+object+" completed."
  PRINT,"  "
  PRINT,"/-----------\"
  PRINT,"| GAME OVER |"
  PRINT,"\-----------/"

END
