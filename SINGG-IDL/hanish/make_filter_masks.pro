PRO make_filter_masks,INDIR=indir,VERBOSE=verbose
; Create a basic bad-column/pixel mask for each run/filter combo, based on its
; basic_mask.fits image and the object images.  This is mainly to mask out 
; any residual "hot" pixels.
; OPTIONAL INPUTS
;   indir            Input proc2 directory (ie,'./Run*/Proc2/'); this 
;                      directory must have Obj/ and Ref/ subdirectories.
;   verbose          Give a lot of extra information

; Do not run until AFTER housekeeper.  In fact, this'll be done as part of
; the proc3_setup routine, just to make sure.

  spawn,'pwd',cdir

  printflag = KEYWORD_SET(verbose)

; indir = 'RunXX/Proc2/'
  IF KEYWORD_SET(indir) THEN BEGIN
    idir = STRTRIM(indir,2)
; If it doesn't end in a slash, add one.
    IF STRMID(idir,0,1,/reverse_offset) NE '/' THEN idir = idir+'/'
; If it's a relative path (either './' or nothing), add the pwd.
    IF STRMID(idir,0,2) EQ './' THEN idir = STRTRIM(cdir[0],2)+STRMID(idir,1,STRLEN(idir)-1)
    IF STRMID(idir,0,1) NE '/' THEN idir = STRTRIM(cdir[0],2)+'/'+idir
  ENDIF ELSE BEGIN
    idir = STRTRIM(cdir[0],2)+'/'
  ENDELSE

  objdir = idir+'Obj/'
  refdir = idir+'Ref/'

  fits_read,refdir+'basic_mask.fits',basic,bhd
  sz = SIZE(basic)

  ccdsatur = 59000
  
; the .lis files have already been made by this point.  We could just
; use them instead.  But, they don't include standards, which are
; actually pretty good for mask-making.

  spawn,"ls "+objdir+"obj???????.fits obj???????.fits.gz",imlist
  IF NOT FILE_TEST(imlist[0]) THEN BEGIN
    PRINT,"ERROR in make_filter_masks: no object images"
    RETURN
  ENDIF

  segfile = STRMID(imlist,0,STRLEN(objdir)+10)+'_seg.fits'

  mask = BYTARR(sz[1],sz[2],22) + 1b
  bad = INTARR(sz[1],sz[2],22)
  fname = STRARR(22)
  fcount = INTARR(22)
  fnum = 0

  filt_rddbfnames,"filter",fnamarr

  spawn,"/bin/rm -f test.fits.gz"
  FOR ii = 0,N_ELEMENTS(imlist)-1 DO BEGIN
    img = readfits(imlist[ii],hd,/SILENT)

    filtname = SXPAR(hd,"FILTNAME")
    filter = singg_filtnam(fnamarr,filtname,pos,/SILENT)
    filter2 = STRTRIM(fnamarr[pos[0],1],2)

    IF filter2 EQ '6850' THEN filter2 = 'cont'

    find = WHERE(fname EQ filter2,count)

    IF count GT 0 THEN BEGIN
      IF printflag THEN PRINT,imlist[ii]," old filter: ",filter
      fi = find[0]
    ENDIF ELSE BEGIN
      IF printflag THEN PRINT,imlist[ii]," NEW filter: ",filter
      fi = fnum
      fname[fnum] = filter2
      fnum = fnum+1
    ENDELSE
    fcount[fi] = fcount[fi] + 1

    bad[*,*,fi] = bad[*,*,fi] + (img GT ccdsatur OR img LT 0)
  ENDFOR

; Now that we've set it up, move to the next step.
  delY = 3
  delX = 0
  FOR ii = 0,fnum-1 DO BEGIN
    IF printflag THEN PRINT,"Creating final mask for filter "+fname[ii]
    badfrac = FLOAT(bad[*,*,ii])/FLOAT(fcount[ii])

;;fits_write,'test_'+STRTRIM(fname[ii],2)+'.fits',badfrac,bhd

    mysky,badfrac,badavg,badsig,mask=(1b-basic),/silent
    mask1 = FLTARR(sz[1],sz[2])
    badarr = (badfrac GT 3.0*badsig)

    ind1 = WHERE(badarr,count)
    y1 = FIX(ind1/sz[1])
    x1 = ind1 - y1*sz[1]

    badcount = INTARR(sz[1],sz[2])
    FOR jj = LONG(0),LONG(count-1) DO BEGIN
      minX = (x1[jj]-delX) > 0
      maxX = (x1[jj]+delX) < (sz[1]-1)
      minY = (y1[jj]-delY) > 0
      maxY = (y1[jj]+delY) < (sz[2]-1)
      junk = WHERE(badarr[minX:maxX,minY:maxY],count3)
      badcount[x1[jj],y1[jj]] = count3
    ENDFOR
    mask1 = (badcount GE 5)

    ind2 = WHERE(badcount GE 3,count2)
    y2 = FIX(ind2/sz[1])
    x2 = ind2 - y2*sz[1]

    badcount2 = INTARR(sz[1],sz[2])
    FOR kk = LONG(0),LONG(count2-1) DO BEGIN
      minX = (x2[kk]-delX) > 0
      maxX = (x2[kk]+delX) < (sz[1]-1)
      minY = (y2[kk]-delY) > 0
      maxY = (y2[kk]+delY) < (sz[2]-1)
      junk = WHERE(mask1[minX:maxX,minY:maxY],count3)
      badcount2[x2[kk],y2[kk]] = count3
    ENDFOR
    mask2 = (badcount2 GE 4)
    junk = WHERE(mask2,count)

    IF printflag THEN PRINT,ii," ",fname[ii]," ",count

; Grow the mask by a couple pixels.  If there WERE no bad pixels,
; don't do anything else and just pass the basic mask through.
    IF count GT 0 THEN BEGIN
      grow_mask,mask2,masko,3.0,goodval=1b,badval=0b
; masko is 1b for BAD pixels.
      maskout = basic AND NOT masko
    ENDIF ELSE BEGIN
      maskout = basic
    ENDELSE

; Remember, for maskout, 1b means usable pixels, 0b means don't use.
    fileout = refdir+"mask_"+STRTRIM(fname[ii],2)+".fits"
    fits_write,fileout,maskout,bhd
  ENDFOR

  IF printflag THEN PRINT,"Filter masking complete."

  RETURN

END
