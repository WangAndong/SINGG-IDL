pro ssoup_align, ll, inputstr, goslow=goslow
  ;
  ;  Align the SINGG optical images and SUNGG (Galex) UV images.
  ;    ll          -> logical unit for log file, which
  ;                   should be open before calling this program
  ;    inputstr    -> a structure which contains:
  ;    hname       -> name of hipass objects in the image
  ;    fimages_in  -> input fits image files.  This is a 4 element
  ;                   string array with elements giving the file 
  ;                   names for the images in the following bands
  ;                   0 : R (or continuum)
  ;                   1 : H-alpha
  ;                   2 : NUV
  ;                   3 : FUV
  ;    fmasks_in   -> input fits mask file names, same arrangemnt 
  ;                   as fimages_in.  These should refer to fits images
  ;                   containing integer or byte arrays.
  ;    mbadval_in  -> value of bad pixels in fmasks_in.  This is a 
  ;                   4 element array arranged as fimages_in.
  ;    skyord      -> Sky order (order of polynomial for sky fitting).
  ;    fimages_out -> output file names, same arrangement as 
  ;                   fimages_in.
  ;    fmask_out   -> name of output mask fits image.  This mask is 
  ;                   valid for all output images
  ;    fmask_sky   -> name of output sky mask file.
  ;    mbadval_out -> value for bad pixels in fmask_out.  If 
  ;                   mbadval_out NE 0 then good pixels have a value 0. 
  ;                   if mbadval_out = 0 then good pixels have a value 1
  ;    fbox        -> Name of output box files
  ;    goslow  -> if set then stragetically placed calls to 
  ;               keywait.pro are used to slow down the processing
  ;               to a speed a user can monitor.
  ;
  ;  G. Meurer 5/2010;  based on sample.pro by J.H. Kim
  ;            7/2010;  added polynomial sky surface fitting
  ;
  ; set constants and default parameters
  asdeg      = 3600.0           ; arcsec/degree
  COMMON bands, band, nband, bandnam, bandavail, nbandavail, combo, ncombo 
  nthresh    = 1024l            ; used to determine where to trim input optical images
  rota       = fltarr(nbandavail)  ; initialize rotations
  scale      = fltarr(nbandavail)  ; inititalize scale
  kbox       = 25               ; convolution kernel size in pixels
  prog       = 'SSOUP_ALIGN: '  ; string for messages
  ;
  ; do some error checking
  plog,ll,prog,'-------------------------- starting '+prog+'----------------------------'
  slow       = keyword_set(goslow)
  if nbandavail ne n_elements(inputstr.fimages_out) then $
     plog,-1,prog,'****WARNING: Number of output images NE number of input images'
  IF inputstr.mbadval_out NE 0b AND inputstr.mbadval_out NE 1b THEN BEGIN 
     plog,-1,prog,'****WARNING: mbadval_out out of range, resetting to 1b'
     inputstr.mbadval_out = 1b
  ENDIF
  ;
  ; set the value for good pixels in the output mask
  mgoodval_out = 0b
  IF inputstr.mbadval_out EQ 0b THEN mgoodval_out = 1b
  ;
  ; store exposure times here
  texp       = make_array(nbandavail, /float, value=1.0)
  ;
  ; read input images and compile headers
  ;
  plog,ll,prog,'reading input images and compiling headers'
  photplam = 0
  photflam = 0
  photfluxha = 0
  photwise = fltarr(4)
  fwhm = fltarr(nbandavail)
  nhd0 = intarr(nbandavail)
  imgs = ptrarr(nbandavail) ; pointers! YAY!
  hdcompile0 = ptrarr(nbandavail)
  for i=0,nbandavail-1 do begin
      fits_read, inputstr.fimages_in[i], img, hdr
      texp[i]    = sxpar(hdr,'EXPTIME')
      ; read wavelength specific headers
      if (bandavail[i] eq band.R) then begin
          fwhm[i]      = SXPAR(hdr,'SEEING')
          photflam     = SXPAR(hdr,'PHOTFLAM')
          photplam     = SXPAR(hdr,'PHOTPLAM')
      endif
      if (bandavail[i] eq band.HALPHA) then begin
          fwhm[i]      = SXPAR(hdr,'SEEING')
          photflux     = SXPAR(hdr,'PHOTFLUX')
      endif
      ; default seeing (Morrissey et al. 2007)
      if (bandavail[i] eq band.NUV) then fwhm[i] = 5.3
      if (bandavail[i] eq band.FUV) then fwhm[i] = 4.2
      ; Wright et al. 2010, AJ, 140, 1868
      if (bandavail[i] eq band.mir_W1) then begin
          fwhm[i] = 6.1
          texp[i] = 7.7 * sxpar(hdr,"MEDCOV") ; fixme? is this correct?
          photwise[0] = 1.935e-3 ; http://wise2.ipac.caltech.edu/docs/release/allsky/expsup/sec2_3f.html
      endif
      if (bandavail[i] eq band.mir_W2) then begin
          fwhm[i] = 6.4
          texp[i] = 7.7 * sxpar(hdr,"MEDCOV") ; fixme? is this correct?
          photwise[1] = 2.7048e-3 ; http://wise2.ipac.caltech.edu/docs/release/allsky/expsup/sec2_3f.html
      endif
      if (bandavail[i] eq band.mir_W3) then begin
          fwhm[i] = 6.5
          texp[i] = 8.8 * sxpar(hdr,"MEDCOV") ; fixme? is this correct?
          photwise[2] = 1.8326e-3 ; http://wise2.ipac.caltech.edu/docs/release/allsky/expsup/sec2_3f.html
      endif
      if (bandavail[i] eq band.mir_W4) then begin
          fwhm[i] = 12.0
          texp[i] = 8.8 * sxpar(hdr,"MEDCOV") ; fixme? is this correct?
          photwise[3] = 5.2269e-2 ; http://wise2.ipac.caltech.edu/docs/release/allsky/expsup/sec2_3f.html
      endif
      img           = img*texp[i]
      getrot, hdr, dum, cdelt
      rota[i]       = dum
      scale[i]      = asdeg*sqrt(abs(cdelt[0]*cdelt[1]))
      imgs[i]       = ptr_new(img)
      hdcompile0[i] = ptr_new(hdr)
      nhd0[i]       = n_elements(hdr)
  endfor
  ; fiducial bands
  ; P.S. these extra parentheses... another sign of a poorly designed programming language
  ofid = (where(bandavail eq band.HALPHA, /null))[0] ; what if these are not there
  ufid = (where(bandavail eq band.FUV, /null))[0]
  mirfid = (where(bandavail eq band.mir_W1, /null))[0]
  ;
  ; convert fwhm values to pixels in the fiducial image
  fwhm       = fwhm / scale[ufid]
  ;
  ; extract info for fiducial optical image
  plog,-1,prog,'determining fiducial images'
  oimg       = *imgs[ofid]
  ohd        = *hdcompile0[ofid]
  ohdp       = ofid
  siz        = size(oimg)
  nxo        = long(siz[1])
  nyo        = long(siz[2])
  ;
  ; extract info for fiducial UV image
  uimg       = *imgs[ufid]
  uhd        = *hdcompile0[ufid]
  uhdp       = ufid
  siz        = size(uimg)
  nxu        = long(siz[1])
  nyu        = long(siz[2])
  ; do the same for MIR (fixme: get rid of this copy/paste)
  mirimg     = *imgs[mirfid]
  mirhd      = *hdcompile0[mirfid]
  mirhdp     = mirfid
  siz        = size(nirimg)
  nxmir      = long(siz[1])
  nymir      = long(siz[2])
  ;
  ; determine limits of pixels to transform using H-alpha image.
  ; do this by collapsing image in each dimension, and finding first
  ; and last row/column that are masked in less than some 
  ; threshold number of columns/rows
  plog,ll,prog,'determining pixel limits of optical images to transform'
  ih = where(bandavail eq band.HALPHA, /null)
  ih = ih[0] ; fixme? should do something intelligent here if HALPHA is not present
  fits_read, inputstr.fmasks_in[ih], imgm, hd             ; read Halpha mask
  pp         = where(imgm EQ inputstr.mbadval_in[ih], npp)  ; index of bad pixels
  imgm       = 0l*long(imgm) + 1l                ; convert to long array
  IF npp GT 0 THEN imgm[pp] = 0l                 ; with bad pixels marked as 0l
  ncgood     = total(imgm, 1)                    ; number of good columns at each row
  nrgood     = total(imgm, 2)                    ; Number of good rows at each column
  pp         = where(ncgood GT nthresh, npp)     ; columns with more good rows than threshold
  IF npp GE nthresh THEN BEGIN 
     trminxo = min(pp)
     trmaxxo = max(pp)
  ENDIF ELSE BEGIN 
     trminxo = 0
     trmaxxo = nxo-1
     plog,ll,prog,'**** warning not enough good columns, entire optical images will be processed'
  ENDELSE 
  pp         = where(nrgood GT nthresh, npp)     ; rows with more good columns than threshold
  IF npp GE nthresh THEN BEGIN 
     trminyo = min(pp)
     trmaxyo = max(pp)
  ENDIF ELSE BEGIN 
     trminyo = 0
     trmaxyo = nyo-1
     plog,ll,prog,'**** warning not enough good rows, entire optical images will be processed'
  ENDELSE 
  ;
  ; construct array for corner coordinates of optical image
  ;cornx      = [-0.5,-0.5,float(nxo)-0.5,float(nxo)-0.5]
  ;corny      = [-0.5,float(nyo)-0.5,float(nyo)-0.5,-0.5]
  cornx      = [trminxo-0.5,trminxo-0.5,trmaxxo+0.5,trmaxxo+0.5]
  corny      = [trminyo-0.5,trminyo-0.5,trmaxyo+0.5,trmaxyo+0.5]
  ;
  ; a similar array for limts to pass to routine that transforms masks
  limits     = [trminxo, trmaxxo, trminyo, trmaxyo] 
  ;
  ; convert corner coords into RA and Dec within fiducial optical image
  xyad,ohd,cornx,corny,corna,cornd
  ;
  ; convert these to pixel position in fiducial UV image
  adxy,mirhd,corna,cornd,cornxmir,cornymir
  adxy,uhd,corna,cornd,cornxu,cornyu
  ;
  ; find pixel range to extract
  trminxmir    = round(min(cornxmir))
  trmaxxmir    = round(max(cornxmir))
  trminymir    = round(min(cornymir))
  trmaxymir    = round(max(cornymir))
  trminxu    = round(min(cornxu))
  trmaxxu    = round(max(cornxu))
  trminyu    = round(min(cornyu))
  trmaxyu    = round(max(cornyu))
  ; fixme
  plog,ll,prog,'Limits of optical image (xmin, xmax, ymin, ymax):  '+numstr(trminxo)+'  '+numstr(trmaxxo)+'  '+numstr(trminyo)+'  '+numstr(trmaxyo)
  plog,ll,prog,'Limits of UV image (xmin, xmax, ymin, ymax)     :  '+numstr(trminxu)+'  '+numstr(trmaxxu)+'  '+numstr(trminyu)+'  '+numstr(trmaxyu)
  plog,ll,prog,'Limits of MIR image (xmin, xmax, ymin, ymax)     :  '+numstr(trminxmir)+'  '+numstr(trmaxxmir)+'  '+numstr(trminymir)+'  '+numstr(trmaxymir)
  IF slow THEN keywait, 'type any key to continue: '
  ;
  ; make data cube for easy storage of intermediate products...
  nxx        = trmaxxmir - trminxmir + 1
  nyy        = trmaxymir - trminymir + 1
  imgtmp     = make_array(nxx, nyy, nbandavail, /float, value=0.0)
  ;
  ; trim UV images based on the above coords
  ; rebin and align optical images based on new fuv image header
  plog,ll,prog,'transforming uv and optical images'
  hdcompile1 = ptrarr(nbandavail)
  nhd1       = intarr(nbandavail)
  ;ifuv = where(bandavail eq band.FUV, /null)
  ;ifuv = ifuv[0]
  ifuv = 4
  ; need to ensure FUV executes first
  for i=ifuv,nbandavail-1 do begin
      if (i ge 4) then begin        
          HEXTRACT,*imgs[i],*hdcompile0[i],img,newhd,trminxmir,trmaxxmir,trminymir,trmaxymir
          imgtmp[*,*,i] = img
          hdcompile1[i] = ptr_new(newhd)
      endif else begin
          HASTROM,*imgs[i],*hdcompile0[i],img,newhd,*hdcompile1[ifuv],MISSING=0
          imgtmp[*,*,i]  = img*(scale[ufid]/scale[i])^2
          hdcompile1[i] = ptr_new(newhd)
     endelse
     nhd1[i] = n_elements(*hdcompile1[i])
     if i eq ifuv then i = -1
     if i eq ifuv-1 then i = ifuv
  endfor
  ;
  ; make data cube for input masks in output coord sys
  msktmp     = make_array(nxx, nyy, nbandavail, /byte, value=0b)
  ;
  ; loop through masks and transform to new coord sys
  plog,ll,prog,'transforming masks'
  uhd        = *hdcompile1[uhdp]     ; fiducial header for transformation
  FOR ii = 0, nbandavail-1 DO BEGIN
     hdi     = *hdcompile0[ii]         ; get input header from hdcompile0
     ;
     ; Only do mask stuff if the mask file name is not an empty string
     if strlen(strtrim(inputstr.fmasks_in[ii],2)) gt 0 then begin 
        plog,ll,prog,'working on mask file '+inputstr.fmasks_in[ii]
        ;
        ; read in input mask
        fits_read, inputstr.fmasks_in[ii], mski, hmm
        ;
        ; transform masks
        plog,ll,prog,'calling ssoup_transform_mask'
        ssoup_transform_mask, hdi, mski, inputstr.mbadval_in[ii], uhd, msko, limits=limits
        msktmp[*,*,ii] = msko
        IF slow THEN keywait, 'type any key to continue: '
     endif 
  ENDFOR 
  ;
  ; determine kernel widths for convolutions
  kfwhm      = fwhm[ufid]^2 - fwhm^2
  pp         = where(kfwhm le 0.0, npp)
  if npp gt 0 then kfwhm[pp] = 0.0
  kfwhm      = sqrt(kfwhm)
  ;
  ; loop through image sets and convolve if needed
  str        = ' '
  for ii = 0, nbandavail-1 do str = str+numstr(kfwhm[ii])+' '
  plog,ll,prog,'will convolve images and grow masks with kernel widths [pixels] ('+strjoin(bandavail, ",") + ") "+str
  for ii = 0, nbandavail-1 do begin 
     hdi            = *hdcompile1[ii] ; input header after alignment
     img            = imgtmp[*,*,ii]
     imgo           = img
     if (ii ne ufid) and (kfwhm[ii] gt 0.0) then begin 
        plog,ll,prog,'working on convolving '+bandavail[ii]+' band image '
        ;
        ; convolve the images that need it
        kern        = psf_gaussian(npixel=kbox,fwhm=kfwhm[ii],/normalize)
        imgo        = convolve(img,kern)
     endif  
     ;
     ; replace in cube  (perhaps not needed)
     imgtmp[*,*,ii] = imgo
     ;
     ; grow the mask if needed
     IF (kfwhm[ii] GE 1.0) and (strlen(strtrim(inputstr.fmasks_in[ii],2)) gt 0) THEN BEGIN 
        plog,ll,prog,'growing '+bandavail[ii]+' band mask'
        mski           = msktmp[*,*,ii]
        grow_mask, mski, msko, kfwhm[ii], goodval=1b, badval=0b
        msktmp[*,*,ii] = msko
     ENDIF 
     IF slow THEN keywait, 'type any key to continue: '
  endfor
  ;
  ; find final mask by summing bad pixel masks and 
  ; and thresholding so that a bad pixel in any of the
  ; indivual masks is bad in the combined masks
  plog,ll,prog,'making final mask'
  msko     = total(long(msktmp),3)
  jj       = where(msko GE 1l, njj)
  msko     = 0b*byte(msko) + mgoodval_out
  IF njj GT 0 THEN msko[jj] = inputstr.mbadval_out
  ;
  ; Make sky mask from final mask
  plog,ll,prog,'making sky mask'
  buffer   = round(200.0*scale[ofid]/scale[ufid])
  msks     = ssoup_askymask(ll, inputstr.hname, *hdcompile1[ufid], buffer, msko)
  ;
  ; Make object to measure only mask
  msks2    = ssoup_askymask(ll, inputstr.hname, *hdcompile1[ufid], 0, msko)
  mopix   = 0*msks2 + inputstr.mbadval_out
  pp       = where(msks2 EQ 1b AND msko EQ mgoodval_out, npp)
  IF npp GT 0 THEN mopix[pp] = mgoodval_out
  ;
  ; **** should apply a template to mask headers
  ; write aligned output mask and sky mask
  plog,ll,prog,'writing sky masks'
  fits_write, inputstr.fmask_out, msko, newhdf
  fits_write, inputstr.fmask_sky, msks, newhdf
  IF slow THEN keywait, 'type any key to continue: '
  ;
  ; Loop to measure sky levels and update headers
  for ii = 0, nbandavail-1 do begin
     hd0    = *hdcompile0[ii]  ; initial header
     hdi    = *hdcompile1[ii]  ; input header after alignment
     img    = imgtmp[*,*,ii]     
     ord    = inputstr.skyord[ii]
     ;
     ; read in and adjust old sky values and errors for new pixel size
     plog,ll,prog,'retrieving original sky level for image #'+numstr(ii)+' (band = '+bandavail[ii]+')'
     afact     = (scale[ufid]/scale[ii])^2
     skylev0   = sxpar(hd0,'skylev',count=njj)
     if njj eq 0 then skylev0 = 0.0
     skysig0   = sxpar(hd0,'skysig',count=njj)
     if njj eq 0 then skysig0 = 0.0
     skysigbx0 = sxpar(hd0,'skysigbx',count=njj)
     if njj eq 0 then skysigbx0 = 0.0
     skylev0   = afact*skylev0*texp[ii]
     skysig0   = afact*skysig0*texp[ii]
     skysigbx0 = afact*skysigbx0*texp[ii]
     plog,ll,prog,' (scaled) original skylev skysig skybox = '+numstr(skylev0)+'  '+numstr(skysig0)+'  '+numstr(skysigbx0)+'   scale factor = '+numstr(afact)
     ;
     ; fit surface to sky and subtract ...
     ; added July 2010
     boxdata  = make_array(6,1,/float,value=1.0)
     boxsize1 = -1
     poisson  = (ii GE 2)
     imgi     = img                       ; save input image
     plog,ll,prog,'fitting sky surface (going to SSOUP_ASKYFIT) ...'
     IF slow THEN keywait, 'type any key to continue: '    
     ssoup_askyfit, ll, img, msks, ord, skypar, eskypar, skysig1, skysigbx1, boxsize1, imgm, $
                    boxdata=boxdata, /subtract, /rebox, /verbose, poisson=poisson, goslow=slow
     ;
     ; redo delta sky calc
     plog,ll,prog,'calculating sky level in object pixels '
     pp       = where(mopix EQ mgoodval_out, npp)
     IF npp GT 0 THEN BEGIN 
        skylev1 = total(imgm[pp])/float(npp)
     ENDIF ELSE BEGIN 
        plog,ll,prog,'ERROR no object pixels ???? '
        stop
     ENDELSE
     plog,ll,prog,' delta_skylev skysig skysigbox = '+numstr(skylev1)+'  '+numstr(skysig1)+'  '+numstr(skysigbx1)
     ;
     ; measure delta sky correction and subtract
     ;boxdata  = make_array(4,1,/float,value=1.0)
     ;boxsize1 = -1
     ;poisson  = (ii GE 2)
     ;ssoup_askylev, ll, img, msks, skylev1, skysig1, skysigbx1, boxsize1, $
     ;               boxdata=boxdata, /subtract, /verbose, poisson=poisson
     ;plog,ll,prog,' delta_skylev skysig skybox = '+numstr(skylev1)+'  '+numstr(skysig1)+'  '+numstr(skysigbx1)
     ;
     ; derive total sky and
     ; place sky values in the header
     skylev    = skylev0+skylev1
     skysig    = skysig1
     skysigbox = skysigbx1
     plog,ll,prog,' final skylev skysig skybox = '+numstr(skylev)+'  '+numstr(skysig)+'  '+numstr(skysigbox)
     ;
     ; renormalize by exposure time
     plog,ll,prog,'renormalizing image and box data by exposure time : '+numstr(texp[ii])
     img          = img/texp[ii]
     skylev       = skylev/texp[ii]
     skysig       = skysig/texp[ii]
     skysigbox    = skysigbox/texp[ii]
     boxdata[0,*] = boxdata[0,*]/texp[ii]
     boxdata[3,*] = boxdata[3,*]/texp[ii]
     boxdata[4,*] = boxdata[4,*]/texp[ii]
     boxdata[5,*] = boxdata[5,*]/texp[ii]
     skypar       = skypar/texp[ii]
     eskypar      = eskypar/texp[ii]
     ;
     ; get description string for parameters and enclose in comments
     npar         = n_elements(skypar)
     nstr         = strtrim(string(indgen(npar)),2)
     dstr         = polysurf(0,0,skypar,/describe)
     pdescr       = ' Sky parameter '+nstr+' ('+dstr+' term)'
     epdescr      = ' Error on sky parameter '+nstr
     ;
     ; put sky parameters in header
     plog,ll,prog,' exposure time normalized skylev skysig skybox = '+numstr(skylev)+'  '+numstr(skysig)+'  '+numstr(skysigbox)
     sxaddpar, hdi, 'SKYLEV', skylev, ' box2box sky level'
     sxaddpar, hdi, 'SKYSIG', skysig, ' pixel-to-pixel sky RMS '
     sxaddpar, hdi, 'SKYSIGBX', skysigbox, ' box2box sky RMS '
     sxaddpar, hdi, 'SKYBOX', boxsize1, ' Box size used in box2box sky analysis [pixels]'
     sxaddpar, hdi, 'SKYSUB', 1, ' Sky subtraction complete? '
     sxaddpar, hdi, 'SKYORD', ord, ' Polynomial order of sky fit'
     for kk = 0, npar-1 do begin 
        sxaddpar, hdi, 'SKYPAR'+nstr[kk], skypar[kk], pdescr[kk]
        sxaddpar, hdi, 'ESKYPAR'+nstr[kk], eskypar[kk], epdescr[kk]
     endfor 
     ;
     ; shove in exptime and photflam for wise
     if bandavail[ii] eq band.mir_W1 or bandavail[ii] eq band.mir_W2 or bandavail[ii] eq band.mir_W3 or bandavail[ii] eq band.mir_W4 then begin
         sxaddpar, hdi, 'EXPTIME', texp[ii], ' Median exposure time in seconds'
         sxaddpar, hdi, 'PHOTFLAM', photwise[ii-4] ; fixme: remove this temporary hack
     endif
     ; Tidy the headers
     plog,ll,prog,'tidying header'
     ssoup_atidyhdr, ll, bandavail[ii], inputstr.fimages_in[ii], inputstr.fimages_out[ii], kfwhm[ii], img, hdi, hdo 
     ;
     ; write output images
     plog,ll,prog,'writing output fits file: '+inputstr.fimages_out[ii]
     fits_write, inputstr.fimages_out[ii], img, hdo
     IF slow THEN keywait, 'type any key to continue: '
     ;
     ; write output boxdata file
     plog,ll,prog,'writing output box data file: '+inputstr.fbox[ii]
     openw,lu,inputstr.fbox[ii],/get_lun
     sz = size(boxdata)
     nb = sz[2]
     FOR jj = 0, nb-1 DO printf,lu,boxdata[0,jj],boxdata[1,jj],boxdata[2,jj],boxdata[3,jj],boxdata[4,jj],boxdata[5,jj]
     plog,ll,prog,'information written for: '+numstr(nb)+' good boxes'
     free_lun,lu
     IF slow THEN keywait, 'type any key to continue: '
  endfor 
  ptr_free,imgs,hdcompile0,hdcompile1 ; don't leak memory
  plog,ll,prog,'finished '
end 
