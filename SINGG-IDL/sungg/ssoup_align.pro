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
  ; FIXME: hardoding!
  fwhm       = [0.0, 0.0, 5.3, 4.2] ; default seeing (Morrissey et al. 2007)
  COMMON bands, band
  nim        = n_elements(inputstr.fimages_in)
  nthresh    = 1024l            ; used to determine where to trim input optical images
  rot        = fltarr(nim)  ; initialize rotations
  scale      = fltarr(nim)  ; inititalize scale
  ufid       = 2                ; points to fiducial uv image
  ofid       = 0                ; points to fiducial optical image
  kbox       = 25               ; convolution kernel size in pixels
  prog       = 'SSOUP_ALIGN: '  ; string for messages
  ;
  ; do some error checking
  plog,ll,prog,'-------------------------- starting '+prog+'----------------------------'
  slow       = keyword_set(goslow)
  if nim ne n_elements(inputstr.fimages_out) then $
     plog,-1,prog,'****WARNING: Number of output images NE number of input images'
  if nim ne 4 then $
     plog,-1,prog,'****WARNING: I think I can only cope with 4 images...'
  ;
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
  texp       = make_array(nim, /float, value=1.0)
  ;
  ; read input images
  ;
  ;FIXME: needs a for loop
  plog,ll,prog,'reading input images'
  ir = where(band eq 'R', /null)
  ir = ir[0]
  ih = where(band eq 'HALPHA', /null)
  ih = ih[0]
  ifuv = where(band eq 'FUV', /null)
  ifuv = ifuv[0]
  inuv = where(band eq 'NUV', /null)
  inuv = inuv[0]
  fits_read, inputstr.fimages_in[ir], imgr, hdr
  fwhm[ir]    = SXPAR(hdR,'SEEING')
  photflamR  = SXPAR(hdR,'PHOTFLAM')
  photplamR  = SXPAR(hdR,'PHOTPLAM')
  texp[ir]    = sxpar(hdr,'EXPTIME')
  imgr       = imgr*texp[ir]
  getrot, hdr, dum, cdelt
  rot[1]     = dum
  scale[ir]   = asdeg*sqrt(abs(cdelt[0]*cdelt[1]))
  ;
  fits_read, inputstr.fimages_in[ih], imgHa, hdHa
  fwhm[ih]    = SXPAR(hdha,'SEEING')
  photfluxHa = SXPAR(hdHa,'PHOTFLUX')
  texp[ih]    = sxpar(hdha,'EXPTIME')
  imgha      = imgha*texp[ih]
  ;photplamHa = SXPAR(hdHa,'FILTER1')
  getrot, hdha, dum, cdelt
  rot[0]     = dum
  scale[ih]   = asdeg*sqrt(abs(cdelt[0]*cdelt[1]))
  ;
  fits_read, inputstr.fimages_in[inuv], imgn, hdn
  texp[inuv]    = sxpar(hdn,'EXPTIME')
  imgn       = imgn*texp[inuv]
  getrot, hdn, dum, cdelt
  rot[2]     = dum
  scale[inuv]   = asdeg*sqrt(abs(cdelt[0]*cdelt[1]))
  ;
  fits_read, inputstr.fimages_in[ifuv], imgf, hdf
  texp[ifuv]    = sxpar(hdf,'EXPTIME')
  imgf       = imgf*texp[3]
  getrot, hdf, dum, cdelt
  rot[3]     = dum
  scale[ifuv]   = asdeg*sqrt(abs(cdelt[0]*cdelt[1]))
  ;
  ; convert fwhm values to pixels in the fiducial image
  fwhm       = fwhm / scale[ufid]
  ;
  ; compile headers
  plog,ll,prog,'compiling headers'
  hdcompile0 = [hdha, hdr, hdn, hdf]
  nhd0       = [n_elements(hdha), n_elements(hdr), n_elements(hdn), n_elements(hdf)]
  ihd00       = make_array(nim, /long, value=0l)
  ihd01       = make_array(nim, /long, value=1l)
  FOR ii = 1, 3 DO ihd00[ii] = ihd00[ii-1]+nhd0[ii-1]  ; start position of header
  ihd01       = ihd00 + nhd0 - 1l                      ; end position of header
  ;
  ; extract info for fiducial optical image
  plog,-1,prog,'determining fiducial images'
  case ofid of 
     1: begin
          oimg = imgr
          ohd  = hdr
        end
     0: begin
          oimg = imgha
          ohd  = hdha
        end
     2: begin
          oimg = imgn
          ohd  = hdn
        end
     3: begin
          oimg = imgf
          ohd  = hdf
        end
     else: begin
          oimg = imgr
          ohd  = hdr
          ofid = 0
        end
  endcase
  ohdp       = ofid
  siz        = size(oimg)
  nxo        = long(siz[1])
  nyo        = long(siz[2])
  ;
  ; extract info for fiducial UV image
  case ufid of 
     1: begin
          uimg = imgr
          uhd  = hdr
        end
     0: begin
          uimg = imgha
          uhd  = hdha
        end
     2: begin
          uimg = imgn
          uhd  = hdn
        end
     3: begin
          uimg = imgf
          uhd  = hdf
        end
     else: begin
          uimg = imgr
          uhd  = hdr
          ufid = 0
        end
  ENDCASE
  uhdp       = ufid
  siz        = size(uimg)
  nxu        = long(siz[1])
  nyu        = long(siz[2])
  ;
  ; make copies of the fiducial headers
  ohd        = hdcompile0[ihd00[ohdp]:ihd01[ohdp]]
  uhd        = hdcompile0[ihd00[uhdp]:ihd01[uhdp]]
  ;
  ; determine limits of pixels to transform using H-alpha image.
  ; do this by collapsing image in each dimension, and finding first
  ; and last row/column that are masked in less than some 
  ; threshold number of columns/rows
  plog,ll,prog,'determining pixel limits of optical images to transform'
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
  adxy,uhd,corna,cornd,cornxu,cornyu
  ;
  ; find pixel range to extract
  trminxu    = round(min(cornxu))
  trmaxxu    = round(max(cornxu))
  trminyu    = round(min(cornyu))
  trmaxyu    = round(max(cornyu))
  plog,ll,prog,'Limits of optical image (xmin, xmax, ymin, ymax):  '+numstr(trminxo)+'  '+numstr(trmaxxo)+'  '+numstr(trminyo)+'  '+numstr(trmaxyo)
  plog,ll,prog,'Limits of UV image (xmin, xmax, ymin, ymax)     :  '+numstr(trminxu)+'  '+numstr(trmaxxu)+'  '+numstr(trminyu)+'  '+numstr(trmaxyu)
  IF slow THEN keywait, 'type any key to continue: '
  ;
  ; make data cube for easy storage of intermediate products...
  nxx        = trmaxxu - trminxu + 1
  nyy        = trmaxyu - trminyu + 1
  imgtmp     = make_array(nxx, nyy, nim, /float, value=0.0)
  ;
  ; trim UV images based on the above coords
  plog,ll,prog,'trimming uv images'
  HEXTRACT,imgf,hdf,img,newhd3,trminxu,trmaxxu,trminyu,trmaxyu
  imgtmp[*,*,ifuv]  = img
  HEXTRACT,imgn,hdn,img,newhd2,trminxu,trmaxxu,trminyu,trmaxyu
  imgtmp[*,*,inuv]  = img
  ;
  ; rebin and align optical images based on new fuv image header
  plog,ll,prog,'transforming optical images'
  HASTROM,imgr,hdr,img,newhd0,newhd3,MISSING=0
  imgtmp[*,*,ir]  = img*(scale[ufid]/scale[0])^2
  HASTROM,imgha,hdha,img,newhd1,newhd3,MISSING=0
  imgtmp[*,*,ih]  = img*(scale[ufid]/scale[1])^2
  ;
  ; recompile headers
  ; another fixme
  hdcompile1 = [newhd1, newhd0, newhd2, newhd3]
  nhd1       = [n_elements(newhd1), n_elements(newhd0), n_elements(newhd2), n_elements(newhd3)]
  ihd10      = make_array(nim, /long, value=0l)
  ihd11      = make_array(nim, /long, value=1l)
  FOR ii = 1, 3 DO ihd10[ii] = ihd10[ii-1]+nhd1[ii-1]  ; start position of header
  ihd11      = ihd10 + nhd1 - 1l                      ; end position of header
  ;
  ; make data cube for input masks in output coord sys
  msktmp     = make_array(nxx, nyy, nim, /byte, value=0b)
  ;
  ; loop through masks and transform to new coord sys
  plog,ll,prog,'transforming masks'
  uhd        = hdcompile1[ihd10[uhdp]:ihd11[uhdp]]     ; fiducial header for transformation
  FOR ii = 0, nim-1 DO BEGIN 
     ;IF ii LE 1 THEN hdi = ohd ELSE hdi = uhd 
     hdi     = hdcompile0[ihd00[ii]:ihd01[ii]]         ; get input header from hdcompile0
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
  str        = '  '
  for ii = 0, nim-1 do str = str+numstr(kfwhm[ii])+'  '
  plog,ll,prog,'will convolve images and grow masks with kernel widths [pixels] (Ha, R, NUV, FUV): '+str
  for ii = 0, nim-1 do begin 
     hdi            = hdcompile1[ihd10[ii]:ihd11[ii]] ; input header after alignment
     img            = imgtmp[*,*,ii]
     imgo           = img
     if (ii ne ufid) and (kfwhm[ii] gt 0.0) then begin 
        plog,ll,prog,'working on convolving '+band[ii]+' band image '
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
        plog,ll,prog,'growing '+band[ii]+' band mask'
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
  msks     = ssoup_askymask(ll, inputstr.hname, hdcompile1[ihd10[ufid]:ihd11[ufid]], buffer, msko)
  ;
  ; Make object to measure only mask
  msks2    = ssoup_askymask(ll, inputstr.hname, hdcompile1[ihd10[ufid]:ihd11[ufid]], 0, msko)
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
  for ii = 0, nim-1 do begin
     hd0    = hdcompile0[ihd00[ii]:ihd01[ii]]  ; initial header
     hdi    = hdcompile1[ihd10[ii]:ihd11[ii]] ; input header after alignment
     img    = imgtmp[*,*,ii]     
     ord    = inputstr.skyord[ii]
     ;
     ; read in and adjust old sky values and errors for new pixel size
     plog,ll,prog,'retrieving original sky level for image #'+numstr(ii)+' (band = '+band[ii]+')'
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
     ; Tidy the headers
     plog,ll,prog,'tidying header'
     ssoup_atidyhdr, ll, band[ii], inputstr.fimages_in[ii], inputstr.fimages_out[ii], kfwhm[ii], img, hdi, hdo 
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
  plog,ll,prog,'finished '
end 
