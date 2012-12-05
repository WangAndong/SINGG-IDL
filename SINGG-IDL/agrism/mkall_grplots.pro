PRO mkall_grplots, filimg, filspc, filrib, filcat, lrange, outdir, kludge_mrd=kludge_mrd, $
                   exptime=exptime, rate=rate, type=type
   ;
   ; Make all plots needed for grism analysis -1d and 2d.
   ;
   ;   filimg  ->  direct image file
   ;   filspc  ->  fits file of 1d spectra binary tables
   ;   filrib  ->  fits file containing 2d spectra cut outs
   ;   filcat  ->  SE catalog of direct image
   ;   lrange  ->  wavelength range for 1d plots
   ;   outdir  ->  directory for output plots
   ;   kludge_mrd ->  you don't wann know...
   ;   exptime ->  if set, exposure time of grism image
   ;   rate    ->  if set then the 1d spectra are in count rate
   ;   type    ->  type of direct image catalog: 'PEARS' or 'GTO'
   ;
   ; G. Meurer 08/2005 updated, added rate kwd.
   pstmp = outdir + 'stmp_'
   pspcc = outdir + 'spcc_'
   pspcf = outdir + 'spcf_'
   pribn = outdir + 'ribn_'
   pscii = outdir + 'spec_'
   flagz    = 9.99
   xsizepl  = 400
   ysizepl  = 180
   winposim = [900,850]
   sizstp   = [60,60]
   winpospl = [900,650]
   expnd    = 2
   window   = 0
   ;
   rd_grdirct_cat, filcat, id, xim, yim, magauto, aim, bim, thetaim, w50, class, type=type
   theta = thetaim * !pi / 180.0
   id    = fix(temporary(id))
   posim = [[xim], [yim]] 
   ;
   ; first make postage stamps of direct image
   fits_read,filimg,img,hdr
   print, 'Working on postage stamps ... '
   loadct,0
   FOR i = 0, n_elements(id)-1 DO plot_dirctstamp, img, id[i], [xim[i], yim[i]], sizstp, $
    expnd, window, pstmp, stampim
   ;
   ; Next make ribbon plots
   print, 'Working on ribbon plots ... '
   lunr = fxposit(filrib, 1)
   FOR i = 0, n_elements(id)-1 DO BEGIN
      image   = mrdfits(lunr, 0, hdr)
      sz      = size(image)
      szplt   = 2*[sz[1],sz[2]]
      implt   = rebin(image,szplt[0],szplt[1],/sample)
      window,window,xsize=szplt[0],ysize=szplt[1]
      tvscl,(-1.0*implt)
      name = namribn(pribn,id[i])
      im = tvrd(true=3)
      WRITE_JPEG,name,im,TRUE=3,QUALITY=100
   ENDFOR 
   close, lunr
   ;
   ; Finally make grism plots
   print, 'Working on 1d plots ... '
   luns = fxposit(filspc, 1)
   setplotcolors
   window,window,xsize=xsizepl,ysize=ysizepl
   FOR i = 0, n_elements(id)-1 DO BEGIN
      IF keyword_set(kludge_mrd) THEN bintab  = kludge_mrdfits(luns, 0, hdr) $
       ELSE bintab  = mrdfits(luns, 0, hdr)
      IF keyword_set(exptime) THEN texp = exptime ELSE texp = 1.0
      grism_updflux, bintab, exptime=texp, rate=rate
      orient  = [aim[i], bim[i], thetaim[i]]
      ;
      ; plot to graphics device
      grism_plot1d, bintab, id[i], orient=orient, lrange=lrange
      grism_fplot1d, bintab, id[i], lrange, orient=orient
      ;
      ; hardcopy plots
      grism_plot1d, bintab, id[i], prefix=pspcc, orient=orient, lrange=lrange
      grism_fplot1d, bintab, id[i], lrange, prefix=pspcf, orient=orient
      ;
      ; ascii file
      fil     = namdat(pscii,id[i])
      grism_outascii, fil, bintab, magauto[i], flagz, xim[i], yim[i], lamlim=lrange
   ENDFOR 
   close, luns
END 

