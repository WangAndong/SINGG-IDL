PRO mkall_grplots_notsonew, filimg, filspc, filrib, filcat, outdir
   pstmp = outdir + 'stmp_'
   pspec = outdir + 'spcc_'
   pribn = outdir + 'ribn_'
   xsizepl  = 400
   ysizepl  = 180
   winposim = [900,850]
   sizstp   = [60,60]
   winpospl = [900,650]
   expnd    = 2
   window   = 0
   ;
   rd_grdirct_cat, filcat, id, xim, yim, magauto, aim, bim, thetaim, w50, class
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
      makepng,name,/color
   ENDFOR 
   close, lunr
   ;
   ; Finally make grism plots
   print, 'Working on 1d plots ... '
   luns = fxposit(filspc, 1)
   setplotcolors
   window,window,xsize=xsizepl,ysize=ysizepl
   FOR i = 0, n_elements(id)-1 DO BEGIN
            bintab  = mrdfits(luns, 0, hdr)
      grism_plot1d, bintab, id[i], pspcc, orient=[aim[i], bim[i], thetaim[i]]
   ENDFOR 
   close, luns
END 

