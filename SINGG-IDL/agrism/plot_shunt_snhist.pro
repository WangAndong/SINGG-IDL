PRO plot_shunt_snhist, fcat, hardfile=hardfile, uscale=uscale
   ;
   ; plot histogram of S/N for the various spectral classifications
   ; from shunt
   ;
   ; G. Meurer  02/2006
   ;
   lsnrange = [0.0, 2.5]
   dlsn     = 0.1
   mrange   = [-20.0, -8.0]
   dm       = 0.5
   charsize = 1.5
   xtitle1  = '!3 log[max(S/N)]'
   xtitle2  = '!3 grism mag'
   ytitle   = '!3 Number'
   ;
   ; determine number of catalogs to read
   ncat     = n_elements(fcat)
   ;
   readcol, fcat[0], id, xim, yim, mag, sclass, skylev, skysig, max_sn, $
    format='(i,f,f,f,a,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,f,f,f)'
   good   = where(max_sn GT 0.0, ngood)
   sclass = strtrim(sclass[good], 2)
   lsn    = alog10(max_sn[good])
   id     = id[good]
   xim    = xim[good]
   yim    = yim[good]
   mag    = mag[good]
   sclass = sclass[good]
   skylev = skylev[good]
   skysig = skysig[good]
   IF ncat GT 1 THEN BEGIN 
      FOR ii = 1, ncat-1 DO BEGIN 
         readcol, fcat[ii], id0, xim0, yim0, mag0, sclass0, skylev0, skysig0, max_sn0, $
          format='(i,f,f,f,a,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,f,f,f)'
         gg     = where(max_sn0 GT 0.0, ngg)
         sclass = [sclass, strtrim(sclass0[gg], 2)]
         lsn    = [lsn, alog10(max_sn0[gg])]
         id     = [id, id0[gg]]
         xim    = [xim, xim0[gg]]
         yim    = [yim, yim0[gg]]
         mag    = [mag, mag0[gg]]
         sclass = [sclass, sclass0[gg]]
         skylev = [skylev, skylev0[gg]]
         skysig = [skysig, skysig0[gg]]
      ENDFOR 
   ENDIF 
   ;
   ; make histograms
   k1     = where(sclass EQ 'U', nk1)
   k2     = where(sclass EQ 'O', nk2)
   k3     = where(sclass EQ 'E', nk3)
   k4     = where(sclass EQ 'A' OR sclass EQ 'M' OR sclass EQ 'K' OR sclass EQ 'B' OR sclass EQ 'S', nk4)
   ;
   ; log s/n histograms 
   nxhist = (lsnrange[1] - lsnrange[0])/dlsn
   xhist  = lsnrange[0] + dlsn*(findgen(nxhist) + 0.5)
   hist1  = histogram(lsn[k1], min=lsnrange[0], max=lsnrange[1], binsize=dlsn)
   hist2  = histogram(lsn[k2], min=lsnrange[0], max=lsnrange[1], binsize=dlsn)
   hist3  = histogram(lsn[k3], min=lsnrange[0], max=lsnrange[1], binsize=dlsn)
   hist4  = histogram(lsn[k4], min=lsnrange[0], max=lsnrange[1], binsize=dlsn)
   ;
   ; apparent mag histograms
   nmhist = (mrange[1] - mrange[0])/dm
   mhist  = mrange[0] + dm*(findgen(nmhist) + 0.5)
   histm1 = histogram(mag[k1], min=mrange[0], max=mrange[1], binsize=dm)
   histm2 = histogram(mag[k2], min=mrange[0], max=mrange[1], binsize=dm)
   histm3 = histogram(mag[k3], min=mrange[0], max=mrange[1], binsize=dm)
   histm4 = histogram(mag[k4], min=mrange[0], max=mrange[1], binsize=dm)
   ;
   ; scale 'U' entries if uscale set
   IF keyword_set(uscale) THEN BEGIN 
      hist1 = hist1 * uscale
      histm1 = histm1 * uscale
   ENDIF 
   ;
   ; make total histograms
   thist2  = hist1 + hist2
   thist3  = hist3 + thist2
   thist4  = hist4 + thist3
   yrange1 = [0.0, 1.1*float(max(thist4))]
   ;
   thistm2 = histm1 + histm2
   thistm3 = histm3 + thistm2
   thistm4 = histm4 + thistm3
   yrange2 = [0.0, 1.1*float(max(thistm4))]
   ;
   ; set up plots
   ;
   ; set the plot parameters
   setbgfg,!white,!black
   !p.multi = [0, 1, 2]
   IF keyword_set(hardfile) THEN BEGIN 
      xs = 6.5
      ys = 0.8*xs
      yoff=3.
      xoff=1.2
      thick    =   2
      set_plot,'ps',/copy, /interpolate
      IF strpos(strlowcase(hardfile), '.eps') GT 0 THEN $
         device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color,/encapsulated ELSE $
         device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color
      ;psp, /eps, xsize=xs, ysize=ys
   ENDIF ELSE BEGIN 
      wxsize   = 650
      wysize   = fix(0.8*float(wxsize))
      window, 0, xsize=wxsize, ysize=wysize
      thick    = 1
   ENDELSE 
   erase
   setplotcolors
   ;
   ; plot log(s/n) histogram
   !p.multi = [0,1,2]
   plotcolorfill, xhist, thist4, color=!green, bottom=thist3, xrange=lsnrange, yrange=yrange1, xstyle=1, ystyle=1, $
    xtitle=xtitle1, ytitle=ytitle, charsize=charsize
   plotcolorfill, xhist, thist3, color=!cyan, bottom=thist2, /noerase
   plotcolorfill, xhist, thist2, color=!white, bottom=hist1, /noerase
   plotcolorfill, xhist, hist1, color=!gray, /noerase
   ;
   ; plot apparent mag histogram
   !p.multi = [1,1,2]
   plotcolorfill, mhist, thistm4, color=!green, bottom=thistm3, xrange=mrange, yrange=yrange2, xstyle=1, ystyle=1, $
    xtitle=xtitle2, ytitle=ytitle, charsize=charsize
   plotcolorfill, mhist, thistm3, color=!cyan, bottom=thistm2, /noerase
   plotcolorfill, mhist, thistm2, color=!white, bottom=histm1, /noerase
   plotcolorfill, mhist, histm1, color=!gray, /noerase
   ;
   ; clean up
   !p.multi   = 0
   !p.noerase = 0
   IF keyword_set(hardfile) THEN BEGIN 
      psend, hardfile, /noprint, /clobber
   ENDIF 
END 
