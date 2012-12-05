PRO plot_field, fgaus, linplt, title=title, hardfile=hardfile, jpg=jpg, $
                pickplots=pickplots, layout=layout, $
                background=background, noempty=noempty
   ffpos      =  'fiberpos.txt'
   exptimes   = 'exptimes.dat'
   fbacklist  =  'backlist.dat'
   fmtg       =  '(i,a,a,x,x,x,f,f,f,f,f,f,f,i,f,f)'
   charsize   = 1.5
   symsize    = 3.0
   ct         = 33
   loadct, 0, /silent
   velrange   = [-200.0,200.0]
   widthrange = [0,420.0]
   velstep    = 50.0
   widthstep  = 50.0
   fluxstep   = 1.0
   eqwstep    = 50.0
   fibdia     = 4.687        ; fiber diameter in arcseconds
   fibspc     = 5.63         ; fiber spacing in arcseconds
   pi         = 3.14159265
   cvel       = 299792.458
   lamzero    = [6548.1001,6562.8169,6583.6001,6678.15,6716.4702,6730.8501]
   nlines     = n_elements(lamzero)
   IF keyword_set(layout) AND NOT(keyword_set(pickplots)) THEN BEGIN
       print, 'Cannot set layout without choosing plots'
       RETURN
   ENDIF ELSE BEGIN
       IF NOT(keyword_set(layout)) AND keyword_set(pickplots) THEN BEGIN
           print, 'Cannot choose plots without setting layout'
           RETURN
       ENDIF
   ENDELSE
   IF NOT(keyword_set(layout)) THEN layout = [2,2]
   IF NOT(keyword_set(pickplots)) THEN pickplots = [1,1,1,1]
   IF total(pickplots) NE layout(0)*layout(1) THEN BEGIN
       print, 'Number of plots doesnt match layout'
       RETURN
   ENDIF
   winratio   = layout(0)/(1.1*layout(1) + 0.12)
   plotpixx   = 450
   nplots     = 4
   titles     = ['Peak velocity (km/s)','log(Flux [counts/s])',$
              'FWHM (km/s)','Equivalent width (!3'+angstsym()+'!6)']
   xtitle     = 'arcseconds'
   ytitle     = 'arcseconds'
   ticks      = [4,1,5,5]
   barposrel  = [0.15, 1.04/1.1, 0.95, 0.98]
   plotposrel = [0.15, 0.12/1.1, 0.95, 0.92/1.1]
   barposdev1 = round(plotpixx * (barposrel * [1,0,1,0] + $
     barposrel * [0,1,0,1]*1.1))
   plotposdev1= round(plotpixx * (plotposrel * [1,0,1,0] + $
     plotposrel * [0,1,0,1]*1.1))
   plotsize   = plotpixx * (plotposrel(2) - plotposrel(0))
   xscale     = float(layout(0))
   yscale     = (1.1*layout(1) + 0.12)/1.1
   barposrel  = barposrel / (xscale*[1,0,1,0] + yscale*[0,1,0,1])
   plotposrel = plotposrel / (xscale*[1,0,1,0] + yscale*[0,1,0,1])
   xshift     = [1.0, 0.0, 1.0, 0.0] / layout(0)
   yshift     = [0.0, 1.1, 0.0, 1.1] / (1.1*layout(1) + 0.12)
   xshiftdev  = [1, 0, 1, 0] * plotpixx
   yshiftdev  = round([0.0, 1.1, 0.0, 1.1] * plotpixx)
   plotcount  = 0
   barpos     = make_array(nplots,4)
   plotpos    = make_array(nplots,4)
   barposdev  = make_array(nplots,4)
   plotposdev = make_array(nplots,4)
   FOR ploty = 0, layout(1)-1 DO BEGIN
       FOR plotx = 0, layout(0)-1 DO BEGIN
           barpos(plotcount,*)     = barposrel + plotx*xshift + $
             (layout(1) - ploty - 1)*yshift
           plotpos(plotcount,*)    = plotposrel + plotx*xshift + $
             (layout(1) - ploty - 1)*yshift
           barposdev(plotcount,*)  = barposdev1 + plotx*xshiftdev + $
             (layout(1) - ploty - 1)*yshiftdev
           plotposdev(plotcount,*) = plotposdev1 + plotx*xshiftdev + $
             (layout(1) - ploty - 1)*yshiftdev
           plotcount=plotcount+1
       ENDFOR
   ENDFOR
   plotsizenorm = plotpos(*,2:3) - plotpos(*,0:1)
   plotsizedev  = plotposdev(0,2:3) - plotposdev(0,0:1)

   titlepos     = [0.5, 1.0 - 0.09/(1.1*layout(1) + 0.12)]
   skyfib       = [2, 16, 22, 37, 54, 70, 80]
   ;
   ; define fiber shape
   angle   = 2.0 * pi / 100.0 * findgen(100)
   fiboutx = 0.5 * fibspc * cos(angle)
   fibouty = 0.5 * fibspc * sin(angle)
   fibinx  = 0.5 * fibdia * cos(angle)
   fibiny  = 0.5 * fibdia * sin(angle)
   ;
   ; read fiber positions
   readcol, ffpos, fid, dxfib, dyfib, format='(i,f,f)', /silent
   ;
   ; read gaussian fit positions
   readcol, fgaus, row, filstr, filstr2, chisq, rms, c1, c2, c3, $
     c4, c5, linnum, zz, rvel, format=fmtg, /silent
   nr = n_elements(row)
   ;
   ; read equivalent widths
   gausspos = strpos(fgaus, 'gauss')
   feqw     = strmid(fgaus, 0, gausspos) + 'eqw' + $
     strmid(fgaus, gausspos+5)
   readcol, feqw, eqw, format='(x,x,x,x,x,x,x,f,x)', /silent
   ;
   ; read exposure times
   readcol, exptimes, filename, nexp, expt, $
     format='(x,x,x,a,i,f)', /silent
   time  = 1800.0
   thisfile = 'obj'+strmid(fgaus,strpos(fgaus,'gauss')+5,13)
   fileline = where(filename EQ thisfile)
   IF fileline NE -1 THEN time = expt(fileline)
   ;
   ; find the dispersion
   gausspos   = strpos(fgaus, 'gauss')
   fitsfile   = strmid(fgaus, 0, gausspos) + 'obj' + $
     strmid(fgaus, gausspos+5, 13) + '.co.cr.ms.fits'
   fitsheader = headfits(fitsfile)
   deltloc    = where(strmid(fitsheader,0,6) EQ 'CDELT1')
   disper     = float(strmid(fitsheader(deltloc), $
                             strpos(fitsheader(deltloc), '=')+1))
   ;
   ; decode apertures from filstr
   id = make_array(nr, /int, value=0)
   FOR ii = 0, nr-1 DO BEGIN 
      k2   = strpos(filstr2[ii],']',/reverse_search)
      istr = strmid(filstr2[ii],0,k2)
      id[ii] = fix(istr)
   ENDFOR 
   ;
   ; match apertures
   ptr = make_array(nr, /int, value=-1)
   FOR ii = 0, nr-1 DO BEGIN 
      jj = where(fid EQ id[ii], njj)
      IF njj EQ 1 THEN ptr[ii] = jj
   ENDFOR
   ;
   ; establish the velocity and width ranges
   vv    = make_array(nr)
   fwhm  = make_array(nr)
   FOR ii = 0, nlines-1 DO BEGIN
       thisline = where(linnum EQ ii+1, nthisline)
       IF nthisline GT 0 THEN BEGIN
           vv(thisline)   = c4(thisline)*cvel/lamzero(ii)
           fwhm(thisline) = c5(thisline)*cvel/lamzero(ii)
       ENDIF
   ENDFOR

   nsky   = n_elements(skyfib)
   notsky = make_array(nr, /integer, value=1)
   FOR ii = 0, nsky-1 DO BEGIN
       issky = where(id EQ skyfib(ii), nskyii)
       IF nskyii GT 0 THEN notsky(issky) = 0
   ENDFOR
   notskylist = where(notsky EQ 1)

   vv         = vv - 0.5*(max(vv(notskylist)) + min(vv(notskylist)))

   maxvel     = velstep*ceil(max(vv(notskylist))/velstep)
   velrange   = [-maxvel,maxvel]
   maxwidth   = velstep*ceil(max(fwhm)/widthstep)
   widthrange = [0,maxwidth]

   ;
   ; isolate the line to plot
   kk  = where(linnum EQ linplt, nkk)
   IF nkk EQ 0 THEN BEGIN
       print, 'no measurements to plot'
       RETURN
   ENDIF
   print, 'number of measurements to plot: ', nkk
   xpos = dxfib[ptr[kk]]
   ypos = dyfib[ptr[kk]]
   id   = id[kk]
   row  = row[kk]
   chisq = chisq[kk]
   rms   = rms[kk]
   c1    = c1[kk]
   c2    = c2[kk]
   c3    = c3[kk]
   c4    = c4[kk]
   c5    = c5[kk]
   zz    = zz[kk]
   vv    = vv[kk]
   fwhm  = fwhm[kk]
   eqw   = eqw[kk]
   logflux = alog10(1.0647*c3*c5/(disper(0)*time(0)))
   ;
   ; determine range to plot
   xrange = [40.0, -80.0]
   yrange = [-40.0, 80.0]

   maxflux   = fluxstep*ceil(max(logflux)/fluxstep)
   fluxrange = [-2.0,maxflux]

;   de         = max(eqw) - min(eqw)
;   eqwrange  = [0.0, max(eqw)+0.1*de]
   maxeqw   = eqwstep*ceil(max(eqw)/eqwstep)
   eqwrange = [0,maxeqw]

   range      = make_array(nplots,2)
   range(0,*) = velrange
   range(1,*) = fluxrange
   range(2,*) = widthrange
   range(3,*) = eqwrange
   ;
   ; convert wavelengths, fluxes and widths to colors
   clr      = make_array(nplots,nkk)
   clr(0,*) = fix(255.0*(vv - range[0,0])/(range[0,1] - range[0,0])+0.5)
   clr(1,*) = fix(255.0*(logflux - range[1,0])/(range[1,1] - range[1,0])+0.5)
   clr(2,*) = fix(255.0*(fwhm - range[2,0])/(range[2,1] - range[2,0])+0.5)
   clr(3,*) = fix(255.0*(eqw - range[3,0])/(range[3,1] - range[3,0])+0.5)
   skycross = make_array(nkk)
   IF max(clr) GT 255 OR min(clr) LT 0 THEN BEGIN
       skycross(where(clr(0,*) GT 255 OR clr(0,*) LT 0)) = 1
       IF max(clr) GT 255 THEN clr(where(clr GT 255)) = 255
       IF min(clr) LT 0 THEN clr(where(clr LT 0)) = 0
   ENDIF
   ;
   ; set plot parameters
   IF keyword_set(hardfile) THEN BEGIN 
      xs = 3.0*layout(0)
      ys = xs/winratio
      yoff=1.0
      xoff=1.0
      thick=2
      set_plot,'ps',/copy, /interpolate
      IF strpos(strlowcase(hardfile), '.eps') GT 0 THEN $
         device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
         /encapsulated, bits_per_pixel=8 ELSE $
         device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
         bits_per_pixel=8
      charsize = 0.4*charsize
      symsize  = 0.6*symsize
   ENDIF ELSE BEGIN 
      wxsize   = plotpixx*layout(0)
      wysize   = fix(float(wxsize)/winratio)
      thick    = 1
      window, 0, xsize=wxsize, ysize=wysize
   ENDELSE 
   ;
   ; make background image if required
   IF keyword_set(background) THEN BEGIN
       ;
       ; read image information
       readcol, fbacklist, bgname, bgfname, bgra1, bgra2, bgdec1, bgdec2, $
         format='(a,a,a,a,a,a)'
       bg         = where(bgname EQ background)
       ;
       ; load image
       read_jpeg, bgfname(bg), bgimage, /grayscale
       ;
       ; get ra and dec information
       get_coords, bgbl, instring = (bgra1(bg) + ' ' + bgdec1(bg))(0)
       get_coords, bgtr, instring = (bgra2(bg) + ' ' + bgdec2(bg))(0)
       raloc      = where(strmid(fitsheader, 0, 3) EQ 'RA ')
       quote1     = strpos(fitsheader(raloc), "'")
       quote2     = strpos(fitsheader(raloc), "'", /reverse_search)
       rastring   = strmid(fitsheader(raloc), quote1 + 1, quote2 - quote1 - 1)
       decloc     = where(strmid(fitsheader, 0, 4) EQ 'DEC ')
       quote1     = strpos(fitsheader(decloc), "'")
       quote2     = strpos(fitsheader(decloc), "'", /reverse_search)
       decstring  = strmid(fitsheader(decloc), quote1 + 1, quote2 - quote1 - 1)
       adstring   = rastring + ' ' + decstring
       adstring   = adstring(0)
       get_coords, fieldad, instring = adstring
       raoffloc   = where(strmid(fitsheader, 0, 7) EQ 'RAOFFST')
       quote1     = strpos(fitsheader(raoffloc), "'")
       quote2     = strpos(fitsheader(raoffloc), "'", /reverse_search)
       raoffstring= strmid(fitsheader(raoffloc), quote1 + 1, quote2 - quote1 - 1)
       decoffloc  = where(strmid(fitsheader, 0, 8) EQ 'DECOFFST')
       quote1     = strpos(fitsheader(decoffloc), "'")
       quote2     = strpos(fitsheader(decoffloc), "'", /reverse_search)
       decoffstring = strmid(fitsheader(decoffloc), quote1 + 1, quote2 - quote1 - 1)
       adoffstring  = raoffstring + ' ' + decoffstring
       adoffstring  = adoffstring(0)
       get_coords, offsetad, instring = adoffstring
       fieldad    = fieldad - offsetad
       IF fieldad(0) LT -24.0 THEN fieldad(0) = fieldad(0) + 24.0
       IF fieldad(0) GT 24.0 THEN fieldad(0) = fieldad(0) - 24.0
       ;
       ; select appropriate section of image and enlarge it
       rarange    = fieldad(0) + [40.0, -80.0] / (15.0 * 3600.0 * cos(fieldad(1)*!pi/180.0))
       decrange   = fieldad(1) + [-40.0, 80.0]/3600.0
       tickx      = where(bgimage(65:*, 65) LT 50, ntickx) + 65
       ticky      = where(bgimage(65, 65:*) LT 50, nticky) + 65
       backpixx   = [tickx(0), tickx(ntickx-1)]
       backpixy   = [ticky(0), ticky(nticky-1)]
       bgimage    = bgimage(backpixx(0):backpixx(1), backpixy(0):backpixy(1))
       scalefac   = (plotsizedev(1)+1) * (bgtr(1) - bgbl(1)) / (120.0/3600.0 * $
                          (backpixy(1) - backpixy(0)))
       bigbg      = bilinear(bgimage, findgen(fix((backpixx(1) - backpixx(0)) * $
                             scalefac)) / scalefac, $
                             findgen(fix((backpixy(1) - backpixy(0)) * $
                             scalefac)) / scalefac)
       newbgbl    = round([(rarange(0) - bgbl(0)) * scalefac * $
                           (backpixx(1) - backpixx(0)) / (bgtr(0) - bgbl(0)), $
                           (decrange(0) - bgbl(1)) * scalefac * $
                           (backpixy(1) - backpixy(0)) / (bgtr(1) - bgbl(1))])
       bigbg      = bigbg(newbgbl(0):newbgbl(0) + plotsizedev(0), $
                          newbgbl(1):newbgbl(1) + plotsizedev(1))
   ENDIF 
   ;
   ; loop over graphs
   plotcount = 0
   FOR jj = 0, nplots-1 DO BEGIN
       ;
       ; initialize graph
       loadct, 0, /silent
       IF pickplots(jj) EQ 1 THEN BEGIN
           IF keyword_set(background) THEN BEGIN
               IF keyword_set(hardfile) THEN tv, bigbg, plotpos(plotcount,0), $
                 plotpos(plotcount,1), xsize=plotsizenorm(plotcount,0), $
                 ysize=plotsizenorm(plotcount,1), /normal $
               ELSE tv, bigbg, plotposdev(plotcount,0), $
                 plotposdev(plotcount,1), /device
           ENDIF
           IF keyword_set(hardfile) THEN plot, dxfib, dyfib, xrange=xrange, $
             yrange=yrange, xstyle=1, ystyle=1, /isotropic, $
             psym=sym(6), symsize=symsize, /nodata, $
             charsize=charsize, xtitle=xtitle, ytitle=ytitle, $
             title=titles(jj), position=plotpos(plotcount,*), /noerase, $
             /normal  $
           ELSE plot, dxfib, dyfib, xrange=xrange, yrange=yrange, $
             xstyle=1, ystyle=1, /isotropic, $
             psym=sym(6), symsize=symsize, /nodata, $
             charsize=charsize, xtitle=xtitle, ytitle=ytitle, $
             title=titles(jj), position=plotposdev(plotcount,*), /noerase, $
             /device
           FOR ii = 0, n_elements(fid)-1 DO BEGIN
               IF NOT(keyword_set(noempty)) OR where(id EQ ii+1) NE -1 THEN BEGIN
                   polyfill, dxfib(ii)+fiboutx, dyfib(ii)+fibouty, color=0
                   polyfill, dxfib(ii)+fibinx, dyfib(ii)+fibiny, color=255
               ENDIF 
           ENDFOR
           loadct, ct, /silent
           ;
           ; plot field
           FOR ii = 0, nkk-1 DO BEGIN
              xx  = [xpos[ii]]
              yy  = [ypos[ii]]
              cc  = clr[jj,ii]
              polyfill, xx(0)+fibinx, yy(0)+fibiny, color=cc
              IF skycross[ii] EQ 1 AND jj EQ 0 THEN BEGIN
                  loadct, 0, /silent
                  delta = fibdia / (2.0 * sqrt(2.0))
                  plots, [xx-delta, xx+delta], $
                    [yy-delta, yy+delta], color=255
                  plots, [xx-delta, xx+delta], $
                    [yy+delta, yy-delta], color=255
                  loadct, ct, /silent
              ENDIF
           ENDFOR
           ;
           ; plot colorbar
           ticks(1) = fluxrange(1) - fluxrange(0)
           colourbar, barpos(plotcount,*), ct, 0, range(jj,*), $
             charsize=charsize, ticks=ticks(jj), minor=5
           plotcount = plotcount + 1
       ENDIF
   ENDFOR
   loadct, 0, /silent
   ;
   ; print a title, if given
   IF keyword_set(title) THEN BEGIN
       xyouts, titlepos(0), titlepos(1), title, color=0, $
         alignment=0.5, /normal, charsize=2.0*charsize
   ENDIF
   ;
   ; make a .jpg file if required
   IF keyword_set(jpg) AND NOT(keyword_set(hardfile)) THEN BEGIN
       im = tvrd(true=3)
       WRITE_JPEG,jpg,im,TRUE=3,QUALITY=100
   ENDIF
   ;
   ; make a .ps file if required
   IF keyword_set(hardfile) THEN BEGIN 
      psend, hardfile, /noprint, /clobber
      set_plot,'x'
   ENDIF 
END 
