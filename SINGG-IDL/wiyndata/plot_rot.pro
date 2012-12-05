PRO plot_rot, params, errors=errors, hardfile=hardfile, $
              jpg=jpg, plottype=plottype, background=background, fieldad=fieldad, $
              fibers=fibers, title=title, fcomb=fcomb
   IF datatype(params) EQ 'STR' THEN BEGIN
       readcol, params, col1, col2, format='(a,f)', /silent
       vsysloc    = where(col1 EQ 'vsys', nvsys)
       vsysloc    = vsysloc(nvsys-1)
       vsys       = col2(vsysloc)
       inc        = col2(vsysloc+1)
       xpos       = col2(vsysloc+2)
       ypos       = col2(vsysloc+3)
       pa         = col2(vsysloc+4)
       readcol, params, rr, vrot, err, format='(f,f,x,f)', /silent
       rrloc      = 0
       FOR ii = 1, n_elements(rr)-1 DO IF rr(ii) LT rr(ii-1) THEN rrloc = ii
       rr         = [0.0, rr(rrloc:*)]
       vrot       = [0.0, vrot(rrloc:*)]
       err        = [0.0, err(rrloc:*)]
   ENDIF ELSE BEGIN
       vsys       = params(0)
       inc        = params(1)
       xpos       = params(2)
       ypos       = params(3)
       pa         = params(4)
       nrr        = (n_elements(params) - 5) / 2
       rr         = params(5:4+nrr)
       vrot       = params(5+nrr:*)
       IF keyword_set(errors) THEN err = errors(5+nrr:*) ELSE err = -1
   ENDELSE
   fmtv       = '(i,f,f,f,f,f,f,f,f,f,f,f,f,f,f)'
   incrad     = inc / !radeg
   parad      = -pa / !radeg
   IF NOT(keyword_set(plottype)) THEN plottype = 0
   ffpos      =  'fiberpos.txt'
   fbacklist  =  'backlist.dat'
   xrange     = [40, -80]
   yrange     = [-40, 80]
   rrange     = [0, 55]
   skyfib     = [2, 16, 22, 37, 54, 70, 80]
   charsize   = 1.5
   titlesize  = 2.0 * charsize
   symsize    = 3.0
   ct         = 33
   loadct, 0, /silent
   fibdia     = 4.687        ; fiber diameter in arcseconds
   fibspc     = 5.63         ; fiber spacing in arcseconds
   velstep    = 50.0
   resstep    = 10.0
   cvel       = 299792.458
   plotpixx   = 450
   xtitle     = 'arcseconds'
   ytitle     = 'arcseconds'
   curvetitle = 'V!lrot!n (km/s)'
   smalltitle = 'Velocity (km/s)'
   plottype   = plottype(0)
   IF plottype EQ 3 THEN BEGIN
       layout     = [2,2]
       nplots     = 4
       smalltitle = ['Observed velocity (km/s)', 'Model velocity (km/s)', $
                     'Residual (km/s)', 'Rotation curve']
       graph      = [0,0,0,1]
   ENDIF ELSE BEGIN
       layout     = [1,1]
       nplots     = 1
       smalltitle = 'Velocity (km/s)'
       graph      = [0]
   ENDELSE
   IF nplots EQ 1 THEN titlesize = 0.8 * titlesize
   titlepos    = [0.5, 1.0 - 0.09/(1.1*layout(1) + 0.12)]
   winratio    = layout(0) / (1.1*layout(1) + 0.12)
   barposrel1  = [0.15, 1.04/1.1, 0.95, 0.98]
   plotposrel1 = [0.15, 0.12/1.1, 0.95, 0.92/1.1]
   barposdev1  = round(plotpixx * (barposrel1 * [1,0,1,0] + $
     barposrel1 * [0,1,0,1]*1.1))
   plotposdev1 = round(plotpixx * (plotposrel1 * [1,0,1,0] + $
     plotposrel1 * [0,1,0,1]*1.1))
   xscale      = float(layout(0))
   yscale      = (1.1*layout(1) + 0.12)/1.1
   barposrel1  = barposrel1 / (xscale*[1,0,1,0] + yscale*[0,1,0,1])
   plotposrel1 = plotposrel1 / (xscale*[1,0,1,0] + yscale*[0,1,0,1])
   xshift     = [1.0, 0.0, 1.0, 0.0] / layout(0)
   yshift     = [0.0, 1.1, 0.0, 1.1] / (1.1*layout(1) + 0.12)
   xshiftdev  = [1, 0, 1, 0] * plotpixx
   yshiftdev  = round([0.0, 1.1, 0.0, 1.1] * plotpixx)
   barposrel  = make_array(nplots,4)
   plotposrel = make_array(nplots,4)
   barposdev  = make_array(nplots,4)
   plotposdev = make_array(nplots,4)
   plotcount  = 0
   FOR ploty = 0, layout(1)-1 DO BEGIN
       FOR plotx = 0, layout(0)-1 DO BEGIN
           IF plotcount LT nplots THEN BEGIN
               barposrel(plotcount,*) = barposrel1 + plotx*xshift + $
                 (layout(1) - ploty - 1)*yshift
               plotposrel(plotcount,*) = plotposrel1 + plotx*xshift + $
                 (layout(1) - ploty - 1)*yshift
               barposdev(plotcount,*)  = barposdev1 + plotx*xshiftdev + $
                 (layout(1) - ploty - 1)*yshiftdev
               plotposdev(plotcount,*) = plotposdev1 + plotx*xshiftdev + $
                 (layout(1) - ploty - 1)*yshiftdev
               plotcount=plotcount+1
           ENDIF
       ENDFOR
   ENDFOR
   plotsizerel = plotposrel(0,2:3) - plotposrel(0,0:1)
   plotsizedev  = plotposdev(0,2:3) - plotposdev(0,0:1)
   ;
   ; define fiber shape
   angle      = 2.0 * !pi / 100.0 * findgen(100)
   fiboutx    = 0.5 * fibspc * cos(angle)
   fibouty    = 0.5 * fibspc * sin(angle)
   fibinx     = 0.5 * fibdia * cos(angle)
   fibiny     = 0.5 * fibdia * sin(angle)
   ;
   ; read fiber positions
   readcol, ffpos, fid, dxfib, dyfib, format='(i,f,f)', /silent
   nfibers    = n_elements(fid)
   IF NOT(keyword_set(fibers)) AND NOT(keyword_set(fcomb)) THEN $
     plotfibers = indgen(nfibers) $
   ELSE IF keyword_set(fibers) THEN BEGIN
       IF datatype(fibers) EQ 'STR' THEN BEGIN
           readcol, fibers, apid, format='(i)'
           plotfibers = apid - 1
       ENDIF ELSE plotfibers = fibers - 1
   ENDIF ELSE BEGIN
       readcol, fcomb, apid, format='(i)'
       plotfibers = apid - 1
   ENDELSE
   fid        = fid(plotfibers)
   dxfib      = dxfib(plotfibers)
   dyfib      = dyfib(plotfibers)
   nfibers    = n_elements(fid)
   ;
   ; calculate distance from center at each point
   xx         = findgen(plotsizedev(0) + 1) * (xrange(1) - xrange(0)) / $
                  plotsizedev(0) + xrange(0)
   xx         = xx # make_array(plotsizedev(1) + 1, value=1)
   yy         = findgen(plotsizedev(1) + 1) * (yrange(1) - yrange(0)) / $
                  plotsizedev(1) + yrange(0)
   yy         = make_array(plotsizedev(0) + 1, value=1) # yy
   aa         = -(xx - xpos) * sin(parad) + (yy - ypos) * cos(parad)
   bb         = ( -(xx - xpos) * cos(parad) - (yy - ypos) * sin(parad) ) / cos(incrad)
   rgrid      = sqrt( aa^2 + bb^2 )
   ;
   ; calculate vv at each point
   vrotgrid   = make_array(plotsizedev(0) + 1, plotsizedev(1) + 1)
   minr       = min(rr, minrpos)
   maxr       = max(rr, maxrpos)
   FOR ii = 0, plotsizedev(0) DO BEGIN
       FOR jj = 0, plotsizedev(1) DO BEGIN
           IF rgrid(ii, jj) LE minr THEN $
             vrotgrid(ii, jj) = vrot(minrpos) ELSE BEGIN
               IF rgrid(ii, jj) GE maxr THEN $
                 vrotgrid(ii, jj) = vrot(maxrpos) ELSE BEGIN
                   rfloorpos        = (where(rr GT rgrid(ii, jj)))(0) - 1
                   vrotgrid(ii, jj) = vrot(rfloorpos) + (rgrid(ii, jj) - $
                     rr(rfloorpos)) * (vrot(rfloorpos+1) - vrot(rfloorpos)) / $
                     (rr(rfloorpos+1) - rr(rfloorpos))
               ENDELSE
           ENDELSE
       ENDFOR
   ENDFOR
   rnonzero   = where(rgrid NE 0, complement=rzero, ncomplement=nrzero)
   costheta   = make_array(plotsizedev(0) + 1, plotsizedev(1) + 1)
   costheta(rnonzero) = aa(rnonzero) / rgrid(rnonzero)
   IF nrzero GT 0 THEN costheta(rzero)    = 0.0
   vv         = vsys + vrotgrid * costheta * sin(incrad)
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
      titlesize = 0.4*titlesize
      symsize  = 0.6*symsize
   ENDIF ELSE BEGIN 
      wxsize   = plotpixx*layout(0)
      wysize   = fix(float(wxsize)/winratio)
      thick    = 1
      window, 0, xsize=wxsize, ysize=wysize
   ENDELSE 
   ;
   ; set up the background, if required
   IF keyword_set(background) THEN BEGIN
       ;
       ; read image information
       background = background(0)
       readcol, fbacklist, bgname, bgfname, bgra1, bgra2, bgdec1, bgdec2, $
         format='(a,a,a,a,a,a)'
       bg         = where(bgname EQ background)
       ;
       ; load image
       read_jpeg, bgfname(bg), bgimage, /grayscale
       ;
       ; read coordinates from fits header if necessary
       IF datatype(fieldad) EQ 'STR' THEN BEGIN
           fitsheader = headfits(fieldad)
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
           get_coords, centerad, instring = adstring
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
           centerad   = centerad - offsetad
           IF centerad(0) LT -24.0 THEN centerad(0) = centerad(0) + 24.0
           IF centerad(0) GT 24.0 THEN centerad(0) = centerad(0) - 24.0
       ENDIF ELSE centerad = fieldad
       ;
       ; select appropriate section of image and enlarge it
       get_coords, bgbl, instring = (bgra1(bg) + ' ' + bgdec1(bg))(0)
       get_coords, bgtr, instring = (bgra2(bg) + ' ' + bgdec2(bg))(0)
       rarange    = centerad(0) + [40.0, -80.0] / (15.0 * 3600.0 * cos(centerad(1)*!pi/180.0))
       decrange   = centerad(1) + [-40.0, 80.0] / 3600.0
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
   IF plottype EQ 1 OR plottype EQ 3 THEN BEGIN
       clr        = make_array(nplots, nfibers)
       ;
       ; find mean v in each fiber
       meanv      = make_array(nfibers)
       FOR ii = 0, nfibers-1 DO $
         meanv(ii) = mean(vv(where( (xx - dxfib(ii))^2 + (yy - dyfib(ii))^2 $
                                    LE 0.5 * fibdia)))
       IF plottype EQ 1 THEN BEGIN
           maxvel     = velstep*max([ceil(max(meanv)/velstep), ceil(max(-meanv)/velstep)])
           velrange   = [-maxvel, maxvel]
           clr        = fix(255.0*(meanv - velrange(0))/(velrange(1) - velrange(0))+0.5)
           ranges     = velrange
       ENDIF ELSE BEGIN 
           ;
           ; read observed data
           readcol, fcomb, obsapid, z1, f1, z2, f2, z3, f3, z4, f4, z5, f5, z6, f6, $
             obsmeanz, obsmeanv, format=fmtv
           nap        = n_elements(apid)
           zgrid      = make_array(6,nap)
           fgrid      = make_array(6,nap)
           zgrid(0,*) = z1
           fgrid(0,*) = f1
           zgrid(1,*) = z2
           fgrid(1,*) = f2
           zgrid(2,*) = z3
           fgrid(2,*) = f3
           zgrid(3,*) = z4
           fgrid(3,*) = f4
           zgrid(4,*) = z5
           fgrid(4,*) = f5
           zgrid(5,*) = z6
           fgrid(5,*) = f6
           vgrid      = zgrid * cvel
           ;
           ; establish the velocity range
           nsky   = n_elements(skyfib)
           notsky = make_array(nap, /integer, value=1)
           FOR ii = 0, nsky-1 DO BEGIN
               issky = where(apid EQ skyfib(ii), nskyii)
               IF nskyii GT 0 THEN notsky(issky) = 0
           ENDFOR
           notskylist = where(notsky EQ 1)
           zvalues    = zgrid(*,notskylist)
           zvalues    = zvalues(where(zvalues GT 0))
           offset     = 0.5*cvel*(max(zvalues) + min(zvalues))
           vgrid      = vgrid - offset
           meanv      = meanv - offset
           maxvel     = velstep*ceil(max(vgrid(*,notskylist))/velstep)
           velrange   = [-maxvel,maxvel]
           clr(0,*)   = fix(255.0*(obsmeanv - velrange(0))/(velrange(1) - velrange(0))+0.5)
           clr(1,*)   = fix(255.0*(meanv - velrange(0))/(velrange(1) - velrange(0))+0.5)
           ;
           ; calculate residual and set separate velocity range
           residual   = meanv - obsmeanv
           maxres     = resstep * ceil(max([residual,-residual])/resstep)
           resrange   = [-maxres, maxres]
           clr(2,*)   = fix(255.0*(residual - resrange(0))/(resrange(1) - resrange(0))+0.5)
           ;
           ; find colours that have gone out of range
           skycross   = make_array(nplots,nfibers)
           IF max(clr) GT 255 OR min(clr) LT 0 THEN BEGIN
               skycross(where(clr GT 255 OR clr LT 0)) = 1
               IF max(clr) GT 255 THEN clr(where(clr GT 255)) = 255
               IF min(clr) LT 0 THEN clr(where(clr LT 0)) = 0
           ENDIF
           ranges     = transpose([[velrange],[velrange],[resrange]])
       ENDELSE
   ENDIF
   ;
   ; loop over graphs
   FOR jj = 0, nplots-1 DO BEGIN
       IF graph(jj) EQ 0 THEN BEGIN
           IF plottype EQ 0 THEN BEGIN
               ;
               ; draw the velocity field over the entire plot
               loadct, ct, /silent
               IF keyword_set(hardfile) THEN tvscl, vv, plotposrel(jj,0), $
                 plotposrel(jj,1), xsize=plotsizerel(0), $
                 ysize=plotsizerel(1), /normal $
               ELSE tvscl, vv, plotposdev(jj,0), $
                 plotposdev(jj,1), /device
               loadct, 0, /silent
           ENDIF ELSE IF keyword_set(background) THEN BEGIN
               IF keyword_set(hardfile) THEN tv, bigbg, plotposrel(jj,0), $
                 plotposrel(jj,1), xsize=plotsizerel(0), $
                 ysize=plotsizerel(1), /normal $
               ELSE tv, bigbg, plotposdev(jj,0), $
                 plotposdev(jj,1), /device
           ENDIF
           ;
           ; draw axes
           IF keyword_set(hardfile) THEN plot, dxfib, dyfib, xrange=xrange, $
             yrange=yrange, xstyle=1, ystyle=1, /isotropic, $
             psym=sym(6), symsize=symsize, /nodata, $
             charsize=charsize, xtitle=xtitle, ytitle=ytitle, $
             title=smalltitle(jj), position=plotposrel(jj,*), /noerase, $
             /normal  $
           ELSE plot, dxfib, dyfib, xrange=xrange, yrange=yrange, $
             xstyle=1, ystyle=1, /isotropic, $
             psym=sym(6), symsize=symsize, /nodata, $
             charsize=charsize, xtitle=xtitle, ytitle=ytitle, $
             title=smalltitle(jj), position=plotposdev(jj,*), /noerase, $
             /device
           IF plottype EQ 1 OR plottype EQ 3 THEN BEGIN
               ;
               ; plot field
               FOR ii = 0, nfibers-1 DO polyfill, dxfib(ii)+fiboutx, dyfib(ii)+fibouty, color=0
               loadct, ct, /silent
               FOR ii = 0, nfibers-1 DO BEGIN
                   xx  = dxfib(ii)
                   yy  = dyfib(ii)
                   cc  = clr(jj,ii)
                   polyfill, xx(0)+fibinx, yy(0)+fibiny, color=cc
                   IF skycross[jj,ii] EQ 1 THEN BEGIN
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
               colourbar, barposrel(jj,*), ct, 0, ranges(jj,*), $
                 charsize=charsize, ticks=4, minor=5
           ENDIF
       ENDIF ELSE BEGIN
           IF err(0) EQ -1 THEN BEGIN
               IF keyword_set(hardfile) THEN plot, rr, vrot, xtitle=xtitle, $
                 ytitle=curvetitle, position=plotposrel(jj,*), /noerase, $
                 /normal, xrange=rrange, xstyle=1, title=curvetitle, $
                 charsize=charsize $
               ELSE plot, rr, vrot, xtitle=xtitle, $
                 ytitle=curvetitle, position=plotposdev(jj,*), /noerase, $
                 /device, xrange=rrange, xstyle=1, title=curvetitle, $
                 charsize=charsize
           ENDIF ELSE BEGIN
               toplot   = where(rr GE rrange(0) AND rr LE rrange(1))
               ymax     = max(vrot(toplot) + err(toplot))
               IF keyword_set(hardfile) THEN ploterror, rr, vrot, err, xtitle=xtitle, $
                 ytitle=curvetitle, position=plotposrel(jj,*), /noerase, $
                 /normal, xrange=rrange, xstyle=1, title=curvetitle, $
                 yrange=[0,ymax], charsize=charsize $
               ELSE ploterror, rr, vrot, err, xtitle=xtitle, $
                 ytitle=curvetitle, position=plotposdev(jj,*), /noerase, $
                 /device, xrange=rrange, xstyle=1, title=curvetitle, $
                 yrange=[0,ymax], charsize=charsize
           ENDELSE
       ENDELSE
   ENDFOR
   ;
   ; print a title, if given
   IF keyword_set(title) THEN BEGIN
       xyouts, titlepos(0), titlepos(1), title, color=0, $
         alignment=0.5, /normal, charsize=titlesize
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
