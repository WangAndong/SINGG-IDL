PRO plot_vel_field, fcomb, all=all, hardfile=hardfile, jpg=jpg, $
                    title=title, background=background, noempty=noempty

   ffpos      =  'fiberpos.txt'
   fbacklist  =  'backlist.dat'
   fmtv       =  '(i,f,f,f,f,f,f,f,f,f,f,f,f,f,f)'
   charsize   = 1.5
   titlesize  = 2.0 * charsize
   symsize    = 3.0
   ct         = 33
   loadct, 0, /silent
   velstep    = 50.0
   fibdia     = 4.687        ; fiber diameter in arcseconds
   fibspc     = 5.63         ; fiber spacing in arcseconds
   pi         = 3.14159265
   cvel       = 299792.458
   layout     = [1,1]
   nplots     = 1
   ;
   ; read data
   readcol, fcomb, apid, z1, f1, z2, f2, z3, f3, z4, f4, z5, f5, z6, f6, $
     meanz, meanv, format=fmtv
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
   donotplot  = make_array(size(vgrid, /dimensions), /int)
   donotplot(where(vgrid EQ 0)) = 1
   IF keyword_set(all) THEN BEGIN
       ;
       ; select an appropriate layout
       linedata   = where(total(zgrid, 2) GT 0, nlinedata)
       IF nlinedata EQ 1 THEN layout = [2,1] ELSE $
         IF nlinedata EQ 3 THEN layout = [2,2] ELSE $
         layout = [3,ceil((nlinedata+1)/3.0)]
       nplots     = nlinedata+1
   ENDIF
   ;
   ; more parameters to set
   IF nplots EQ 1 THEN titlesize = 0.8 * titlesize
   winratio   = layout(0)/(1.1*layout(1) + 0.12)
   titles     = make_array(nplots, /string)
   titles(0)  = 'Flux-weighted mean velocity (km/s)'
   linenames = ['[NII] 6548','!6H!7a!6 6563','[NII] 6584',$
                'HeI 6678','[SII] 6716','[SII] 6731']
   IF keyword_set(all) THEN titles(1:*) = linenames(linedata) + $
     ' velocity (km/s)'
   xtitle     = 'arcseconds'
   ytitle     = 'arcseconds'
   plotpixx   = 450
   ticks      = 4
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
   barpos     = make_array(nplots,4)
   plotpos    = make_array(nplots,4)
   barposdev  = make_array(nplots,4)
   plotposdev = make_array(nplots,4)
   plotcount  = 0
   FOR ploty = 0, layout(1)-1 DO BEGIN
       FOR plotx = 0, layout(0)-1 DO BEGIN
           IF plotcount LT nplots THEN BEGIN
               barpos(plotcount,*) = barposrel + plotx*xshift + $
                 (layout(1) - ploty - 1)*yshift
               plotpos(plotcount,*) = plotposrel + plotx*xshift + $
                 (layout(1) - ploty - 1)*yshift
               barposdev(plotcount,*)  = barposdev1 + plotx*xshiftdev + $
                 (layout(1) - ploty - 1)*yshiftdev
               plotposdev(plotcount,*) = plotposdev1 + plotx*xshiftdev + $
                 (layout(1) - ploty - 1)*yshiftdev
               plotcount=plotcount+1
           ENDIF
       ENDFOR
   ENDFOR
   plotsizenorm = plotpos(*,2:3) - plotpos(*,0:1)
   plotsizedev  = plotposdev(0,2:3) - plotposdev(0,0:1)
   titlepos   = [0.5, 1.0 - 0.09/(1.1*layout(1) + 0.12)]
   xrange     = [40.0, -80.0]
   yrange     = [-40.0, 80.0]
   skyfib     = [2, 16, 22, 37, 54, 70, 80]
   ;
   ; define fiber shape
   angle   = 2.0 * pi / 100.0 * findgen(100)
   fiboutx = 0.5 * fibspc * cos(angle)
   fibouty = 0.5 * fibspc * sin(angle)
   fibinx = 0.5 * fibdia * cos(angle)
   fibiny = 0.5 * fibdia * sin(angle)
   ;
   ; read fiber positions
   readcol, ffpos, fid, dxfib, dyfib, format='(i,f,f)', /silent
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
   vgrid      = vgrid - 0.5*cvel*(max(zvalues) + min(zvalues))
   maxvel     = velstep*ceil(max(vgrid(*,notskylist))/velstep)
   velrange   = [-maxvel,maxvel]
   ;
   ; assign colours to velocities
   clr        = make_array(nplots,nap)
   clr(0,*)   = fix(255.0*(meanv - velrange(0))/(velrange(1) - velrange(0))+0.5)
   IF nplots GT 1 THEN clr(1:*,*) = $
     fix(255.0*(vgrid(linedata,*) - velrange(0))/(velrange(1) - velrange(0))+0.5)
   skycross = make_array(nplots,nap)
   IF max(clr) GT 255 OR min(clr) LT 0 THEN BEGIN
       skycross(where(clr GT 255 OR clr LT 0)) = 1
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
      charsize  = 0.4*charsize
      titlesize = 0.4*titlesize
      symsize   = 0.6*symsize
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
       combpos    = strpos(fcomb, 'comb')
       ffits      = strmid(fcomb, 0, combpos) + 'obj' + strmid(fcomb, combpos+4, 13) + $
         '.co.cr.ms.fits'
       fitsheader = headfits(ffits)
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
       IF keyword_set(background) THEN BEGIN
           IF keyword_set(hardfile) THEN tv, bigbg, plotpos(plotcount,0), $
             plotpos(plotcount,1), xsize=plotsizenorm(plotcount,0), $
             ysize=plotsizenorm(plotcount,1), /normal $
           ELSE tv, bigbg, plotpos(plotcount,0), $
             plotpos(plotcount,1), /normal
       ENDIF
       plot, dxfib, dyfib, xrange=xrange, yrange=yrange, $
         xstyle=1, ystyle=1, /isotropic, $
         psym=sym(6), symsize=symsize, /nodata, $
         charsize=charsize, xtitle=xtitle, ytitle=ytitle, $
         title=titles(jj), position=plotpos(plotcount,*), /noerase
       FOR ii = 0, n_elements(fid)-1 DO BEGIN
           plotflag = 1
           apinlist = where(apid EQ ii+1, isapinlist)
           IF keyword_set(noempty) THEN BEGIN
               IF isapinlist EQ 0 THEN plotflag = 0 $
               ELSE IF jj NE 0 THEN IF zgrid(linedata(jj-1), apinlist) EQ 0 THEN plotflag = 0
           ENDIF 
           IF plotflag EQ 1 THEN BEGIN
               polyfill, dxfib(ii)+fiboutx, dyfib(ii)+fibouty, color=0
               polyfill, dxfib(ii)+fibinx, dyfib(ii)+fibiny, color=255
           ENDIF
       ENDFOR
       loadct, ct, /silent
       ;
       ; plot field
       FOR ii = 0, nap-1 DO BEGIN
          plotflag = 0
          IF jj EQ 0 THEN plotflag = 1 ELSE $
            IF donotplot(linedata(jj-1),ii) EQ 0 THEN plotflag = 1
          IF plotflag EQ 1 THEN BEGIN
              xx  = dxfib(apid[ii]-1)
              yy  = dyfib(apid[ii]-1)
              cc  = clr[jj,ii]
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
          ENDIF
       ENDFOR
       ;
       ; plot colorbar
       colourbar, barpos(plotcount,*), ct, 0, velrange, $
         charsize=charsize, ticks=ticks, minor=5
       plotcount = plotcount + 1
   ENDFOR
   loadct, 0, /silent
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
