PRO shunt_direct_stamp, im_direct, hdrd, dxstamp, dystamp, zrange_d, id, rad, decd, bxra, bxdec, $
                        xoffset=xoffset, yoffset=yoffset, hardfile=hardfile
   ;
   ; Plot a postage stamp of the direct image
   ;
   ; im_direct -> direct image array
   ; hdrd      -> fits header for direct image
   ; dxstamp   -> xsize of postage stamp
   ; dystamp   -> ysize of postage stamp
   ; zrange_d  -> pixel display (grayscale) range for displaying
   ; rad       -> right ascension of source
   ; decd      -> declination of source
   ; bxra      -> right ascension corners of error box for source 
   ; bxdec     -> declination corners of error box for source
   ; hardfile  -> if set this is the name of the postscript or 
   ;              encapsulated postscript file that will be written,
   ;              if not set, the display is to window, 0
   ; xoffset   -> if set then this value is ADDED to the 
   ;              header predicted positions to get the real X position
   ;              for extraction
   ; yoffset   -> if set then this value is ADDED to the 
   ;              header predicted positions to get the real Y position
   ;              for extraction
   ;
   ; G. Meurer 09/2005
   ;
   ; calculate window size
   buf   = 50
   wxsize = 2*(buf+dxstamp)
   wysize = 2*(buf+dystamp)
   wsiz   = float([wxsize, wysize, wxsize, wysize])
   pos    = float([buf, buf, buf+2*dxstamp, buf+2*dystamp])/wsiz
   xtitle = '!3 Column [pixels]'
   ytitle = '!3 Row [pixels]'
   title  = '!3 direct image of source # '+strtrim(string(id),2)
   empty  = '!3 no image available'
   ;
   ; open window or postscript file
   loadct,0
   IF keyword_set(hardfile) THEN BEGIN 
      aspect = float(wysize)/float(wxsize)
      xs     = 2.0
      ys     = aspect*xs
      yoff   = 2.
      xoff   = 1.2
      thick  = 2
      set_plot,'ps',/copy, /interpolate
      charsize = 0.5
      IF strpos(strlowcase(hardfile), '.eps') GT 0 THEN $
         device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color,/encapsulated ELSE $
         device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color
      ;psp, /eps, xsize=xs, ysize=ys
   ENDIF  ELSE BEGIN 
      thick    = 1
      window, 0, xsize=wxsize, ysize=wysize
      charsize = 1.1
   ENDELSE 
   ;
   ; convert ra,dec positions to pixel coords
   adxy, hdrd, rad, decd, xx, yy
   adxy, hdrd, bxra, bxdec, bxxx, bxyy
   IF keyword_set(xoffset) THEN BEGIN 
      xx = xx + xoffset
      bxxx = bxxx + xoffset
   ENDIF 
   IF keyword_set(yoffset) THEN BEGIN 
      yy = yy + yoffset
      bxyy = bxyy + yoffset
   ENDIF 
   ;
   ; decide whether to display the grayscale or a dummy blank image
   sz = size(im_direct)
   IF xx GE 0 AND xx LE sz[1] AND yy GE 0 AND yy LE sz[2] THEN BEGIN 
      ;
      ; get pixel limits for plot
      i0 = fix(xx - 0.5*float(dxstamp)+0.5) > 0 < (sz[1]-1)
      i1 = i0 + dxstamp - 1 > 0 < sz[1]-1
      j0 = fix(yy - 0.5*float(dystamp)+0.5) > 0 < (sz[2]-1)
      j1 = j0 + dystamp - 1 > 0 < sz[2]-1
      ;
      ; make image to plot
      zr = ssqrt(zrange_d)
      im = im_direct[i0:i1,j0:j1]
      im = 255 - bytscl(ssqrt(im), min=zr[0], max=zr[1], top=255)
      xrange = [i0, i1] + [-0.5, 0.5]
      yrange = [j0, j1] + [-0.5, 0.5]
      kk     = indgen(5) MOD 4
      xplt   = bxxx[kk]
      yplt   = bxyy[kk]
      ;
      ; display plot, overplot with error box and put axes around the lot
      tvscale, im, position=pos, /nointerpolation
      plot, xplt, yplt, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, psym=0, linestyle=0, $
            position=pos, /noerase, xtitle=xtitle, ytitle=ytitle, charsize=charsize, title=title, thick=thick+1
   ENDIF ELSE BEGIN 
      ;
      ; plot empty panel with message in it
      xrange = [-0.5, 0.5]*dxstamp
      yrange = [-0.5, 0.5]*dystamp
      kk     = indgen(5) MOD 4
      xplt   = bxxx[kk]
      yplt   = bxyy[kk]
      xp     = 0.5*total(xrange)
      yp     = 0.5*total(yrange)
      ;
      ; display plot, overplot with error box and put axes around the lot
      plot, xplt, yplt, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, psym=0, /nodata, $
            position=pos, /noerase, xtitle=xtitle, ytitle=ytitle, charsize=charsize, title=title
      xyouts, xp, yp, empty, charsize=charsize, alignment=0.5
   ENDELSE 
   ;
   ; finish
   !p.multi   = 0
   !p.noerase = 0
   IF keyword_set(hardfile) THEN BEGIN 
      device,/close
      spawn,'mv -f idl.ps '+hardfile 
      device,/portrait
      set_plot,'X'
      setbgfg,255,0
   ENDIF 
END 
