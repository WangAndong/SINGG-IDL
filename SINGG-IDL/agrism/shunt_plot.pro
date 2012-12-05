PRO shunt_plot, id, xsq, ysq, gr_ribbon, gr_spec, gr_sky, gr_crcut, sq_stamp, $
                lim_ribbon, lim_spec, lim_crcut, lim_stamp, $
                zrange_gr, zrange_sq, hardfile=hardfile
   ;
   ; Put grayscales and plots onto a single window, for SN hunt project
   ; xsq,ysq    -> position of source in squashed grism image
   ; gr_ribbon  -> 2d ribbon image of spectrum in grism image
   ; gr_spec    -> 1d collapsed spectrum
   ; gr_sky     -> 1d collapsed sky spectrum
   ; gr_crcut   -> 1d cross dispersion cut
   ; sq_stmp    -> stamp of squashed image
   ; lim_ribbon -> pixel limits of ribbon in grism image
   ; lim_spec   -> pixel limits of 1d spec (before collapsing) in grism image
   ; lim_crcut  -> pixel limits of cross dispersion cut (before 
   ;               collapsing) in grism image
   ; lim_stamp  -> pixel limits of squashed image stamp
   ; zrange_gr  -> grayscale plot range for grism image
   ; zrange_sq  -> grayscale plot range for squashed grism image
   ;
   ; G. Meurer 09/2005
   ;           01/2006: now plot sky spectrum and spectrum before sky subtraction.
   ;
   buf      = 40
   ;
   ; compute window size and make window
   szrbn  = size(gr_ribbon)
   szstmp = size(sq_stamp)
   wxsize = 4*buf + 3*szrbn[1] + 2*szstmp[1]
   wysize = 3*buf + 6*szrbn[2] + 2*szstmp[2]
   ;
   ; compute position of ribbon and stamp
   wsiz    = float([wxsize, wysize, wxsize, wysize])
   posr    = float([2*buf, 2*buf+2*szstmp[2], buf+3*szrbn[1], 2*buf+2*szstmp[2]+6*szrbn[2]])/wsiz
   poss    = float([3*buf+3*szrbn[1], buf, 3*buf+3*szrbn[1]+2*szstmp[1], buf+2*szstmp[2]])/wsiz
   posspec = float([2*buf,buf,buf+3*szrbn[1],buf+2*szstmp[2]]/wsiz)
   poscut  = float([3*buf+3*szrbn[1], 2*buf+2*szstmp[2], $
                    3*buf+3*szrbn[1]+2*szstmp[1], 2*buf+2*szstmp[2]+6*szrbn[2]]/wsiz)
   loadct,0
   IF keyword_set(hardfile) THEN BEGIN 
      aspect = float(wysize)/float(wxsize)
      xs     = 6.5
      ys     = aspect*xs
      yoff   = 2.
      xoff   = 1.2
      thick  = 2
      set_plot,'ps',/copy, /interpolate
      charsize = 0.6
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
   ; display grayscale ribbon
   loadct,0
   zr      = ssqrt(zrange_gr)
   temp    = 255 - bytscl(ssqrt(gr_ribbon), min=zr[0], max=zr[1], top=255)
   tvscale, temp, position=posr, /nointerpolation
   ;
   ; put box around ribbon plot, draw lines at location of 
   ; spectrum cut
   xrange  = float([lim_ribbon[0], lim_ribbon[1]]) + [-0.5, 0.5]
   yrange  = float([lim_ribbon[2], lim_ribbon[3]]) + [-0.5, 0.5]
   xplt    = float([lim_spec[0], lim_spec[1], lim_spec[1], lim_spec[0], lim_spec[0]]) + $
             [-0.5, 0.5, 0.5, -0.5, -0.5]
   yplt    = float([lim_spec[2], lim_spec[2], lim_spec[3], lim_spec[3], lim_spec[2]]) + $
             [-0.5, -0.5, 0.5, 0.5, -0.5]
   title   = '!3 '+strtrim(string(id),2)+' ('+strtrim(string(xsq),2)+','+strtrim(string(ysq),2)+')'
   xtitle  = ' '
   ytitle  = '!3 row'
   plot, xplt, yplt, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, psym=0, linestyle=2, $
         position=posr, /noerase, xtitle=xtitle, ytitle=ytitle, charsize=charsize, title=title
   oplot, xplt, yplt, linestyle=0, color=255, thick=3
   plot, xplt, yplt, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, psym=0, linestyle=2, $
         position=posr, /noerase, xtitle=xtitle, ytitle=ytitle, charsize=charsize, title=title
   ;
   ; display grayscale stamp
   zr      = ssqrt(zrange_sq)
   temp    = 255 - bytscl(ssqrt(sq_stamp), min=zr[0], max=zr[1], top=255)
   tvscale, temp, position=poss, /nointerpolation
   ;
   ; put box around stamp, & circle around source
   xrange  = float([lim_stamp[0], lim_stamp[1]]) + [-0.5, 0.5]
   yrange  = float([lim_stamp[2], lim_stamp[3]]) + [-0.5, 0.5]
   xplt    = [xsq]
   yplt    = [ysq]
   xtitle  = '!3 column'
   ytitle  = '!3 row'
   plot, xplt, yplt, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, psym=sym(6), $
         position=poss, /noerase, symsize=3.5, xtitle=xtitle, ytitle=ytitle, charsize=charsize
   ;
   ; plot spectrum
   setplotcolors
   xplt    = float(lim_spec[0]) + findgen(szrbn[1])
   xrange  = [lim_spec[0], lim_spec[1]]
   y1      = min(gr_spec)
   y2      = max(gr_spec)
   yr      = y2 - y1
   yrange  = [y1, y2] + 0.05*yr*[-1.0, 2.5]
   xtitle  = '!3 column'
   ytitle  = '!3 flux'
   plot, xplt, gr_spec, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, psym=10, charsize=charsize, $
    xmargin=[5,2], ymargin=[2,2], position=posspec, /noerase, xtitle=xtitle, ytitle=ytitle
   yplt    = gr_spec + gr_sky
   oplot, xplt, yplt, psym=10, color=!dgray
   oplot, xplt, gr_sky, psym=10, color=!blue
   oplot, xplt, gr_spec, psym=10
   ;
   ; plot cut
   xplt    = float(lim_ribbon[2]) + findgen(szrbn[2])
   xrange  = [lim_ribbon[2], lim_ribbon[3]]
   y1      = min(gr_crcut)
   y2      = max(gr_crcut)
   yr      = y2 - y1
   yrange  = [y1, y2] + 0.1*yr*[-1.0, 1.0]
   xtitle  = '!3 row'
   ytitle  = ' '
   plot, xplt, gr_crcut, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, psym=10, charsize=charsize, $
    xmargin=[5,2], ymargin=[2,2], position=poscut, /noerase, xtitle=xtitle, ytitle=ytitle, ytickinterval=0.1
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
