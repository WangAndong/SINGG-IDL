FUNCTION extract_collapsex, image, xr, yr
   ;
   ; Extract a rectangular region of an image; 
   ; collapse along rowa to make 1d column array
   i0  = fix(min(float(xr))+0.5)
   i1  = fix(max(float(xr))+0.5)
   j0  = fix(min(float(yr))+0.5)
   j1  = fix(max(float(yr))+0.5)
   ni  = i1 - i0 + 1
   sub = image[i0:i1,j0:j1]
   sp  = make_array(ni,/float)
   FOR i = 0, ni-1 DO BEGIN 
      sp[i] = total(sub[i,*])
   ENDFOR 
   return, sp
END 

PRO test_grshift
   filg   = 'j8eb01g800l_sh133_msk.fits'
   fild   = 'detectionImage_sharp133.fits'
   dxmed  = 13
   ; obj142
   ; xobj   = 2737.903
   ; yobj   = 2923.160
   ; dy     = 2.0
   ; obj 257
   xobj   = 834.051
   yobj   = 1883.417
   dy     = 2.0
   ; obj 265
   ; xobj   = 889.311
   ; yobj   = 1775.457 
   ; dy     = 4
   dxbeam = [-10, 150]
   ;
   k0     = fix(xobj - dxbeam[1] - 0.5*dxmed + 0.5)
   k1     = fix(xobj - dxbeam[0] + 0.5*dxmed + 0.5)
   j0     = fix(yobj - 0.5*dy + 0.5)
   j1     = fix(yobj + 0.5*dy + 0.5)
   xr     = [k0, k1]
   yr     = [j0, j1]
   icen   = fix(xobj+0.5)-k0
   ikp0   = icen - fix(dxmed/2.0)
   ikp1   = icen + fix(dxmed/2.0)
   ;
   x      = k0 + indgen(k1 - k0 + 1)
   ;
   fits_read, filg, img, hdg
   fits_read, fild, imd, hdd
   ;
   spg  = extract_collapsex(img, xr, yr)
   spd  = extract_collapsex(imd, xr, yr)
   ;
   ; set only pixels outside of median box around line to 0.0
   spg2 = 0.0*spg
   spg2[ikp0:ikp1] = spg[ikp0:ikp1]
   ;
   spgn = spg2 / total(spg2)
   spdn = spd / total(spd)
   ;
   ymin = min([spgn, spdn])
   ymax = max([spgn, spdn])
   dy   = ymax - ymin
   yrp  = [ymin-0.05*dy, ymax+0.05*dy]
   plot, x, spgn, xrange=xr, yrange=yrp, xstyle=1, ystyle=1, linestyle=0, $
    xtitle='column', ytitle='Normalized flux', color=!black
   oplot, x, spgn, linestyle=0, color=!blue
   oplot, x, spdn, linestyle=0, color=!red
   keywait, 'Press any key to continue'
   ;
   ; Roll my own Fourier cross correlation
   cfunc    = grm_cfunc(spgn, spdn)
   nc       = n_elements(cfunc)
   chan     = findgen(nc)
   xr       = [min(chan), max(chan)]
   grm_avsigclip, cfunc, 3.0, 10, cmean, crms, nuse, nrej, nit
   ;
   ; subtract clipped mean from correlation function and 
   ; normalize by clipped_rms.  This is what we will fit.
   cfunc2   = (cfunc - cmean)/crms
   ;
   plot,chan,cfunc,xrange=xr,xstyle=1,xtitle='element',ytitle='Cross correlation'
   yp       = cmean + 0.0*xr
   oplot,xr,yp,linestyle=0,color=!black
   yp       = cmean + crms + 0.0*xr
   oplot,xr,yp,linestyle=2,color=!red
   yp       = cmean - crms + 0.0*xr
   oplot,xr,yp,linestyle=2,color=!red
   yp       = cmean + 3.0*crms + 0.0*xr
   oplot,xr,yp,linestyle=1,color=!blue
   yp       = cmean - 3.0*crms + 0.0*xr
   oplot,xr,yp,linestyle=1,color=!blue
   ;
   grm_cpeak_fit, cfunc, crms, lev, height, shift, width, window=float(2.0*dxmed)
   ;
   p = [lev, shift, width/2.35, height]
   cfit = gaussp_n_const(chan, p)
   oplot, chan, cfit, color=!dgreen
   print, 'level=', lev
   print, 'Height=', height
   print, 'Shift=', shift
   print, 'Width=', width
   
END 
