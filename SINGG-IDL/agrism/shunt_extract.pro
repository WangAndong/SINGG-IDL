PRO shunt_extract, im_grism, im_squash, nbin, xsq, ysq, dxrange, $
                   dyrbn, dyspec, dybuf, dxcrcut, dxstamp, dystamp, $
                   gr_ribbon, gr_spec, gr_sky, gr_crcut, sq_stamp, $
                   lim_ribbon, lim_spec, lim_crcut, lim_stamp, $
                   sky_lev, sky_sig, max_sn
   ;
   ; extract various 2d and 1d images and cuts from the grism and 
   ; squashed grism image.
   ;
   ; im_grism   -> grism image (unbinned)
   ; im_squash  -> grism image (squashed)
   ; nbin       -> bin size in x
   ; xsq        -> x pixel position of source to extract in squashed image
   ; ysq        -> y pixel position of source to extract in squashed image
   ; dxrange    -> range of x offsets relative to first order position
   ; dyspec     -> the width of the spectrum to collapse, in rows
   ; dybuf      -> buffer between sky rows and object spectrum.
   ; dyrbn      -> the width of the 2d ribbon to extract in rows
   ; dxcrcut    -> width in columns to sum for cross disp cut.
   ; dxstamp    -> column size of context postage stamp.
   ; dystamp    -> row size of context postage stamp.
   ; gr_ribbon  <- 2d ribbon image of spectrum in grism image, 
   ;               after subtracting an average sky spectrum
   ; gr_spec    <- 1d collapsed spectrum, after sky subtraction
   ; gr_sky     <- median smoothed average sky spectrum per row over gr_ribbon
   ; gr_crcut   <- 1d cross dispersion cut (sky subtracted)
   ; sq_stmp    <- stamp of squashed image (sky subtracted)
   ; lim_ribbon <- pixel limits of ribbon in grism image
   ; lim_spec   <- pixel limits of 1d spec (before collapsing) in grism image
   ; lim_crcut  <- pixel limits of cross dispersion cut (before 
   ;               collapsing) in grism image
   ; lim_stamp  <- pixel limits of squashed image stamp
   ; sky_lev    <- mean sky level (from outer quartiles of gr_spec)
   ; sky_sig    <- average rms in sky regions after smoothed sky subtraction.
   ; max_sn     <- maximum S/N
   ;
   ; all outputs starting with lim_ are four elements integer arrays:
   ; [min_x, max_x, min_y, max_y]
   ; units are pixel positions (not IDL array positions).
   ;
   ; G. Meurer 9/2005
   ;           1/2006 : add sky subtraction and related pars
   ;                    (gr_sky, sky_lev, sky_sig, max_sn)
   ;
   ; arbitrarily hard-wired parameters
   winsk = 7   ; size of median window size for sky spectrum
   ;
   ; get sizes of input arrays
   szgr  = size(im_grism)
   szsq  = size(im_squash)
   nxgr  = szgr[1]
   nygr  = szgr[2]
   nxsq  = szsq[1]
   nysq  = szsq[2]
   ;
   ; convert squashed image position to grism image position
   ygr   = ysq
   xgr   = xsq*float(nbin) + 0.5*float(nbin + 1)
   ;
   ; determine the limits for the various cutouts
   x1         = fix(xgr + 0.5) + dxrange[0] > 1 < nxgr
   x2         = fix(xgr + 0.5) + dxrange[1] > 1 < nxgr
   y1         = fix(ygr + 0.5) - fix(0.5*float(dyrbn)) > 1 < nygr
   y2         = y1 + dyrbn - 1 > 1 < nygr
   lim_ribbon = [x1, x2, y1, y2]
   y3         = fix(ygr + 0.5) - fix(0.5*float(dyspec)) > 1 < nygr
   y4         = y3 + dyspec - 1 > 1 < nygr
   lim_spec   = [x1, x2, y3, y4]
   x1         = fix(xgr + 0.5) - fix(0.5*float(dxcrcut)) > 1 < nxgr
   x2         = x1 + dxcrcut - 1 > 1 < nxgr
   lim_crcut  = [x1, x2, y1, y2]
   x1         = fix(xsq + 0.5) - fix(0.5*float(dxstamp)) > 1 < (nxsq - dxstamp + 1)
   x2         = x1 + dxstamp - 1
   y1         = fix(ysq + 0.5) - fix(0.5*float(dystamp)) > 1 < nysq
   y2         = y1 + dystamp - 1 > 1 < nysq
   lim_stamp  = [x1, x2, y1, y2]
   ;
   ; extract 2d cutouts
   gr_ribbon  = im_grism[lim_ribbon[0]-1:lim_ribbon[1]-1,lim_ribbon[2]-1:lim_ribbon[3]-1]
   sq_stamp   = im_squash[lim_stamp[0]-1:lim_stamp[1]-1,lim_stamp[2]-1:lim_stamp[3]-1]
   ;
   ; extract 1d collapsed cuts
   nn         = lim_spec[1] - lim_spec[0] + 1
   gr_spec    = make_array(nn, /float, value=0.0)
   gr_sky     = make_array(nn, /float, value=0.0)
   FOR ii = 0, nn - 1 DO BEGIN 
      i0          = lim_spec[0]-1+ii
      gr_spec[ii] = total(im_grism[i0,lim_spec[2]-1:lim_spec[3]-1])
      arr         = [im_grism[i0,lim_ribbon[2]-1:lim_spec[2]-2-dybuf], im_grism[i0,lim_spec[3]+dybuf:lim_ribbon[3]-1]]
      gr_sky[ii]  = biweight_mean(arr)
   ENDFOR 
   ;
   ; smooth sky and subtract from 1D spectrum and ribbon
   nn         = lim_ribbon[3] - lim_ribbon[2] + 1
   FOR ii = 0, nn-1 DO gr_ribbon[*,ii] = gr_ribbon[*,ii] - gr_sky
   gr_sky     = medsmooth(gr_sky, winsk)
   gr_spec    = gr_spec - float(dyspec)*gr_sky
   ;
   ; Use mean in first and last quartiles of sky spec as mean sky
   ; level.  Use rms of residuals in first and last quartiles of 
   ; sky rows.
   m3         = lim_ribbon[1] - lim_ribbon[0]
   m1         = (m3 + 1)/4 - 1
   m2         = m3 - (m3 + 1)/4
   dysky      = (dyrbn - dyspec - 2*dybuf)/2
   m4         = dysky - 1
   m5         = dyrbn - dysky
   arr        = [gr_sky[0:m1], gr_sky[m2:m3]]
   sky_lev    = biweight_mean(arr)
   arr        = [gr_ribbon[0:m1,0:m4], gr_ribbon[0:m1,m5:dyrbn-1], gr_ribbon[m2:m3,0:m4], gr_ribbon[m2:m3,m5:dyrbn-1]]
   dum        = biweight_mean(arr,sky_sig)
   ;
   ; subtract local sky from stamp of squashed image
   sq_stamp   = sq_stamp - float(nbin)*sky_lev
   ;
   ; get cross dispersion cut
   gr_crcut   = make_array(nn, /float, value=0.0)
   ;
   ; ok crcut is within ribbon in columns, and the rows are the same 
   ; so take crcut from sky subtracted ribbon.
   i0 = lim_crcut[0] - lim_ribbon[0]
   i1 = lim_crcut[1] - lim_ribbon[0]
   FOR jj = 0, nn-1 DO gr_crcut[jj] = total(gr_ribbon[i0:i1,jj])
   ;
   ; normalize cross cut by total
   gr_crcut   = gr_crcut / abs(total(gr_crcut))
   ;
   ; get peak S/N of spectrum
   max_sn     = max(gr_spec)/(sky_sig*sqrt(dyspec))
END 
