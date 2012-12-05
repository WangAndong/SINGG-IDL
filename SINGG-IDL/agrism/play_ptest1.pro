PRO play_ptest1
   ;
                    ; Playing with PEARS test data.  Originally the idea
                    ; was to try to derive a convolution kernal to apply
                    ; to the 2d grism image.  But that idea fell by the
                    ; way-side ..
   ; make convolution kernel to apply to 2D grism image 
   ; in order to find the first order spectra of point like
   ; sources.
   ; 
   ; G. Meurer  9/2005
   ;
   COMMON sensmod, lams, sens, esens, hsens
   setplotcolors
   filsens    = '/home/meurer/ACS/Grism/aXe/conf/ACS.WFC.1st.sens.4.fits'
   extsens    = 1
   dxbeam     = [-10, 150]
   xref       = 2284.0
   yref       = 2168.0
   dldp_a_0   = [4771.42, -0.00121212, -0.00247013, -4.42695e-6, -2.06821e-6, 7.93817e-7]
   dldp_a_1   = [41.3419, 0.000786183, -0.00112364, -2.47549e-8, -2.74095e-8, 2.61703e-8]
   dldp_a_2   = [7.71266e-04, 1.3607e-7, 1.53419e-7, 4.75e-10, -6.3533e-11, 6.58144e-11]
   beta       = [1.0, 0.0, -1.0, -2.0]
   cplt       = [!red, !black, !dgreen, !dblue]
   ;
   wdir       = '/home/meurer/ACS/Grism/PEARS/Testdata/'
   starcat    = 'Plots/ptest_mstar_sn.cat'
   grim       = 'grism_edgemsk.fits'
   dxcut      = 31.0
   dycut      = 15.0
   dxpeak     = 65.0
   ;
   cd, wdir, current=cwd
   ;
   ; This is the position where we will evaluate the wavelength solution.
   xx         = 2155.0
   yy         = 2155.0
   ;
   ; evaluate wavelength solution
   a0         = eval_axe_poly(dldp_a_0, xx, yy, xref=xref, yref=yref)
   a1         = eval_axe_poly(dldp_a_1, xx, yy, xref=xref, yref=yref)
   a2         = eval_axe_poly(dldp_a_2, xx, yy, xref=xref, yref=yref)
   ;
   ; initialize sensitivity model
   setsensmod, filsens, extsens
   ;
   ; take fiducial wavelength to be where sensitivity curve peaks
   kk         = sort(sens)
   lamfid     = lams[kk[0]]
   ;
   ; Generate pixel offsets for apertures
   ; convert to wavelength
   ; find sensitivity * dlam 
   dx         = float(dxbeam[0] + indgen(dxbeam[1] - dxbeam[0] + 1))
   dx2        = dx + 1.0
   lam        = a0 + a1*dx + a2*dx^2
   lam2       = a0 + a1*dx2 + a2*dx2^2
   dlam       = lam2 - lam
   slam       = interpol(sens,lams,lam)*dlam
   ;
   ; normalize by peak slam
   kk         = reverse(sort(slam))
   slam       = slam/slam[kk[0]]
   ;
   ; do some plots
   xtitle     = '!3 offset [pixels]'
   ytitle     = '!3 function'
   xrange     = dxbeam
   yrange     = [0.0, 1.2]
   charsize   = 1.5
   thick      = 1
   ;
   plot, dx, slam, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, $
    xtitle=xtitle, ytitle=ytitle, charsize=charsize, thick=thick, xthick=thick, ythick=thick, charthick=thick
   FOR jj = 0, n_elements(beta)-1 DO BEGIN 
      yy  = slam*(lam/lamfid)^beta[jj]
      yy  = yy / max(yy)
      oplot, dx, yy, color=cplt[jj], thick=thick+1
   ENDFOR
   ;
   keywait, 'Type anything to go to next part of program'
   ;
   readcol, starcat, id, xst, yst, format='(i,f,f)'
   nst = n_elements(id)
   fits_read, grim, imgg, hdrg
   szg = size(imgg)
   ncol = szg[1]
   nrow = szg[2]
`   xrange = float(fix([-0.5*dycut-1.0, 0.5*dycut+1.0]))
   yrange = [-0.1, 0.5]
   yplt   = [0.0, 0.0]
   xtitle = 'Row offset'
   ytitle = 'total normalized level'
   charsize = 1.5
   symsize  = 1.5
   plot, xrange, yplt, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, $
    xtitle=xtitle, ytitle=ytitle, charsize=charsize, thick=thick, xthick=thick, ythick=thick, charthick=thick 
   ;
   ; prepare for plots
   FOR kk = 0, nst-1 DO BEGIN 
      jj0   = (fix(yst[kk] - 0.5*dycut - 1.0) > 0) < (nrow - 1)
      jj1   = ((jj0 + fix(dycut) - 1) > 0) < (nrow - 1)
      ii0   = (fix(xst[kk] + dxpeak - 0.5*dxcut - 1.0) > 0) < (ncol-1)
      ii1   = ((ii0 + fix(dxcut) - 1) > 0) < (ncol - 1)
      stamp = imgg[ii0:ii1,jj0:jj1]
      njj   = jj1 - jj0 + 1
      xplt  = jj0 + findgen(njj) - yst[kk] + 1.0
      yplt  = make_array(njj)
      FOR jj = 0, njj-1 DO yplt[jj] = total(stamp[*,jj])
      ynorm  = total(yplt)
      yplt  = yplt / ynorm
      oplot, xplt, yplt, psym=sym(1), symsize=symsize
      oplot, xplt, yplt, psym=0
   ENDFOR 
   ;
   cd, cwd
END 
