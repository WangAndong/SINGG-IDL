PRO grism_classify, bintab, id, objpar, lrange, nsmooth, nsig, $
                    luf, lum, lui, minsn, maxsn, ncontam, result, $
                    minsigma=minsigma, fnorm=fnorm, pfx=pfx
   ;
   ; automatically classify spectrum, update logs
   ;
   ; bintab   -> binary table containing spectrum
   ; id       -> object id
   ; objpar   -> object parameters
   ;             objpar[0] : xim
   ;             objpar[1] : yim
   ;             objpar[2] : mag
   ;             objpar[3] : a
   ;             objpar[4] : b
   ;             objpar[5] : theta
   ;             objpar[6] : w50
   ;             objpar[7] : class
   ; lrange   -> wavelength range for line finding/plotting
   ; nsmooth  -> number of pixels used for smoothing
   ; nsig     -> Minimum S/N for em line finding.
   ; luf      -> logical unit for fitted lines log
   ; lum      -> logical unit for M* / SN sources
   ; lui      -> logical unit for rejected em line sources
   ; minsn    <- Minimum S/N
   ; maxsn    <- Maximum S/N
   ; ncontam  <- Number of zero order sources in bounding box.
   ; result   <-  0 : no emission lines fines
   ;              N : emission line(s) found and fitted
   ;             -1 : classified as M* or SN
   ;             -2 : rejected candidate emission line
   ; minsigma -> Minimum Gaussian sigma for fit
   ; fnorm    -> Normalizing flux
   ;
   ; G.R. Meurer 9/02
   ; G.R. Meurer 7/04 window position specified
   ; G.R. Meurer 1/07 apply taper to ends of spectrum
   ;
   xsizepl  = 400
   ysizepl  = 180
   wxpos    = 801
   wypos0   = 800 - fix(1.2*float(ysizepl))
   wypos1   = wypos0 - fix(1.2*float(ysizepl))
   ;
   IF keyword_set(pfx) THEN prefix = pfx ELSE prefix = 'spec_gfit_'
   ; print, prefix
   ; stop
   result   = 0
   IF NOT keyword_set(fnorm) THEN fnorm = 1.0e-18
   ;
   ; read spectrum and error
   flxi     = bintab.flux / fnorm
   eflxi    = bintab.ferror / fnorm
   lam      = bintab.lambda
   ;
   ; apply taper to ends
   grism_taperspec, flxi, eflxi, lam, lrange, nsmooth, flx, eflx, power=2
   ;
   ; set lrangefit = lrange
   lrangefit = lrange
   ;
   disp     = (max(lam) - min(lam))/(n_elements(lam)-1)
   IF NOT keyword_set(minsigma) THEN minsigma = disp * 2.3 / 2.3548
   dxfit    = nsmooth * 2.0 * disp
   flxu     = flx
   gmod     = 0.0*flx
   t1       = strtrim(string(id),2)
   t3       = strtrim(bintab.id,2)
   title    = t1 
   k        = 0
   gfit     = 0b
   ;
   REPEAT BEGIN 
      stop_now = 1b
      ;
      linefind, flxu, eflx, nsmooth, nsig, flxsm, resid, nem, nabs, sn0, sn1, $
       xsn0, xsn1, lrange=lrangefit, lam=lam
      IF k EQ 0 THEN BEGIN 
         ; 
         ; Store min & max S/N
         minsn = sn0
         maxsn = sn1
      ENDIF 
      IF nem GT 0 THEN BEGIN 
         ;
         ; show plots to the user
         window, 0, xsize=xsizepl, ysize=ysizepl, xpos=wxpos, ypos=wypos0
         grism_plot1db, lam, flx, eflx, flxsm, gmod, lrange, title=title, vline=xsn0
         y        = resid / eflx
         ey       = 1.0 + 0.0*eflx
         window, 1, xsize=xsizepl, ysize=ysizepl, xpos=wxpos, ypos=wypos1
         grism_plot1da, lam, y, ey, lrange, title=title, ytitle='Residual / error', vline=xsn0
         ;
         ; query disposition from the user
         disposition = grcl_disposition(k)
         IF disposition EQ 'L' THEN BEGIN 
            read, l1, l2, prompt='Enter wavelength range for line finding/fitting : ', format='(f,f)'
            ll        = [l1, l2]
            lrangefit = [min(ll),max(ll)]
            stop_now  = 0b
         ENDIF 
         IF disposition EQ 'G' THEN BEGIN 
            ;
            ; fit line
            grism_linefit, lam, flxu, eflx, resid, lrangefit, minsigma, dxfit, p
            ;
            ; display fit, assess quality
            xr     = p[3] + dxfit*[-0.5, 0.5]
            window, 0, xsize=xsizepl, ysize=ysizepl, xpos=wxpos, ypos=wypos0
            grism_plotfit, lam, flxu, eflx, xr, p, title='Profile over fit region'
            fit    = gmod + gauss_n_quad(lam, p)
            contsp = flxsm + p[0] + p[1]*lam + p[2]*lam*lam
            cont   = spline(lam, flxsm, float(p[3]))
            ew     = float(p[5])/cont
            yr     = (flx - fit) / eflx
            ey     = 1.0 + 0.0*eflx
            window, 1, xsize=xsizepl, ysize=ysizepl, xpos=wxpos, ypos=wypos1
            grism_plot1da, lam, yr, ey, xr, title='Residuals over fit region', ytitle='Residual / error'
            qual  = grcl_qual()
            print, qual
            ;
            ; update (sum of) Gaussian model
            gmod     = gmod + gauss1(lam, p[3:5])
            flxu     = flx - gmod
            stop_now = qual EQ 0b
            gfit     = 1b
            k        = k + 1
            ;
            ; make structure for results, make log entry
            gres     = create_struct('linenum', fix(k), 'quality', fix(qual), $
                                     'center', float(p[3]), 'width', float(p[4]*2.3548), $
                                     'flux', float(p[5]*fnorm), $
                                     'continuum', cont*fnorm, 'ew', ew)
            grcl_logline, luf, id, objpar, minsn, maxsn, ncontam, results=gres
         ENDIF 
         IF disposition EQ 'I' OR disposition EQ 'M' THEN BEGIN 
            IF disposition EQ 'I' THEN lu = lui ELSE lu = lum
            grcl_logline, lu, id, objpar, minsn, maxsn, ncontam
            stop_now = 1b
         ENDIF 
         ;
         ; finish up
      ENDIF 
   ENDREP UNTIL nem LE 0 OR stop_now 
   ;
   IF gfit THEN BEGIN
      ;
      ; make final nice plots
      window, 0, xsize=xsizepl, ysize=ysizepl, xpos=wxpos, ypos=wypos0
      ;pfile = nampng(prefix, id)
      pfile = namps(prefix, id)
      print, pfile
      grism_plot1db, lam, flx, eflx, flxsm, gmod, lrange, title=title, pfile=pfile
      y        = resid / eflx
      ey       = 1.0 + 0.0*eflx
      window, 1, xsize=xsizepl, ysize=ysizepl, xpos=wxpos, ypos=wypos1
      grism_plot1da, lam, y, ey, lrange, title=title, ytitle='Residual / error'
   ENDIF 
END 
