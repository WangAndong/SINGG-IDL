PRO iter_sixlin, xx, yy, kref, nsigma, aa, sigaa, bb, sigbb, $
                 sigxx, sigyy, nused, $
                 nitmax=nitmax, niter=niter, indused=indused 
   ;
   ; iteratively do linear fits of xx <-> yy using sixlin.pro
   ; checkingout nsigma deviates in reference fit.
   ;
   ; xx,yy   -> 1d arrays containing points to be fit.
   ; kref    -> reference index of fit to use.  This should
   ;            be an integer in range 0:5 indicating which of 
   ;            the six fits from sixlin to use when deciding 
   ;            which points to keep.
   ; nsigma  -> specifies when the residuals are high enough to reject
   ;            in next iteration.  Points with 
   ;            abs({x,y}resid) > nsigma*sig{xx,yy} are rejected.
   ; aa      <- 6 elem Array of y intercepts for the final 6 fits 
   ;            results from, sixlin.
   ; sigaa   <- 6 elem Array for uncertainties on aa.
   ; bb      <- 6 elem Array of slopes for the final 6 fit results
   ;            from sixlin.
   ; sigbb   <- 6 elem array of uncertainties on bb.
   ; sigxx   <- 6 elem array of x residual dispersions
   ; sigyy   <- 6 elem array of y residual dispersions
   ; nused   <- number of points used in final fit
   ; nitmax  -> if set, the maximum number of iterations
   ; nitused <- if set, returned as the number of iterations performed.
   ; indused <- If set, returned as the indecis to the points fit.
   ;            
   ; G. Meurer 04/2005
   ;
   IF NOT keyword_set(nitmax) THEN nitmax = 10
   niter         = 0
   nn            = n_elements(xx)
   good          = indgen(nn)
   use           = make_array(nn, /byte, value=1b)
   REPEAT BEGIN 
      niter      = niter + 1
      use_new    = use
      jj         = where(use_new EQ 1b, njj)
      ;
      ; make sure total(xx*yy) NE 0 before calling sixlin
      xxx        = double(xx[jj])
      yyy        = double(yy[jj])
      avgx       = mean(xxx)
      avgy       = mean(yyy)
      xxx        = xxx - avgx
      yyy        = yyy - avgy
      IF total(xxx*yyy) NE 0.0 THEN BEGIN 
         sixlin, xx[jj], yy[jj], aa, sigaa, bb, sigbb
         sixsig, xx[jj], yy[jj], aa, bb, sigxx, sigyy
      ENDIF ELSE BEGIN 
         momx    = float(moment(xxx))
         momy    = float(moment(yyy))
         aa      = make_array(6, /float, value=float(avgy))
         bb      = make_array(6, /float, value=0.0)
         sigaa   = make_array(6, /float, value=sqrt(momy[1]/float(n_elements(xxx)-1)))
         sigbb   = make_array(6, /float, value=0.0)
         sigxx   = make_array(6, /float, value=sqrt(momx[1]))
         sigyy   = make_array(6, /float, value=sqrt(momy[1]))
      ENDELSE 
      aaa        = aa[kref]
      bbb        = bb[kref]
      sigx       = sigxx[kref]
      sigy       = sigyy[kref]
      IF bbb NE 0.0 THEN BEGIN 
         ccc        = -1.0*aaa/bbb
         ddd        = 1.0/bbb
         xres       = xx - (ccc + ddd*yy)
         yres       = yy - (aaa + bbb*xx)
         good       = where(abs(xres) LE nsigma*sigx AND abs(yres) LE nsigma*sigy,nused)
         use        = 0b*use_new
         use[good]  = 1b
      ENDIF ELSE use = use_new
      kk         = where(use NE use_new, nchange)
   ENDREP UNTIL niter EQ nitmax OR nchange EQ 0 
   IF keyword_set(indused) THEN indused = good
END 
