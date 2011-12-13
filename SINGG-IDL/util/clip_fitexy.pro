PRO clip_fitexy, xx, yy, exx, eyy, nsig, aa, bb, sigxx, sigyy, sig_ab, rchisq, qq, ngood, good
   ;
   ; Do a n-sigma clipped fit to x vs y where there are 
   ; errors on both quantities
   ;
   ; G. Meurer 3/2005
   nin     =  n_elements(xx)
   good    =  indgen(nin)
   use_new =  make_array(nin, /byte, value=1b)
   kkmax   =  10
   kk      =  0
   ;
   REPEAT BEGIN 
      kk      =  kk + 1
      print, 'CLIP_FITEXY: starting iteration number: ', kk
      use     =  use_new
      good    =  where(use EQ 1b, ngood)
      fitexy, xx[good], yy[good], aa, bb, sig_ab, chisq, qq, x_sig=exx[good], y_sig=eyy[good]
      cc      = -1.0*aa/bb
      dd      =  1.0/bb
      yyfit   =  aa + bb*xx
      xxfit   =  cc + dd*yy
      dyy     =  yy - yyfit
      dxx     =  xx - xxfit
      sigxx   =  sqrt(total(dxx[good]^2)/float(ngood - 2))
      sigyy   =  sqrt(total(dyy[good]^2)/float(ngood - 2))
      use_new =  0b*use
      jj      =  where(abs(dxx) LE float(nsig)*sigxx AND abs(dyy) LE float(nsig)*sigyy, nnew)
      use_new[jj] = 1b
      change  =  where(use_new NE use, nchange)
   ENDREP UNTIL nchange EQ 0 OR kk EQ kkmax
   rchisq     =  chisq / float(ngood - 2)
END 
