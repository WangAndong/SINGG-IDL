PRO grism_zerocontam, boxx, boxy, id, xim, yim, mag, w50, $
                      xoff0, yoff0, maglim, w50lim, xbuff, ybuff, $
                      nmatch, idm, xgr, ygr
   ;
   ; Test to see if there are any zeroth order sources in spectrum box.
   ;
   ; boxx    -> X coords of box conatining spectrum.
   ; boxy    -> Y coords of box containing spectrum.
   ; id      -> ID numbers from catalog
   ; xim     -> x position of direct image
   ; yim     -> Y position of direct image
   ; mag     -> mag from direct image
   ; w50     -> FWHM from direct image
   ; xoff0   -> X offset of zeroth order from direct image
   ; yoff0   -> Y offset of zeroth order from direct image
   ; maglim  -> Only look for matches brighter than this
   ; w50lim  -> Only look for matches smaller than this
   ; xbuff   -> increase search boxx by +/- this amount
   ; ybuff   -> increase search boxy by +/- this amount
   ; nmatch  <- number of contaminating sources found
   ; idm     <- id numbers of matching sources
   ; xgr     <- x position of matches on grism image
   ; ygr     <- y position of matches on grism image
   ;
   ; G.R. Meurer 9/02
   k     = where(mag LE maglim AND w50 LE w50lim, ngood)
   IF ngood GT 0 THEN BEGIN 
      xch         = xim[k] + xoff0
      ych         = yim[k] + yoff0
      ich         = id[k]
      sx          = sort(boxx)
      sy          = sort(boxy)
      bx          = boxx
      by          = boxy
      bx[sx[0:1]] = boxx[sx[0:1]] - xbuff
      bx[sx[2:3]] = boxx[sx[2:3]] + xbuff
      by[sx[0:1]] = boxy[sx[0:1]] - ybuff
      by[sy[2:3]] = boxy[sy[2:3]] + ybuff
      ; Eventually want to check if point is in polygon.  This does not work:
      ; contam      = polyfillv(bx, by, xch, xych) 
      ;
      ; quick kludge - look at rectangle aligned with image
      xmin        = min(bx)
      xmax        = max(bx)
      ymin        = min(by)
      ymax        = max(by)
      contam      = where(xch GE xmin AND xch LE xmax AND ych GE ymin AND ych LE ymax, nmatch)
      IF (nmatch GT 0) THEN BEGIN 
         idm      = ich[contam]
         xgr      = xch[contam]
         ygr      = ych[contam]
         print, 'Number of contaminating sources : ', nmatch
         FOR i = 0, nmatch-1 DO print, i, idm[i], xgr[i], ygr[i]
      ENDIF 
   ENDIF ELSE BEGIN 
      nmatch = 0
   ENDELSE 
   
END 
