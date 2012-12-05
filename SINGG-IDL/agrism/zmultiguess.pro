PRO zmultiguess, lamline, elam, zguess, zmin, zmax, oguess, threshodds, source, nsrc, $
                 zbest, srcbest, lam0, lineid, status, nmatch, zbsrc, matchsrc, peakidsrc
   ;
   ; Estimate best redshift from a variety of redshift guesses, 
   ; and a line at fixed observed wavelength.
   ; The guesses are indexed by "source"  - the source of the 
   ; redshift estimates, with priority given to lowest source
   ; index.  
   ;
   ; lamline    -> wavelength of line
   ; elam       -> uncertainty in wavelength
   ; zguess     -> Redshift peak guesses
   ; zmin       -> min redshift of range of each peak
   ; zmax       -> max redshift of range of each peak
   ; oguess     -> odds of peaks from bpz
   ; threshodds -> odds threshold for considering peaks
   ; source     -> source index for each guess
   ; nsrc       -> maximum number of sources
   ; zbest      <- best guess redshift of line
   ; srcbest    <- lowest source index matching zbest
   ; lam0       <- rest wavelength of best guess line ID
   ; lineid     <- line identification
   ; status     <- status of identification in lowest matching source 
   ;               index:
   ;              -1: invalid bpz results
   ;               0: best match corresponds to peak with
   ;                  highest odds.
   ;               1: best match corresponds to lower odds 
   ;                  peak.
   ;               2: closest redshift match corresponds to
   ;                  highest odds peak but redshift not in
   ;                  the allowed redshift range of this peak;
   ;                  selected redhsift within range of lower
   ;                  odds peak.
   ;               3: no lines in any redshift range
   ; nmatch     <- number of lines within any valid peaks of 
   ;               matching source.
   ; zbsrc      <- best guess redshift for each possible source
   ; matchsrc   <- 1 if zbest = zbsrc
   ;               0 if zbsrc < 0
   ;              -1 if zbest ne zbsrc and zbsrc ge 0.0
   ; peakid     <- position of peak in zbpz array
   ;
   ; G. Meurer 09/2004
   ;
   ; make arrays for storing results for each source
   zbsrc     = make_array(nsrc, /float, value=-1.0)
   matchsrc  = make_array(nsrc, /byte, value=0b)
   peakidsrc = make_array(nsrc, /int, value=-1)
   statsrc   = make_array(nsrc, /int, value=-1)
   nmatchsrc = make_array(nsrc, /int, value=-1)
   linidsrc  = make_array(nsrc, /string, value='UNKNOWN')
   lam0src   = make_array(nsrc, /float, value=-1.0)
   ;
   ; run zbpz_line on guesses for each source
   FOR i = 0, nsrc-1 DO BEGIN 
      k = where(source EQ i, nk) 
      IF nk GT 0 THEN BEGIN 
         zbpz         = zguess[k]
         zbpzmin      = zmin[k]
         zbpzmax      = zmax[k]
         oddsbpz      = oguess[k]
         zbpz_line, lamline, elam, zbpz, zbpzmin, zbpzmax, oddsbpz, threshodds, $
               zl, og, pid, l0, lid, nm, st
         zbsrc[i]     = zl
         peakidsrc[i] = pid
         statsrc[i]   = st
         nmatchsrc[i] = nm
         linidsrc[i]  = lid
         lam0src[i]   = l0
      ENDIF 
   ENDFOR 
   best = where(statsrc GE 0 AND statsrc LE 1,nb)
   ok   = where(statsrc GE 0,nok)
   ;
   ; Make the pick
   pick = -1
   IF nb GE 1  THEN pick = best[0] ELSE IF nok GE 1 THEN pick =ok[0]
   ;
   ;
   IF pick GE 0 THEN BEGIN 
      srcbest = pick
      zbest   = zbsrc[pick]
      lam0    = lam0src[pick]
      lineid  = linidsrc[pick]
      status  = statsrc[pick]
      nmatch  = nmatchsrc[pick]
   ENDIF ELSE BEGIN 
      srcbest = -1
      zbest   = -1.0
      lam0    = -1.0
      lineid  = 'NONE'
      status  = -1
      nmatch  = 0
   ENDELSE 
   matchsrc = fix(zbest EQ zbsrc AND zbsrc GE 0.0) - fix(zbest NE zbsrc AND zbsrc GE 0.0)
END 
