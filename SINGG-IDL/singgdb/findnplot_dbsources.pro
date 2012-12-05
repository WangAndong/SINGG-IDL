PRO findnplot_dbsources, hdr, dbnam, nfound, list, namobj, xpos, ypos, $
                         ra, dec, sma, pa, axrat, $
                         mask=mask, kwdnam=kwdnam, kwdrad=kwdrad, kwdpa=kwdpa, $
                         kwdrat=kwdrat, defrad=defrad, sclrad=sclrad, angoff=angoff, $
                         ratcode=ratcode, dbdeg=dbdeg, fillcol=fillcol, lincol=lincol, $
                         symcol=symcol, plsym=plsym, symsiz=symsiz, thick=thick, close=close
   ;
   ; Find sources in database that are in an image, 
   ; overplot them on graphics device/window.
   ;
   ; Befoe running this procedure, the input fits file header
   ; (hdr) must be read in.  The procedure setplotcolors should
   ; have been run.  allplotting is done with oplot or polyfill, so
   ; these are overplots and the drawing box should already be there. 
   ;
   ; hdr     -> header structure of image.  
   ; dbnam   -> database to open
   ; nfound  <- number of sources found
   ; list    <- list of entries in dbnam that are in the (unmasked)
   ;            image.
   ; xpos    <- x position (column) of sources in the image.
   ; ypos    <- y position (row) of sources in the image.
   ; ra      <- Right ascension (degrees) of sources in the image.
   ; dec     <- Declination (degrees) of sources in the image.
   ; sma     <- semi-major axis length (arcsec) of sources in the 
   ;            image.
   ; pa      <- position angle of sources in the image
   ; axrat   <- axial ratio of sources in the image.
   ; mask    -> if set, this corresponds to a 2d image giving 
   ;            good/bad pixel mask.  **** not yet implemented ***
   ; kwdnam  -> database keyword for object name.  If '' or not set 
   ;            then all objects arnamed <dbnam>_obj
   ; kwdrad  -> database keyword for object size.  If '' or not set
   ;            then size = defrad (see below).
   ; kwdpa   -> database keyword for position angle, If '' or not set
   ;            then pa = 0.0 is assumed.
   ; kwdrat  -> database keyword foir axial ratio. If '' or not set 
   ;            then axrat=1.0 is assumed.
   ; defrad  -> default radius.  If not set then the default radius 
   ;            is 0.0
   ; sclrad  -> The radii extracted from the database are multiplied
   ;            sclrad to get the semi-major axis radius in arcsec.
   ;            For example sclrad = 0.5 converts diameter to radius
   ;            sclrad = 60.0 converts size in arcmin to arcsec. If
   ;            not set then sclrad=1.0
   ; angoff  -> Angle offset in degrees between PA from database and
   ;            PA measuired from N -> E.  If not set then angoff=0.0.
   ; ratcode -> This specifies what is done with the axis ratios.
   ;            ratcode = 0b  axrat = axrat (default)
   ;            ratcode = 1b  axrat = size/axrat (axrat is
   ;                          initially minor axis szie.
   ;            ratcode = 2b  axrat = 1.0/axrat (axrat is initially
   ;                          minor/major axis ratio).
   ; dbdeg   -> If set database RA values are in degrees (otherwise
   ;            they are in hours).
   ; fillcol -> The color to fill in the ellipse for the object.  If
   ;            not set or <= 0 then the ellipse is not filled in.
   ; lincol  -> Color to outline the ellipse with.  If not set or <= 0
   ;            then an outline is not drawn.
   ; symcol  -> Color to plot the symbol at the central position.  If
   ;            not set or <= 0 then the central symbol is not drawn.
   ; plsym   -> code for sym() function giving symbol to plot.  If 
   ;            not set or <= 0 then plsym=1 is adopted.
   ; symsiz  -> Size of central symbol to plot.  If not set or <= 0
   ;            then symsiz = 1.0 is adopted.
   ; thick   -> If set the line thickness
   ; close   -> If set then the database is closed at the end of 
   ;            processing
   ;
   ;  G. Meurer (2/2005)
   ;
   radian  = 180.0/!pi
   ;
   ; check optional parameters, setting defaults
   radeg   = keyword_set(dbdeg)
   IF NOT keyword_set(kwdnam)  THEN kwdnam  = ''
   IF NOT keyword_set(kwdrad)  THEN kwdrad  = ''
   IF NOT keyword_set(kwdpa)   THEN kwdpa   = ''
   IF NOT keyword_set(kwdrat)  THEN kwdrat  = ''
   IF NOT keyword_set(defrad)  THEN defrad  = 0.0
   IF NOT keyword_set(sclrad)  THEN sclrad  = 1.0
   IF NOT keyword_set(angoff)  THEN angoff  = 0.0
   IF NOT keyword_set(ratcode) THEN ratcode = 0b
   IF NOT keyword_set(fillcol) THEN fillcol = 0
   IF NOT keyword_set(lincol)  THEN lincol  = 0
   IF NOT keyword_set(symcol)  THEN symcol  = 0
   IF NOT keyword_set(plsym)   THEN plsym   = 1
   IF NOT keyword_set(symsiz)  THEN symsiz  = 1.0
   IF defrad LE 0.0  THEN defrad = 0.0
   IF sclrad LE 0.0  THEN sclrad = 1.0
   IF ratcode LE 0.0 THEN ratcode = 0b
   IF plsym LE 0.0   THEN plsym = 1
   ;
   ; if ellipses are to be drawn, then
   ; make a unit circle - will need this later
   IF fillcol GT 0 OR lincol GT 0 THEN BEGIN 
      npoints = 361
      circ    = circle(0.0, 0.0, 1.0, npoints=npoints)
      xcirc   = reform(circ[0,*], npoints)
      ycirc   = reform(circ[1,*], npoints)
   ENDIF 
   ;
   ; get image dimensions, pixel size, from header
   nx      = sxpar(hdr, 'NAXIS1')
   ny      = sxpar(hdr, 'NAXIS2')
   cd00    = sxpar(hdr, 'CDELT1')
   cd11    = sxpar(hdr, 'CDELT2')
   IF cd00 NE 0.0 AND cd11 NE 0.0 THEN BEGIN 
      dx   = 3600.0*sqrt(cd00*cd00)
      dy   = 3600.0*sqrt(cd11*cd11)
   ENDIF ELSE BEGIN 
      cd00 = sxpar(hdr, 'CD1_1')
      cd01 = sxpar(hdr, 'CD1_2')
      cd10 = sxpar(hdr, 'CD2_1')
      cd11 = sxpar(hdr, 'CD2_2')
      dx   = 3600.0*sqrt(cd00*cd00 + cd01*cd01)
      dy   = 3600.0*sqrt(cd10*cd10 + cd11*cd11)
   ENDELSE 
   ;
   ; If defrad > 0 then extend search area beyond image
   IF defrad GT 0.0 THEN BEGIN 
      ddx    = defrad/dx
      ddy    = defrad/dy
      xrange = [0.0 - ddx, float(nx) + ddx]
      yrange = [0.0 - ddy, float(ny) + ddy]
   ENDIF ELSE BEGIN 
      xrange = [0.0, float(nx)-1.0]
      yrange = [0.0, float(ny)-1.0]
   ENDELSE 
   ;
   ; open database and find positions of objects
   grm_imdbase, hdr, dbnam, list, xpos=xpos, ypos=ypos, radeg=radeg, $
                xrange=xrange, yrange=yrange
   ;
   nfound = n_elements(list)
   IF nfound GE 1 AND list[0] LE 0 THEN nfound = 0
   ;
   ; remove masked sources if a mask is past
   IF nfound GE 1 AND keyword_set(mask) THEN BEGIN 
      kk = where(mask[fix(xpos-0.5),fix(ypos-0.5)] GT 0, nfound)
      IF nfound GT 0 THEN BEGIN 
         list = list[kk]
         xpos = xpos[kk]
         ypos = ypos[kk]
      ENDIF  
   ENDIF 
   IF nfound GT 0 THEN BEGIN 
      ;
      dbext, list, 'ra,dec',ra, dec
      IF strlen(strtrim(kwdnam,2)) EQ 0 THEN namobj = make_array(nfound, /string, value=dbnam+'_obj') $
                                        ELSE dbext, list, kwdnam, namobj
      IF strlen(strtrim(kwdrad,2)) EQ 0 THEN sma    = make_array(nfound, /float, value=defrad) $
                                        ELSE dbext, list, kwdrad, sma
      IF strlen(strtrim(kwdpa,2)) EQ 0  THEN pa     = make_array(nfound, /float, value=0.0) $
                                        ELSE dbext, list, kwdpa, pa
      IF strlen(strtrim(kwdrat,2)) EQ 0 THEN axrat  = make_array(nfound, /float, value=1.0) $
                                        ELSE dbext, list, kwdrat, axrat
      ;
      sma   = float(sma)
      pa    = float(pa)
      axrat = float(axrat)
      ;
      CASE ratcode OF 
         0b: axrat = axrat
         1b: axrat = sma/axrat
         2b: axrat = 1.0/axrat
         ELSE: axrat = axrat
      ENDCASE 
      sma   = sma*sclrad
      pa    = pa + angoff
      parad = pa/radian
      FOR kk = 0, nfound-1 DO BEGIN 
         cosd = cos(dec[kk]/radian)
         rac  = (ra[kk]-sma[kk]*(xcirc*cos(parad[kk])/axrat[kk] - ycirc*sin(parad[kk]))/(3600.0*cosd)) MOD 360.0
         decc = dec[kk]+sma[kk]*(xcirc*sin(parad[kk])/axrat[kk] + ycirc*cos(parad[kk]))/3600.0
         adxy, hdr, rac, decc, xp, yp
         IF fillcol GT 0 THEN polyfill, xp, yp, color=fillcol
         IF lincol  GT 0 THEN oplot, xp, yp, psym=0, color=lincol, linestyle=0, thick=thick
      ENDFOR 
      IF symcol GT 0 AND symsiz GT 0 THEN oplot, xpos, ypos, psym=sym(plsym), color=symcol, symsize=symsiz
   ENDIF 
   IF keyword_set(close) THEN dbclose
END 
