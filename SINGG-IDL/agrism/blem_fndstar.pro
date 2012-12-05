PRO blem_fndstar, i0, i1, j0, j1, catdr, idstar, xstar, ystar, $
                  clstar_min=clstar_min, awkfil=awkfil
   ;
   ; Find stars listed within sextractor catalog within a specified 
   ; area in the direct image.  
   ;
   ; i0     -> lower column limit for search (idl array coords)
   ; i1     -> upper column limit for search (idl array coords)
   ; j0     -> lower row limit for search (idl array coords)
   ; j1     -> upper row limit for search (idl array coords)
   ; catdr  -> name of sextractor catalog file
   ; idstar <- returned as ID of the selected star in the search area.
   ;           -1 : no stars found
   ;           -2 : ambiguous - more than one star in area
   ; xstar  <- column position of selected star (idl array coords)
   ; ystar  <- row position of selected star (idl array coords)
   ; clstar_min -> if set, minimum value for class_star for a source
   ;               to be considered a star.  default is clstar_min=0.5.
   ;               Note - do not set clstar_min=0.0; because of the 
   ;               way idl works this will be considered to mean 
   ;               "use the default".  Use a small negative value 
   ;               (e.g. -0.001) instead.
   ; awkfil -> awkfile used by blem_rdsex to parse sextractor catalog.
   ;
   ; G. Meurer 06/2004
   ;
   ; defaults:
   idstar = -1
   xstar  = -1.0
   ystar  = -1.0
   IF keyword_set(clstar_min) THEN cmin = clstar_min ELSE cmin = 0.5
   ; 
   ; parse the catalog, keep the stars
   blem_rdsex, catdr, id, x, y, mag, a, b, theta, fwhm, flag, class_star, awkfil=awkfil
   good = where(class_star GE cmin AND x GE float(i0+1) AND x LE float(i1+1) $
                AND y GE float(j0+1) AND y LE float(j1+1),ng)
   print, 'BLEM_FNDSTAR: Number of stars in area = ', ng
   ;
   ; only proceed if there are stars in the box
   IF ng EQ 1 THEN BEGIN 
      idstar = id[good]
      xstar  = x[good] - 1.0
      ystar  = y[good] - 1.0
      print, 'BLEM_FNDSTAR: id of star in selection box : ', idstar
   ENDIF ELSE BEGIN 
      IF ng GT 1 THEN idstar = -2
   ENDELSE 
END 
