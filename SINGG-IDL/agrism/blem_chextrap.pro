PRO blem_chextrap, xg, yg, ap, dxbeam, dxmed, filsgoodg, apallgood
   ;
   ; Check to see if all corners of each extraction aperture are
   ; good.  
   ;
   ; xg        -> X positions to check in the grism image
   ; yg        -> Y positions to check in the grism image
   ; ap        -> aperture width in pixels
   ; dxbeam    -> X range of aXe "beam" relative to direct image.
   ;              this should be a 2 element array.
   ; dxmed     -> size of median filtering box
   ; filsgoodg -> String array of filenames for the files 
   ;              containing the polynomial vertices defining the
   ;              good areas in grism image.  Pass [''] to have
   ;              all apertures marked as good.
   ; apallgood <- Output byte array - 1b correspond extraction
   ;              aperture with all four corners in good areas;
   ;              0b corresponds to apertures with at least one
   ;              corner outside of the good areas
   ;
   ; G. Meurer 06/2004
   ; G. Meurer 09/2006 allow [''] to be passed as filsgoodg, resulting
   ;              in all apertures marked as good.
   ;
   ; check to see how many elements are in filsgoodg, if only one 
   ; and it is '' then assume everything is good
   np         =  n_elements(filsgoodg)
   ng         =  n_elements(xg)
   IF filsgoodg[0] EQ '' AND np EQ 1 THEN BEGIN 
      apallgood  =  make_array(ng, /byte, value=1b)
   ENDIF ELSE BEGIN 
      ;
      ; will have to check each position against each polygon,
      ; so make storage for each test
      goodtest   =  make_array(np,4,ng,/byte)
      ;
      ; Make array of positions to check.
      xcorn      =  make_array(4,ng,/float,value=0.0)
      ycorn      =  make_array(4,ng,/float,value=0.0)
      xcorn[0,*] =  float(fix(xg - dxbeam[1] - 0.5*dxmed + 0.5))
      xcorn[1,*] =  xcorn[0,*]
      xcorn[2,*] =  float(fix(xg - dxbeam[0] + 0.5*dxmed + 0.5))
      xcorn[3,*] =  xcorn[2,*]
      ycorn[0,*] =  float(fix(yg - 0.5*ap + 0.5) - 1)
      ycorn[1,*] =  float(fix(yg + 0.5*ap + 0.5) - 1)
      ycorn[2,*] =  ycorn[1,*]
      ycorn[3,*] =  ycorn[0,*]
      xcorn      =  reform(xcorn, 4*ng, /overwrite)
      ycorn      =  reform(ycorn, 4*ng, /overwrite)
      ;
      ; Loop through each good area polygon
      FOR j = 0, np-1 DO BEGIN 
         readcol, filsgoodg[j], xp, yp, format='(f,f)'
         xp              = xp - 1
         yp              = yp - 1
         good            = inside(xcorn, ycorn, xp, yp)
         goodtest[j,*,*] = reform(good, 4, ng)
      ENDFOR 
      ;
      ; combine results for all apertures, all corners
      good2      =  total(goodtest, 1)
      good3      =  total(good2, 1)
      apallgood  =  make_array(ng, /byte, value=0b)
      gd         =  where(good3 EQ 4, ng)
      IF ng GT 0 THEN apallgood[gd] = 1b
   ENDELSE 
END 
