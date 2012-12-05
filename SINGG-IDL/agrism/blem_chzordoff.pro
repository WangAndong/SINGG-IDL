PRO blem_chzordoff, xg, yg, cx, cy, tmat, filsgoodd, zordpass
   ;
   ; Check if the positions can contain a zero order image of 
   ; a source outside of the good areas of the direct image.
   ;
   ; xg        -> X positions to check in the grism image
   ; yg        -> Y positions to check in the grism image
   ; cx        -> x zeropoint of zeroth order to direct transformation
   ; cy        -> y zeropoint of zeroth order to direct transformation
   ; tmat      -> transformation matrix
   ; filsgoodd -> String array of filenames for the files 
   ;              containing the polynomial vertices defining the
   ;              good areas in the direct images.
   ; zordpass  <- returned as 1b where the grism position can not 
   ;              contain the zeroth order image of a source 
   ;              outside the good areas of the direct image.  
   ;              Returned as 0b if the position may contain a 
   ;              zeroth order image of an off direct image source.
   ;
   ; G. Meurer 06/2004
   ; G. Meurer 09/2004 Now mark all positions good if filsgoodd is a 
   ;                   single element array ['']. 
   ; 
   ng         =  n_elements(xg)
   ;
   ; will have to check each position against each polygon,
   ; so make storage for each test
   np         =  n_elements(filsgoodd)
   IF np EQ 1 AND filsgoodd[0] EQ '' THEN BEGIN 
      zordpass   = make_array(ng, /byte, value=1b)
   ENDIF ELSE BEGIN 
      xd         =  cx + tmat[0]*xg + tmat[1]*yg
      yd         =  cy + tmat[2]*xg + tmat[3]*yg
      goodtest   =  make_array(np,ng,/byte)
      ;
      ; Loop through each good area polygon
      FOR j = 0, np-1 DO BEGIN 
         IF filsgoodd[j] NE '' THEN BEGIN 
            readcol, filsgoodd[j], xp, yp, format='(f,f)'
            xp            = xp - 1
            yp            = yp - 1
            good          = inside(xd, yd, xp, yp)
            goodtest[j,*] = reform(good, ng)
         ENDIF ELSE BEGIN 
            ;
            ; In this instance we know that more than one file name
            ; was passed.  So if there is an empty string mark test
            ; as failed, otherwise all positions would automatically be 
            ; passed.  We only want to do that if no filenames were 
            ; passed in filsgoodg
            goodtest[j,*] = 0b
         ENDELSE 
      ENDFOR 
      ;
      ; combine results for all good areas
      good       = total(goodtest, 1)
      gd         = where(good GE 1, n)
      zordpass   = make_array(ng, /byte, value=0b)
      IF n GT 0 THEN zordpass[gd] = 1b
   ENDELSE 
END 
