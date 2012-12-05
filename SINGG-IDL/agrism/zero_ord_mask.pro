PRO zero_ord_mask, direct_im, grism_im, grism_out, t, fluxrat, grcut, grow=grow, invmat=invmat, $
                   mask_out=mask_out
   ;
   ; direct_im -> Name of direct image
   ; grism_im  -> Name of grism image to apply mask to.
   ; grism_out -> output masked grism image,  masked pixels set to 0.0 
   ;              (maybe should write a byte mask instead?)
   ; t         -> trans structure from mrmatch [A, B, C, D, E, F]
   ; fluxrat   -> ratio direct/grism image fluxes
   ; grcut     -> level in modelled grism image at which to apply 
   ;              grism mask.
   ; grow      -> number of pixels to grow around each masked pixel.
   ; invmat    -> if set invert transformation matrix before applying
   ; mask_out  -> name of mask to be written
   ;
   ; G.R. Meurer 12/2002
   ; G.R. Meurer 12/2002 fixed to allow different direct & grism 
   ;                     image sizes
   ; G.R. Meurer 07/2006 allow output mask to be written
   ;
   maskvalb = 1b
   maskval  = 0.0
   ; Invert match transformation matrix if needed
   IF keyword_set(invmat) THEN BEGIN 
      cx     = -1.0*t[0]
      cy     = -1.0*t[3]
      amat   = [[t[1], [t[2]]], [t[4], t[5]]]
      tmat   = float(invert(amat, status, /double))
      IF status NE 0 THEN BEGIN 
         print, '**** ERRROR in BLIND_EMFIND.  Could not invert transformation matrix'
         print, '**** Status from INVERT : ', status
         return
      ENDIF 
   ENDIF ELSE BEGIN 
      cx     = t[0]
      cy     = t[3]
      tmat   = [[t[1], [t[2]]], [t[4], t[5]]]
   ENDELSE 
   print, cx, cy
   print, tmat
   ;
   ; Read in fits files, get dimensions
   fits_read, direct_im, imd, hdd
   fits_read, grism_im, img, hdg
   szd     = size(imd)
   nxd     = szd[1]
   nyd     = szd[2]
   szg     = size(img)
   nxg     = szg[1]
   nyg     = szg[2]
   ;
   ; find pixels above cut level in direct image: 
   ; remember to convert to standard convention: first pixel is 1.0, 1.0
   ;   Perhaps we should grow the area around each pixel by convolution...
   pixd    = where(imd GT grcut*fluxrat)
   xd      = float(pixd MOD nxd) + 1.0
   yd      = float(pixd / nxd) + 1.0
   ;
   ; transform to pixel positions in output image:
   ; conversion is to idl convention
   IF keyword_set(invmat) THEN BEGIN 
      xg     = (xd + cx)*tmat[0] + (yd + cy)*tmat[1]
      yg     = (xd + cx)*tmat[2] + (yd + cy)*tmat[3]
   ENDIF ELSE BEGIN 
      xg     = cx + xd*tmat[0] + yd*tmat[1]
      yg     = cy + xd*tmat[2] + yd*tmat[3]
   ENDELSE 
   xg      = fix(xg + 0.5) - 1
   yg      = fix(yg + 0.5) - 1
   ;
   ; make byte size mask
   imm     = make_array(nxg, nyg, /byte, value=0b)
   ;
   ; set _valid_ transformed pixels to maskvalb
   k       = where(xg GE 0 AND xg LE nxg-1 AND yg GE 0 AND yg LE nyg-1, nk)
   IF nk GT 0 THEN imm[xg[k],yg[k]] = maskvalb
   ;
   ; grow the mask if requested
   IF keyword_set(grow) THEN BEGIN 
      IF grow GT 0.0 THEN grow_mask, imm, imo, grow, goodval=maskvalb ELSE imo = temporary(imm)
   ENDIF ELSE BEGIN 
      imo = temporary(imm)
   ENDELSE 
   ;
   ; apply the mask
   k       = where(imo EQ maskvalb, nk)
   IF nk GT 0 THEN img[k] = maskval
   ;
   ; write output image
   ;   should probably write something to header..
   fits_write, grism_out, img, hdg
   ;
   ; write output mask if requested
   IF keyword_set(mask_out) THEN fits_write, mask_out, imo, hdg
END 
