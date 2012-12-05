PRO grism_oplotcontam, bintab, xrange, yrange, color=color
   ; Plot shaded rectangles over yrange where bintab.contam > 0
   ;
   k = where(bintab.contam GT 0, nk)
   IF nk LE 0 THEN return 
   IF NOT keyword_set(color) THEN color = 0.50 * !D.N_COLORS
   ;
   ; wavelength steps
   l = bintab.lambda
   dl = l
   FOR i = 1, n_elements(dl)-1 DO dl[i] = l[i] - l[i-1]
   dl[0] = dl[1] 
   ;
   ; plot filled rectangle at each contaminated point
   FOR j = 0, nk-1 DO BEGIN 
      xc = l[k[j]]
      dx = 0.5*dl[k[j]]
      x  = [xc-dx, xc-dx, xc+dx, xc+dx]
      i0 = where(x LT min(xrange), ni0)
      i1 = where(x GT max(xrange), ni1)
      y  = [yrange[0], yrange[1], yrange[1], yrange[0]]
      IF ni0 NE 4 AND ni1 NE 4 THEN BEGIN 
         IF ni0 GT 0 THEN x[i0] = min(xrange)
         IF ni1 GT 0 THEN x[i1] = max(xrange)
         polyfill, x, y, color=color
      ENDIF 
   ENDFOR 
END 
