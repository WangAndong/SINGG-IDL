PRO plot_zcomp, ztest, ytest, ng, zguess, zguessa, zguessb, oguess, source, zbest, ezbest, title, hardfile=hardfile
   ;
   ; Plot redshift tests, first guesses and adopted redshift 
   ; w. uncertainty
   ;
   ; ztest    -> Test redshifts from wavelengths of lines
   ; ytest    -> Y position on plot for tests
   ; ng       -> number of redshift guesses
   ; zguess   -> redshift guesses
   ; zguessa  -> redshift guesses - error
   ; zguessb  -> redshift guesses + error
   ; oguess   -> Odds of guess, used to set symbol size
   ; source   -> source of guess (0-4)
   ; zbest    -> best redshift
   ; ezbest   -> unc in best redshift
   ; title    -> plot title
   ; hardfile -> output hardfile name
   ;
   ; G. Meurer 12/2004
   ;
   ; setup stuff
   xrange     = [0.0, 5.0]
   yrange     = [0.0, 6.0]
   scolor     = [!red, !magenta, !blue, !cyan]
   ssym       = [3, 4, 1, 5]
   ypl        = [2.0, 3.0, 4.0, 5.0]
   aspect     = 0.75
   ;
   IF keyword_set(hardfile) THEN BEGIN 
      ys = 6.5
      xs = ys/aspect
      yoff=3.
      xoff=1.2
      set_plot,'ps',/copy, /interpolate
      IF strpos(hardfile, '.eps') GT 0 THEN device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color,/encapsulated $
                                       ELSE device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color
      ;psp, /eps, xsize=xs, ysize=ys
   ENDIF  ELSE BEGIN 
      wysize   = 400
      wxsize   = fix(wysize/aspect)
      window, 0, xsize=wxsize, ysize=wysize
   ENDELSE 
   setplotcolors 
   setbgfg,!white,!black
   erase
   plot, ztest, ytest, psym=sym(4), xrange=[0.0, 5.0], yrange=yrange, xstyle=1, ystyle=1, $
    xtitle='Redshift', ytitle=' ', title=title
   IF abs(ezbest) LE 0.5 THEN BEGIN 
      xx  = [zbest-ezbest, zbest+ezbest, zbest+ezbest, zbest-ezbest, zbest-ezbest]
      yy  = [yrange[0], yrange[0], yrange[1], yrange[1], yrange[0]]
      polyfill, xx, yy, color=!lgray
   ENDIF 
   oplot, ztest, ytest, psym=sym(4), color=!dgreen
   FOR jj = 0, ng-1 DO BEGIN 
      xx  = [zguess[jj]]
      yy  = [ypl[source[jj]]]
      s  = oguess[jj]/0.4
      oplot, xx, yy, psym=sym(ssym[source[jj]]), color=scolor[source[jj]], symsize=s
      xx  = [zguessa[jj], zguessb[jj]]
      yy  = ypl[source[jj]]*[1.0, 1.0]
      IF keyword_set(hardfile) THEN oplot, xx, yy, linestyle=0 ELSE oplot, xx, yy, linestyle=0, color=scolor[source[jj]]
   ENDFOR 
   !p.multi   = 0
   !p.noerase = 0
   IF keyword_set(hardfile) THEN BEGIN 
      psend, hardfile, /noprint, /clobber
   ENDIF 
END 
