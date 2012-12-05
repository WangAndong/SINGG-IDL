PRO test_sym
   ;
   ; test plots with sym
   x = findgen(30)
   y = findgen(30)
   plot,x,y,sym=0,xrange=[-1,31],yrange=[-1,31],xstyle=1,ystyle=1
   FOR i = 0, 30 DO BEGIN 
      x = [i]
      y = x
      oplot,x,y,psym=sym(i),symsize=1.5
   END 
END
