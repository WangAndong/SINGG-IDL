PRO ptest_plotclass
   ;
   ; plot star-galaxy classifier as a function of mag_auto
   ;
   ; G. Meurer 09/2005
   fcat = 'A_cayenne_goods_epoch1.cat'
   fmtc = '(x,x,x,x,x,f,x,x,x,x,x,x,x,x,f)'
   mtitle = '!3 mag!dauto!n '
   ctitle = '!3 class!dstar!n '
   ntitle = '!3 Number'
   charsize = 1.5
   symsize = 1.0
   thick = 1
   dc    = 0.01
   ;
   readcol, fcat, mag, class, format=fmtc
   xrange = [max(mag) + 0.25, min(mag) - 0.25]
   crange = [-0.1, 1.1]
   setplotcolors
   plot, mag, class, xrange=xrange, yrange=crange, xstyle=1, ystyle=1, psym=sym(4), $
    xtitle=mtitle, ytitle=ctitle, charsize=charsize, symsize=symsize, $
    thick=thick, xthick=thick, ythick=thick
   ;
   xhist = crange[0] + dc*(findgen((crange[1]-crange[0])/dc) + 0.5)
   chist = histogram(class, binsize=dc, min=crange[0], max=crange[1])
   hrange = [0.0, 1.1*max(chist)]
   keywait, 'type anything for next plot'
   plot, xhist, chist, xrange=crange, yrange=hrange, xstyle=1, ystyle=1, psym=10, $
    xtitle=ctitle, ytitle=ntitle, thick=thick, xthick=thick, ythick=thick, charsize=charsize
END 
