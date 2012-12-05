PRO grism_triplot_aug02, elm, dirctim, idim, posim, aim, bim, theta, filspc, filrib, wstp, wspc, wrib, $
                   pstp, pspc, prib, hspc, hrib, stampim, bintab, ribbon
   ;
   winposim = [900,850]
   sizstp   = [60,60]
   winpospl = [900,650]
   id       = idim[elm]
   ext      = elm + 1
   pos      = [posim[elm,0], posim[elm,1]]
   orient   = [aim[elm], bim[elm], theta[elm]]
   xsizepl  = 400
   ysizepl  = 180
   xsizetv  = 170
   ysizetv  = 30
   winpostv = [900,550]
   expnd    = 2
   ;
   loadct,0
   plot_dirctstamp, dirctim, id, pos, sizstp, expnd, wstp, pstp, stampim, winpos=winposim
   ;
   ribbon_plot,filrib,ext,id,wrib,prib,hrib,ribbon,winpos=winpostv
   ;
   setplotcolors
   window,wspc,xsize=xsizepl,ysize=ysizepl,xpos=winpospl[0],ypos=winpospl[1]
   print,ext,id,'   ',pspc,orient
   grism_plot,filspc,ext,id,pspc,hspc,bintab,orient=orient
   ;
END

PRO grism_triplot, elm, dirctim, idim, posim, filspc, filrib, wstp, wspc, wrib, $
                   pstp, pspc, prib, hspc, hrib, stampim, bintab, ribbon
   ;
   winposim = [900,850]
   sizstp   = [60,60]
   winpospl = [900,650]
   id       = idim[elm]
   ext      = elm + 1
   pos      = [posim[elm,0], posim[elm,1]]
   xsizepl  = 400
   ysizepl  = 180
   xsizetv  = 170
   ysizetv  = 30
   winpostv = [900,550]
   expnd    = 2
   ;
   plot_dirctstamp, dirctim, id, pos, sizstp, expnd, wstp, pstp, stampim, winpos=winposim
   ;
   window,wspc,xsize=xsizepl,ysize=ysizepl,xpos=winpospl[0],ypos=winpospl[1]
   grism_plot,filspc,ext,id,pspc,hspc,bintab
   ;
   ribbon_plot,filrib,ext,id,wrib,prib,hrib,ribbon,winpos=winpostv
   ;
END
