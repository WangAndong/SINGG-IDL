PRO grism_dualplot, filspc, filstp, wspc, wstp, ext, pspc, pstp, $
                    hspc, hstp, bintab, img
   ; filspc -> input 1d spectra multi-extension bintab file
   ; filstp -> input 2d stamp (ribbon) multi-extension image file
   ; wspc   -> window for 1d spectra
   ; wstp   -> window for image display
   ; ext    -> extension number to plot
   ; pspc   -> prefix for output 1d spec .png plot
   ; pstp   -> prefix for output 2d image plot
   ; hspc   <- output header for binary table 1d spectra
   ; hstp   <- output header for stamp image
   ; bintab <- output binary table
   ; img    <- output stamp image
   ;
   xsizepl = 400
   ysizepl = 180
   xpospl  = 900
   ypospl  = 850
   xsizetv = 170
   ysizetv  = 30
   winpostv = [900,780]
   ;
   window,wspc,xsize=xsizepl,ysize=ysizepl,xpos=xpospl,ypos=ypospl
   grism_plot,filspc,ext,pspc,hspc,bintab
   ;
   ribbon_plot,filstp,ext,wstp,pstp,hstp,img,winpos=winpostv
   ;
END 
