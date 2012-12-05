PRO test_plot_concat
   fspc = 'output/test_concat_1.SPC.fits'
   ext  = 1
   xsizepl = 400
   ysizepl = 180
   luns = fxposit(fspc, ext)
   lrange = [5800., 9800.]
   pspcf = ''
   orient = 0
   setplotcolors
   window,0,xsize=xsizepl,ysize=ysizepl
   bintab  = mrdfits(luns, 0, hdr)
   grism_fplot1d, bintab, ext, lrange, pspcf, orient=orient
   free_lun, luns
END 
