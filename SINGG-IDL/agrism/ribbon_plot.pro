PRO ribbon_plot, file, ext, id, window, prefix, hdr, image, winpos=winpos
   ; Extract a ribbon image and plot it
   ;
   ; file   -> fits file containing binary fits table extensions, 
   ;           one for each image.
   ; ext    -> the extension number to extract
   ; id     -> id #
   ; window -> window to plot to.
   ; prefix <- begining of file name for output .png file.  
   ;           If prefix = '' then no hardcopy is made.
   ; hdr    <- header of extension
   ; image  <- iamge array
   ; winpos -> position of window
   image   = mrdfits(file, ext, hdr)
   sz      = size(image)
   szplt   = 2*[sz[1],sz[2]]
   implt   = rebin(image,szplt[0],szplt[1],/sample)
   ;
   IF keyword_set(winpos) THEN BEGIN 
      IF n_elements(winpos) NE 2 THEN BEGIN
         print, 'ERROR in ribbon_plot: winpos must be a 2 element array'
         return
      ENDIF 
      window,window,xsize=szplt[0],ysize=szplt[1],$
       xpos=winpos[0],ypos=winpos[1]
   ENDIF ELSE window,window,xsize=szplt[0],ysize=szplt[1]
   tvscl,(-1.0*implt)
   ;
   pfx     = trim(prefix,2)
   IF strlen(pfx) GT 0 THEN BEGIN 
      ;name = sxpar(hdr,'EXTNAME',count=count)
      ;IF count EQ 0 THEN name = 'ext'+trim(string(ext),2)
      ;name = pfx+name+'.png'
      name = namribn(pfx,id)
      makepng,name,/color
   ENDIF 
END 
