PRO plot_dirctstamp, image, id, pos, siz, expnd, window, prefix, stampim, winpos=winpos
   ; extract a postage stamp image and plot it
   ;
   ; image   -> Input image array
   ; pos     -> 2 element position of center of extraction
   ; siz     -> 2 element size of extraction
   ; expnd   -> expansion factor for display
   ; window  -> window to display in
   ; prefix  -> begining of file name for output .png file.  
   ;            If prefix = '' then no hardcopy is made.
   ; stampim <- 2d image of stamp.
   ;
   ; G. Meurer 01/2007 apply signed square root function to stamp
   ; 
   imsiz   = size(image)
   i0      = fix(pos[0] - siz[0]/2)
   i1      = i0 + siz[0] - 1
   j0      = fix(pos[1] - siz[1]/2)
   j1      = j0 + siz[1] - 1
   i0      = min([max([i0,0]),imsiz[1]-1])
   i1      = min([max([i1,0]),imsiz[1]-1])
   j0      = min([max([j0,0]),imsiz[2]-1])
   j1      = min([max([j1,0]),imsiz[2]-1])
   ;
   stampim = ssqrt(image[i0:i1,j0:j1])
   sizstmp  = [i1-i0+1,j1-j0+1]
   ;
   szplt   = fix(expnd)*sizstmp
   implt   = rebin(stampim,szplt[0],szplt[1],/sample)
   IF keyword_set(winpos) THEN BEGIN 
      IF n_elements(winpos) NE 2 THEN BEGIN
         print, 'ERROR in plot_dirctstamp: winpos must be a 2 element array'
         return
      ENDIF 
      window,window,xsize=szplt[0],ysize=szplt[1],$
       xpos=winpos[0],ypos=winpos[1]
   ENDIF ELSE window,window,xsize=szplt[0],ysize=szplt[1]
   tvscl,(-1.0*implt)
   pfx     = strtrim(prefix,2)
   IF strlen(pfx) GT 0 THEN BEGIN 
      name = pfx+strtrim(string(id),2)+'.jpg'
      ; name  = namstmp(pfx,id)
      im = tvrd(true=3)
      WRITE_JPEG,name,im,TRUE=3,QUALITY=100
   ENDIF 
END
