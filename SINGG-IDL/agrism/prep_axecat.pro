PRO prep_axecat, fili, filo, zeropoint, faintlim, brightlim, dimlim, bmin, asmall, colkeep=colkeep
   ;
   ; turn a detection image catalog into a catalog suitable for 
   ; driving aXe.
   ;
   ; fili      -> input catalog
   ; filo      -> output catalog
   ; zeropoint -> magnitude system zeropoint
   ; faintlim  -> objects fainter than this are rejected from catalog
   ; brightlim -> objects brighter than this are rejected from the catalog
   ; dimlim    -> objects fainter than this are made to be round
   ;              (& small)
   ; bmin      -> objects with B_IMAGE smaller than this are rejected 
   ; asmall    -> objects with A_IMAGE smaller than this are made round 
   ;              with A_IMAGE reset to asmall.
   ; magbright -> bright end magnitude limit
   ; colkeep   -> if set columns of input file to awk out.
   ;              There should be 15 columns specifying in the
   ;              follwing order:
   ; #   1 NUMBER          Running object number
   ; #   2 X_IMAGE         Object position along x                       [pixel]
   ; #   3 Y_IMAGE         Object position along y                       [pixel]
   ; #   4 X_WORLD         Barycenter position along world x axis        [deg]
   ; #   5 Y_WORLD         Barycenter position along world y axis        [deg]
   ; #   6 MAG_AUTO        Kron-like elliptical aperture magnitude       [mag]
   ; #   7 A_IMAGE         Profile RMS along major axis                  [pixel]
   ; #   8 B_IMAGE         Profile RMS along minor axis                  [pixel]
   ; #   9 THETA_IMAGE     Position angle (CCW/x)                        [deg]
   ; #  10 A_WORLD         Profile RMS along major axis (world units)    [deg]
   ; #  11 B_WORLD         Profile RMS along minor axis (world units)    [deg]
   ; #  12 THETA_WORLD     Position angle (CCW/world-x)                  [deg]
   ; #  13 FWHM_IMAGE      FWHM assuming a gaussian core                 [pixel]
   ; #  14 FWHM_WORLD      FWHM assuming a gaussian core                 [deg]
   ; #  15 CLASS_STAR      S/G classifier output
   ;
   ;              default colkeep = [1,2,3,29,30,4,39,40,43,41,42,44,58,59,69]
   ;              should work for standard apsis detectionImage.cat files
   ;
   ; This program based heavily on axecat.pro by Z. Tsvetanov.
   ; 
   ; G. Meurer 07/2004
   ;
   pixsize    = 0.05  ; arcsec, probably should be free parameter
   filtemp    = '_temp.cat'
   qq         = "'"
   fmt        = '(i,f,f,f,f,f,f,f,f,f,f,f,f,f,f)'
; set header
   hdrcat = strarr(15)
   hdrcat[0]  = '#   1 NUMBER          Running object number'
   hdrcat[1]  = '#   2 X_IMAGE         Object position along x                       [pixel]'
   hdrcat[2]  = '#   3 Y_IMAGE         Object position along y                       [pixel]'
   hdrcat[3]  = '#   4 X_WORLD         Barycenter position along world x axis        [deg]'
   hdrcat[4]  = '#   5 Y_WORLD         Barycenter position along world y axis        [deg]'
   hdrcat[5]  = '#   6 MAG_AUTO        Kron-like elliptical aperture magnitude       [mag]'
   hdrcat[6]  = '#   7 A_IMAGE         Profile RMS along major axis                  [pixel]'
   hdrcat[7]  = '#   8 B_IMAGE         Profile RMS along minor axis                  [pixel]'
   hdrcat[8]  = '#   9 THETA_IMAGE     Position angle (CCW/x)                        [deg]'
   hdrcat[9]  = '#  10 A_WORLD         Profile RMS along major axis (world units)    [deg]'
   hdrcat[10] = '#  11 B_WORLD         Profile RMS along minor axis (world units)    [deg]'
   hdrcat[11] = '#  12 THETA_WORLD     Position angle (CCW/world-x)                  [deg]'
   hdrcat[12] = '#  13 FWHM_IMAGE      FWHM assuming a gaussian core                 [pixel]'
   hdrcat[13] = '#  14 FWHM_WORLD      FWHM assuming a gaussian core                 [deg]'
   hdrcat[14] = '#  15 CLASS_STAR      S/G classifier output'
   ; awk out appropriate columns to a temporary file
   ; this is where zeropoint comes in...
   ; magnitude and B_IMAGE rejection done here
   ;
   mfaint     = strtrim(string(faintlim - zeropoint),2)
   mbrite     = strtrim(string(brightlim - zeropoint),2)
   coldefault = [1,2,3,29,30,4,39,40,43,41,42,44,58,59,69]
   IF NOT keyword_set(colkeep) THEN colkeep = coldefault  $
      ELSE IF n_elements(colkeep) NE 15 THEN colkeep = coldefault 
   mcol       = strtrim(string(colkeep[5]),2)
   bcol       = strtrim(string(colkeep[7]),2)
   awkcmd     = 'awk '+qq+'substr($1,1,1) != "#" && $'+mcol+' < '+mfaint+$
                ' && $'+mcol+' > '+mbrite+ ' && $'+bcol+' > '+strtrim(string(bmin),2)+$
                ' {printf "%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s\n"'
   FOR i = 0, 5 DO awkcmd = awkcmd +',$'+strtrim(string(colkeep[i]),2)
   awkcmd     = awkcmd + '+' + strtrim(string(zeropoint),2)                 
   FOR i = 6, 14 DO awkcmd = awkcmd +',$'+strtrim(string(colkeep[i]),2)
   awkcmd     = awkcmd + '}'+qq+' ' + fili + ' > ' + filtemp
   file_delete, filtemp, /allow_nonexistent, /quiet
   print, awkcmd
   spawn, awkcmd
   ;
   ; read in temporary catalog
   readcol, filtemp, num, x_image,y_image,x_world,y_world,mag_auto,$
    a_image,b_image,theta_image,a_world,b_world,theta_world,$
    fwhm_image,fwhm_world,class_star,format=fmt
   ;
   ; find faint or small things, make them round & small
   k              = where(mag_auto GE dimlim OR a_image LE asmall)
   a_image[k]     = asmall
   b_image[k]     = asmall
   theta_image[k] = -90.0
   ;
   a_world        = a_image * pixsize / 3600.0
   b_world        = b_image * pixsize / 3600.0
; write output catalog
   colwrite, filo, num, x_image,y_image,x_world,y_world,mag_auto,$
     a_image,b_image,theta_image,a_world,b_world,theta_world,$
     fwhm_image,fwhm_world,class_star, header=hdrcat

END 
