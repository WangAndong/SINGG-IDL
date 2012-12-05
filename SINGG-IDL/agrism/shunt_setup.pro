PRO shunt_setup, shdir, xbin, dxrange, dyrbn, dyspec, dybuf, dxcrcut, dxstamp, dystamp, awkstr, $
                 parx, pary, exdirct, eydirct, keep_class, keep_color, verbose=verbose
   ;
   ; copy setup files into current directory
   ; set other parameters
   ;
   ; G. Meurer 09/2005
   ; G. Meurer ~12/2005  added dybuf
   ; G. Meurer 02/2006 remove xoffset, replace with parx, pary giving 
   ;                   coordinate transformation coefficients
   ;                   add 'D' to keep class & another 'yellow' to keep_color
   IF keyword_set(verbose) THEN print, 'SHUNT_SETUP: Copying SE inpar and param files'
   cmd = 'cp -f '+shdir+'firstord_finder.inpar .'
   spawn, cmd
   cmd = 'cp -f '+shdir+'firstord_finder.param .'
   spawn, cmd
   IF keyword_set(verbose) THEN print, 'SHUNT_SETUP: setting other parameters'
   xbin    = 25                ; binning factor in x for making squashed image
   dxrange = [-100, 80]        ; range of offsets in unbinned image from peak to plot
   dyrbn   = 17                ; width of ribbon for plotting
   dyspec  =  5                ; width of region for collapsing and extraction
   dybuf   = 1                 ; number of blank rows on each side between spectrum and sky 
   dxcrcut = 31                ; column width for extraction of cross-dispersion cut
   dxstamp = 125               ; column stamp size in squashed grism image
   dystamp = 125               ; row stamp size in squashed grism image
   ;xoffset = -70.0             ; offset in columns between grism image peak and direct image
   ;exdirct =  90.0             ; direct image error box length in columns
   ;eydirct =  13.0             ; direct image error box width in rows
   ;
   ; run sjunt_calmatch & shunt_calfit to get parx, pary
   parx = [-87.071212, 25.068543, -0.0014398046]  ; parameters for squash -> x coord transf.
   pary = [1.6457073, -0.00095596930, 0.99971512] ; parameters for squash -> y coord transf 
   exdirct =  70.0             ; direct image error box length in columns
   eydirct =  11.0             ; direct image error box width in rows
   awkstr  = '$1 == "#" || ($4 < 90.0 && $18 > 2.25 && $15 < 2.0 && $14 < 2.0*$15 && $18 < 5.2)'
   keep_class = 'SAMKBEUDO'
   keep_color = ['green','blue','red','red', 'magenta','cyan','yellow','yellow','white']
END 
