PRO grism_pages_all, setup=setup
   wd       = 'Output/'                    ; this is where program is run from
   outdir   = 'Plots/'                     ; directory off of wd for plots
   pstmp    = 'stmp_'                      ; prefix for stamps
   pspcc    = 'spcc_'                      ; prefix for count rate spec
   pspcf    = 'spcf_'                      ; prefix for flux calibrated spec 
   pribn    = 'ribn_'                      ; prefix for ribbon plot
   pscii    = 'spec_'                      ; prefix for ascii spec 
   filimg   = 'detectionImage.fits'        ; direct image name
   filgrim  = 'g800l_drz_sci.fits'         ; grism image name
   filspc   = 'g800l_1.SPC.fits'           ; aXe extracted spectra
   filrib   = 'g800l_1.STP.fits'           ; aXe extracted ribbons
   filcat   = 'g800l_1.cat'                ; aXe catalog
   filsens  = 'ACS.WFC.1st.sens.5.fits'    ; sensistivity curve
   exts     = 1                            ; extension for sensitivity curve
   pgpfx    = 'grism_all_'                 ; html page prefix
   lrange   = [5800.0, 9800.0]             ; wavelength range for plotting
   dark     = 8.0/3600.0                   ; dark count rate [DN/s]
   covar    = 1.0                          ; pixel-pixel covariance (?)
   rowsperpage = 50                        ; number of objects per page
   type     = 'GTO'
   ;
   ; execute commands in setup file, if it exists
   IF keyword_set(setup) THEN BEGIN 
      readfmt, setup, '(a120)', line
      nl = n_elements(line)
      FOR i = 0, nl-1 DO BEGIN 
         lin = strtrim(line[i],2)
         print, lin
         IF strmid(lin,0,1) NE ';' THEN result = execute(lin)
      ENDFOR 
   ENDIF 
   ;
   ; go to working directory
   cwd = ''
   cd, wd, current=cwd
   ;
   print,'Setting error model ... '
   fits_read,filgrim,dum,hgrim,/header_only
   exptime = sxpar(hgrim, 'EXPTIME')
   seterrmod2, hgrim, dark=dark, covar=covar, /verbose
   setsensmod, filsens, exts
   ;
   print,'Making plots & data files ... '
   mkall_grplots, filimg, filspc, filrib, filcat, lrange, outdir, exptime=exptime, rate=rate, type=type
   ;
   print,'Making web pages ... '
   rd_grdirct_cat, filcat, id, xim, yim, magauto, aim, bim, thetaim, w50, class, type=type
   cd,outdir
   posim       = [[xim], [yim]] 
   npage       = 1
   i0          = 0
   WHILE i0 LE n_elements(id)-1 DO  BEGIN 
      i1       = min([i0 + rowsperpage,n_elements(id)])-1
      elm      = i0 + indgen(i1-i0+1)
      filhtml  = pgpfx + strtrim(string(npage),2) + '.html'
      title    = 'Grism spectra - page : ' + strtrim(string(npage),2) 
      grism_page_sep02, filhtml, title, elm, id, posim, aim, bim, thetaim, $
       w50, magauto, class,  pstmp, pspcf, pspcc, pribn, pscii
      npage    = npage + 1
      i0       = i1 + 1
      print, ' '
      print, ' completed page : ', filhtml
      print, ' '
   ENDWHILE 
   ;
   ; go back to original directory
   cd, cwd
END 

