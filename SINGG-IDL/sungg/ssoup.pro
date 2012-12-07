pro SSOUP, infile=infile, logfile=logfile, goslow=goslow
  ;
  ; SSOUP: SINGG/SUNGG Optical-Ultraviolet Pipeline
  ; 
  ; Run the SINGG/SUNGG Optical-Ultraviolet Pipeline
  ;    infile  -> optional name of file that inputs are taken from
  ;               default="ssoup.in"
  ;    outfile -> optional log file name
  ;               default="ssoup.log"
  ;    goslow  -> if set then stragetically placed calls to 
  ;               keywait.pro are used to slow down the processing
  ;               to a speed a user can monitor.
  ;
  ; **** TTD:
  ; - output sky models
  ; - put polynomial sky model in headers
  ;
  ; G. Meurer (ICRAR/UWA) 6/2010
  ; G. Meurer (ICRAR/UWA) 5/2011: The following procedures were updated:
  ; - ssoup_calprof.pro: * now reintegrates surface brightness profiles
  ;                        after dust correction.  
  ;                      * improved error model introduced for Halpha
  ;                      * upper limits and flags introduced
  ;                      * output data file format changed
  ;                      * works with multiple galaxies to first order
  ;
  ; - ssoup_plotsprofs:  * reads new files
  ;                      * handles limits
  ;
  ; - ssoup_plothafuv:   * reads new files
  ;                      * handles limits
  ; G. Meurer (ICRAR/UWA) 8/2012:
  ;                      * implement sky box plotting
  ;                        (required changes to ssoup_inputs, ssoup_mkhtml)
  ;                      * improve documentation
  ;
  fili      = 'ssoup.in'
  flog      = 'ssoup.log'
  prog      = 'SSOUP: '
  ll        = -1
  sdb       = 'singg_sample'
  fecntrat  = 0.03
  srcdir    = '.'
  basedir   = '.'
  outdir    = 'HTML'
  bxdef     = 15      
  
  ; load prerequisites
  astrolib
  setplotcolors
  COMMON bands, band, nband
  
  ; **** Note bxdef is the sky box size, it is currently hard wired 
  ; into ssoup_askyfit.  It should be an optional input parameter
  ;
  if not keyword_set(infile) then infile=fili
  if not keyword_set(logfile) then logfile=flog
  slow      = keyword_set(goslow)
  ;
  ; open logfile
  openw, ll, flog, /get_lun
  plog,ll,prog,'------------------ starting SSOUP ------------------------'
  plog,ll,prog,'getting inputs from file: '+infile
  ;
  ; get inputs
  ssoup_inputs, infile, ll, inputstr
  IF slow THEN keywait, 'type any key to continue: '
  ;
  IF inputstr.status THEN BEGIN
     ;
     ; image alignment
     plog,ll,prog,'starting image alignment'
     ssoup_align, ll, inputstr, goslow=slow
     ;
     ; make sky box plots
     plog,ll,prog,'making plots of sky boxes'
     FOR ii = 0, nband-1 DO BEGIN 
        ssoup_plotboxes, ll, bxdef, inputstr.hname, band[ii], inputstr.fbox[ii], inputstr.fbplotj[ii] 
        ssoup_plotboxes, ll, bxdef, inputstr.hname, band[ii], inputstr.fbox[ii], inputstr.fbplote[ii] 
     ENDFOR 
     ;
     ; extract radial profiles
     IF slow THEN keywait, 'type any key to continue: '
     plog,ll,prog,'extracting profiles'
     ssoup_profiles, ll, inputstr.fimages_out, inputstr.fmask_out, inputstr.hname, $ 
                     (inputstr.fprofs_out),  /verbose, shapepar='OPT'
     ;
     ; get foreground dust absorption
     dbopen,sdb
     list = dbmatch('name', inputstr.hname[0])
     ebv  = 0.0
     IF list[0] NE -1 THEN dbext,list,'ebv',ebv
     dbclose
     plog,ll,prog,'using E(B-V) = '+numstr(ebv)
     ;
     IF slow THEN keywait, 'type any key to continue: '
     plog,ll,prog,'preparing to make colour images'
     phpl = make_array(4,/float,value=0.0)
     phfl = phpl
     ;
     ; read in the output fits images so as to create 3 color 
     ; preview images.  First just read a header to get image size
     fits_read, inputstr.fimages_out[0], img, hd, /header_only
     nx   = sxpar(hd,'NAXIS1')
     ny   = sxpar(hd,'NAXIS2')
     imgc = make_array(nx,ny,4,/float,value=0.0)
     FOR ii = 0, nband-1 DO BEGIN
        fits_read, inputstr.fimages_out[ii], img, hd
        imgc[*,*,ii] = img
        IF band[ii] EQ 'HALPHA' THEN phfl[ii] = sxpar(hd,'photflux') ELSE phfl[ii] = sxpar(hd,'photflam')
        phpl[ii] = sxpar(hd, 'photplam')
     ENDFOR
     ;
     ; make preview images
     IF slow THEN keywait, 'type any key to continue: '
     plog,ll,prog,'making low cut jpg images'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,inputstr.fjpg_low,ebv=ebv,goslow=slow
     plog,ll,prog,'making high cut jpg images'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,inputstr.fjpg_high,ebv=ebv,/highcut,goslow=slow
     ;
     plog,ll,prog,'making low cut jpg images with bad objects masked out'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,inputstr.fjpg_mlow1,ebv=ebv,maskcmd=1,omask=inputstr.fmask_out,smask=inputstr.fmask_sky,goslow=slow
     plog,ll,prog,'making high cut jpg images with bad objects masked out'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,inputstr.fjpg_mhigh1,ebv=ebv,maskcmd=1,omask=inputstr.fmask_out,smask=inputstr.fmask_sky,/highcut,goslow=slow
     ;
     plog,ll,prog,'making low cut jpg images with only bad objects shown '
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,inputstr.fjpg_imlow1,ebv=ebv,maskcmd=-1,omask=inputstr.fmask_out,smask=inputstr.fmask_sky,goslow=slow
     plog,ll,prog,'making high cut jpg images with only bad objects shown '
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,inputstr.fjpg_imhigh1,ebv=ebv,maskcmd=-1,omask=inputstr.fmask_out,smask=inputstr.fmask_sky,/highcut,goslow=slow
     ;
     plog,ll,prog,'making low cut jpg images showing only sky pixels'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,inputstr.fjpg_mlow2,ebv=ebv,maskcmd=2,omask=inputstr.fmask_out,smask=inputstr.fmask_sky,goslow=slow
     plog,ll,prog,'making high cut jpg images showing only sky pixelst'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,inputstr.fjpg_mhigh2,ebv=ebv,maskcmd=2,omask=inputstr.fmask_out,smask=inputstr.fmask_sky,/highcut,goslow=slow
     ;
     plog,ll,prog,'making low cut jpg images showing only pixels excluded from sky'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,inputstr.fjpg_imlow2,ebv=ebv,maskcmd=-2,omask=inputstr.fmask_out,smask=inputstr.fmask_sky,goslow=slow
     plog,ll,prog,'making high cut jpg images showing only pixels excluded from sky'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,inputstr.fjpg_imhigh2,ebv=ebv,maskcmd=-2,omask=inputstr.fmask_out,smask=inputstr.fmask_sky,/highcut,goslow=slow
     ;
     plog,ll,prog,'making low cut jpg images showing pixels used in source  measurements '
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,inputstr.fjpg_mlow3,ebv=ebv,maskcmd=3,omask=inputstr.fmask_out,smask=inputstr.fmask_sky,goslow=slow
     plog,ll,prog,'making high cut jpg images showing pixels used in source  measurements '
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,inputstr.fjpg_mhigh3,ebv=ebv,maskcmd=3,omask=inputstr.fmask_out,smask=inputstr.fmask_sky,/highcut,goslow=slow
     ;
     plog,ll,prog,'making low cut jpg images showing pixels not used in source  measurements'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,inputstr.fjpg_imlow3,ebv=ebv,maskcmd=-3,omask=inputstr.fmask_out,smask=inputstr.fmask_sky,goslow=slow
     plog,ll,prog,'making high cut jpg images showing pixels not used in source  measurements'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,inputstr.fjpg_imhigh3,ebv=ebv,maskcmd=-3,omask=inputstr.fmask_out,smask=inputstr.fmask_sky,/highcut,goslow=slow
     ;
     ; Compare results to database values
     plog,ll,prog,'comparing db vs ssoup results'
     ssoup_compresults, ll, inputstr.hname, phpl, ebv, band, inputstr.fprofs_out, inputstr.fcompare
     IF slow THEN keywait, 'type any key to continue: '
     ;
     ; calibrate surface brightness profiles
     plog,ll,prog,'making calibrated profiles'
     ssoup_calprof, ll, band, phpl, ebv, inputstr.fprofs_out, inputstr.scalprof, inputstr.fcalprof, $
                    (inputstr.scalprof0), inputstr.fcalprof0, fecntrat=fecntrat
     IF slow THEN keywait, 'type any key to continue: '
     ;
     ; plot surface brightness profiles
     ; **** the following should be adjusted to allow for multiple ELGs
     plog,ll,prog,'Creating surface brightness and color profiles (jpg)'
     ssoup_plotsprofs, ll, inputstr.hname, inputstr.scalprof, inputstr.scalprof0, inputstr.profjpg
     plog,ll,prog,'Creating surface brightness and color profiles (postscript)'
     ssoup_plotsprofs, ll, inputstr.hname, inputstr.scalprof, inputstr.scalprof0, inputstr.profps
     IF slow THEN keywait, 'type any key to continue: '
     ;
     ; create Ha/FUV plots 
     ; **** the following should be adjusted to allow for multiple ELGs
     plog,ll,prog,'Creating raw Halpha/FUV versus surface brightness plots (jpg)'
     ssoup_plothafuv, ll, inputstr.hname, inputstr.scalprof, inputstr.hafuvjpg
     plog,ll,prog,'Creating raw Halpha/FUV versus surface brightness plots (postscript)'
     ssoup_plothafuv, ll, inputstr.hname, inputstr.scalprof, inputstr.hafuvps
     IF slow THEN keywait, 'type any key to continue: '
     plog,ll,prog,'Creating dust corrected Halpha/FUV versus surface brightness plots (jpg)'
     ssoup_plothafuv, ll, inputstr.hname, inputstr.scalprof0, inputstr.hafuvjpg0, /dcorr
     plog,ll,prog,'Creating dust corrected Halpha/FUV versus surface brightness plots (postscript)'
     ssoup_plothafuv, ll, inputstr.hname, inputstr.scalprof0, inputstr.hafuvps0, /dcorr
     IF slow THEN keywait, 'type any key to continue: '
     ;
     ; Mark up results 
     ssoup_mkhtml, ll,  srcdir, basedir, outdir, hname, filjl, filjh, filjlm1, filjhm1, $
                   filjlm2, filjhm2, filjlm3, filjhm3, filjlim1, filjhim1, $
                   filjlim2, filjhim2, filjlim3, filjhim3, fcomp, scalprof, $
                   fcalprof, scalprof0, fcalprof0, profjpg, profps, $
                   hafuvjpg, hafuvps, hafuvjpg0, hafuvps0, fbplotj, fbplote, /uselink
  ENDIF ELSE BEGIN 
     plog,ll,prog,'could not run pipeline because inputs were incorrect'
  ENDELSE
  ;
  plog, ll,prog,'----------------- finished SSOUP ------------------------'
  free_lun, ll
end 
