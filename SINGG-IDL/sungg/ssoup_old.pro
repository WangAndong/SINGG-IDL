pro SSOUP_old, infile=infile, logfile=logfile, goslow=goslow
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
  ; G. Meurer 6/2010
  ; G. Meurer 5/2011 this is old version, calls to ssoup_calprof, 
  ;                  ssoup_plotsprofs, ssoup_plothafuv, are replaced by
  ;                  ssoup_calprof_old, ssoup_plotsprofs_old, 
  ;                  ssoup_plothafuv_old, respectively.
  ;
  fili      = 'ssoup.in'
  flog      = 'ssoup.log'
  prog      = 'SSOUP_OLD: '
  ll        = -1
  sdb       = 'singg_sample'
  band      = ['R', 'HALPHA', 'NUV', 'FUV']
  fecntrat  = 0.03
  srcdir    = '.'
  basedir   = '.'
  outdir    = 'HTML'
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
  ssoup_inputs, fili, ll, hname, fimages_in, fmasks_in, mbadval_in, $
                fimages_out, fmask_out, fmask_sky, mbadval_out, skyord, $
                fprofs_out, fbox, filjl, filjh, filjlm1, filjhm1, $
                filjlm2, filjhm2, filjlm3, filjhm3, filjlim1, filjhim1, $
                filjlim2, filjhim2, filjlim3, filjhim3, fcomp, scalprof, $
                fcalprof, scalprof0, fcalprof0, profjpg, profps, $
                hafuvjpg, hafuvps, hafuvjpg0, hafuvps0, status
  IF slow THEN keywait, 'type any key to continue: '
  ;
  IF status THEN BEGIN
     plog,ll,prog,'starting image alignment'
     ssoup_align, ll, hname, fimages_in, fmasks_in, mbadval_in, skyord, $
                  fimages_out, fmask_out, fmask_sky, mbadval_out, fbox, $
                  goslow=slow
     ;
     IF slow THEN keywait, 'type any key to continue: '
     plog,ll,prog,'extracting profiles'
     ssoup_profiles, ll, band, fimages_out, fmask_out, hname, $ 
                     fprofs_out,  /verbose, shapepar='OPT'
     ;
     dbopen,sdb
     list = dbmatch('name',hname[0])
     ebv  = 0.0
     IF list[0] NE -1 THEN dbext,list,'ebv',ebv
     dbclose
     plog,ll,prog,'using E(B-V) = '+numstr(ebv)
     ;
     IF slow THEN keywait, 'type any key to continue: '
     plog,ll,prog,'preparing to make colour images'
     phpl = make_array(4,/float,value=0.0)
     phfl = phpl
     fits_read, fimages_out[0], img, hd, /header_only
     nx   = sxpar(hd,'NAXIS1')
     ny   = sxpar(hd,'NAXIS2')
     imgc = make_array(nx,ny,4,/float,value=0.0)
     FOR ii = 0, 3 DO BEGIN
        fits_read, fimages_out[ii], img, hd
        imgc[*,*,ii] = img
        IF band[ii] EQ 'HALPHA' THEN phfl[ii] = sxpar(hd,'photflux') ELSE phfl[ii] = sxpar(hd,'photflam')
        phpl[ii] = sxpar(hd, 'photplam')
     ENDFOR
     IF slow THEN keywait, 'type any key to continue: '
     plog,ll,prog,'making low cut jpg images'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,filjl,ebv=ebv,goslow=slow
     plog,ll,prog,'making high cut jpg images'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,filjh,ebv=ebv,/highcut,goslow=slow
     ;
     plog,ll,prog,'making low cut jpg images with bad objects masked out'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,filjlm1,ebv=ebv,maskcmd=1,omask=fmask_out,smask=fmask_sky,goslow=slow
     plog,ll,prog,'making high cut jpg images with bad objects masked out'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,filjhm1,ebv=ebv,maskcmd=1,omask=fmask_out,smask=fmask_sky,/highcut,goslow=slow
     ;
     plog,ll,prog,'making low cut jpg images with only bad objects shown '
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,filjlim1,ebv=ebv,maskcmd=-1,omask=fmask_out,smask=fmask_sky,goslow=slow
     plog,ll,prog,'making high cut jpg images with only bad objects shown '
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,filjhim1,ebv=ebv,maskcmd=-1,omask=fmask_out,smask=fmask_sky,/highcut,goslow=slow
     ;
     plog,ll,prog,'making low cut jpg images showing only sky pixels'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,filjlm2,ebv=ebv,maskcmd=2,omask=fmask_out,smask=fmask_sky,goslow=slow
     plog,ll,prog,'making high cut jpg images showing only sky pixelst'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,filjhm2,ebv=ebv,maskcmd=2,omask=fmask_out,smask=fmask_sky,/highcut,goslow=slow
     ;
     plog,ll,prog,'making low cut jpg images showing only pixels excluded from sky'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,filjlim2,ebv=ebv,maskcmd=-2,omask=fmask_out,smask=fmask_sky,goslow=slow
     plog,ll,prog,'making high cut jpg images showing only pixels excluded from sky'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,filjhim2,ebv=ebv,maskcmd=-2,omask=fmask_out,smask=fmask_sky,/highcut,goslow=slow
     ;
     plog,ll,prog,'making low cut jpg images showing pixels used in source  measurements '
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,filjlm3,ebv=ebv,maskcmd=3,omask=fmask_out,smask=fmask_sky,goslow=slow
     plog,ll,prog,'making high cut jpg images showing pixels used in source  measurements '
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,filjhm3,ebv=ebv,maskcmd=3,omask=fmask_out,smask=fmask_sky,/highcut,goslow=slow
     ;
     plog,ll,prog,'making low cut jpg images showing pixels not used in source  measurements'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,filjlim3,ebv=ebv,maskcmd=-3,omask=fmask_out,smask=fmask_sky,goslow=slow
     plog,ll,prog,'making high cut jpg images showing pixels not used in source  measurements'
     ssoup_mkjpg,ll,imgc,band,phfl,phpl,filjhim3,ebv=ebv,maskcmd=-3,omask=fmask_out,smask=fmask_sky,/highcut,goslow=slow
     ;
     plog,ll,prog,'comparing db vs ssoup results'
     ssoup_compresults, ll, hname, phpl, ebv, band, fprofs_out, fcomp
     IF slow THEN keywait, 'type any key to continue: '
     ;
     plog,ll,prog,'making calibrated profiles'
     ssoup_calprof_old, ll, band, phpl, ebv, fprofs_out, scalprof, fcalprof, $
                    scalprof0, fcalprof0, fecntrat=fecntrat
     IF slow THEN keywait, 'type any key to continue: '
     ;
     ; surface brightness profiles
     ; **** the following should be adjusted to allow for multiple ELGs
     plog,ll,prog,'Creating surface brightness and color profiles (jpg)'
     ssoup_plotsprofs_old, ll, hname, scalprof, scalprof0, profjpg
     plog,ll,prog,'Creating surface brightness and color profiles (postscript)'
     ssoup_plotsprofs_old, ll, hname, scalprof, scalprof0, profps
     IF slow THEN keywait, 'type any key to continue: '
     ;
     ; Ha/FUV plots 
     ; **** the following should be adjusted to allow for multiple ELGs
     plog,ll,prog,'Creating raw Halpha/FUV versus surface brightness plots (jpg)'
     ssoup_plothafuv_old, ll, hname, scalprof, hafuvjpg
     plog,ll,prog,'Creating raw Halpha/FUV versus surface brightness plots (postscript)'
     ssoup_plothafuv_old, ll, hname, scalprof, hafuvps
     IF slow THEN keywait, 'type any key to continue: '
     plog,ll,prog,'Creating dust corrected Halpha/FUV versus surface brightness plots (jpg)'
     ssoup_plothafuv_old, ll, hname, scalprof0, hafuvjpg0, /dcorr
     plog,ll,prog,'Creating dust corrected Halpha/FUV versus surface brightness plots (postscript)'
     ssoup_plothafuv_old, ll, hname, scalprof0, hafuvps0, /dcorr
     IF slow THEN keywait, 'type any key to continue: '
     ;
     ; Mark up results 
     ssoup_mkhtml, ll,  srcdir, basedir, outdir, hname, filjl, filjh, filjlm1, filjhm1, $
                   filjlm2, filjhm2, filjlm3, filjhm3, filjlim1, filjhim1, $
                   filjlim2, filjhim2, filjlim3, filjhim3, fcomp, scalprof, $
                   fcalprof, scalprof0, fcalprof0, profjpg, profps, $
                   hafuvjpg, hafuvps, hafuvjpg0, hafuvps0, /uselink
  ENDIF ELSE BEGIN 
     plog,ll,prog,'could not run pipeline because inputs were incorrect'
  ENDELSE
  ;
  plog, ll,prog,'----------------- finished SSOUP ------------------------'
  free_lun, ll
end 
