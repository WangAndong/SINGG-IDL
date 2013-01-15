pro ssoup_cp_wmagfile, ll, outtype, filo, ngal, pt0, pt1, rad, mprof, emproft, emprofs, emprofc, $
                       mcfn, emcfn, mcnr, emcnr, lewr, elewr, lewf, elewf
  ;
  ; open and write radial profiles in magnitude units. 
  ; This needs to be done a few times so this is done in a separate 
  ; procedure. 
  ; 
  ;  ll      -> logical unit of logfile
  ;  outtype -> Output type, needed to write header of file
  ;             0 : raw surface brightness
  ;             1 : raw enclosed flux
  ;             2 : dust corrected surface brightness
  ;             3 : dust corrected enclosed flux
  ;  filo    -> output filename
  ;  ngal    -> number of galaxies
  ;  pt0     -> first element for each galaxy
  ;  pt1     -> last element for each galaxy
  ;  rad     -> radius
  ;  mprof   -> magnitude profile this is 2D array [4,nn]
  ;             for now assume 1 = R, 0 = Halpha, 2 = NUV, 3 = FUV
  ;  emproft -> total mag error
  ;  emprofs -> sky error for Halpha
  ;  emprofc -> continuum subtraction for Halpha
  ;  mcfn    -> colour FUV-NUV
  ;  emcfn   -> error in FUV-NUV
  ;  mcnr    -> colour NUV-R
  ;  emcnr   -> error in NUV-R
  ;  lewr    -> log(EW(Halpha)) = log(F_Halpha/f_R)
  ;  elewr   -> error in above
  ;  lewf    -> log(F_Halpha/f_FUV)
  ;  elewf   -> error in above
  ;
  ; G. Meurer 05/2011
  ;
  ; TTD: * copy columns file to output directory
  ;      * pass nsigma (detection limit threshold) and write to file
  ;
  ; set header stuff
  
  ; TODO: make this less bad
  COMMON bands, band, nband, bandnam, bandavail, nbandavail, combo, ncombo 
  iha  = where(bandavail eq band.HALPHA, /null)
  ir   = where(bandavail eq band.R, /null)
  inuv = where(bandavail eq band.NUV, /null)
  ifuv = where(bandavail eq band.FUV, /null)
  
  prog    = 'SSOUP_CP_WMAGFILE: '
  fmto    = '(f7.2,f8.3,f6.3,f9.3,f6.3,f6.3,f6.3,f8.3,f6.3,f8.3,f6.3,f8.3,f6.3,f8.3,f6.3,f8.3,f6.3,f8.3,f6.3)'
  case outtype of 
     0: begin ;
           hlines1 = '# Surface quantities (in annuli)'
           hlines2 = '#  sma   mu_R   err     lSHa   etot  esky  ecnt   mu_nuv err    mu_fuv err     C(f-n) err    C(n-R) err    lHa/R err     lHa/f err  '
           typ     = 'annular surface quantities '
        end
     1: begin 
           hlines1 = '# Integral quantities (in apertures)'
           hlines2 = '#  sma   mg_R   err     lSHa   etot  esky  ecnt   mg_nuv err    mg_fuv err     C(f-n) err    C(n-R) err    lHa/R err     lHa/f err  '
           typ     = 'aperture integrated quantities'
        end
     2: begin 
           hlines1 = '# Surface quantities, dust corrected (in annuli)'
           hlines2 = '#  sma   mu_R   err     lSHa   etot  esky  ecnt   mu_nuv err    mu_fuv err     C(f-n) err    C(n-R) err    lHa/R err     lHa/f err  '
           typ     = 'dust corrected annular surface quantities '
        end
     3: begin 
           hlines1 = '# Integral quantities, dust corrected (in apertures)'
           hlines2 = '#  sma   mg_R   err     lSHa   etot  esky  ecnt   mg_nuv err    mg_fuv err     C(f-n) err    C(n-R) err    lHa/R err     lHa/f err  '
           typ     = 'dust corrected aperture integrated quantities'
        end
  endcase 
  ;
  ; let users know what we are doing
  plog,ll,prog,'will write file of type: '+numstr(outtype)+' ('+typ+')'
  plog,ll,prog,'opening file: '+filo
  ;
  ; open output file
  openw, lu, filo, /get_lun
  ;
  ; write header lines
  printf,-1,hlines1
  printf,-1,hlines2
  printf,lu,hlines1
  printf,lu,hlines2
  ;
  ; write rest of output file
  FOR jj = 0, ngal-1 DO BEGIN 
     ptt0     = pt0[jj]    ; this saves me some typing
     ptt1     = pt1[jj]    ; this saves me some typing
     ;
     IF ngal GT 1 THEN BEGIN 
        printf,-1,'# galaxy index #'+numstr(jj+1)
        printf,lu,'# galaxy index #'+numstr(jj+1)
     ENDIF 
     FOR ii = ptt0, ptt1 DO BEGIN 
        printf,-1,rad[ii],mprof[ii,ir],emproft[ii,ir],mprof[ii,iha],emproft[ii,iha],$
               emprofs[ii],emprofc[ii],mprof[ii,inuv],emproft[ii,inuv],mprof[ii,ifuv],$
               emproft[ii,ifuv],mcfn[ii],emcfn[ii],mcnr[ii],emcnr[ii],$
               lewr[ii],elewr[ii],lewf[ii],elewf[ii],format=fmto
        printf,lu,rad[ii],mprof[ii,ir],emproft[ii,ir],mprof[ii,iha],emproft[ii,iha],$
               emprofs[ii],emprofc[ii],mprof[ii,inuv],emproft[ii,inuv],mprof[ii,ifuv],$
               emproft[ii,ifuv],mcfn[ii],emcfn[ii],mcnr[ii],emcnr[ii],$
               lewr[ii],elewr[ii],lewf[ii],elewf[ii],format=fmto
     ENDFOR 
  ENDFOR 
  free_lun, lu
  ;
  plog,ll,prog,'file closed, finished'
end
