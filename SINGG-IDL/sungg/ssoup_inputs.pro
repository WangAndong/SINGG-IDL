pro ssoup_inputs, fili, ll, inputstr
   ; Get parameters needed for a run of sungg_pipe
   ;
   ;   fili         -> input file, where all the input parameters 
   ;                   are kept
   ;   ll           -> logical unit to write log file entries to
   ;   inputstr     <- a structure containing all the following:
   ;   hname        <- hipass names
   ;   fimages_in   <- input fits image names
   ;   fmasks_in    <- mask image names
   ;   mbadval_in   <- bad values
   ;   fimages_out  <- output fits image names
   ;   fmask_out    <- output mask
   ;   fmask_sky    <- output sky mask
   ;   mbadval_out  <- bad value 
   ;   skyord       <- sky order
   ;   fprofs_out   <- name of output profile files
   ;   fbox         <- name of output box files
   ;   fbplotj      <- name of output sky box plots (jpg format)
   ;   fbplote      <- name of output sky box plots (eps format)
   ;   fjpg_low     <- name of low cut jpg images
   ;   fjpg_high    <- name of high cut jpg images
   ;   fjpg_mlow1   <- name of low cut jpg images masked with maskcmd=1
   ;   fjpg_mhigh1  <- name of low cut jpg images masked with maskcmd=1
   ;   fjpg_mlow2   <- name of low cut jpg images masked with maskcmd=2
   ;   fjpg_mhigh2  <- name of low cut jpg images masked with maskcmd=2
   ;   fjpg_mlow3   <- name of low cut jpg images masked with maskcmd=3
   ;   fjpg_mhigh3  <- name of low cut jpg images masked with maskcmd=3
   ;   fjpg_imlow1  <- name of low cut jpg images masked with maskcmd=-1
   ;   fjpg_imhigh1 <- name of low cut jpg images masked with maskcmd=-1
   ;   fjpg_imlow2  <- name of low cut jpg images masked with maskcmd=-2
   ;   fjpg_imhigh2 <- name of low cut jpg images masked with maskcmd=-2
   ;   fjpg_imlow3  <- name of low cut jpg images masked with maskcmd=-3
   ;   fjpg_imhigh3 <- name of low cut jpg images masked with maskcmd=-3
   ;   fcompare     <- name of db vs. ssoup comparison file 
   ;   scalprof     <- name of ascii output calibrated surface 
   ;                   brightness/color profiles
   ;   fcalprof     <- name of ascii output calibrated enclosed 
   ;                   flux / color profiles
   ;   scalprof0    <- name of ascii output calibrated surface 
   ;                   brightness/color profiles - dust corrected
   ;   fcalprof0    <- name of ascii output calibrated enclosed 
   ;                   flux / color profiles - dust corrected
   ;   profjpg      <- name of output profile plot in jpg format
   ;   profps       <- name of output profile plot in ps format
   ;   hafuvjpg     <- name of output raw Halpha/fuv plot in jpg format
   ;   hafuvps      <- name of output raw Halpha/fuv plot in ps format
   ;   hafuvjpg0    <- name of output dust corr Halpha/fuv plot in jpg format
   ;   hafuvps0     <- name of output dust corr Halpha/fuv plot in ps format
   ;   status       <- status
   ;
   ; G. Meurer 6/2010 (ICRAR/UWA)
   ; G. Meurer 8/2012 (ICRAR/UWA) add inputs for box plots
   prog         = 'SSOUP_INPUTS: '
   COMMON bands, band, nband, nclr, bandnam
      band      = ['R', 'HALPHA', 'NUV', 'FUV']
      nband     = n_elements(band)
      nclr      = factorial(nband)/(6*factorial(nband-3)) ; number of 3 color combos
      bandnam   = ['R', 'H&alpha;', 'NUV', 'FUV']
   ;
   ; initialize all the variables
   inputstr = { $
     hname        : '', $
     fimages_in   : strarr(nband), $
     fmasks_in    : strarr(nband), $
     mbadval_in   : make_array(nband, /byte, value=1b), $
     fimages_out  : strarr(nband), $
     fmask_out    : '', $
     fmask_sky    : '', $
     skyord       : intarr(nband), $
     mbadval_out  : 1b, $
     fprofs_out   : strarr(nband), $
     fbox         : strarr(nband), $
     fbplotj      : strarr(nband), $
     fbplote      : strarr(nband), $
     fjpg_low     : strarr(nclr), $
     fjpg_high    : strarr(nclr), $
     fjpg_mlow1   : strarr(nclr), $
     fjpg_mhigh1  : strarr(nclr), $
     fjpg_mlow2   : strarr(nclr), $
     fjpg_mhigh2  : strarr(nclr), $
     fjpg_mlow3   : strarr(nclr), $
     fjpg_mhigh3  : strarr(nclr), $
     fjpg_imlow1  : strarr(nclr), $
     fjpg_imhigh1 : strarr(nclr), $
     fjpg_imlow2  : strarr(nclr), $
     fjpg_imhigh2 : strarr(nclr), $
     fjpg_imlow3  : strarr(nclr), $
     fjpg_imhigh3 : strarr(nclr), $
     fcompare     : '', $
     scalprof     : '', $
     fcalprof     : '', $
     scalprof0    : '', $
     fcalprof0    : '', $
     profjpg      : '', $
     profps       : '', $
     hafuvjpg     : '', $
     hafuvps      : '', $
     hafuvjpg0    : '', $
     hafuvps0     : '', $
     status       : 0b $
   }
   plog,ll,prog,'----------------------- starting SSOUP_INPUTS ---------------------------'
   ;
   ; first read in the whole file, then just save the keyword lines.
   plog,ll,prog,'reading input file: '+fili
   fmt      = '(a120)'
   readfmt, fili, fmt, line
   jj       = where(strpos(line, ' = ') GT 0, nkwd)
   kline    = line[jj]
   ;
   ; make arrays to store things
   keywd    = make_array(nkwd, /string, value=' ')
   value    = make_array(nkwd, /string, value=' ')
   ;
   ; keywd is array of all keywords in header lines
   ; value is array of the corresponding columns
   FOR ii = 0, nkwd-1 DO BEGIN 
      str       = kline[ii]
      keywd[ii] = strupcase(gettok(str, ' '))
      dum       = gettok(str, ' ')
      value[ii] = strtrim(str,2)
   ENDFOR 
   ;
   ; now extract parameters from keyword value pairs
   inputstr.hname            = pfplt_kwdread('HNAME',keywd,value,'',usetype='STRING')
   for i=0, nband-1 do begin
      inputstr.fimages_in[i]    = pfplt_kwdread('FILI_'       + band[i], keywd,value,'',usetype='STRING')
      inputstr.fmasks_in[i]     = pfplt_kwdread('FILM_'       + band[i], keywd,value,'',usetype='STRING')
      inputstr.skyord[i]        = pfplt_kwdread('SKYORD_'     + band[i], keywd,value,'',usetype='INT')
      inputstr.fimages_out[i]   = pfplt_kwdread('FILO_'       + band[i], keywd,value,'',usetype='STRING')
      inputstr.fprofs_out[i]    = pfplt_kwdread('FILP_'       + band[i], keywd,value,'',usetype='STRING')
      inputstr.fbox[i]          = pfplt_kwdread('FBOX_'       + band[i], keywd,value,'',usetype='STRING')
      inputstr.fbplotj[i]       = pfplt_kwdread('FBPLOT_JPG_' + band[i], keywd,value,'',usetype='STRING')
      inputstr.fbplote[i]       = pfplt_kwdread('FBPLOT_EPS_' + band[i], keywd,value,'',usetype='STRING')
   ENDFOR
   inputstr.fmask_out        = pfplt_kwdread('FILM_OUT',keywd,value,'',usetype='STRING')
   inputstr.fmask_sky        = pfplt_kwdread('FILM_SOUT',keywd,value,'',usetype='STRING')
   inputstr.fjpg_low[0]      = pfplt_kwdread('FJPGL_HNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_low[1]      = pfplt_kwdread('FJPGL_HRF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_low[2]      = pfplt_kwdread('FJPGL_HRN',keywd,value,'',usetype='STRING')
   inputstr.fjpg_low[3]      = pfplt_kwdread('FJPGL_RNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_high[0]     = pfplt_kwdread('FJPGH_HNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_high[1]     = pfplt_kwdread('FJPGH_HRF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_high[2]     = pfplt_kwdread('FJPGH_HRN',keywd,value,'',usetype='STRING')
   inputstr.fjpg_high[3]     = pfplt_kwdread('FJPGH_RNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mlow1[0]    = pfplt_kwdread('FJPGL_MSK1_HNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mlow1[1]    = pfplt_kwdread('FJPGL_MSK1_HRF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mlow1[2]    = pfplt_kwdread('FJPGL_MSK1_HRN',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mlow1[3]    = pfplt_kwdread('FJPGL_MSK1_RNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mhigh1[0]   = pfplt_kwdread('FJPGH_MSK1_HNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mhigh1[1]   = pfplt_kwdread('FJPGH_MSK1_HRF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mhigh1[2]   = pfplt_kwdread('FJPGH_MSK1_HRN',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mhigh1[3]   = pfplt_kwdread('FJPGH_MSK1_RNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mlow2[0]    = pfplt_kwdread('FJPGL_MSK2_HNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mlow2[1]    = pfplt_kwdread('FJPGL_MSK2_HRF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mlow2[2]    = pfplt_kwdread('FJPGL_MSK2_HRN',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mlow2[3]    = pfplt_kwdread('FJPGL_MSK2_RNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mhigh2[0]   = pfplt_kwdread('FJPGH_MSK2_HNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mhigh2[1]   = pfplt_kwdread('FJPGH_MSK2_HRF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mhigh2[2]   = pfplt_kwdread('FJPGH_MSK2_HRN',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mhigh2[3]   = pfplt_kwdread('FJPGH_MSK2_RNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mlow3[0]    = pfplt_kwdread('FJPGL_MSK3_HNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mlow3[1]    = pfplt_kwdread('FJPGL_MSK3_HRF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mlow3[2]    = pfplt_kwdread('FJPGL_MSK3_HRN',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mlow3[3]    = pfplt_kwdread('FJPGL_MSK3_RNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mhigh3[0]   = pfplt_kwdread('FJPGH_MSK3_HNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mhigh3[1]   = pfplt_kwdread('FJPGH_MSK3_HRF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mhigh3[2]   = pfplt_kwdread('FJPGH_MSK3_HRN',keywd,value,'',usetype='STRING')
   inputstr.fjpg_mhigh3[3]   = pfplt_kwdread('FJPGH_MSK3_RNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imlow1[0]   = pfplt_kwdread('FJPGL_IMSK1_HNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imlow1[1]   = pfplt_kwdread('FJPGL_IMSK1_HRF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imlow1[2]   = pfplt_kwdread('FJPGL_IMSK1_HRN',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imlow1[3]   = pfplt_kwdread('FJPGL_IMSK1_RNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imhigh1[0]  = pfplt_kwdread('FJPGH_IMSK1_HNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imhigh1[1]  = pfplt_kwdread('FJPGH_IMSK1_HRF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imhigh1[2]  = pfplt_kwdread('FJPGH_IMSK1_HRN',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imhigh1[3]  = pfplt_kwdread('FJPGH_IMSK1_RNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imlow2[0]   = pfplt_kwdread('FJPGL_IMSK2_HNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imlow2[1]   = pfplt_kwdread('FJPGL_IMSK2_HRF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imlow2[2]   = pfplt_kwdread('FJPGL_IMSK2_HRN',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imlow2[3]   = pfplt_kwdread('FJPGL_IMSK2_RNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imhigh2[0]  = pfplt_kwdread('FJPGH_IMSK2_HNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imhigh2[1]  = pfplt_kwdread('FJPGH_IMSK2_HRF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imhigh2[2]  = pfplt_kwdread('FJPGH_IMSK2_HRN',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imhigh2[3]  = pfplt_kwdread('FJPGH_IMSK2_RNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imlow3[0]   = pfplt_kwdread('FJPGL_IMSK3_HNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imlow3[1]   = pfplt_kwdread('FJPGL_IMSK3_HRF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imlow3[2]   = pfplt_kwdread('FJPGL_IMSK3_HRN',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imlow3[3]   = pfplt_kwdread('FJPGL_IMSK3_RNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imhigh3[0]  = pfplt_kwdread('FJPGH_IMSK3_HNF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imhigh3[1]  = pfplt_kwdread('FJPGH_IMSK3_HRF',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imhigh3[2]  = pfplt_kwdread('FJPGH_IMSK3_HRN',keywd,value,'',usetype='STRING')
   inputstr.fjpg_imhigh3[3]  = pfplt_kwdread('FJPGH_IMSK3_RNF',keywd,value,'',usetype='STRING')
   inputstr.fcompare         = pfplt_kwdread('FCOMPARE',keywd,value,'',usetype='STRING')
   inputstr.scalprof         = pfplt_kwdread('SCALPROF',keywd,value,'',usetype='STRING')
   inputstr.fcalprof         = pfplt_kwdread('FCALPROF',keywd,value,'',usetype='STRING')
   inputstr.scalprof0        = pfplt_kwdread('SCALPROF0',keywd,value,'',usetype='STRING')
   inputstr.fcalprof0        = pfplt_kwdread('FCALPROF0',keywd,value,'',usetype='STRING')
   inputstr.profjpg          = pfplt_kwdread('PROFJPG',keywd,value,'',usetype='STRING')
   inputstr.profps           = pfplt_kwdread('PROFPS',keywd,value,'',usetype='STRING')
   inputstr.hafuvjpg         = pfplt_kwdread('HAFUVJPG',keywd,value,'',usetype='STRING')
   inputstr.hafuvps          = pfplt_kwdread('HAFUVPS',keywd,value,'',usetype='STRING')
   inputstr.hafuvjpg0        = pfplt_kwdread('HAFUVJPG0',keywd,value,'',usetype='STRING')
   inputstr.hafuvps0         = pfplt_kwdread('HAFUVPS0',keywd,value,'',usetype='STRING')
   ;
   ; **** should probably allow badvalues to be read in...
   ;
   ; check status of essential input images
   existi         = make_array(nband, /byte, value=0b)
   existm         = make_array(nband, /byte, value=0b)
   for ii = 0,nband-1 do begin
      inf         = file_info(inputstr.fimages_in[ii])
      existi[ii]  = inf.exists
      if strlen(inputstr.fmasks_in[ii]) gt 0 then begin 
         ;
         ; only check existence if file name is given
         inf         = file_info(inputstr.fmasks_in[ii])
         existm[ii]  = inf.exists
      endif else begin 
         ;
         ; no file name given, pass the existence test nevertheless
         ; (a default maks will be asumed)
         existm[ii]  = 1b
      endelse 
   endfor 
   qqi            = where(existi ne 1b, nqqi)
   qqm            = where(existm ne 1b, nqqm)
   if nqqi gt 0 or nqqm gt 0 then begin
      inputstr.status      = 0b
      plog,ll,prog,'The following input files do not exist: '
      for ii = 0, nqqi-1 do plog,ll,' ',inputstr.fimages_in[qqi[ii]]
      for ii = 0, nqqm-1 do plog,ll,' ',inputstr.fmasks_in[qqm[ii]]
   endif else begin
      plog,ll,prog,'all input files have been verified to exist'
      inputstr.status      = 1b
   endelse 
   ;
   ; check that output file names are not empty
   qqo             = where(strtrim(inputstr.fimages_out,2) eq '',nqqo)
   qqp             = where(strtrim(inputstr.fprofs_out,2) eq '',nqqp)
   if nqqo gt 0 or nqqp gt 0 then begin
      inputstr.status      = 0b
      plog,ll,prog,'The following output images or profile names are empty: '
      for ii = 0, nqqo-1 do plog,ll,' ','output image name for band = '+band[qqo[ii]]
      for ii = 0, nqqm-1 do plog,ll,' ','output profile file name for band = '+band[qqm[ii]]
   endif else begin
      plog,ll,prog,'checked that output image and profile names are reasonable'
   endelse 
   ;
   
   if strtrim((inputstr.fmask_out),2) eq '' then begin
      inputstr.status = 0b
      plog,ll,prog,'the output mask file name is empty'
   endif
   if strtrim((inputstr.fmask_sky),2) eq '' then begin 
      inputstr.status = 0b
      plog,ll,prog,'the output sky mask file name is empty'
   endif
   ;
   plog,ll,prog,'status on return from '+prog+' '+numstr(fix(inputstr.status))
   ;
end 
