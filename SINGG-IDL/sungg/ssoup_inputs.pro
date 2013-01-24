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
   ;   profjpg      <- name of output profile plot in jpg format (must contain at least one
   ;                   %d to allow for multiple galaxies)
   ;   profps       <- name of output profile plot in ps format (with %d as above)
   ;   hafuvjpg     <- name of output raw Halpha/fuv plot in jpg format
   ;   hafuvps      <- name of output raw Halpha/fuv plot in ps format
   ;   hafuvjpg0    <- name of output dust corr Halpha/fuv plot in jpg format
   ;   hafuvps0     <- name of output dust corr Halpha/fuv plot in ps format  
   ;   mir_profjpg  <- JPG filename for mid-infrared profile plot (with %d as above)
   ;   mir_profps   <- PS filename for mid-infrared profile plot (with %d as above)
   ;   status       <- status
   ;
   ; G. Meurer 6/2010 (ICRAR/UWA)
   ; G. Meurer 8/2012 (ICRAR/UWA) add inputs for box plots
   prog         = 'SSOUP_INPUTS: '
   COMMON bands, band, nband, bandnam, bandavail, nbandavail, combo, ncombo
      bandavail = [''] ; these are the bands we have for this galaxy
   ;
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
   ; count how many bands we have
   for i=0, nband-1 do begin
      tmp = pfplt_kwdread('FILI_'       + band.(i), keywd,value,'',usetype='STRING')
      if tmp ne '' then begin ; we might have an image for this band
          inf = file_info(tmp)
          if inf.read then begin
              bandavail = [bandavail, band.(i)]
          endif
      endif
   endfor
   ; now we know how big the arrays in the structure are going to be
   ; so we can now initialize them
   bandavail = bandavail[1:*]
   nbandavail = n_elements(bandavail)
   ncombo = factorial(nbandavail)/(6*factorial(nbandavail-3))
   combo = transpose(combigen(nbandavail, 3))
   combostr = strjoin(strmid(bandavail[combo], 0, 2)) ; generates RHN, RHF, etc.
   inputstr = { $
     hname        : '', $
     fimages_in   : strarr(nbandavail), $
     fmasks_in    : strarr(nbandavail), $
     mbadval_in   : make_array(nbandavail, /byte, value=1b), $
     fimages_out  : strarr(nbandavail), $
     fmask_out    : '', $
     fmask_sky    : '', $
     skyord       : intarr(nbandavail), $
     mbadval_out  : 1b, $
     fprofs_out   : strarr(nbandavail), $
     fbox         : strarr(nbandavail), $
     fbplotj      : strarr(nbandavail), $
     fbplote      : strarr(nbandavail), $
     fjpg_low     : strarr(ncombo), $
     fjpg_high    : strarr(ncombo), $
     fjpg_mlow1   : strarr(ncombo), $
     fjpg_mhigh1  : strarr(ncombo), $
     fjpg_mlow2   : strarr(ncombo), $
     fjpg_mhigh2  : strarr(ncombo), $
     fjpg_mlow3   : strarr(ncombo), $
     fjpg_mhigh3  : strarr(ncombo), $
     fjpg_imlow1  : strarr(ncombo), $
     fjpg_imhigh1 : strarr(ncombo), $
     fjpg_imlow2  : strarr(ncombo), $
     fjpg_imhigh2 : strarr(ncombo), $
     fjpg_imlow3  : strarr(ncombo), $
     fjpg_imhigh3 : strarr(ncombo), $
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
     mir_profjpg  : '', $
     mir_profps   : '', $
     status       : 0b $
   }
   ; read stuff in
   inputstr.hname               = pfplt_kwdread('HNAME',keywd,value,'',usetype='STRING')
   for i=0,nbandavail-1 do begin
      inputstr.fimages_in[i]   = pfplt_kwdread('FILI_'       + band.(i), keywd,value,'',usetype='STRING')
      inputstr.fmasks_in[i]    = pfplt_kwdread('FILM_'       + band.(i), keywd,value,'',usetype='STRING')
      inputstr.skyord[i]       = pfplt_kwdread('SKYORD_'     + band.(i), keywd,value,'',usetype='INT')
      inputstr.fimages_out[i]  = pfplt_kwdread('FILO_'       + band.(i), keywd,value,'',usetype='STRING')
      inputstr.fprofs_out[i]   = pfplt_kwdread('FILP_'       + band.(i), keywd,value,'',usetype='STRING')
      inputstr.fbox[i]         = pfplt_kwdread('FBOX_'       + band.(i), keywd,value,'',usetype='STRING')
      inputstr.fbplotj[i]      = pfplt_kwdread('FBPLOT_JPG_' + band.(i), keywd,value,'',usetype='STRING')
      inputstr.fbplote[i]      = pfplt_kwdread('FBPLOT_EPS_' + band.(i), keywd,value,'',usetype='STRING')
   endfor
   inputstr.fmask_out           = pfplt_kwdread('FILM_OUT',keywd,value,'',usetype='STRING')
   inputstr.fmask_sky           = pfplt_kwdread('FILM_SOUT',keywd,value,'',usetype='STRING')
   for i=0, ncombo-1 do begin
      inputstr.fjpg_low[i]      = pfplt_kwdread('FJPGL_'       + combostr[i], keywd,value,'',usetype='STRING')
      inputstr.fjpg_high[i]     = pfplt_kwdread('FJPGH_'       + combostr[i], keywd,value,'',usetype='STRING')
      inputstr.fjpg_mlow1[i]    = pfplt_kwdread('FJPGL_MSK1_'  + combostr[i], keywd,value,'',usetype='STRING')
      inputstr.fjpg_mhigh1[i]   = pfplt_kwdread('FJPGH_MSK1_'  + combostr[i], keywd,value,'',usetype='STRING')
      inputstr.fjpg_mlow2[i]    = pfplt_kwdread('FJPGL_MSK2_'  + combostr[i], keywd,value,'',usetype='STRING')
      inputstr.fjpg_mhigh2[i]   = pfplt_kwdread('FJPGH_MSK2_'  + combostr[i], keywd,value,'',usetype='STRING')
      inputstr.fjpg_mlow3[i]    = pfplt_kwdread('FJPGL_MSK3_'  + combostr[i], keywd,value,'',usetype='STRING')
      inputstr.fjpg_mhigh3[i]   = pfplt_kwdread('FJPGH_MSK3_'  + combostr[i], keywd,value,'',usetype='STRING')
      inputstr.fjpg_imlow1[i]   = pfplt_kwdread('FJPGL_IMSK1_' + combostr[i], keywd,value,'',usetype='STRING')
      inputstr.fjpg_imhigh1[i]  = pfplt_kwdread('FJPGH_IMSK1_' + combostr[i], keywd,value,'',usetype='STRING')
      inputstr.fjpg_imlow2[i]   = pfplt_kwdread('FJPGL_IMSK2_' + combostr[i], keywd,value,'',usetype='STRING')
      inputstr.fjpg_imhigh2[i]  = pfplt_kwdread('FJPGH_IMSK2_' + combostr[i], keywd,value,'',usetype='STRING')
      inputstr.fjpg_imlow3[i]   = pfplt_kwdread('FJPGL_IMSK3_' + combostr[i], keywd,value,'',usetype='STRING')
      inputstr.fjpg_imhigh3[i]  = pfplt_kwdread('FJPGH_IMSK3_' + combostr[i], keywd,value,'',usetype='STRING')
   endfor
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
   inputstr.mir_profjpg      = pfplt_kwdread('MIRPROFJPG',keywd,value,'',usetype='STRING')
   inputstr.mir_profps       = pfplt_kwdread('MIRPROFPS',keywd,value,'',usetype='STRING')
   ;
   ; **** should probably allow badvalues to be read in...
   ;
   ; check status of essential input images
   existm = 1b
   for ii = 0,nbandavail-1 do begin
      inf         = file_info(inputstr.fmasks_in[ii])
      if not inf.read then begin
          plog,ll,prog,'The following input file does not exist: '
          plog,ll,' ',inputstr.fmasks_in[ii]
          existm = 0b
      endif
   endfor
   if existm eq 1 then begin
      plog,ll,prog,'all input files have been verified to exist'
      inputstr.status      = 1b
   endif
   ;
   ; check that output file names are not empty
   qqo             = where(strtrim(inputstr.fimages_out,2) eq '',nqqo)
   qqp             = where(strtrim(inputstr.fprofs_out,2) eq '',nqqp)
   if nqqo gt 0 or nqqp gt 0 then begin
      inputstr.status      = 0b
      plog,ll,prog,'The following output images or profile names are empty: '
      for ii = 0, nqqo-1 do plog,ll,' ','output image name for band = '+band.(qqo[ii])
      for ii = 0, nqqm-1 do plog,ll,' ','output profile file name for band = '+band.(qqm[ii])
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
