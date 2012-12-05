PRO ssoup_askyfit, ll, img, masks, order, skypar, eskypar, skysig, skysigbox, boxsize, $
                   skymod, boxdata=boxdata, subtract=subtract, rebox=rebox, verbose=verbose, $
                   poisson=poisson, goslow=goslow
  ;
  ; SINGG/SUNGG Optical Uv Pipeline Align SKYFIT
  ;
  ;  Measure sky in boxes, fit a constant level or a polynomial surface
  ;  to the sky boxes.
  ;
  ;  ll        -> logical unit for log messages
  ;  img       -> image array to be measured
  ;  masks     -> mask where pixels that can be used in sky measurement 
  ;               are marked 0b
  ;  order     -> order of fit to sky [Number of parameters in fit]:
  ;               0: constant sky level [1]
  ;               1: linear (plane)     [3]
  ;               2: quadratic          [6]
  ;               3: cubic              [10]
  ;  skypar    <- fitted parameters defining sky.  See polysurf.pro for
  ;               definition of parameters
  ;  skysig    <- pixel to pixel dispersion in sky level
  ;  skysigbox <- box to box dispersion in sky level
  ;  nskybox   <- number of boxes used in the measurement
  ;  boxsize   <> box size used in measurements
  ;               if boxsize <= 1 then the default is used and this
  ;               is then returned in boxsize
  ;  skymod    -> the modeled sky surface
  ;  boxdata   <> if set then this is returned as an array of box by box
  ;               results having dimensions [6,nbox]  where
  ;               boxdata[*,0] = average
  ;               boxdata[*,1] = x
  ;               boxdata[*,2] = y 
  ;               boxdata[*,3] = sigma
  ;               boxdata[*,4] = fit
  ;               boxdata[*,5] = residual
  ;  subtract  -> set true if skylev is to be subtracted
  ;  rebox     -> if set then a final pass of box2boxbg4 is
  ;               performed on the residuals _after_ the fitting.  
  ;               This may result in a change in the number of boxes...
  ;               rebox is only done if subtract is also set
  ;  verbose   -> if set more verbose output is shown
  ;  poisson   -> if set then pixel-pixel uncertainty is likely to
  ;               be at the Poissonian limit (e.g. galex images)
  ;
  ; NOTE if Poisson is set then img should be in counts without any 
  ;      sky subtraction
  ;
  ; G. Meurer 6/2010  ICRAR/UWA
  ;           1/2011  ICRAR/UWA (add goslow keyword)
  ;
  sdb       = 'singg_derived'
  udb1      = 'sungg_derived_2010'
  udb2      = 'sungg_derived_zz'
  prog      = 'SSOUP_ASKYFIT: '
  bxdef     = 15
  ;bxdef     = 25
  frej      = 0.1                ; allow up to this fraction of boxes to be rejected
  tolsky    = 0.01               ; fractional tolerance in skylev, change must be <= tolsky*skysig
  tolsig    = 0.001              ; fractional tolerance in skysig, change must be <= tolsig*skysig
  tolnum    = 0.5                ; maximum fraction of boxes to be rejected
  nitmax    = 52
  nsigma    = 3.0
  pslimit   = 9.0
  ;
  ; decide on verbose and silent
  vb        = keyword_set(verbose)
  slow      = keyword_set(goslow)
  silent    = not vb
  ;
  ; decide whether to use mysky depending on poisson
  IF keyword_set(poisson) THEN use_sky = 0b ELSE use_sky=1b
  ;
  ; set boxsize to default if passed as <= 1
  if boxsize le 1 then boxsize = bxdef
  ;
  ; if verbose then tell parameters of run
  if vb then begin 
     plog,ll,prog,'-------------------------- starting'+prog+'-----------------------------'
     plog,ll,prog,'boxsize   = '+numstr(boxsize)
     plog,ll,prog,'frej      = '+numstr(frej)+' (maximum frac of boxes that can be rejected per iteration)'
     plog,ll,prog,'tolnum    = '+numstr(tolnum)+' (maximum total number of boxes that can be rejected)'
     plog,ll,prog,'tolsky    = '+numstr(tolsky)+' (tolerance on sky level)'
     plog,ll,prog,'tolsig    = '+numstr(tolsig)+' (tolerance on skysigbox)'
     plog,ll,prog,'nsigma    = '+numstr(nsigma)+' (tolerance in skysigbox for first iteration)'
     plog,ll,prog,'poisson   = '+numstr(fix(keyword_set(poisson)))+' Poissonian limited data?'
     plog,ll,prog,'use_sky   = '+numstr(fix(use_sky))+' use mysky (MMM) to get sky levs?'
     if keyword_set(subtract) then plog,ll,prog,'will subtract the final sky level'
     if keyword_set(boxdata) then plog,ll,prog,'will return box data'
     plog,ll,prog,' '
     plog,ll,prog,'subsequent results given as "results> " iteration nbox skylev skysig skysigbox '
  endif 
  ;
  ; zeroth pass sky level (use wrapper for MMM)
  mysky, img, skylev0, skysig0, mask=masks
  ;
  ; fix sigma for poissonian case
  IF keyword_set(poisson) AND skylev0 LE pslimit THEN skysig0 = sqrt(skylev0)
  nit       = 0
  if vb then plog,ll,prog,'results> '+ljust(nit,3)+' --- '+numstr(skylev0)+'  '+numstr(skysig0)+'  --- '
  ;
  ; now first pass with box2boxbg4
  if vb and slow then plog,ll,prog,'first pass with BOX2BOXBG4'
  IF slow THEN keywait, 'type any key to continue: '    
  box2boxbg4, img, boxsize, nsigma=nsig_init, use_sky=use_sky, /nodisplay, mask=masks, $
              results=skyres, startguess=[skylev0,skysig0], boxinfo=bdata, num_boxes=nbox, $
              reject_frac=frej, poisson=poisson
  nit       = nit+1
  if vb then plog,ll,prog,'results> '+ljust(nit,3)+' '+numstr(nbox)+' '+numstr(skyres[0])+'  '+numstr(skysig0)+'  '+numstr(skyres[1])
  ;
  skylev    = skyres[0]
  skysig    = skysig0     ; for now just taking initial MMM results.
  skysigbox = skyres[1]
  ;
  ; prepare for surface fitting
  xbox      = reform(bdata[1,*],nbox)  ; x position
  ybox      = reform(bdata[2,*],nbox)  ; y position
  sbox      = reform(bdata[0,*],nbox)  ; sky level
  esbox     = reform(bdata[3,*],nbox)  ; error in sky
  ubox      = make_array(nbox, /byte, value=1b)  ; will use this to mark which boxes to use in the fit
  ;
  ; **** somewhere around here calculate scaled sma dist to each box
  ;      and use this to decide whether to fit boxes, on setting an
  ;      apropriate switch
  ;
  ; set up fitting parameters
  CASE order OF 
     0: npar = 1
     1: npar = 3
     2: npar = 6
     3: npar = 10
     ELSE: stop, prog+'inappropriate order = '+numstr(order)
  ENDCASE 
  start     = make_array(npar, /float, value=0.0)
  start[0]  = skylev
  ;
  ; Fit the surface
  plog,ll,prog,'Fitting sky surface using MPFIT2DFUNC, order= '+numstr(order)
  func      = "POLYSURF"
  IF slow THEN keywait, 'type any key to continue: '    
  skypar    = mpfit2dfun(func, xbox, ybox, sbox, esbox, start, $
                         bestnorm=bestnorm, dof=dof, maxiter=nitmax, niter=nit, perror=eskypar, $
                         yfit=sboxfit, status=status)
  plog,ll,prog,'status from MPFIT2dFUNC = '+numstr(status)
  plog,ll,prog,'degrees of freedom      = '+numstr(dof)
  plog,ll,prog,'chi^2                   = '+numstr(bestnorm)
  plog,ll,prog,'reduced chi^2           = '+numstr(bestnorm/dof)
  plog,ll,prog,'number of iterations    = '+numstr(nit)
  for ii = 0,npar-1 do plog,ll,prog,'skypar['+numstr(ii)+'] = '+numstr(skypar[ii])+' +/- '+numstr(eskypar[ii])
  ;
  ; calculate rms of sky level after fit
  resid     = sbox - sboxfit
  skysigbox = sqrt(total(resid^2)/dof)
  plog,ll,prog,'skysigbox               = '+numstr(skysigbox)
  ;
  ; calculate sky model
  plog,ll,prog,'calculating sky surface '
  IF slow THEN keywait, 'type any key to continue: '    
  sz         = long(size(img))
  npix       = sz[1]*sz[2]
  xx         = findgen(npix) mod sz[1]
  yy         = float(long(findgen(npix)/sz[1]))
  skymod     = polysurf(xx,yy,skypar)
  ;
  ; subtract sky if requested
  if keyword_set(subtract) then begin 
     plog,ll,prog,'subtracting sky model'
     IF slow THEN keywait, 'type any key to continue: '    
     img          = img - skymod
     ;plog,ll,prog,'box data will be sky subtracted'
     ;bdata[0,*]   = resid
     plog,ll,prog,'recalculating mean sky level and sigma'
     mysky, img, skylev2, skysig2, mask=masks
     nit          = nit + 1
     if vb then plog,ll,prog,'results> '+ljust(nit+1,3)+' --- '+numstr(skylev2)+'  '+numstr(skysig2)+'  --- '
     skysig       = skysig2
     ;
     ; now last pass with box2boxbg4
     if keyword_set(rebox) then begin 
        if vb then plog,ll,prog,'recalculating boxes using residual image '
        IF slow THEN keywait, 'type any key to continue: '    
        nbox_old   = nbox
        box2boxbg4, img, boxsize, nsigma=nsig_init, use_sky=use_sky, /nodisplay, mask=masks, $
                    results=skyres, startguess=[skylev2,skysig2], boxinfo=bdata, num_boxes=nbox, $
                    reject_frac=frej, poisson=poisson
        nit        = nit+1
        skysigbox  = skyres[1]
        if vb then plog,ll,prog,'results> '+ljust(nit,3)+' '+numstr(nbox)+'  '+numstr(skyres[0])+'  '+numstr(skysig)+'  '+numstr(skyres[1])
        if vb then plog,ll,prog,'Nbox(old) = '+strtrim(string(nbox_old),2)+' , Nbox(final) = '+strtrim(string(nbox),2)
        ;
        ; note trickiness to get values as they would be without 
        ; the model subtraction
        resid      = bdata[0,*]
        xbox       = bdata[1,*]
        ybox       = bdata[2,*]
        sboxfit    = polysurf(xbox,ybox,skypar)
        bdata[0,*] = resid + sboxfit            
     endif 
  endif  
  ;
  ; save full boxdata if requested
  if keyword_set(boxdata) then begin 
     plog,ll,prog,'populating boxdata array'
     IF slow THEN keywait, 'type any key to continue: '    
     boxdata      = make_array(6,nbox,/float)
     if not (keyword_set(subtract) and keyword_set(rebox)) then begin 
        xbox      = bdata[1,*]
        ybox      = bdata[2,*]
        sboxfit   = polysurf(xbox,ybox,skypar)
        resid     = bdata[0,*] - sboxfit
     endif 
     boxdata[0,*] = bdata[0,*]
     boxdata[1,*] = bdata[1,*]
     boxdata[2,*] = bdata[2,*]
     boxdata[3,*] = bdata[3,*]
     boxdata[4,*] = sboxfit
     boxdata[5,*] = resid
  endif 
  ;
END 
