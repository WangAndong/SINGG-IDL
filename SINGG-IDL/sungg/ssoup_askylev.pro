pro ssoup_askylev, ll, img, masks, skylev, skysig, skysigbox, boxsize, $
                   boxdata=boxdata, subtract=subtract, verbose=verbose, $
                   poisson=poisson
  ;
  ; SINGG/SUNGG Optical Uv Pipeline Align skylev 
  ; measure and optionally subtract skylevel from image
  ;
  ;  img       -> image array to be measured
  ;  masks     -> mask where pixels that can be used in sky measurement 
  ;               are marked 0b
  ;  skylev    <- measured sky level
  ;  skysig    <- pixel to pixel dispersion in sky level
  ;  skysigbox <- box to box dispersion in sky level
  ;  nskybox   <- number of boxes used in the measurement
  ;  boxsize   <> box size used in measurements
  ;               if boxsize <= 1 then the default is used and this
  ;               is then returned in boxsize
  ;  subtract  -> set true if skylev is to be subtracted
  ;  poisson   -> if set then pixel-pixel uncertainty is likely to
  ;               be at the Poissonian limit (e.g. galex images)
  ;
  ; NOTE if Poisson is set then img should be in counts without any 
  ;      sky subtraction
  ;
  ; G. Meurer 6/2010
  ;
  sdb       = 'singg_derived'
  udb1      = 'sungg_derived_2010'
  udb2      = 'sungg_derived_zz'
  prog      = 'SSOUP_ASKYLEV: '
  bxdef     = 15
  frej      = 0.1                ; allow up to this fraction of boxes to be rejected
  tolsky    = 0.01               ; fractional tolerance in skylev, change must be <= tolsky*skysig
  tolsig    = 0.001              ; fractional tolerance in skysig, change must be <= tolsig*skysig
  tolnum    = 0.5                ; maximum fraction of boxes to be rejected
  nitmax    = 52
  nsig_init = 3.0
  nsig_fin  = 5.0
  pslimit   = 9.0
  ;
  ; decide on verbose and silent
  vb        = keyword_set(verbose)
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
     plog,ll,prog,'nitmax    = '+numstr(nitmax)+' (maximum number of iterations) '
     plog,ll,prog,'nsig_init = '+numstr(nsig_init)+' (tolerance in skysigbox for first iteration)'
     plog,ll,prog,'nsig_fin  = '+numstr(nsig_fin)+' (tolerance in skysigbox for later iterations)'
     plog,ll,prog,'poisson   = '+numstr(fix(keyword_set(poisson)))+' Poissonian limited data?'
     plog,ll,prog,'use_sky   = '+numstr(fix(use_sky))+' use mysky (MMM) to get sky levs?'
     if keyword_set(subtract) then plog,ll,prog,'will subtract the final sky level'
     if keyword_set(boxdata) then plog,ll,prog,'will return box data'
     plog,ll,prog,' '
     plog,ll,prog,'subsequent results given as "results> " iteration skylev skysig skysigbox '
  endif 
  ;
  ; zeroth pass sky level (use wrapper for MMM)
  mysky, img, skylev0, skysig0, mask=masks
  ;
  ; fix sigma for poissonian case
  IF keyword_set(poisson) AND skylev0 LE pslimit THEN skysig0 = sqrt(skylev0)
  nit       = 0
  if vb then plog,ll,prog,'results> '+ljust(nit,3)+' '+numstr(skylev0)+'  '+numstr(skysig0)+'  --- '
  ;
  ; now first pass with box2boxbg4
  box2boxbg4, img, boxsize, nsigma=nsig_init, use_sky=use_sky, /nodisplay, mask=masks, $
              results=skyres, startguess=[skylev0,skysig0], boxinfo=bdata, num_boxes=oldnum, $
              reject_frac=frej, poisson=poisson
  nit       = nit+1
  if vb then plog,ll,prog,'results> '+ljust(nit,3)+' '+numstr(skyres[0])+'  '+numstr(skysig0)+'  '+numstr(skyres[1])
  ;
  ; prepare for iterations 
  sigdiff   = 1.0
  skydiff   = 1.0
  numratio  = 1.0
  ;
  ; iterate until differences in skylev and skysigbox are within 
  ; tolerances but without throwing out too many boxes nor
  ; iterating too many times
  WHILE (((sigdiff GT tolsig) AND (skydiff GT tolsky)) AND numratio GT 0.5 AND nit LE nitmax) DO BEGIN
    oldsky   = skyres
    box2boxbg4,img,boxsize,nsigma=nsig_fin,use_sky=use_sky,/nodisplay,$
               mask=mask,results=skyres,startguess=[oldsky[0],oldsky[1]],$
               boxinfo=bdata,num_boxes=num_boxes,reject_frac=rejfrac, poisson=poisson
    sigdiff  = ABS((oldsky[0] - skyres[0])/oldsky[1])
    skydiff  = ABS((oldsky[1] - skyres[1])/oldsky[1])
    numratio = FLOAT(num_boxes)/FLOAT(oldnum)
    nit      = nit + 1
    if vb then plog,ll,prog,'results> '+ljust(nit,3)+' '+numstr(skyres[0])+'  '+numstr(skysig0)+'  '+numstr(skyres[1])
  ENDWHILE
  ;
  ; load results
  skylev    = skyres[0]
  skysig    = skysig0     ; for now just taking initial MMM results.
  skysigbox = skyres[1]
  ;
  ; subtract if required from both images and box levels
  if keyword_set(subtract) then begin 
     if vb then plog,ll,prog,'subtracting sky level = '+numstr(skyres[0])
     img          = img - skyres[0]
     bdata[0,*]   = bdata[0,*] - skyres[0]
  endif 
  ;
  boxdata = bdata
  ;
end
