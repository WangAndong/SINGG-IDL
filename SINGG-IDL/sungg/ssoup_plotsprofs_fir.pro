pro ssoup_plotsprofs_fir, ll, savprof, fjpg, feps, epilepsy=epilepsy, integrated=integrated
  ;
  ; Plots far-infrared radial profiles. I don't know what other stuff is
  ; interesting here, so this is a separate file.
  ; 
  ;   ll         -> logical unit number for plot
  ;   savprof    -> where to find the profile saveset
  ;   fjpg       -> output plot file name (JPG), must contain "%d"
  ;   feps       -> optput plot file name (EPS), must contain "%d"
  ;   epilepsy   -> whether we should display images on the screen
  ;   integrated -> if set, plot integrated quantities
  ;   
  ; S. Andrews (ICRAR/UWA) 1/2013
  ;
  prog      = 'SSOUP_PLOTSPROFS_FIR: '
  plog,ll,prog,'--------------------- starting '+prog+'---------------------------------'
  
  ; flags
  mflag   =  99.999  ; magnitude flag value
  emflag  =   9.999  ; error mag flag
  clflag  =   7.777  ; colour lower limit flag
  
  ; plot parameters
  charsize  = 1.5
  symsize   = 1.0
  thick     = 2
  if keyword_set(integrated) then begin
      ;abrange   = [20.0,0.0] 
      abrange = [-15, -9]
      abtitle   = '!3 log F!dFUV!n [erg cm!u-2!ns!u-1!n]'
  endif else begin
      ;abrange   = [27.0,10.0] 
      abrange = [-17, -11]
      abtitle = '!3 log F!dFUV!n [erg cm!u-2!ns!u-1!n arcsec!u-2!n]'
  endelse
  rtitle    = '!3 semi-major axis [arcsec]' 
  
  ; read in profile files
  plog,ll,prog,'reading in surface brightness profile saveset'
  restore,savprof
  ngal = n_elements(allprofiles)
  for i=0,ngal-1 do begin
    ; reform filenames based on galaxy number
    fjpg_1 = string(i, format='(%"' + fjpg + '")') 
    feps_1 = string(i, format='(%"' + feps + '")')

    ; rename a few things
    radius = *(allprofiles[i].radius)
    if not keyword_set(integrated) then begin
      model  = *(allprofiles[i].fir_model)
      ; ...
    endif else begin
      model  = *(allprofiles[i].fir_model_int)
      ; ...
    endelse
    rmax      = max(radius)
    
    ; start plot
    rrange    = [0.0,rmax]   
    xs       = 6.5
    ys       = 5.0
    yoff     = 3.0
    xoff     = 0.4
    wxsize   = 600
    ansize   = 1.0
    ssoup_plot_init,feps_1,xs,ys,xoff,yoff
    plog,ll,prog,'plotting fir surface brightness profiles'
    title = ngal gt 1 ? hname + ":S" + numstr(i+1) : hname    
    !p.noerase = 1
    plot, radius, alog10(model), xrange=rrange, yrange=abrange, xstyle=1, ystyle=1, $
          charsize=charsize, symsize=symsize, thick=thick, xthick=thick, ythick=thick, $
          xtitle=rtitle, ytitle=abtitle, title=title, charthick=thick, $
          xmargin=[8,8], ymargin=[4,4]
    ;
    ;
    ; ------------------------------------------------------------------
    ; finish plot
    plog,ll,prog,'Will write plotfile: '+feps_1
    !p.multi   = 0
    !p.noerase = 0
    ssoup_plot_finish,fjpg_1,feps_1,wxsize,epilepsy=epilepsy 
 endfor
 undefine,allprofiles
 plog,ll,prog,'finished '
end