pro ssoup_overlay_prof, rad, prof, prof0, eprof0, symsize, thick, $
    plot_col, j, k, l, m, nj, nk, nl, nm, j0, k0, l0, m0, nj0, nk0, nl0, nm0
  ;
  ; Prevention of copypasta in radial profile plot creation.
  ; 
  ; rad        -> radii to plot
  ; prof       -> profile to plot
  ; prof0      -> dust corrected profile to plot
  ; eprof0     -> error in above
  ; symsize    -> plot symbol size
  ; thick      -> plot line thickness
  ; plot_col   -> plot_col(our)
  ; the rest   -> stuff that comes out of ssoup_psp_indices
  ;
  ; S. Andrews (ICRAR/UWA) 01/13
  
  IF nj GT 0 THEN BEGIN 
     oplot, rad[j], prof[j], thick=thick, color=plot_col, linestyle=1
     if nk  gt 0 then oplot, rad[k] , prof[k],   thick=thick,   color=plot_col, linestyle=0
     if nj0 gt 0 then oplot, rad[j0], prof0[j0], thick=thick+1, color=plot_col, linestyle=1
     if nk0 gt 0 then begin 
         oplot, rad[k0], prof0[k0], symsize=symsize, color=plot_col, psym=gsym(1)
         oplot, rad[k0], prof0[k0], thick=thick+1, color=plot_col
         oploterror_old, rad[k0], prof0[k0], 0.0*rad[k0], eprof0[k0], $
               /nohat, errthick=thick+1, psym=3, errcolor=plot_col, color=plot_col
     endif 
     if nl0 gt 0 then oplot, rad[l0], prof0[l0], color=plot_col, psym=gsym(19), symsize=symsize
     if nm0 gt 0 then oplot, rad[m0], prof0[m0], color=plot_col, psym=gsym(18), symsize=symsize
  ENDIF 
end