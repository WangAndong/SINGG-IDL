pro ssoup_plotkron, ll, savprofile, fjpg, feps, epilepsy=epilepsy
  ;
  ; Diagnostic plot for R_max in the Kron radius calculation: 
  ; (1) R_max = where profile < k*skysigbx 
  ; (2) $R_kron(Rmax) = \frac{ \int_0^Rmax I(r) r^2 dr} {\int_0^Rmax I(r) r dr}
  ; 
  ; ll         -> logical unit number for plot
  ; savprofile  -> where to find the profile saveset
  ; fjpg        -> JPEG output filename, must contain "%d"
  ; feps        -> (E)PS output filename, must contain "%d"
  ; epilepsy    -> whether to plot to the screen
  ;
  ; S. Andrews (ICRAR/UWA) 01/2013
  
  prog = 'SSOUP_PLOTKRON:'
  plog,ll,prog,'------------------------- starting '+prog+'---------------------------------'
  ; restore/rename
  restore,savprofile
  ngal = n_elements(allprofiles)
  nb = n_elements(bname)
  
  ; plot stuff
  charsize = 1.5
  symsize  = 1.0
  thick    = 2
  xtitle   = "!3 r!Imax!N [arcsec]"
  ytitle   = "!3 r!Ikron!N [arcsec]"
  xs       = 6.5
  ys       = 5.0
  yoff     = 3.0
  xoff     = 0.4
  wxsize   = 600
  ansize   = 1.0
  
  for k=0, ngal-1 do begin
      ; deformat file names
      fjpg1 = string(k, format='(%"' + fjpg + '")') 
      feps1 = string(k, format='(%"' + feps + '")')
      rad  = *(allprofiles[k].radius)
      nr = n_elements(rad)
      
      ; calculate kron radius
      rkron = dblarr(nb, nr)
      inner_rad = [0.0d, rad]
      for j=0,nb-1 do begin
          prof = *(allprofiles[k].mprof[j])
          nr = min( [n_elements(rad), n_elements(prof)], /nan)
          for i=0,nr-1 do begin
              temp = (rad[0:i]^2 - inner_rad[0:i]^2) * rad[0:i] * prof[0:i]
              x = total(temp * rad[0:i], /nan)
              y = total(temp, /nan)
              rkron[j, i] = x/y
          endfor
      endfor
      
      ; set up plot
      yrange    = [min(rkron, /nan)*0.9, max(rkron, /nan)*1.1]
      xrange    = [0, max(rad, /nan)*1.1]
      ssoup_plot_init, feps1, xs, ys, xoff, yoff
      !p.noerase = 1
      colors = [ !black, !green, !blue, !red, !yellow, !magenta, !cyan, !brown ]
      plot, rad, rkron[0,*], xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, $
              charsize=charsize, symsize=symsize, thick=thick, xthick=thick, ythick=thick, $
              xtitle=xtitle, ytitle=ytitle, title=hname, charthick=thick, $
              xmargin=[8,8], ymargin=[4,4]
      for i=1,nb-1 do oplot, rad, rkron[i,*], thick=thick, color=colors[i], linestyle=1
      plog,ll,prog,'Writing plot file: ' + feps1
      ssoup_plot_finish, fjpg1, feps1, wxsize, epilepsy=epilepsy
  endfor
  plog,ll,prog,'finished.'
end
  