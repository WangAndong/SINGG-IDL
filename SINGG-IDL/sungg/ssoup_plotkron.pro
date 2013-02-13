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
  
  prog = 'SSOUP_PLOTKRON: '
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
      nr = make_array(nb, /integer, value=n_elements(rad))
      
      ; calculate kron radius
      rkron = ptrarr(nb)
      inner_rad = [0.0d, rad]
      for j=0,nb-1 do begin
          prof = 10.0^(-0.4**(allprofiles[k].mprof[j]))
          i = min( [n_elements(rad), n_elements(prof)], /nan) -1
          x = total((rad[0:i] - inner_rad[0:i]) * rad[0:i]^2 * prof[0:i], /nan, /cumulative, /double)
          y = total((rad[0:i] - inner_rad[0:i]) * rad[0:i]   * prof[0:i], /nan, /cumulative, /double)
          rkron[j] = ptr_new(x/y)
      endfor
      
      ; set up plot
      yrange    = [0, max(*rkron[0], /nan)*1.1]
      xrange    = [0, max(rad, /nan)*1.1]
      ssoup_plot_init, feps1, xs, ys, xoff, yoff
      title = ngal gt 1 ? hname + ":S" + numstr(k+1) : hname
      !p.noerase = 1
      colors = [ !black, !green, !blue, !red, !dyellow, !magenta, !cyan, !brown ]
      plot, rad[0:nr[0]-1], *(rkron[0]), xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, $
              charsize=charsize, symsize=symsize, thick=thick, xthick=thick, ythick=thick, $
              xtitle=xtitle, ytitle=ytitle, title=title, charthick=thick, $
              xmargin=[8,8], ymargin=[4,4]
      for i=1,nb-1 do begin
          oplot, rad[0:nr[i]-1], *(rkron[i]), thick=thick, color=colors[i], linestyle=1
          plots, [allprofiles[k].r50[i], allprofiles[k].r50[i]], color=colors[i]
      endfor
      plog,ll,prog,'Writing plot file: ' + feps1
      ssoup_plot_finish, fjpg1, feps1, wxsize, epilepsy=epilepsy
  endfor
  plog,ll,prog,'finished.'
end
  