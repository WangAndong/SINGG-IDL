pro ssoup_plotkron, fjpg, feps, prof, rad, epilepsy=epilepsy
  ;
  ; Diagnostic plot for R_max in the Kron radius calculation: 
  ; (1) R_max = where profile < k*skysigbx 
  ; (2) $R_kron(Rmax) = \frac{ \int_0^Rmax I(r) r^2 dr} {\int_0^Rmax I(r) r dr}
  ;
  ; fjpg     -> JPEG output filename
  ; feps     -> (E)PS output filename
  ; prof     -> array of profiles to plot
  ; rad      -> and the corresponding radii
  ; epilepsy -> whether to plot to the screen
  ;
  ; S. Andrews (ICRAR/UWA) 01/2013
  
  nr = n_elements(rad)
  COMMON bands, band, nband, bandnam, bandavail, nbandavail
  rkron = dblarr(nbandavail, nr)
  inner_rad = [0.0d, rad]
  for j=0,nbandavail-1 do begin
      for i=0,nr-1 do begin
          temp = (rad[0:i]^2 - inner_rad[0:i]^2) * rad[0:i] * prof[0:i]
          x = total(temp * rad[0:i], /nan)
          y = total(temp, /nan)
          rkron[j, i] = x/y
      endfor
  endfor
  
  ; set up plot
  charsize = 1.5
  symsize  = 1.0
  thick    = 2
  xtitle   = "!3 r!Imax!N [arcsec]"
  ytitle   = "!3 r!Ikron!N [arcsec]"
  yrange    = [min(rkron, /nan)*0.9, max(rkron, /nan)*1.1]
  xrange    = [0, max(rad, /nan)*1.1]
  xs       = 6.5
  ys       = 5.0
  yoff     = 3.0
  xoff     = 0.4
  wxsize   = 600
  ansize   = 1.0
  ssoup_plot_init, feps, xs, ys, xoff, yoff
  !p.noerase = 1
  colors = [ !black, !green, !blue, !red, !yellow, !magenta, !cyan, !brown ]
  plot, rad, rkron[0,*], xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, $
          charsize=charsize, symsize=symsize, thick=thick, xthick=thick, ythick=thick, $
          xtitle=xtitle, ytitle=ytitle, title="Kron radius diagnostic", charthick=thick, $
          xmargin=[8,8], ymargin=[4,4]
  for i=1,nbandavail-1 do oplot, rad, rkron[i,*], thick=thick, color=colors[i], linestyle=1
  ssoup_plot_finish, fjpg, feps, wxsize, epilepsy=epilepsy
end
  