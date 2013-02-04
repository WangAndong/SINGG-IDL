pro ssoup_plotkron, fjpg, feps, prof, rad, skysigbx, epilepsy=epilepsy
  ;
  ; Diagnostic plot for the constant k in the Kron radius calculation: 
  ; (1) R_max = where profile < k*skysigbx 
  ; (2) $R_kron(Rmax) = \frac{ \int_0^Rmax I(r) r^2 dr} {\int_0^Rmax I(r) r dr}
  
  k = 0.1d * dindgen(100)+0.1d
  rkron = dblarr(100)
  inner_rad = [0.0d, rad]
  for i=0,n_elements(k)-1 do begin
      rmax = min(where(prof lt k[i]*skysigbx and prof ne 0 and finite(prof), count))
      if count lt 1 then rmax = n_elements(rad)-1
      temp = (rad[0:rmax]^2 - inner_rad[0:rmax]^2) * rad[0:rmax] * prof[0:rmax]
      x = total(temp * rad[0:rmax], /nan)
      y = total(temp, /nan)
      rkron[i] = x/y
  endfor
  
  ; set up plot
  charsize = 1.5
  symsize  = 1.0
  thick    = 2
  xtitle   = "!3 k"
  ytitle   = "!3 r!Ikron!N [arcsec]"
  yrange    = [min(rkron, /nan)*0.9, max(rkron, /nan)*1.1]
  xrange    = [0, 10]
  xs       = 6.5
  ys       = 5.0
  yoff     = 3.0
  xoff     = 0.4
  wxsize   = 600
  ansize   = 1.0
  ssoup_plot_init, feps, xs, ys, xoff, yoff
  plot, k, rkron, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, $
          charsize=charsize, symsize=symsize, thick=thick, xthick=thick, ythick=thick, $
          xtitle=xtitle, ytitle=ytitle, title="aaaaa", charthick=thick, $
          xmargin=[8,8], ymargin=[4,4]
  ssoup_plot_finish, fjpg, feps, wxsize, epilepsy=epilepsy
end
  