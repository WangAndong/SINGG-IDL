PRO resetplot

;set plotting colors to black on white

  device, decomposed=0, retain=2
  device, true_color=24
  device, get_visual_depth=depth
  print, 'Display depth: ', depth
  print, 'Color table size: ', !d.table_size

setplotcolors
setbgfg, !white, !black

!p.background=!d.n_colors-1
!p.color=0

END
