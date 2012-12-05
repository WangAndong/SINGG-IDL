PRO hicats_comp
   ;
   ; make some comparison plots of HIcat values selected from different 
   ; versions of HICAT
   ;
   ; G. Meurer, Feb 04
   setplotcolors
   charsize = 1.5
   ;
   readcol, 'singg_hicats_match.dat', nam, rastr, decstr, sp, sint, w, vel, d, $
    dum0, nm0, dis0, nam0, sp0, sint0, w0, vel0, $
    dum1, nm1, dis1, nam1, sp1, sint1, w1, vel1, $
    format='(a,a,a,f,f,f,f,f,a,i,f,a,f,f,f,f,a,i,f,a,f,f,f,f)'
   ;
   good0    = where(nm0 GT 0,ng0)
   sprat0   = sp0[good0]/sp[good0]
   sintrat0 = sint0[good0]/sint[good0]
   wdiff0   = w0[good0] - w[good0]
   veldiff0 = vel0[good0] - vel[good0]
   good1    = where(nm1 GT 0,ng1)
   sprat1   = sp1[good1]/sp[good1]
   sintrat1 = sint1[good1]/sint[good1]
   wdiff1   = w1[good1] - w[good1]
   veldiff1 = vel1[good1] - vel[good1]
   ;
   print, 'Number of matches with May02 version : ', ng0
   print, 'Number of matches with Feb04 version : ', ng1
   ;
   ; plot velocity differences
   yr       = [min([veldiff0, veldiff1]), max([veldiff0, veldiff1])]
   dvv      = 0.05*(yr[1]-yr[0])
   yr       = [min([veldiff0, veldiff1])-dvv, max([veldiff0, veldiff1])+dvv]
   xr       = [0.0, max(vel)+100.0]
   ;plot, vel[good0], veldiff0, xrange=xr, yrange=yr, xstyle=1, ystyle=1, psym=1, $
   plot, vel[good0], veldiff0, xrange=xr, yrange=[-200.0, 1000.0], xstyle=1, ystyle=1, psym=sym(1), $
    xtitle='V!dr!n [km/s]', ytitle='V!dHIcat!n - V!dused!n', charsize=charsize
   oplot, vel[good0], veldiff0, psym=sym(1), color=!cyan
   oplot, vel[good1], veldiff1, psym=sym(10), color=!blue
   keywait, 'Press any key to continue'
   ;
   ; plot width differences
   yr       = [min([wdiff0, wdiff1]), max([wdiff0, wdiff1])]
   dww      = 0.05*(yr[1]-yr[0])
   yr       = [min([wdiff0, wdiff1])-dww, max([wdiff0, wdiff1])+dww]
   xr       = [0.0, max(w)+100.0]
   ;plot, w[good0], wdiff0, xrange=xr, yrange=yr, xstyle=1, ystyle=1, psym=1, $
   plot, w[good0], wdiff0, xrange=[0,1000], yrange=[-100,200], xstyle=1, ystyle=1, psym=sym(1), $
    xtitle='W!d50!n [km/s]', ytitle='W!dHIcat!n - W!dused!n', charsize=charsize
   oplot, w[good0], wdiff0, psym=sym(1), color=!cyan
   oplot, w[good1], wdiff1, psym=sym(10), color=!blue
   keywait, 'Press any key to continue'
   ;
   plot, vel[good0], wdiff0, xrange=[0.0, max(vel)+100.0], yrange=[-100,100], xstyle=1, ystyle=1, psym=sym(1), $
    xtitle='V!dr!n [km/s]', ytitle='W!dHIcat!n - W!dused!n', charsize=charsize
   oplot, vel[good0], wdiff0, psym=sym(1), color=!cyan
   oplot, vel[good1], wdiff1, psym=sym(10), color=!blue
   keywait, 'Press any key to continue'
   ;
   ; plot peak flux ratios
   yr       = [min([sprat0, sprat1]), max([sprat0, sprat1])]
   dy       = 0.05*(yr[1]-yr[0])
   yr       = [min([sprat0, sprat1])-dy, max([sprat0, sprat1])+dy]
   xr       = [-0.1, max(sp)+0.2]
   plot, sp[good0], sprat0, xrange=xr, yrange=yr, xstyle=1, ystyle=1, psym=sym(1), $
    xtitle='S!dpeak!n [Jy]', ytitle='S!dpeak!n(HIcat) / S!dpeak!n(used)', charsize=charsize
   oplot, sp[good0], sprat0, psym=sym(1), color=!cyan
   oplot, sp[good1], sprat1, psym=sym(10), color=!blue
   keywait, 'Press any key to continue'
   ;
   plot, vel[good0], sprat0, xrange=[0.0, max(vel)+100.0], yrange=yr, xstyle=1, ystyle=1, psym=sym(1), $
    xtitle='V!dr!n [km/s]', ytitle='S!dpeak!n(HIcat) / S!dpeak!n(used)', charsize=charsize
   oplot, vel[good0], sprat0, psym=sym(1), color=!cyan
   oplot, vel[good1], sprat1, psym=sym(10), color=!blue
   keywait, 'Press any key to continue'
   ;
   ; plot integrated flux ratios
   yr       = [min([sintrat0, sintrat1]), max([sintrat0, sintrat1])]
   dy       = 0.05*(yr[1]-yr[0])
   yr       = [min([sintrat0, sintrat1])-dy, max([sintrat0, sintrat1])+dy]
   xr       = [-0.1, max(sp)+0.2]
   plot, sp[good0], sintrat0, xrange=xr, yrange=yr, xstyle=1, ystyle=1, psym=sym(1), $
    xtitle='F!dHI!n [Jy km/s]', ytitle='F!dHI!n(HIcat) / F!dHI!n(used)', charsize=charsize
   oplot, sp[good0], sintrat0, psym=sym(1), color=!cyan
   oplot, sp[good1], sintrat1, psym=sym(10), color=!blue
   keywait, 'Press any key to continue'
   ;
   plot, vel[good0], sintrat0, xrange=[0.0, max(vel)+100.0], yrange=yr, xstyle=1, ystyle=1, psym=sym(1), $
    xtitle='V!dr!n [km/s]', ytitle='F!dHI!n(HIcat) / F!dHI!n(used)', charsize=charsize
   oplot, vel[good0], sintrat0, psym=sym(1), color=!cyan
   oplot, vel[good1], sintrat1, psym=sym(10), color=!blue
   keywait, 'Press any key to continue'
   ;
   ; 
   ;stop
END
