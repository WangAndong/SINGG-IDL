PRO profgen_test, hardfile=hardfile

aa=angstsym()

;title='J1042-23'
title = 'J2257-41'
xtitle='!3 R [arcsec]'
ytitle1='!4 R!3 [cgs]'
ytitle2='!3 log(!4R !3!dH!4a!3!n/!4R !3!dFUV!n ['+aa+']) '
ytitle3='!3 FUV-NUV [ABmag]'


aspect = 3.
charsize = 2.
symsize = 1.0


readcol, 'J2257-41surfprof.txt', newradbin, newrsb, ersb,newhasb, ehasb, newfuvsb, efuvsb, $
                                 newnuvsb, enuvsb, fuvab,efuvab,  nuvab, enuvab,altehasb


nn = n_elements(newradbin)

finalr = newradbin[nn-1]

for ii = 0, nn -1 do begin
eha=(ehasb[ii]/newhasb[ii])/alog(10)
efuv=(efuvsb[ii]/newfuvsb[ii])/alog(10)
enuv=(enuvsb[ii]/newnuvsb[ii])/alog(10)

print, newradbin[ii],eha, efuv, enuv

if efuv gt 0.3 or enuv gt 0.3 or eha gt 0.3 then begin 
finalr = newradbin[ii]
goto, jump
endif

endfor
jump:
print, finalr

xrange=[0,finalr]

;plot surface brightness and compare



   IF keyword_set(hardfile) THEN BEGIN
      xs    = 8.0
      ys    = 3.0*xs/aspect
      yoff  = 8.0
      xoff  = 3.0
      thick = 2
      set_plot,'ps',/copy, /interpolate
      IF strpos(strlowcase(hardfile), '.eps') GT 0 THEN $
         device,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color,/encapsulated ELSE $
         device,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color
      charsize = 0.6*charsize
      symsize  = 0.6*symsize
   ENDIF ELSE BEGIN
      wxsize   = 800
      wysize   = fix(3.0*float(wxsize/aspect))       
      thick    = 2 
      window, 0, xsize=wxsize, ysize=wysize
   ENDELSE

   

   setplotcolors
   ;
   ; plot surface brightnss results
   !p.noerase = 0 
   multiplot,[1,3]
   !p.position = [0.15,0.65, 0.95,0.925]
;   !p.multi   = [3, 1, 3]  
;   hasb = alog10(newhasb)

   plot, newradbin, alog10(newhasb), xrange=xrange, yrange=[-21, -14], $
         color=!black,xstyle=2, ystyle=2, $
         title=title, ytitle=ytitle1, charsize=charsize, psym=1,$
         thick=thick, xthick=thick, ythick=thick, charthick=thick, $
         xmargin=[10,10],ymargin=[20,0]
   oploterror_old, newradbin, alog10(newhasb), (ehasb/newhasb)/alog(10), /nohat, color=!black

   oplot, newradbin, alog10(newfuvsb), color=!blue, psym=2
   oploterror_old, newradbin, alog10(newfuvsb), (efuvsb/newfuvsb)/alog(10), /nohat, $
                 errcolor=!blue

   oplot, newradbin, alog10(newnuvsb), color=!red, psym=2
   oploterror_old, newradbin, alog10(newnuvsb), (enuvsb/newnuvsb)/alog(10), /nohat, $
                  errcolor=!red

;   !p.multi = [2,1,3]
   multiplot
   !p.position = [0.15,0.375, 0.95, 0.65]
   plot, newradbin, alog10(newhasb/newfuvsb), xrange=xrange,xstyle=1, ystyle=2, color=!black, $
         ytitle=ytitle2, charsize=charsize, psym=1, $
         thick=thick, xthick=thick, ythick=thick, charthick=thick, $
         xmargin=[15,10],ymargin=[0,0]
   oploterror_old, newradbin, alog10(newhasb/newfuvsb), $
               sqrt((ehasb/newhasb)^2+(efuvsb/newfuvsb)^2)/alog(10), /nohat, $
               errthick=thick, errcolor=!black,  psym=1

;   !p.multi = [1,1,3]
   multiplot
   !p.position = [0.15,0.1, 0.95, 0.375]
   !x.tickname=''
   plot, newradbin, fuvab - nuvab , xrange=xrange,xstyle=1, ystyle=2, color=!black, $
         xtitle=xtitle, ytitle=ytitle3, charsize=charsize,psym=1, $
         thick=thick, xthick=thick, ythick=thick, charthick=thick, $
         xmargin=[55,10],ymargin=[0,50]
   oploterror_old, newradbin, fuvab-nuvab, sqrt(efuvab^2+enuvab^2)
;   oploterror_old, newradbin, fuvab-nuvab, efuvab+enuvab
   !p.noerase = 0

If keyword_set(hardfile) then BEGIN
 psend, hardfile, /noprint, /clobber
ENDIF

END
