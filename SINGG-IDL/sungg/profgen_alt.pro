PRO profgen_alt, hardfile=hardfile

;aa=angstsym()

;title='J0334-51'
title = 'J2257-41'
xtitle='!3 R [arcsec]'
ytitle1='!4 R!d!3R !n [cgs]'
ytitle2='!4 R!d!3H!4a !n!3 [cgs]'
ytitle3='!4 R!d!3 FUV !n [cgs]'
ytitle4='!4 R !d!3 NUV !n [cgs]'

npscale = 1.5
photfuv = 1.4e-15
photnuv = 2.06e-16
exptimefuv = 1675.0
exptimenuv = 1675.0

aspect = 3.
charsize = 2.
symsize = 1.0


readcol, 'J2257-41surfprof.txt', newradbin, newrsb, ersb,newhasb, ehasb, newfuvsb, efuvsb, $
                                 newnuvsb, enuvsb, fuvab,efuvab,  nuvab, enuvab,altehasb 

read_profile_header, 'J2257-41_R_ss_brightness.profile', tabler

pixsizer = tabler.pixsize
photr = tabler.flux_scale

pfplt_extractprof,'J2257-41_R_ss_brightness.profile', 0, smar, fintr, dfr, altngoodr, altnbadr, $
                  altsbr, altesbr, dfrawr, efintskr

altrsb = altsbr * photr / (pixsizer)^2
altesbr = sqrt(altesbr^2 + efintskr^2) * photr / (pixsizer)^2

read_profile_header, 'J2257-41_Rsub_ss_brightness.profile', tableha

pixsizeha = tableha.pixsize
photha = tableha.flux_scale

pfplt_extractprof,'J2257-41_Rsub_ss_brightness.profile',1b, sma,fint,df,altngood,altnbad,altsb,esb,dfraw,efintsk,efintcn

althasb = altsb * photha / (pixsizeha)^2
;altehasb = sqrt(efintsk^2+efintcn^2) * photha / (pixsizeha)^2

restore, '/data3/sungg/iwong/WORKING/RAD_PROFILES/NGC7424.fuv.rpc.dat'
nfuv=n_elements(lolim)
radfuv=make_array(nfuv,/float, value=0)
altfuvsb=make_array(nfuv,/float,value=0)
altefuvsb=make_array(nfuv,/float,value=0)

badrad=where(finite(altfuvsb) eq 0)

if badrad[0] ne -1 then begin
sbfuv[badrad] = 9999999999999.9
endif

for ll = 0, nfuv-1 do begin

radfuv[ll]= (lolim[ll] + hilim[ll])/2*npscale
altfuvsb[ll]=(avgval[ll]-medbkgd)*photfuv
altefuvsb[ll] = sqrt((sqrt(totval[ll])*photfuv/exptimefuv/npix[ll]/npscale^2)^2 $
               + (rmsbkgd*photfuv)^2)

endfor ; ll

nnuv=n_elements(lolim)
radnuv=make_array(nnuv,/float, value=0)
altnuvsb=make_array(nnuv,/float,value=0)
altenuvsb=make_array(nnuv,/float,value=0)


restore, '/data3/sungg/iwong/WORKING/RAD_PROFILES/NGC7424.nuv.rpc.dat'
nnuv=n_elements(lolim)
radnuv=make_array(nnuv,/float, value=0)
sbnuv=make_array(nnuv,/float,value=0)

badrad=where(finite(altnuvsb) eq 0)

if badrad[0] ne -1 then begin
sbnuv[badrad] = 9999999999999.9
endif

for ll = 0, nnuv-1 do begin

radnuv[ll]= (lolim[ll] + hilim[ll])/2*npscale
altnuvsb[ll]=(avgval[ll]-medbkgd)*photnuv
altenuvsb[ll] = sqrt((sqrt(totval[ll])*photnuv/exptimenuv/npix[ll]/npscale^2)^2 $
               + (rmsbkgd*photnuv)^2)

endfor ; ll


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
   multiplot,[1,4]
   !p.position = [0.15,0.7, 0.95,0.9]
;   !p.multi   = [3, 1, 3]
;   hasb = alog10(newhasb)

   plot, newradbin, alog10(newrsb), xrange=xrange, yrange=[-21, -14], $
         color=!black,xstyle=2, ystyle=2, $
         title=title, ytitle=ytitle1, charsize=charsize, psym=1,$
         thick=thick, xthick=thick, ythick=thick, charthick=thick, $
         xmargin=[10,10],ymargin=[20,0]
   oploterror_old, newradbin, alog10(newrsb), (ersb/newrsb)/alog(10), /nohat, color=!black

   oplot, sma*pixsizeha, alog10(altrsb), color=!blue, psym=2
;   oploterror_old, sma*pixsizeha, alog10(altrsb), (altesbr/altrsb)/alog(10), /nohat, errcolor=!blue



   !p.position = [0.15,0.5, 0.95,0.7]
;   !p.multi   = [3, 1, 3]  
;   hasb = alog10(newhasb)

   plot, newradbin, alog10(newhasb), xrange=xrange, yrange=[-21, -14], $
         color=!black,xstyle=2, ystyle=2, $
         ytitle=ytitle2, charsize=charsize, psym=1,$
         thick=thick, xthick=thick, ythick=thick, charthick=thick, $
         xmargin=[10,10],ymargin=[20,0]
   oploterror_old, newradbin, alog10(newhasb), (ehasb/newhasb)/alog(10), /nohat, color=!black
   oploterror_old, newradbin, alog10(newhasb), (altehasb/newhasb)/alog(10), color=!red
   oplot, sma*pixsizeha, alog10(althasb), color=!blue, psym=2
;   oploterror_old, sma*pixsizeha, alog10(althasb), (altehasb/althasb)/alog(10), /nohat, errcolor=!blue


;   !p.multi = [2,1,3]
   multiplot
   !p.position = [0.15,0.3, 0.95, 0.5]
   plot, newradbin, alog10(newfuvsb), xrange=xrange, yrange=[-21, -14], $
         color=!black,xstyle=2, ystyle=2, $
         ytitle=ytitle3, charsize=charsize, psym=1,$
         thick=thick, xthick=thick, ythick=thick, charthick=thick, $
         xmargin=[10,10],ymargin=[20,0]
   oploterror_old, newradbin, alog10(newfuvsb), (efuvsb/newfuvsb)/alog(10), /nohat, $
                 errcolor=!black
   oplot, radfuv, alog10(altfuvsb), color=!red, psym=2
   oploterror_old, radfuv, (altefuvsb/altfuvsb)/alog(10), /nohat, errcolor=!red

;   !p.multi = [1,1,3]
   multiplot
   !p.position = [0.15,0.1, 0.95, 0.3]
   !x.tickname=''
   plot, newradbin, alog10(newnuvsb), xrange=xrange, yrange=[-21, -14], $
         color=!black,xstyle=2, ystyle=2, $
         xtitle=xtitle, ytitle=ytitle4, charsize=charsize, psym=1,$
         thick=thick, xthick=thick, ythick=thick, charthick=thick, $
         xmargin=[10,10],ymargin=[20,0]

   oploterror_old, newradbin, alog10(newnuvsb), (enuvsb/newnuvsb)/alog(10), /nohat, $
                  errcolor=!black
   oplot, radnuv, alog10(altnuvsb), color=!red, psym=2
   oploterror_old, radnuv, (altenuvsb/altnuvsb)/alog(10), /nohat, errcolor=!red
 


   !p.noerase = 0

If keyword_set(hardfile) then BEGIN
 psend, hardfile, /noprint, /clobber
ENDIF

END
