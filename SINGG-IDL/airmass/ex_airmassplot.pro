;----------------------------------------------------------------------
; example use of airmassplot
; see header of airmassplot

readcol,'stdstars.dat',name,rah,ram,ras,d,m,s,format='a,f,f,f,f,f,f'

ra=tenv(rah,ram,ras)*15.
dec=tenv(d,m,s)
startdate = [4, 27, 2001, 18] ; month, day year, hour
duration = 10 ; hours
step = 15     ; minutes

;generate plot on screen
airmassplot, 'apo', startdate, duration, step, name, ra, dec, /dst

;save the above generated screen plot to a gif
makegif,'test.gif',/color

;generate plot as a poscript file 
airmassplot, 'apo', startdate, duration, step, name, ra, dec, /dst,$
             ps='test.ps'

;----------------------------------------------------------------------
;specifiy some colors directly

readcol,'objectlist.dat',name,rah,ram,ras,d,m,s,format='a,f,f,f,f,f,f'

ra=tenv(rah,ram,ras)*15.
dec=tenv(d,m,s)
startdate = [5, 14, 2001, 17] ; month, day year, hour
duration = 10 ; hours
step = 15     ; minutes

;to specify colors onscreen 
set_plot,'x' ; make sure X window device selected for setplotcolors  
setplotcolors
colors=[!black,!red,!blue,!brown,!dgreen]
airmassplot, 'apo', startdate, duration, step, name, ra, dec, /dst,$
 colors=colors

;to specify colors in poscript file
set_plot,'ps' ; make sure X window device selected for setplotcolors
setplotcolors
colors=[!black,!red,!blue,!brown,!dgreen]
airmassplot, 'apo', startdate, duration, step, name, ra, dec, /dst,$
 colors=colors, ps='objairmass.ps'

end
