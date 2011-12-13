;--------------------------------------------------------------------
; xtickmark function changes tickmarks from decimal hours to (HH:MM)

FUNCTION myxticks, axis, index, value
 IF value Ge 24 THEN value = value - 24
 hr = fix(value)
 min = fix((value-hr)* 60.)
RETURN, STRING(hr, min, FORMAT="(i2.2, ':', i2.2)")

end

;--------------------------------------------------------------------
; main airmassplot

PRO airmassplot, obsname, startdate, duration, step, name, ra, dec,$
                 dst=dst, ps=ps, yrange=yrange

;+
; PURPOSE:
;  plots airmass as a function of local civil time and universal time
;
; INPUTS:
;  obsname     - Observatory short name. (from astrolib observatory)  
;                To print a list of known names: IDL>airmass, '?'
;  [startdate] - local civil [month,day,year,hour] at start of obs.
;  duration    - length of observation in HOURS
;  step        - step size in MINUTES for each airmass calculation
;  [name]      - string array of object names (suggest <= 20 objects)
;  [ra]        - array of object Right ascension in decimal degrees
;  [dec]       - array of object Declination in decimal degrees
;  dst         - set keyword if daylight savings time
;  ps          - set keyword to name of postscript output file.
;                default output is to screen
;  [yrange]    - if set, the range of airmass values to plot on the 
;                vertical (y) axis.  defalut is 0.5 to 2.5
;
; EXAMPLE: 
;     to plot the airmass for an object starting at 18:00 hr (DST)
;     local time on 4/27/2001 at the Apache Point Observatory over 
;     a 10 hours period with the airmass cumputed for each 15 minute 
;     interval:
;
;     name=['Mrk273']
;     ra = tenv(13,44,42.11)*15.
;     dec= tenv(+55,53,12.6)
;     startdate = [4, 27, 2001, 18]
;     airmassplot, 'apo', startdate, 10, 15, name, ra, dec, $
;                  /dst, ps='output.ps'
;
;     also see ex_airmassplot.pro
;
; This is essentialy a wrapper for airmass.pro by Marc W. Buie.
; Several other routines by Marc Buie are called:
;   airmass    hangle     refrac
;   airindex   badpar     lsidtim
;
; Many Astrolib routines are used.
;
; The old RSI routine timegen (updated by MS) is also used.
;
; The following routines are needed:
;   setplotcolors (M. Seibert)
;   blegend (variation of legend by E. Burgh)
;
; Written by M. Seibert (JHU 2001)
; G. Meurer (ICRAR/UWA)  12/2011
; * add yrange optional parameter

;--------------------------------------------------------------------
; check to see if no paramters

IF n_params() EQ 0 THEN BEGIN
 print,'USAGE: airmassplot, obsname, startdate, duration, step, '+$
       'name, ra, dec, dst=dst, ps=ps'
 GOTO, finish
endif

yr = [0.5,2.5]
if not keyword_set(yrange) then yrange=yr

;--------------------------------------------------------------------
; find observatory info

IF obsname EQ '?' THEN BEGIN
 observatory,'',obs_struct
 forprint, obs_struct.name," ("+obs_struct.observatory+")"
 GOTO, finish
ENDIF ELSE begin
 observatory,obsname,obs_struct
ENDELSE

;--------------------------------------------------------------------
; convert everything to radians 

rar=ra*!pi/180.
decr=dec*!pi/180.
lat = obs_struct.latitude*!pi/180.
lon = obs_struct.longitude*!pi/180.

;--------------------------------------------------------------------
; range of civil observatory time and  UT

jdcnv, startdate[2], startdate[0], startdate[1], $
 startdate[3] + (obs_struct.tz - keyword_set(dst)), startjuldateUT

ut = timegen(fix(ceil(duration*4)), units='Minutes', step_size=step,$
             start=startjuldateUT)

caldat, ut , month_ut, day_ut, year_ut, hour_ut, minute_ut

caldat, ut - (obs_struct.tz - keyword_set(dst))/24.,$
 month, day, year,hour,minute

;--------------------------------------------------------------------
; create array to store airmass

am=fltarr(n_elements(name),n_elements(ut))

;--------------------------------------------------------------------
; compute airmass

FOR i=0,n_elements(name)-1 DO BEGIN
 FOR j=0,n_elements(ut)-1 DO begin
  am[i,j] = airmass(ut[j],rar[i],decr[i],lat,lon)
 ENDFOR
ENDFOR

;--------------------------------------------------------------------
; plot the results

setplotcolors
setbgfg,!white,!black

IF keyword_set(ps) THEN begin
 set_plot,'ps'
 device,filename=ps,$
 xsize=20,ysize=20,xoffset=1.0,yoffset=5,$
 /color, bits=8
ENDIF

hourplot = hour + minute/60.
c = where(hourplot LT hourplot[0],count)
IF count NE 0 THEN hourplot[c] = hourplot[c] + 24 

plot, hourplot, am[0,*], yrange=yrange, ystyle=1,$
 xtitle=strn(month[0])+'/'+strn(day[0])+'/'+strn(year[0])+'  '+$
 obs_struct.name+" Civil Time ", ytitle="Airmass",$
 xtick_get=v,/nodata,xstyle=9,xtickformat='myxticks',$
 pos=[.1,.1,.95,.9], charsize=1.25

utrange = !x.crange  + (obs_struct.tz - keyword_set(dst))
axis, xaxis=1, xrange=utrange, xstyle=1, $
 xtitle= strn(month_ut[0])+'/'+strn(day_ut[0])+'/'+$
  strn(year_ut[0])+'  Universal Time',$
 xtickformat='myxticks', charsize=1.25

FOR i=0,n_elements(name)-1 DO BEGIN
oplot, hourplot, am[i,*],color=!d.n_colors-3-i,psym=-sym(i+1),$
       symsize=1.2
ENDFOR

;--------------------------------------------------------------------
; create the legend 5xN

usym = indgen(n_elements(name))
usym[*] = 1
symb = indgen(n_elements(name))
clr = lindgen(n_elements(name))

FOR i = 0 ,n_elements(name)-1 do begin
 symb[i] = (i+1)
 clr[i]=!d.n_colors-3-i
ENDFOR

cols = ceil((n_elements(name)-1)/5.) > 1

FOR i = 0, cols-1 DO begin
 a=i*5
 b=(a+4) < (n_elements(name)-1)
 blegend,name[a:b],psym=symb[a:b],upsym=usym[a:b],$
         colors=clr[a:b],charsize=1.2,$
         position = [.12+.2*i,.25],/normal,box=0
ENDFOR

;--------------------------------------------------------------------
; close files

finish:

IF keyword_set(ps) THEN begin
 device, /close
 set_plot,'x'
 setbgfg,!white,!black
ENDIF

end

