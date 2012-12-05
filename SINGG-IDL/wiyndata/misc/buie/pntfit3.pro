;+
; NAME:
;	pntfit3
; PURPOSE: (one line)
;	Fit model to telescope pointing data.
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;	pntfit,haobs,decobs,hatrue,dectrue,sidtime,coefs
; INPUTS:
;	haobs   - Observed hour angle in degrees.
;	decobs  - Observed declination in degrees.
;	hatrue  - Catalog hour angle in degrees.
;	dectrue - Catalog declination in degrees.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
;	TITLE   - Label to put on plot.
; OUTPUTS:
;	coefs   - Pointing coefficients.
;	hafix   - Observed hour angle corrected by model (degrees).
;	decfix  - Observed declination corrected by model (degrees).
;	delh    - Correction applied to hour angle in arc sec.
;	deld    - Correction applied to declination in arc sec.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	4/26/93 - Written by Marc W. Buie, Lowell Observatory.
;-
pro pntfit3,haobs,decobs,hatrue,dectrue,coefs,hafix,decfix,delh,deld, $
            TITLE=title

;z=where(dectrue lt 90 and dectrue gt 50)
;haobs=haobs(z)
;hatrue=hatrue(z)
;dectrue=dectrue(z)
;decobs=decobs(z)

lat =  35.09683102d0  ; Larry's values
lon = 111.5359167d0

;lat=35.0967189d0    ; GPS value for 42"
;lon=111.5365053d0

lat_r     = lat     / 180.0d0 * !dpi
lon_r     = lon     / 180.0d0 * !dpi
haobs_r   = haobs   / 180.0d0 * !dpi
decobs_r  = decobs  / 180.0d0 * !dpi
hatrue_r  = hatrue  / 180.0d0 * !dpi
dectrue_r = dectrue / 180.0d0 * !dpi

; Compute altitude and azimuth for observations
az_r  = atan(sin(haobs_r),cos(haobs_r)*sin(lat_r)-tan(decobs_r)*cos(lat_r))+!pi
alt_r = asin(sin(lat_r)*sin(decobs_r)+cos(lat_r)*cos(decobs_r)*cos(haobs_r))
az    = az_r  * !radeg
alt   = alt_r * !radeg
zen   = 90-alt

xx = cos(hatrue_r)
yy = sin(hatrue_r)
zz = sin(lat_r) * cos(dectrue_r) - cos(lat_r) * sin(dectrue_r) * xx
xind = [[xx],[yy],[zz]]
xind = transpose(xind)
yind = (dectrue - decobs) * 3600.0d0
wt = yind*0+1.0d0
coeff=regress(xind,yind,wt,yfit,const,sigma,ftest,r,rmul,chisqr)
resid=yind-yfit
print,xind[*,0]
print,yind[0]

a0 =  const
a1 = -coeff[0]
a2 = -coeff[1]
a3 = -coeff[2]

print,'Declination fit: Chisqr',chisqr,'   R = ',rmul
print,'  a0 = ',a0
print,'  a1 = ',a1,' +/- ',sigma[0],'   r = ',r[0]
print,'  a2 = ',a2,' +/- ',sigma[1],'   r = ',r[1]
print,'  a3 = ',a3,' +/- ',sigma[2],'   r = ',r[2]
print,' Scatter ',mean(abs(resid)),' arc-sec',format='(a,f6.2,a)'
print,' RMS     ',sqrt(total(resid^2)/(n_elements(resid)-1))

ss = 1.0/cos(dectrue_r)
tt = tan(dectrue_r)
;qq = sin(lat_r) * tan(dectrue_r) + cos(lat_r) * cos(hatrue_r)
hh = -a1 * yy * tt + a2 * xx * tt + a3 * cos(lat_r) * ss * yy

;xind2 = [[ss],[tt],[qq],[hatrue]]
xind2 = [[ss],[tt],[haobs],[decobs*haobs],[haobs^2]]
xind2 = transpose(xind2)
yind2 = (hatrue - haobs) * 3600.0d0 - hh

coeff2=regress(xind2,yind2,wt,yfit2,const2,sigma2,ftest2,r2,rmul2,chisqr2)
resid2=yind2-yfit2

print,xind2[*,0]
print,yind2[0]

b0 =  const2
b1 =  coeff2[0]
b2 = -coeff2[1]
;b3 =  coeff2[2]
;b4 =  coeff2[3]
b4 =  coeff2[2]
b5 =  coeff2[3]
b6 =  coeff2[4]

print,'Right Ascension fit: Chisqr',chisqr2,'   R = ',rmul2
print,'  b0 = ',b0
print,'  b1 = ',b1,' +/- ',sigma2[0],'   r = ',r2[0]
print,'  b2 = ',b2,' +/- ',sigma2[1],'   r = ',r2[1]
;print,'  b3 = ',b3,' +/- ',sigma2[2],'   r = ',r2[2]
;print,'  b4 = ',b4,' +/- ',sigma2[3],'   r = ',r2[3]
print,'  b4 = ',b4,' +/- ',sigma2[2],'   r = ',r2[2]
print,'  b5 = ',b5,' +/- ',sigma2[3],'   r = ',r2[3]
print,'  b6 = ',b6,' +/- ',sigma2[4],'   r = ',r2[4]
print,' Scatter ',mean(abs(resid2*cos(dectrue_r))),' arc-sec',format='(a,f6.2,a)'
print,' RMS     ',sqrt(total((resid2*cos(dectrue_r))^2)/(n_elements(resid2)-1))

deld = a0 - a1 * xx - a2 * yy - a3 * zz
;delh = b0 + b1 * ss - b2 * tt + hh + b3 * qq + b4 * hatrue
delh = b0 + b1 * ss - b2 * tt + hh + b4 * haobs + b5 * decobs * haobs + b6 * haobs^2

hafix  = haobs  + delh/3600.0d0
decfix = decobs + deld/3600.0d0

; Reform coefficients to put in physical domain.
coefs    = dblarr(11)
coefs[0] = a0
coefs[1] = b0
coefs[2] = sqrt(a1^2 + a2^2)
coefs[3] = atan(a2,a1) * !radeg
coefs[4] = a3
coefs[5] = b1
coefs[6] = b2
;coefs[7] = b3
coefs[8] = b4
;coefs[8] = 0.
coefs[9] = b5
coefs[10] = b6

if coefs[3] lt 0 then coefs[3] = coefs[3] + 360.0d0

;Print physical coefficients
print,coefs[0],'  Zero point in dec (")'
print,coefs[1],'  Zero point in ra (")'
print,coefs[2],'  Angle between true and instrumental poles (")'
print,coefs[3],'  Angle between line of pole and true meridian (deg)'
print,coefs[4],'  Telescope tube droop in HA and DEC (")'
print,coefs[5],'  Angle between optical and telescope tube axes (")'
print,coefs[6],'  Mechanical orthogonality of RA and DEC axes (")'
;print,coefs[7],'  Dec axis flexure (")'
print,coefs[8],'  HA encoder scale error ("/degree)'
print,coefs[9],'  HA dependency on ha*Dec ("/degree^2)'
print,coefs[10],'  HA dependency on ha^2 ("/degree^2)'

setwin,1
p_multi=!p.multi
!p.multi=[0,2,2]
plot,hatrue,yind,psym=4,ytit='Dec difference (")',xtit='E     Hour Angle (degrees)     W'
plot,hatrue,resid,psym=4,ytit='Dec residual (")',xtit='E     Hour Angle (degrees)     W'
plot,hatrue,yind2*cos(dectrue_r),psym=4,ytit='HA difference (")',xtit='E     Hour Angle (degrees)     W'
plot,hatrue,resid2*cos(dectrue_r),psym=4,ytit='HA residual (")',xtit='E     Hour Angle (degrees)     W'
if keyword_set(title) then xyouts,0.5,0.98,title,align=0.5,/normal
!p.multi=0

setwin,7
!p.multi=[0,3,2]
plot,dectrue,resid,psym=5,xtit='Declination',ytit='Dec Residual (")'
plot,dectrue,resid2*cos(dectrue_r),psym=5,xtit='Declination',ytit='HA Residual (")'
plot,dectrue,sqrt(resid^2+(resid2*cos(dectrue_r))^2),psym=5,xtit='Declination',ytit='Residual (")'
plot,alt,resid,psym=5,xtit='Altitude',ytit='Dec Residual (")',xr=[90,0]
plot,alt,resid2*cos(dectrue_r),psym=5,xtit='Altitude',ytit='HA Residual (")',xr=[90,0]
plot,alt,sqrt(resid^2+(resid2*cos(dectrue_r))^2),psym=5,xtit='Altitude',ytit='Residual (")',xr=[90,0]
if keyword_set(title) then xyouts,0.5,0.985,title,align=0.5,/normal
!p.multi=0

setwin,4
!p.multi=[0,1,7]
plot,resid,psym=5,xtit='Point number',ytit='Dec Residual (")'
plot,resid2*cos(dectrue_r),psym=5,xtit='Point number',ytit='HA Residual (")'
plot,hatrue,psym=5,xtit='Point number',ytit='Hour Angle'
plot,dectrue,psym=5,xtit='Point number',ytit='Declination'
plot,alt,psym=5,xtit='Point number',ytit='Altitude'
plot,az,psym=5,xtit='Point number',ytit='Azimuth'
plot,yind2*cos(dectrue_r),psym=5,xtit='Point number',ytit='HA correction'
!p.multi=0

setwin,5
!p.multi=[0,1,2]
plot,dectrue,(hatrue-haobs)*cos(dectrue_r)*3600.,psym=5,xtit='Declination',ytit='HA correction'
plot,hatrue,(hatrue-haobs)*cos(dectrue_r)*3600.,psym=5,xtit='Hour Angle',ytit='HA correction'
!p.multi=0

setwin,6,xsize=500,ysize=500
px=(90-alt)/90*sin(az_r)
py=-(90-alt)/90*cos(az_r)
ang=findgen(101)/100*!pi*2.0
sf=1.1
plot,px,py,psym=8,xstyle=5,ystyle=5,xr=[-1,1]*sf,yr=[-1,1]*sf, $
   xmargin=[0,0],ymargin=[0,0],position=[0.,0.,1.,1.]
for dralt=10.0,90,10 do begin
   oplot,dralt/90*sin(ang),dralt/90*cos(ang)
   xyouts,0,dralt/90,strtrim(string(fix(90-dralt)),2),align=0.5
   xyouts,0,-dralt/90,strtrim(string(fix(90-dralt)),2),align=0.5,orient=180
endfor
for draz=-180.,170,15 do begin
   dpx=[0.,sin(draz/!radeg)]
   dpy=[0.,cos(draz/!radeg)]
   oplot,dpx,dpy
endfor
for draz=15,80,15 do begin
   xyouts,1.05*sin(draz/!radeg),-1.05*cos(draz/!radeg),strtrim(string(draz),2), $
      align=0.5,orient=draz
   oplot,dpx,dpy
endfor
for draz=105,170,15 do begin
   xyouts,1.05*sin(draz/!radeg),-1.05*cos(draz/!radeg),strtrim(string(draz),2), $
      align=0.5,orient=draz
   oplot,dpx,dpy
endfor
for draz=195,260,15 do begin
   xyouts,1.05*sin(draz/!radeg),-1.05*cos(draz/!radeg),strtrim(string(draz),2), $
      align=0.5,orient=draz
   oplot,dpx,dpy
endfor
for draz=285,350,15 do begin
   xyouts,1.05*sin(draz/!radeg),-1.05*cos(draz/!radeg),strtrim(string(draz),2), $
      align=0.5,orient=draz
   oplot,dpx,dpy
endfor
xyouts,1.05,0,'W',align=0.5,orient=90
xyouts,-1.05,0,'E',align=0.5,orient=270
xyouts,0,1.05,'S',align=0.5,orient=0
xyouts,0,-1.05,'N',align=0.5,orient=180
if keyword_set(title) then xyouts,0.05,0.98,title,align=0,/normal

setwin,0

end
