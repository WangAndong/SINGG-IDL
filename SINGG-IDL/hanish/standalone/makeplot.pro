PRO makeplot,fili,ps=ps

; fili: /data/sample.dat.  It's a list of every object in the SINGG
; target set, its characteristics, and how it's been used

; /ps replaces the normal window output with a postscript dump

  psflag = KEYWORD_SET(ps)

; Format for sample.dat:
  readcol_new,fili,object,source,raS,decS,id,Speak,IntSdv,W50,Vhel,Vshap,$
                   obs,lMH1,lon,lat,$
                   format="A,A,A,A,A,F,F,I,F,F,A,F,I,I",comment="#"

  n_objs = N_ELEMENTS(object)

; Convert RA, Dec strings to numeric values in decimal degrees
  ra = FLTARR(n_objs)
  dec = FLTARR(n_objs)
  dummy = FLTARR(2)
  
  FOR ii = 0, n_objs-1 DO BEGIN
    hr = FLOAT(STRMID(raS[ii],0,2))
    rmin = FLOAT(STRMID(raS[ii],3,2))
    rsec = FLOAT(STRMID(raS[ii],6,5))

    ra[ii] = (hr + (rmin/60.d0) + (rsec/3600.d0))

    sign = STRMID(decS[ii],0,1)
    deg = FLOAT(STRMID(decS[ii],1,2))
    dmin = FLOAT(STRMID(decS[ii],4,2))
    dsec = FLOAT(STRMID(decS[ii],7,4))

    dec[ii] = deg + (dmin/60.d0) + (dsec/3600.d0)
    IF (sign EQ "-") THEN dec[ii] = (-1.0*dec[ii])
  ENDFOR

; Next, convert RA and Dec into Lat/Lon

  glactc,ra,dec,2000,lon,lat,1

  unobs = WHERE(obs EQ "N",n_unobs)
  IF n_unobs EQ 0 THEN unobs = dummy
  good = WHERE(obs EQ "Y",n_good)
  bad = WHERE((obs EQ "Yc") OR (obs EQ "YSc") OR (obs EQ "Y:"),n_bad)

  PRINT,"Total galaxies: ",n_objs
  PRINT,"Unobserved: ",n_unobs
  PRINT,"Observed, bad data: ",n_bad
  PRINT,"Observed, good data: ",n_good

; Create four plots:

  IF NOT psflag THEN BEGIN
;    SET_PLOT,'X'
;    HELP,/device
    DEVICE, RETAIN=2, DECOMPOSED=0
    WINDOW,XSIZE=800,YSIZE=800
  ENDIF
  IF psflag THEN set_plot,'PS'
  !P.MULTI=[0,0,0,0,0]

  setplotcolors
; was 4,1,5,!blue,6,!red
  unobs_sym = 1
  unobs_clr = 1
  good_sym = 1
  good_clr = !blue
  bad_sym = 2
  bad_clr = !magenta

; PLOT 1: RA, Dec -> Equal-Area Projection
; using observed samples with one color and all samples with another

  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,/color,bits_per_pixel=8,FILENAME='RA_Dec_EA.ps'
  ENDIF

; Convert RA, Dec to X,Y in a Aitoff equal-area projection
  AITOFF,(15.0*ra),dec,x1,y1

  PLOT,dummy,dummy,XSTYLE=4,YSTYLE=4,/isotropic,$
       TITLE="RA [hr] vs. Declination [deg]",$
       CHARSIZE=1.5,XRANGE=[-180.0,180.0],YRANGE=[-90.0,90.0]
  OPLOT,x1[unobs],y1[unobs],PSYM=SYM(unobs_sym),COLOR=unobs_clr
  OPLOT,x1[good],y1[good],PSYM=SYM(good_sym),COLOR=good_clr
  OPLOT,x1[bad],y1[bad],PSYM=SYM(bad_sym),COLOR=bad_clr

  AITOFF_GALACTIC,45,15,!dgreen

  AITOFF_GRID,30,10,LABEL=2

  IF NOT psflag THEN BEGIN
    makepng,"RA_Dec_EA.png"
    PRINT,"Press any key to continue"
    key = GET_KBRD(1)
  ENDIF

; PLOT 2: Lat, Lon -> Equal-Area Projection
; using observed samples with one color and all samples with another

  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,/color,bits_per_pixel=8,FILENAME='Lat_Lon_EA.ps'
  ENDIF

  AITOFF,lon,lat,x2,y2

  PLOT,dummy,dummy,XSTYLE=4,YSTYLE=4,/isotropic,$
       TITLE="Galactic Longitude [deg] vs. Latitude [deg]",$
       XRANGE=[-180.0,180.0],YRANGE=[-90.0,90.0],CHARSIZE=1.5
  OPLOT,x2[unobs],y2[unobs],PSYM=SYM(unobs_sym),COLOR=unobs_clr
  OPLOT,x2[good],y2[good],PSYM=SYM(good_sym),COLOR=good_clr
  OPLOT,x2[bad],y2[bad],PSYM=SYM(bad_sym),COLOR=bad_clr

  AITOFF_GRID,30,10,/LABEL

  IF NOT psflag THEN BEGIN
    makepng,"Lat_Lon_EA.png"
    PRINT,"Press any key to continue"
    key = GET_KBRD(1)
  ENDIF

; PLOT 3: RA distribution histogram.  
; Separate into three categories: 
; All, Observed, Observed but not usable (cloudy, Schmidt, etc.)

  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,/color,bits_per_pixel=8,FILENAME='RAhist.ps'
  ENDIF

; Note that unlike the other ones, we want to keep these all together
  plothist,ra,bin=0.5,XTITLE="RA [hr]",YTITLE="Number",/fill,$
           XRANGE=[0,24],XSTYLE=1,YRANGE=[0,40],CHARSIZE=1.5,FCOLOR=200

  observed = [good,bad]

  plothist,ra[observed],bin=0.5,/overplot,/fill,FCOLOR=bad_clr
  plothist,ra[good],bin=0.5,/overplot,/fill,FCOLOR=good_clr
;  plothist,ra[bad],bin=0.5,/overplot,/fill,FCOLOR=bad_clr

  IF NOT psflag THEN BEGIN
    makepng,"RAhist.png"
    PRINT,"Press any key to continue"
    key = GET_KBRD(1)
  ENDIF

; PLOT 4: Vr vs. log(M(HI))

  sz=2.0
  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,bits_per_pixel=8,/color,FILENAME='VvsMHI.ps'
    sz=1.0
  ENDIF

  PLOT,dummy,dummy,CHARSIZE=sz,$
       XTITLE="V!dr!n [km/s]",YTITLE="log(M!dHI!n/M"+sunsymbol()+")",$
       XRANGE=[0.0,12000.0],YRANGE=[6.8,11.2],XSTYLE=1,YSTYLE=1
  OPLOT,Vhel[unobs],lMH1[unobs],PSYM=SYM(unobs_sym),COLOR=unobs_clr
;  OPLOT,Vhel[bad],lMH1[bad],PSYM=SYM(bad_sym),COLOR=bad_clr
  OPLOT,Vhel[good],lMH1[good],PSYM=SYM(good_sym),COLOR=good_clr
  OPLOT,Vhel[bad],lMH1[bad],PSYM=SYM(bad_sym),COLOR=bad_clr
; For whatever reason, the postscript screws up if you plot blue before red

  IF NOT psflag THEN makepng,"VvsMHI.png" ELSE DEVICE,/CLOSE

END
