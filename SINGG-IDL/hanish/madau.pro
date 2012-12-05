PRO madau,SMALL=small,PS=ps,HOR=hor,DOT=dot,BW=bw,LOG=log,VERT=vert,SUNGG=sungg,NOSINGG=nosingg
; Makes a Madau plot (star formation density vs. redshift)
; OPTIONAL INPUTS
;  /small      Only plots from z=0 to z=2
;  /ps         Postscript output
;  /hor        Add horizontal error bars
;  /dot        Replace plotting symbology with colored dots
;  /bw         Plot in black-and-white
;  /log        Replace "z" on x-axis with "log(1+z)"
;  /vert       Plot two plots top-and-bottom instead of side to side
;  /sungg      Include the SUNGG results
;  /nosingg    DON'T include the SINGG data point.

; paper: small, bw, ps, log
; poster: small, ps, dot, sungg

  psflag = KEYWORD_SET(ps)
  horflag = KEYWORD_SET(hor)
  dotflag = KEYWORD_SET(dot)
  colorflag = NOT KEYWORD_SET(bw)
  logflag = KEYWORD_SET(log)
  vertflag = KEYWORD_SET(vert)
  smallflag = KEYWORD_SET(small)
  sunggflag = KEYWORD_SET(sungg)
  singgflag = 1b-KEYWORD_SET(nosingg)

  IF dotflag AND NOT colorflag THEN BEGIN
    PRINT,"ERROR in madau: can't combine /dot with /bw"
    RETURN
  ENDIF

  IF psflag THEN BEGIN
    IF dotflag THEN outfile = "madau2" $
               ELSE outfile = "madau"
    IF NOT smallflag THEN outfile = outfile+'_full'
    IF NOT singgflag THEN outfile = outfile+'_nosingg'
    outfile = outfile+'.eps'
  ENDIF

; z is redshift
  al10 = ALOG(10.d0)

  IF logflag THEN BEGIN
    xmin = -0.01
    IF smallflag THEN xmax = 0.5 ELSE xmax = 0.8
  ENDIF ELSE BEGIN
    xmin = -0.1
    IF smallflag THEN xmax = 2.1 ELSE xmax = 6.0
  ENDELSE

  datafile = !singgdir+"/madau.txt"
  readcol_new,datafile,z,delz,lsfd,errplus,errminus,type,dustcorr,source, $
              COMMENT="#",FORMAT="F,F,F,F,F,I,F,A",/SILENT
; lsfd will be the dust-corrected value.

  n_surveys = N_ELEMENTS(z)

  error = FLTARR(2,n_surveys)
  FOR ii = 0,n_surveys-1 DO BEGIN
    error[0,ii] = errminus[ii]
    error[1,ii] = errplus[ii]
  ENDFOR
  IF NOT horflag THEN delz = delz*0.d0

;  haindex = WHERE(type EQ 0,numha)
;  uvindex = WHERE(type EQ 1,numuv)
;  irindex = WHERE(type EQ 2,numir)
;  myindex = WHERE(type EQ 3,nummy)
;  ivyindex = WHERE(type EQ 4,numivy)

; patch dustcorr; if a dust correction isn't given, assume 0.4 dex.
  hanodust = WHERE(type EQ 0 AND dustcorr LT 0.01,count)
  IF count GT 0 THEN dustcorr[hanodust] = 0.4 ;;
  uvnodust = WHERE(type EQ 1 AND dustcorr LT 0.01,count)
  IF count GT 0 THEN dustcorr[uvnodust] = 0.4 ;;

  lsfd_nodust = lsfd - dustcorr

  dummy = FLTARR(2)

; y is log(SFRD)
  ymin = -2.5
  ymax = -0.0

  IF NOT psflag THEN BEGIN
    set_plot,'X'
    setplotcolors
    setbgfg,!white,!black
    DEVICE, RETAIN=2, DECOMPOSED=0
    IF vertflag THEN BEGIN
      WINDOW,XSIZE=600,YSIZE=1100
      !P.MULTI=[0,1,2,0,0]
    ENDIF ELSE BEGIN
      WINDOW,XSIZE=1200,YSIZE=600
      !P.MULTI=[0,2,1,0,0]
    ENDELSE

    charsz = 2.0
    thick = 1.0
    width = 0.0
    symsz = 1.5

    title1="Star formation history, without dust correction"
    title2="Star formation history, with dust correction"
  ENDIF ELSE BEGIN
    set_plot,'PS'
    setplotcolors
    IF vertflag THEN BEGIN
      xs = 5.5
      ys = 7.0
      !P.MULTI=[0,1,2,0,0]
    ENDIF ELSE BEGIN
      xs = 10.5
      ys = 4.5
      !P.MULTI=[0,2,1,0,0]
    ENDELSE
    xoff = 1.2
    yoff = 3.0
    IF STRPOS(strlowcase(outfile), '.eps') GT 0 THEN $
       DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
              bits_per_pixel=8,/encapsulated ELSE $
       DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
              bits_per_pixel=8

    charsz = 1.5
    thick = 2.0
    width = 0.0
    symsz = 1.0

    title1=""
    title2=""
  ENDELSE

; All arrays are [visible, UV, IR, SINGG, SUNGG]
  IF colorflag THEN color = [!dgreen,!purple,!orange,!green,!lpurple] $
               ELSE color = [!black,!black,!black,!black,!black]
  fg_clr = !black

  IF dotflag THEN BEGIN
    syms = [1,1,1,23,4] ; circle, circle, circle, star, diamond
    outclr = [!ddgray,!ddgray,!ddgray,!black,!black]
    inclr = color
    scale = [1.0,1.0,1.0,2.0,1.5]
  ENDIF ELSE BEGIN
    syms = [1,6,13,23,4] ; circle, hollow, asterisk, star, diamond
    outclr = color
    inclr = [-1,-1,-1,!gray,!gray]
    scale = [1.0,1.0,1.0,2.0,1.5]
  ENDELSE

  IF logflag THEN BEGIN
    xarr = ALOG10(1.0+z)
    delxarr = (delz/al10) / (1.0+z)
    xtitle = "!6log(1+z)"
  ENDIF ELSE BEGIN
    xarr = z
    delxarr = delz
    xtitle = "!6z" ; !6Redshift
  ENDELSE

; Without dust correction
  PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],CHARSIZE=charsz, $
       COLOR=fg_clr,XSTYLE=1,YSTYLE=1,XTITLE=xtitle,CHARTHICK=thick, $
       CLIP=[xmin,ymin,xmax,ymax],NOCLIP=0, $
       YTITLE="!6log(!7q!6'!DSFR!N [h!D70!N M!Dsolar!N yr!E-1!N Mpc!E-3!N])", $
       TITLE=title1,XMARGIN=[7.5,1.5],YMARGIN=[3.2,1]
  XYOUTS,(0.1*(xmax-xmin)+xmin),(0.88*(ymax-ymin)+ymin), $
         STRTRIM('(a)',2),CHARSIZE=charsz,COLOR=fg_clr, $
         ALIGNMENT=0.5,CHARTHICK=thick

  FOR ii = 0,n_surveys-1 DO BEGIN
    IF type[ii] LE 2 OR (type[ii] EQ 3 AND singgflag) OR $
       (type[ii] EQ 4 AND sunggflag) THEN BEGIN
      ERRPLOT,xarr[ii],(lsfd_nodust[ii]-errminus[ii]), $
           (lsfd_nodust[ii]+errplus[ii]),COLOR=color[type[ii]],WIDTH=width

      PLOTS,xarr[ii],lsfd_nodust[ii],PSYM=SYM(syms[type[ii]]),COLOR=outclr[type[ii]], $
            THICK=thick,SYMSIZE=1.5*symsz*scale[type[ii]],CLIP=[xmin,ymin,xmax,ymax],NOCLIP=0

      IF inclr[type[ii]] GT 0 THEN PLOTS,xarr[ii],lsfd_nodust[ii], $
            PSYM=SYM(syms[type[ii]]),COLOR=inclr[type[ii]],THICK=thick,SYMSIZE=symsz*scale[type[ii]], $
            CLIP=[xmin,ymin,xmax,ymax],NOCLIP=0

      IF NOT psflag AND horflag THEN PLOTS,xarr[ii]+delxarr[ii]*[-1.0,1.0], $
            lsfd_nodust[ii]*[1.0,1.0],COLOR=color[type[ii]], $
            THICK=thick,CLIP=[xmin,ymin,xmax,ymax],NOCLIP=0
    ENDIF
  ENDFOR

; Now, with dust corrections.
  PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],CHARSIZE=charsz, $
       COLOR=fg_clr,XSTYLE=1,YSTYLE=1,XTITLE=xtitle,CHARTHICK=thick, $
       CLIP=[xmin,ymin,xmax,ymax],NOCLIP=0, $
       YTITLE="!6log(!7q!6!DSFR!N [h!D70!N M!Dsolar!N yr!E-1!N Mpc!E-3!N])", $
       TITLE=title2,XMARGIN=[7.5,1.5],YMARGIN=[3.2,1]
  XYOUTS,(0.1*(xmax-xmin)+xmin),(0.88*(ymax-ymin)+ymin), $
         STRTRIM('(b)',2),CHARSIZE=charsz,COLOR=fg_clr, $
         ALIGNMENT=0.5,CHARTHICK=thick

  FOR ii = 0,n_surveys-1 DO BEGIN
    IF type[ii] LE 2 OR (type[ii] EQ 3 AND singgflag) OR $
       (type[ii] EQ 4 AND sunggflag) THEN BEGIN
      ERRPLOT,xarr[ii],(lsfd[ii]-errminus[ii]), $
           (lsfd[ii]+errplus[ii]),COLOR=color[type[ii]],WIDTH=width

      PLOTS,xarr[ii],lsfd[ii],PSYM=SYM(syms[type[ii]]),COLOR=outclr[type[ii]], $
            THICK=thick,SYMSIZE=1.5*symsz*scale[type[ii]],CLIP=[xmin,ymin,xmax,ymax],NOCLIP=0

      IF inclr[type[ii]] GT 0 THEN PLOTS,xarr[ii],lsfd[ii], $
            PSYM=SYM(syms[type[ii]]),COLOR=inclr[type[ii]],THICK=thick,SYMSIZE=symsz*scale[type[ii]], $
            CLIP=[xmin,ymin,xmax,ymax],NOCLIP=0

      IF NOT psflag AND horflag THEN PLOTS,xarr[ii]+delxarr[ii]*[-1.0,1.0], $
            lsfd[ii]*[1.0,1.0],COLOR=color[type[ii]], $
            THICK=thick,CLIP=[xmin,ymin,xmax,ymax],NOCLIP=0
    ENDIF
  ENDFOR

; Now, the fitting part.
  zsize = 0.1
  breakrange = [0.5,1.5]
  num_z = ROUND((breakrange[1]-breakrange[0])/zsize)+1
  zbreak = FINDGEN(num_z)*zsize + breakrange[0]

  slope_z1 = FLTARR(2,num_z)
  slope_z2 = FLTARR(2,num_z)
  err = (errplus + errminus) / 2.0
  PRINT,"z   slope1 sig  z=0  sig  slope2  sig  z=zbreak"

  IF logflag THEN BEGIN
    xbreak = ALOG10(1.0+zbreak)
  ENDIF ELSE BEGIN
    xbreak = zbreak
  ENDELSE

  FOR ii = 0,num_z-1 DO BEGIN
    ind1 = WHERE(z LE zbreak[ii],count)
    ind2 = WHERE(z GT zbreak[ii],count)

    fitexy,xarr[ind1],lsfd[ind1],a,b,x_sig=delxarr[ind1],y_sig=err[ind1],sigab
    slope_z1[*,ii] = [a,b]
    slope_z2[0,ii] = a + b*xbreak[ii]
    slope = poly_fit((xarr[ind2]-xbreak[ii]),((lsfd[ind2]-slope_z2[0,ii])/(xarr[ind2]-xbreak[ii])),0,MEASURE_ERRORS=(err[ind2]/(xarr[ind2]-xbreak[ii])),SIGMA=err2)
    slope_z2[1,ii] = slope[0]

    PRINT,zbreak[ii],slope_z1[1,ii],sigab[1],slope_z1[0,ii],sigab[0], $
                     slope_z2[1,ii],err2[0],slope_z2[0,ii], $
          FORMAT='(F3.1," ",2(F5.3," "),F6.3," ",F5.3," ",3(F6.3," "))'
  ENDFOR

  good = 5 ; which zbreak is our best one
  IF NOT psflag THEN BEGIN
    OPLOT,[xmin,xbreak[good]],[(slope_z1[0,good]+slope_z1[1,good]*xmin),slope_z2[0,good]], $ ,slope_z2[0,good]+slope_z2[1,good]*(xmax-xbreak[good])], $
          COLOR=!gray160,LINESTYLE=2,THICK=3.0
  ENDIF

; Now, do the by-type fits
  ind3 = WHERE(z LE zbreak[good])
  ind4 = ind3[WHERE(type[ind3] EQ 0 OR type[ind3] EQ 3)] ; visible
  fitexy,xarr[ind4],lsfd[ind4],a,b,x_sig=delxarr[ind4],y_sig=err[ind4],sigab
  PRINT,"  visible: slope=",b," +/- ",sigab[1]," zero=",a," +/- ",sigab[0]
  ind4 = ind3[WHERE(type[ind3] EQ 1)] ; UV
  fitexy,xarr[ind4],lsfd[ind4],a,b,x_sig=delxarr[ind4],y_sig=err[ind4],sigab
  PRINT,"  UV: slope=",b," zero=",a
  ind4 = ind3[WHERE(type[ind3] EQ 2)] ; IR
  fitexy,xarr[ind4],lsfd[ind4],a,b,x_sig=delxarr[ind4],y_sig=err[ind4],sigab
  PRINT,"  IR: slope=",b," zero=",a

  IF NOT psflag THEN BEGIN
    OPLOT,[xbreak[good],xmax],slope_z2[0,good]+[0.0,slope_z2[1,good]*(xmax-xbreak[good])], $
          COLOR=!gray160,LINESTYLE=2,THICK=3.0
    FOR ii = 0,num_z-1 DO BEGIN
      OPLOT,[xmin,xbreak[ii],xmax],[(slope_z1[0,ii]+slope_z1[1,ii]*xmin),slope_z2[0,ii],slope_z2[0,ii]+slope_z2[1,ii]*(xmax-xbreak[ii])], $
            COLOR=!red,LINESTYLE=1
    ENDFOR

;    zlow = FINDGEN(100*LONG(1.0)+1)/100.0
;    yplot = slope_test[0] + slope_test[1]*ALOG10(1.0+zlow)
;    OPLOT,zlow,yplot,COLOR=!blue,LINESTYLE=2
;; now add high fit

  ENDIF

  IF psflag THEN BEGIN
    !p.multi   = 0
    !p.noerase = 0
    psend,!outdir+outfile,/noprint,/clobber
    PRINT,"Postscript file written to ",!outdir+outfile
  ENDIF

END
