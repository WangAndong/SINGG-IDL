PRO singg_density_plot,xvar,xrange,xbinsize,frac, $
                       xtitle,psflag,bwflag,CHARSZ=charsz,XTICK=xtick, $
                       LABEL=label,NOSTEP=nostep,DEBUG=debug, $
                       COLOR=color,STYLE=style,NOBIN=nobin,OVER=over,NBIN=nbin
; Make the standard SINGG plot, combining a cumulative curve with a histogram
; INPUTS
;   xvar[N]      Variable to be plotted on the X-axis.
;   xrange[2]    [min,max] values for xvar
;   xbinsize     Size of bins to be stored into.
;   frac[N,M]    Fractional contribution for each galaxy to the
;                  various luminosity densities.  
;   xtitle       Text string to be used on the x-axis.
;   psflag       Whether we're in postscript mode.
;   bwflag       Whether we're in black-'n-white mode.
; OPTIONAL INPUTS
;   label        If set, places label on graph (usually '(a)' or similar)
;   /nostep      Don't draw as a step function, just connect dots.
;   /debug       Print out table of information.
;   color[M]     List of colors to use in the plot
;   style[M]     List of styles to use in the plot
;   /nobin       Don't draw the binned histograms, just the cumulative

  sz = SIZE(frac)

  IF N_ELEMENTS(xvar) NE sz[1] THEN BEGIN
    PRINT,"ERROR in singg_density_plot: array size mismatch ", $
          N_ELEMENTS(xvar),sz[1]
    RETURN
  ENDIF

  IF bwflag THEN color_base = [!ddgray,!black,!gray,!dgray,!black,!black] $
            ELSE color_base = [!blue,!red,!dgreen,!orange,!purple,!brown]

  overflag = KEYWORD_SET(over)
  IF NOT KEYWORD_SET(color) THEN color = color_base[0:sz[2]-1]
  IF NOT KEYWORD_SET(style) THEN style = INTARR(sz[2])
  binflag = NOT KEYWORD_SET(nobin)
  stepflag = NOT KEYWORD_SET(nostep)
  dummy = FLTARR(3)

  labelflag = KEYWORD_SET(label)
  IF NOT labelflag THEN label = "()"

; patch
  frac_R = frac[*,0]
  frac_Ha = frac[*,1]
  type = ["R","Ha","N","?","?","?"]
  IF sz[2] EQ 3 THEN frac_N = frac[*,2]

  IF NOT psflag THEN BEGIN
    IF NOT KEYWORD_SET(charsz) THEN charsz = 3.0
    symsz = 1.5
    symsz2 = 0.1 ; change to >1.0 if you want to debug
    thick = 1.0
  ENDIF ELSE BEGIN
    IF NOT KEYWORD_SET(charsz) THEN charsz = 2.0
    symsz = 1.0
    symsz2 = 0.1
    thick = 2.0
  ENDELSE
  spacing = 0.10

  num_bins = ABS(LONG((xrange[1]-xrange[0])/xbinsize + 0.5))
  binval = FINDGEN(num_bins+1)*xbinsize + xrange[0]
  IF num_bins LT 0 THEN BEGIN
    PRINT,"ERROR in singg_density_plot: sign incorrect"
    RETURN
  ENDIF

  yfrac = FLTARR(num_bins,sz[2])

  IF xbinsize GE 0.0 THEN BEGIN
    ind = SORT(xvar)
    valid = WHERE((xvar[ind] GE xrange[0] AND xvar[ind] LE xrange[1]) $
                   AND FINITE(xvar[ind]),numS)
  ENDIF ELSE BEGIN
    ind = SORT(-1.0*xvar)
    valid = WHERE((xvar[ind] LE xrange[0] AND xvar[ind] GE xrange[1]) $
                   AND FINITE(xvar[ind]),numS)
  ENDELSE

  IF numS EQ 0 THEN BEGIN
    PRINT,"ERROR in singg_density_plot: no valid entries within X range"
    PRINT,xrange[0],MIN(xvar),xrange[1],MAX(xvar)
    RETURN
  ENDIF

  IF KEYWORD_SET(debug) THEN BEGIN
    PRINT,"      xvar     total R fraction  total Ha fraction     R frac      Ha frac"
    FOR ii = 0,N_ELEMENTS(xvar)-1 DO BEGIN
      PRINT,xvar[ind[ii]],TOTAL(frac[ind[0:ii],0]),TOTAL(frac[ind[0:ii],1]), $
            frac[ind[ii],0],frac[ind[ii],1]
    ENDFOR
  ENDIF

; FINITE filters out NaN and Inf results
  ylow = FLTARR(sz[2])
  low = WHERE(xvar[ind] LT xrange[0] AND xvar[ind] LT xrange[1] AND $
              FINITE(xvar[ind]),nlow)
  IF nlow GT 0 THEN BEGIN
    FOR ii = 0,sz[2]-1 DO BEGIN
      ylow[ii] = TOTAL(frac[ind[low],ii])
    ENDFOR
  ENDIF

  IF stepflag THEN maxS = numS*2+1 ELSE maxS = numS
  xplot = FLTARR(maxS+2)
  yplot = FLTARR(maxS+2,sz[2])

  xval = [xrange[0],xvar[ind[valid]],xrange[1]]

  yval = FLTARR(numS+2,sz[2])

  fval = FLTARR(numS+2,sz[2])
  fval[0,*] = ylow
  FOR ii = 0,numS-1 DO BEGIN
    FOR jj = 0,sz[2]-1 DO BEGIN
      fval[ii+1,jj] = frac[ind[valid[ii]],jj]
    ENDFOR
  ENDFOR

  FOR ii = 0,numS+1 DO BEGIN
    FOR jj = 0,sz[2]-1 DO BEGIN
      yval[ii,jj] = TOTAL(fval[0:ii,jj])
    ENDFOR
    IF stepflag THEN BEGIN
      xplot[MAX([0,2*ii-1]):2*ii] = xval[ii]
      FOR jj = 0,sz[2]-1 DO BEGIN
        yplot[2*ii:MIN([maxS+1,2*ii+1]),jj] = yval[ii,jj]
      ENDFOR
    ENDIF ELSE BEGIN
      xplot[ii] = xval[ii]
      yplot[ii,0:sz[2]-1] = yval[ii,0:sz[2]-1]
    ENDELSE
    IF ii GT 0 AND ii LT numS+1 THEN BEGIN
      IF xbinsize GE 0.0 THEN junk = MIN(SQRT(xval[ii]-binval),binnum,/NAN) $
                         ELSE junk = MIN(SQRT(binval-xval[ii]),binnum,/NAN)
      FOR jj = 0,sz[2]-1 DO BEGIN
        IF binnum GE 0 AND binnum LT num_bins THEN yfrac[binnum,jj] = yfrac[binnum,jj] + fval[ii,jj]
      ENDFOR
    ENDIF
  ENDFOR

  ymin = 0.0
  ymax = 1.05 ; can be anywhere up to 1.199
  IF NOT overflag THEN BEGIN
; Set up the plot area.
    IF KEYWORD_SET(xtick) THEN BEGIN
      PLOT,dummy,dummy,XRANGE=[xrange[0],xrange[1]],YRANGE=[ymin,ymax], $
           XTICKINTERVAL=xtick, $
           XTITLE="!6"+xtitle,CHARSIZE=charsz,COLOR=!black, $
           YTITLE="!7d!8l!6/!8l!6",XSTYLE=1,YSTYLE=1,THICK=thick, $
           XMARGIN=[6,1.5],YMARGIN=[3.5,0.5], $
           CLIP=[xrange[0],ymin,xrange[1],ymax],NOCLIP=0
    ENDIF ELSE BEGIN
      PLOT,dummy,dummy,XRANGE=[xrange[0],xrange[1]],YRANGE=[ymin,ymax], $
           XTITLE="!6"+xtitle,CHARSIZE=charsz,COLOR=!black, $
           YTITLE="!7d!8l!6/!8l!6",XSTYLE=1,YSTYLE=1,THICK=thick, $
           XMARGIN=[6,1.5],YMARGIN=[3.5,0.5], $
           CLIP=[xrange[0],ymin,xrange[1],ymax],NOCLIP=0
    ENDELSE
  ENDIF

  IF binflag THEN BEGIN
    FOR ii = 0,num_bins-1 DO BEGIN
      POLYFILL,[binval[ii],binval[ii],binval[ii+1],binval[ii+1]], $
               [ymin,yfrac[ii,0],yfrac[ii,0],ymin],COLOR=!gray
      FOR jj = 0,sz[2]-1 DO BEGIN
        PLOTS,[binval[ii],binval[ii],binval[ii+1],binval[ii+1]], $
              [ymin,yfrac[ii,jj],yfrac[ii,jj],ymin],COLOR=color[jj],THICK=thick
        PLOTS,[binval[ii],binval[ii+1]], $
              [yfrac[ii,jj],yfrac[ii,jj]],COLOR=color[jj],THICK=2.0*thick
        IF jj GT 0 THEN BEGIN
          POLYFILL,[binval[ii],binval[ii],binval[ii+1],binval[ii+1]], $
                   [ymin,yfrac[ii,jj],yfrac[ii,jj],ymin],COLOR=color[jj], $
                   /LINE_FILL,SPACING=spacing,THICK=thick,ORIENTATION=135.0
        ENDIF
      ENDFOR
    ENDFOR
  ENDIF

  index = WHERE((xplot LE xrange[1] AND xplot GE xrange[0]) OR $
                (xplot GE xrange[1] AND xplot LE xrange[0]),count)
  IF count GT 0 THEN BEGIN
    FOR ii = 0,sz[2]-1 DO BEGIN
      PLOTS,xplot[index],yplot[index,ii],COLOR=color[ii],THICK=2*thick,PSYM=-SYM(1), $
            NOCLIP=0,CLIP=[xrange[0],ymin,xrange[1],ymax],SYMSIZE=symsz2,LINESTYLE=style[ii]
    ENDFOR
  ENDIF

  IF KEYWORD_SET(nbin) THEN BEGIN
    index = WHERE((xvar LE xrange[1] AND xvar GE xrange[0]) OR $
                  (xvar GE xrange[1] AND xvar LE xrange[0]),count)
    FOR ii = 0,count-1 DO BEGIN
      FOR jj = 0,sz[2]-1 DO BEGIN
        PLOTS,xvar[index[ii]],frac[index[ii],jj]*nbin[index[ii]],COLOR=color[jj],PSYM=SYM(jj+1),SYMSIZE=symsz/2.0
      ENDFOR
    ENDFOR
  ENDIF

  IF labelflag AND NOT overflag THEN BEGIN
    XYOUTS,(0.1*(xrange[1]-xrange[0])+xrange[0]),(0.8*(ymax-ymin)+ymin), $
;           CHARSIZE=charsz/2.0, $
           CHARSIZE=1.5, $
           STRTRIM(label,2),COLOR=!black, $
           ALIGNMENT=0.5,CHARTHICK=thick
  ENDIF

  perin = FLOAT([10,25,50,75,90])/100.0
  npers = N_ELEMENTS(perin)
;  IF xbinsize LT 0.0 THEN perin = 1.0-perin ; not needed

  perout = FLTARR(npers,sz[2]) - 999.0
  PRINT,label," ",xtitle
  FOR ii = 0,sz[2]-1 DO BEGIN
    FOR jj = 0,npers-1 DO BEGIN
      IF perin[jj] LE MAX(yval[*,ii]) AND perin[jj] GE MIN(yval[*,ii]) THEN $
      perout[jj,ii] = findzero((yval[*,ii]-perin[jj]),xval,/NOEXT)
    ENDFOR
    PRINT,"  "+type[ii]+"  ",perout[*,ii]
  ENDFOR

  RETURN

END
