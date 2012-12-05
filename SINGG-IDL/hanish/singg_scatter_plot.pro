PRO singg_scatter_plot,xvar,yvar,clip,xtitle,ytitle,charsz,symsz,SB=sb, $
                       XMARGIN=xmargin,YMARGIN=ymargin,TITLE=title,THICK=thick

  dummy = FLTARR(3)
  IF NOT KEYWORD_SET(title) THEN title = ''
  IF NOT KEYWORD_SET(thick) THEN thick = 1.0

  xmin = clip[0]
  xmax = clip[2]
  ymin = clip[1]
  ymax = clip[3]

  IF KEYWORD_SET(xmargin) AND KEYWORD_SET(ymargin) THEN BEGIN
    PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],TITLE=title, $
         COLOR=!black,XTITLE=xtitle,YTITLE=ytitle,CHARSIZE=charsz, $
         CLIP=clip,THICK=thick,XSTYLE=1,YSTYLE=1,NOCLIP=0, $
         XMARGIN=xmargin,YMARGIN=ymargin
  ENDIF ELSE BEGIN
    PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],TITLE=title, $
         COLOR=!black,XTITLE=xtitle,YTITLE=ytitle,CHARSIZE=charsz, $
         CLIP=clip,THICK=thick,XSTYLE=1,YSTYLE=1,NOCLIP=0
  ENDELSE

  IF KEYWORD_SET(sb) THEN BEGIN
    index = WHERE(yvar GT ymin AND yvar LT ymax AND NOT sb,count)
    IF count GT 0 THEN BEGIN
      PLOTS,xvar[index],yvar[index],THICK=thick,NOCLIP=0,PSYM=SYM(1), $
            COLOR=!ddgray,CLIP=clip,SYMSIZE=symsz
      PLOTS,xvar[index],yvar[index],THICK=thick,NOCLIP=0,PSYM=SYM(1), $
            COLOR=!dmagenta,CLIP=clip,SYMSIZE=symsz*0.75
    ENDIF

    indexSB = WHERE(yvar GT ymin AND yvar LT ymax AND sb,countSB)
    IF countSB GT 0 THEN BEGIN
      PLOTS,xvar[indexSB],yvar[indexSB],THICK=thick,NOCLIP=0,PSYM=SYM(1), $
            COLOR=!ddgray,CLIP=clip,SYMSIZE=symsz
      PLOTS,xvar[indexSB],yvar[indexSB],THICK=thick,NOCLIP=0,PSYM=SYM(1), $
            COLOR=!dmagenta,CLIP=clip,SYMSIZE=symsz*0.75
    ENDIF
  ENDIF ELSE BEGIN
    PLOTS,xvar,yvar,THICK=thick,NOCLIP=0,PSYM=SYM(1),COLOR=!ddgray, $
          CLIP=clip,SYMSIZE=symsz
    PLOTS,xvar,yvar,THICK=thick,NOCLIP=0,PSYM=SYM(1),COLOR=!dmagenta, $
          CLIP=clip,SYMSIZE=symsz*0.75
  ENDELSE

  delY = MEAN(xvar-yvar) ; Only if they're logarithmic
  slope = LADFIT(xvar,yvar)
  sig = STDDEV(yvar - (slope[1]*xvar + slope[0]))
  PRINT,xtitle+' ',ytitle
  PRINT,'  Mean of y/x = ',delY,'; y = ',slope[1],'x + ',slope[0],' +/- ',sig
  PLOTS,[xmin,xmax],[xmin*slope[1]+slope[0],xmax*slope[1]+slope[0]], $
        LINESTYLE=2,COLOR=!black,CLIP=clip,NOCLIP=0
  PLOTS,[xmin,xmax],[xmin*slope[1]+slope[0]+sig, $
                     xmax*slope[1]+slope[0]+sig], $
        LINESTYLE=1,COLOR=!black,CLIP=clip,NOCLIP=0
  PLOTS,[xmin,xmax],[xmin*slope[1]+slope[0]-sig, $
                     xmax*slope[1]+slope[0]-sig], $
        LINESTYLE=1,COLOR=!black,CLIP=clip,NOCLIP=0

  RETURN
END
