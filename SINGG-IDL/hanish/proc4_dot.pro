PRO proc4_dot,Rid,Timg,dw,masklist,Omask,Oimg,mode, $
              dotclr,ellcolor,dotspace,dotsize,dotsym,normal,clip
; Subroutine of proc4.pro, draws the highlighting on the TV plot.

  sz = [2,dw[2]-dw[0]+1,dw[3]-dw[1]+1]
  FOR xx = 0,FIX(sz[1]/dotspace)-1 DO BEGIN
    FOR yy = 0,FIX(sz[2]/dotspace)-1 DO BEGIN
      xpos = xx*dotspace + FIX(dotspace/2.0)
      ypos = yy*dotspace + FIX(dotspace/2.0)
      IF mode LE 1 THEN BEGIN
        index = WHERE(Rid EQ Timg[xpos+dw[0],ypos+dw[1]],count)
        IF count GT 0 THEN BEGIN
          plotclr = dotclr[masklist[index[0]]+1]
          PLOTS,FLOAT(xpos)/normal[0],FLOAT(ypos)/normal[1], $
                COLOR=plotclr,PSYM=SYM(dotsym),SYMSIZE=dotsize,/NORMAL, $
                NOCLIP=0,CLIP=clip
        ENDIF
      ENDIF ELSE BEGIN
        IF Oimg[xpos+dw[0],ypos+dw[1]] NE 0 THEN BEGIN
          Omval = Omask[xpos+dw[0],ypos+dw[1]]
          IF Omval GT 0 THEN BEGIN
            PLOTS,FLOAT(xpos)/normal[0],FLOAT(ypos)/normal[1], $
                  COLOR=ellcolor[Omval-1],PSYM=SYM(dotsym),SYMSIZE=dotsize,/NORMAL, $
                  NOCLIP=0,CLIP=clip
          ENDIF ELSE BEGIN
            PLOTS,FLOAT(xpos)/normal[0],FLOAT(ypos)/normal[1], $
                  COLOR=dotclr[3],PSYM=SYM(dotsym),SYMSIZE=dotsize,/NORMAL, $
                  NOCLIP=0,CLIP=clip
          ENDELSE
        ENDIF
      ENDELSE
    ENDFOR
  ENDFOR

; Refresh the lines on the edge of the plot area, in case our symbols
; leaked a bit.
  PLOTS,clip[2]*[1.0,1.0],[clip[1],clip[3]],COLOR=!black,/NORMAL
  PLOTS,[clip[0],clip[2]],clip[3]*[1.0,1.0],COLOR=!black,/NORMAL

  RETURN

END
