PRO proc4_text,text,textcolor,charsz,xmax,ymax,textclip,CENTER=center

  COMMON proc4text,textarr,n_tlines

  IF STRLEN(text) GT 150 THEN BEGIN
    PRINT,'ERROR in proc4_text: string too long ",STRLEN(text)
; Instead of returning here, let it deliver its own message!
    text = 'ERROR: string too long '+STRTRIM(STRING(STRLEN(text)),2)+' '+STRMID(text,0,100)
    textcolor = !orange
  ENDIF

  textsize = 0.008*charsz
;    textclip = [0.0,ysep,xsep,ysep+(1.0-ysep)/3.0]
;  text_x = (textclip[0]+textclip[2])/2.0  ; xsep/2
;  text_y = (textclip[1]+textclip[3])/2.0 - textsize/2.0  ; ysep + (1-ysep)*(n/(n-2)) -t/2

; textarea = [xsize-2,(bsize/2)-2]
  textarea = [ROUND(FLOAT(xmax)*(textclip[2]-textclip[0]))-2, $
              ROUND(FLOAT(ymax)*(textclip[3]-textclip[1]))-2]

  textclear = INTARR(textarea[0],textarea[1]) + !white

  PRINT,text
  tv,textclear,ROUND(FLOAT(xmax)*textclip[0])+1, $
               ROUND(FLOAT(ymax)*textclip[1])+1

  IF KEYWORD_SET(center) THEN BEGIN
; Centered
    text_x = (textclip[0]+textclip[2])/2.0
    align = 0.5
  ENDIF ELSE BEGIN
; Left-justified is the default
    text_x = textclip[0]+textsize/4.0
    align = 0
  ENDELSE

  IF n_tlines GT 1 THEN textarr[1:n_tlines-1] = textarr[0:n_tlines-2]
  textarr[0] = text

; This'll cause the text to scroll UP (the bottom line is the recent
; one).  If you want it the other way, replace the first part with
; 'textclip[3] - ' instead of 'textclip[1] + '
  text_y = textclip[1] + (textclip[3]-textclip[1])*(0.5+FINDGEN(n_tlines))/FLOAT(n_tlines) - textsize/2.0

  FOR ii = 0,n_tlines-1 DO BEGIN
    IF ii EQ 0 THEN textclr = textcolor ELSE textclr = !ddgray
    XYOUTS,text_x,text_y[ii],textarr[ii],COLOR=textclr,CHARSIZE=charsz, $
           CHARTHICK=1.0,ALIGNMENT=align,CLIP=textclip,NOCLIP=0,/NORMAL
  ENDFOR

  RETURN
END
