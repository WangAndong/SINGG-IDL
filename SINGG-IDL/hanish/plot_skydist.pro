PRO plot_skydist,Zbox,Zbin,Skybox,Skybin,Sky,bwflag,Zmin,Zmax,$
                 Zscale,Skyscale,Zguess,Title,MASK=mask

; Zbox,Skybox[0,*] is the raw data, Zbin,Skybin is the binned data

  Skymin = Sky[0] - (2.0*Sky[1])
  Skymax = Sky[0] + (4.0*Sky[1])

  xmax = Zmax*Zscale
  IF MAX(Zbox) LT Zmax THEN xmax = MAX(Zbox)*Zscale

  str1 = Title+"("+STRTRIM(STRING(Zguess*Zscale),2)+" arcsec)"
  str2 = "Sky level = "+STRTRIM(STRING(Sky[0]*Skyscale),2)+" +/- "+$
         STRTRIM(STRING(Sky[1]*Skyscale),2)+" counts/sec"

  dummy=FLTARR(2)
  IF bwflag THEN BEGIN
    bin_clr = !black
    box_clr = !gray
    good_clr = !ddgray
    guess_clr = !ddgray
    err_clr = !dgray
  ENDIF ELSE BEGIN
    bin_clr = !blue
    box_clr = !dblue
    good_clr = !dgreen
    guess_clr = !dpink
    err_clr = !dmagenta
  ENDELSE

  thick = 1.0
  charsz = 2.0
;  thick = 2.0
;  charsz = 3.0

  PLOT,dummy,dummy,COLOR=1,CHARSIZE=charsz,THICK=thick,CHARTHICK=thick, $
        XRANGE=[Zmin*Zscale,xmax],XSTYLE=9,YSTYLE=9,NOCLIP=0,$
        YRANGE=[Skymin*Skyscale,Skymax*Skyscale],$
        XTITLE="Semi-major axis [arcsec]",YTITLE="Sky level [counts/sec]",$
        TITLE=str1,SUBTITLE=str2
  clip = [Zmin*Zscale,Skymin*Skyscale,xmax,Skymax*Skyscale]

  OPLOT,(Zbox*Zscale),(Skybox[0,*]*Skyscale),PSYM=SYM(4),$
        SYMSIZE=1.0,COLOR=box_clr,CLIP=clip
  IF KEYWORD_SET(mask) THEN BEGIN
    ind = WHERE(mask[Skybox[1,*],Skybox[2,*]] EQ 0b,count)
    OPLOT,(Zbox[ind]*Zscale),(Skybox[0,ind]*Skyscale),PSYM=SYM(4),$
          SYMSIZE=1.0,COLOR=good_clr,CLIP=clip
  ENDIF

  OPLOT,(Zbin*Zscale),(Skybin*Skyscale),PSYM=SYM(1),$
        SYMSIZE=2.0,COLOR=bin_clr,CLIP=clip

  IF Zguess GT 0.0 THEN BEGIN
    PLOTS,(Zguess*Zscale)*[1.0,1.0],[Skymin,Skymax]*Skyscale,COLOR=guess_clr
    PLOTS,(Zguess*Zscale)*SQRT(2.0)*[1.0,1.0],[Skymin,Skymax]*Skyscale, $
          COLOR=guess_clr,LINESTYLE=1
    PLOTS,[Zmin*Zscale,xmax],(Sky[0]*Skyscale)*[1.0,1.0],COLOR=guess_clr
    PLOTS,[Zmin*Zscale,xmax],(Sky[0]+Sky[1])*Skyscale*[1.0,1.0], $
          COLOR=err_clr,LINESTYLE=2
    PLOTS,[Zmin*Zscale,xmax],(Sky[0]-Sky[1])*Skyscale*[1.0,1.0], $
          COLOR=err_clr,LINESTYLE=2
  ENDIF

END
