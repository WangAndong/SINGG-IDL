PRO setplotcolors, basetable, list=list, test=test, gray=gray, $
                   colors=colors, low=low, redistribute=redistribute, $
                   RC=rc, GC=gc, BC=bc

;syntax: setplotcolors [,basetable, /list, /test, gray=, colors=, /low]
;
;Parameters:
; basetable = colortable to load and modify (default=0)
; 
;Keywords:
; list   - print list of color names available
; test   - plot colors available in a window
; gray   - number of grayscale colors to establish
; colors - name of long array to store index values of colors
; low    - put colors at low end of color table (24 bit contour plots)
;          do not use low of using 256 or less colors.
; redistribute - redistribute the n_colors - (2 + num_colors)
;                levels that are not named colors over the full range of
;                the original color table.
; OPTIONAL OUTPUTS:
;   r,g,b       Color tables to be modified if you want.
;Description:
; Loads a base color table (linear B&W color table (0) is default)
; and then modifies the color table starting with !d.n_colors-2
; (unless low keyword is set) from the values specified in the datafile
; setplotcolors_rgb.dat. To add or remove colors modify data table.
;
; Sets system variables to the index of each color specified.
; i.e. !red = color index for red etc. If grayscale option is selected
; the color names are gray1, gray2 etc. (use test or list to view names)
;
;Example:
;  setplotcolors <- setup the colors from setplotcolors_rgb.dat
;  setplotcolors, /test <- setup the colors and plots them
;
; once setplotcolors has been run at least once try:
;  setbgfg, !gray  <- set background color to gray
;  setplotcolors, /test <- plots colors on gray background
;
; set up 10 shades of gray for a contour plot:
;  pcolors=lonarr(10)
;  setplotcolors,gray=10,colors=pcolors,/low
;  (contour does not like high end of color table in 24 bit mode)
;  contour,...,c_colors=pcolors  
;
; to remove color table modifications reload a color table:
;  loadct,0
;
; Mark Seibert (July,2000)
;
; Now supports 24 bit true color (MS 3/01)
;
; Added grayscale, low & colors keyword options (MS 6/01)

;read color names & RGB values (0-255)
  readcol_new,'/home/meurer/IDL/Pro/Misc/setplotcolors_rgb.dat',$
              c_R,c_G,c_B,c_name,FORMAT='I,I,I,A',/SILENT
  num_colors = N_ELEMENTS(c_name)

  n_colors = !d.n_colors < 256
  n_grays = n_colors - num_colors
  IF KEYWORD_SET(gray) THEN BEGIN
    IF gray GT 0 THEN n_grays = gray
  ENDIF

; Old method counted down from 200 to (200/n_grays).
; New way counts from 250 down to 5.
;  g_val = 200 - FIX(FINDGEN(n_grays)/FLOAT(n_grays) * 200.0)
  g_val = 250 - FIX(FINDGEN(n_grays)/FLOAT(n_grays-1) * 245.0)
; Index in reverse order.
  c_R = [c_R,g_val]
  c_G = [c_G,g_val]
  c_B = [c_B,g_val]

  g_name = 'gray'+STRTRIM(STRING(INDGEN(n_grays)+1),2)
  c_name = [c_name,g_name]
  num_colors = num_colors + n_grays

  IF KEYWORD_SET(colors) THEN colors=LONARR(num_colors)

;load color table & get RGB values
  table = 0 ; default B&W linear
  IF N_PARAMS() GT 0 THEN table = basetable
  LOADCT,table,/SILENT
  TVLCT, rc, gc, bc, /GET

  IF KEYWORD_SET(low) THEN n_colors = num_colors+2

; If redistribute keyword is set, redistribute the (low)
; n_colors-(2+num_colors) left in the color table to 
; roughly evenly spaced values over the full range of the 
; original table.  Actually nearest neighbor interpolation is 
; so that only the same colors in the input table are used.
;  (G. Meurer)
;
  IF KEYWORD_SET(redistribute) THEN BEGIN 
    rold  =  rc
    gold  =  gc
    bold  =  bc
    n_new = n_colors - (1 + num_colors)
    ii    = INDGEN(n_new)
    jj    = FIX(0.5 + FLOAT(n_colors - 2)*FLOAT(ii)/FLOAT(n_new-1))
    rc[ii] = rold[jj]
    gc[ii] = gold[jj]
    bc[ii] = bold[jj]
  ENDIF 

; Setup the colors & system variables specified above

; First, check to see if the system variables have already been set.
  defsysv,'!ngrays',EXISTS=syscheck
  IF syscheck THEN junk=EXECUTE('!ngrays = '+STRING(n_grays)) $
              ELSE defsysv,'!ngrays',n_grays
  FOR ii = 0,num_colors-1 DO BEGIN
    cvar = '!'+c_name[ii]
;    cval = LONG(n_colors-(2+ii)) ; Count from 254 down.
    cval = LONG(n_colors-(1+ii)) ; Count from 255 down.

    rc[cval]=c_R[ii] 
    gc[cval]=c_G[ii]
    bc[cval]=c_B[ii]

    IF !d.n_colors GT 256 THEN BEGIN
; Our normal method works fine for 8-bit color.  For 24-bit, we have
; to alter cval at this point.
      cval = cval + 256L * (cval + 256L * cval)
      DEVICE,DECOMPOSED=0
    ENDIF
    IF KEYWORD_SET(colors) THEN colors[ii]=cval
    defsysv,cvar,EXISTS=syscheck
    IF syscheck THEN junk=EXECUTE(STRTRIM(cvar,2)+' = '+STRING(cval)) $
                ELSE defsysv,cvar,cval
  ENDFOR
;modify the color table
  TVLCT, rc, gc, bc

  IF KEYWORD_SET(list) THEN BEGIN
    PRINT
    PRINT,'The following colors may be accesed via system variables:'
    PRINT
    PRINT,c_name
    PRINT
    PRINT,'!colorname = colortable index for colorname'
    PRINT,'l = light; d = dark'
    PRINT,'Ex: plot,x,y,color=!red'
    PRINT
  ENDIF

  IF KEYWORD_SET(test) THEN BEGIN
  WINDOW,6,XSIZE=100.*CEIL(num_colors/20.),YSIZE=310,$
         TITLE='Plot Colors Added'
    FOR ii = 0,num_colors-1 DO BEGIN
      x = 10 + 5*(ii - (ii MOD 20))
      y = 10+((ii MOD 20)*15.)
      IF !d.n_colors LE 256 THEN BEGIN 
        XYOUTS,x,y,c_name[ii],COLOR=n_colors-(1+ii),$
               CHARSIZE=1.25,CHARTHICK=1.25,/DEVICE
      ENDIF ELSE BEGIN
        cval=n_colors-(1+ii) + 256L * (n_colors-(1+ii) + 256L * n_colors-(1+ii))  
        XYOUTS,x,y,c_name[ii],COLOR=cval,$
               CHARSIZE=1.25,CHARTHICK=1.25,/DEVICE
      ENDELSE
    ENDFOR
  ENDIF

  RETURN

END


