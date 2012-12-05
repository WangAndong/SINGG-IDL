PRO setplotcolors, basetable, list=list, test=test, gray=gray,$
                   colors=colors, low=low, redistribute=redistribute

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
; redistribute - redistribute the n_colors - (2 + n_elements(c_name))
;                levels that are not named colors over the full range of
;                the original color table.
;
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
;
; Mark Seibert (July,2000)
;
; Now supports 24 bit true color (MS 3/01)
;
; Added grayscale, low & colors keyword options (MS 6/01)
;
; put setplotcolors_rgb.dat in the same directory as this file
findpro,'setplotcolors',DIRLIST=Dirlist,/noprint
fname = Dirlist[0] + 'setplotcolors_rgb.dat'

;read color names & RGB values (0-255)
readcol,fname,c_R,c_G,c_B,c_name,format='I,I,I,A',/silent

IF keyword_set(gray) THEN BEGIN
 c_R = indgen(gray+2)
 c_name=strarr(gray+2)
 c_R[0]=255
 c_R[1]=0
 c_name[0]='white'
 c_name[1]='black'
 FOR i=2,n_elements(c_R)-1 DO BEGIN
    c_R[i]=200 - fix(200./gray)*(i-2)
    c_name[i]='gray'+strn(i-1)
 endfor  
 c_G = c_R
 c_B = c_R
ENDIF ELSE begin
 readcol,fname,c_R,c_G,c_B,c_name,format='I,I,I,A',/silent
ENDELSE

IF keyword_set(colors) THEN colors=lonarr(n_elements(c_name))

;load color table & get RGB values
table = 0 ;default B&W linear
IF n_params() GT 0 THEN table = basetable
loadct,table,/silent
TVLCT, R, G, B, /get

n_colors = !d.n_colors < 256
IF keyword_set(low) THEN n_colors = n_elements(c_R)+2

;
; If redistribute keyword is set, redistribute the (low)
; n_colors-(2+n_elements(c_name)) left in the color table to 
; roughly evenly spaced values over the full range of the 
; original table.  Actually nearest neighbor interpolation is 
; so that only the same colors in the input table are used.
;  (G. Meurer)
;
IF keyword_set(redistribute) THEN BEGIN 
   rold  =  R
   gold  =  G
   bold  =  B
   n_new = n_colors - (1 + n_elements(c_name))
   ii    = indgen(n_new)
   jj    = fix(0.5 + float(n_colors - 2)*float(ii)/float(n_new-1))
   r[ii] = rold[jj]
   g[ii] = gold[jj]
   b[ii] = bold[jj]
ENDIF 


;setup the colors & system variables specified above
FOR ii = 0, n_elements(c_name)-1 DO begin
   cvar = '!'+c_name[ii]
; defsysv,cvar,n_colors-(2+i)  
   R[n_colors-(2+ii)]=c_R[ii] 
   G[n_colors-(2+ii)]=c_G[ii]
   B[n_colors-(2+ii)]=c_B[ii]

   IF !d.n_colors LE 256 THEN BEGIN
      ; 8 bit mode
      defsysv,cvar,n_colors-(2+ii) 
      IF keyword_set(colors) THEN colors[ii]=n_colors-(2+ii)
   ENDIF ELSE BEGIN 
      ; 24 bit mode
      cval=n_colors-(2+ii) + 256L * (n_colors-(2+ii) + 256L * n_colors-(2+ii))
      defsysv,cvar,cval 
      device,decomposed=0
      IF keyword_set(colors) THEN colors[ii]=cval
   endelse

ENDFOR
;modify the color table
TVLCT, R, G, B

IF keyword_set(list) THEN BEGIN
print
print,'The following colors may be accesed via system variables:'
print
print,c_name
print
print,'!colorname = colortable index for colorname'
print,'l = light; d = dark'
print,'Ex: plot,x,y,color=!red'
print
endif

IF keyword_set(test) THEN BEGIN
window,6,xsize=100.*ceil(n_elements(c_name)/10.),ysize=170,$
       title='Plot Colors Added'
FOR ii = 0,n_elements(c_name)-1 DO BEGIN
 x = 10 + 10*(ii - (ii MOD 10))
 y = 10+((ii MOD 10)*15.)
 IF !d.n_colors LE 256 THEN BEGIN 
  xyouts,x,y,c_name[ii],color=n_colors-(2+ii),$
        charsize=1.25,charthick=1.25,/device
  ; print,c_name[ii],n_colors-(2+i)
 ENDIF ELSE BEGIN
  cval=n_colors-(2+ii) + 256L * (n_colors-(2+ii) + 256L * n_colors-(2+ii))  
  xyouts,x,y,c_name[ii],color=cval,$
        charsize=1.25,charthick=1.25,/device
  ; print,c_name[ii],cval
 ENDELSE

ENDFOR
ENDIF
END


