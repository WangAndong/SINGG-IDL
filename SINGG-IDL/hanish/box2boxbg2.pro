PRO box2boxbg2, img,hdr, bxw, Nsigma=Nsigma, mask=mask, use_sky=use_sky, $
               startguess=startguess, outfile=outfile, nodisplay=nodisplay,$
               results=results, boxinfo=boxinfo, num_boxes=num_boxes, $
               silent=silent,reject_frac=reject_frac

; box2box background levels
; Needs Astrolib
; requires stv.pro for display
;
; file      - fits file name
; bxw       - box width in pixels
; Nsigma    - exclude boxes with mean >(<) image mean+(-)Nsigma*rms  
; mask      - 2D binary array same size as image. Pixels = 1b are exclude
; use_sky   - use astrolib sky routine to get image mean and rms
;             as opposed to just taking the image mean and rms.
;             This is ussually a better option.
; startguess- optional array that provides a starting value for the
;             image [sky,sigma] instead of taking the image mean and rms 
;             or using the Astrolib sky routine. The use_sky keyword
;             will be used for box and box2box means if specified.
;
; outfile   - string of named outputfile
; nodisplay - do not display image and boxes
;
; results   - named array to contain output [sky,sigma,sdom]
; boxinfo   - named array to contain output boxmean, x, and y in 3xn array
; num_boxes - total number of boxes in boxinfo
; silent    - set keyword to prevent all printing to screen
; reject_frac - rejection fraction; only throw out a box if more than this 
;               fraction of pixels are masked.
;
; Expects fits file to have a bunit keyword to be used in output
;
; M. Seibert 11/01
;
;example of self iteration: 
;
; box2boxbg,'name.fits', 50, Nsigma=3.,/use_sky,$
;           results=results,mask=mask
; print,result
; diff=1
;
; WHILE diff GT .01 DO BEGIN
;  oldresults=results
;  box2boxbg,'name.fits', 50, Nsigma=2.,mask=mask,$
;            results=results,startguess=[results[0],results[1]],/use_sky
;  diff=abs((oldresults[0]-results[0])/oldresults[0])
; ENDWHILE
;

IF n_params() ne 3 THEN BEGIN
 print,"USAGE: box2boxbg, file, bxw [, Nsigma=Nsigma, $"
 print,"        mask=mask, use_sky=use_sky, outfile=outfile"
 print,"        startguess=startguess,results=results, silent=silent]"
 return
ENDIF

IF NOT KEYWORD_SET(reject_frac) THEN reject_frac = 0.0

;---------------------------------------------------------------
;display image -- will scale image from -3 to +7 sigma
IF NOT keyword_set(nodisplay) THEN stv,img,/auto 

IF NOT keyword_set(silent) THEN print,'working...'

;---------------------------------------------------------------
;define boxes
s=size(img)
sx=s[1]
sy=s[2]

num_x =fix(sx/(bxw+2))
num_y =fix(sy/(bxw+2))

xc=intarr(num_x)
yc=intarr(num_y)

FOR i = 0,num_x-1 DO xc[i]=1+i*(bxw+2)
FOR i = 0,num_y-1 DO yc[i]=1+i*(bxw+2)

xcorner=intarr(n_elements(xc)*n_elements(yc))
ycorner=intarr(n_elements(xc)*n_elements(yc))

FOR j=0,num_y-1 DO begin
FOR k=j*(num_x),j*(num_x)+(num_x-1) DO begin
   xcorner[k]=xc[k-j*(num_x)]
   ycorner[k]=yc[j]
 endfor
ENDFOR

check = where(xcorner,count)
IF count EQ 0 THEN BEGIN
 print,"ERROR: No boxes defined." 
 results=[0,0]
 return
endif

n_boxes=n_elements(xcorner)

;---------------------------------------------------------------
;find region of interest indicies
roi=lonarr(n_boxes,bxw^2) ; array of 1-dimensional indicies for each box
                          ; ex: roi[box#,values]

for i=0,n_boxes-1 do begin
 roi[i,*]=polyfillv([xcorner[i],xcorner[i]+bxw,xcorner[i]+bxw,xcorner[i]],$
                    [ycorner[i],ycorner[i],ycorner[i]+bxw,ycorner[i]+bxw],$
                    sx,sy)
endfor

;---------------------------------------------------------------
;exclude boxes with values greater than image mean + Nsigma*rms
IF NOT keyword_set(Nsigma) THEN BEGIN 
  limit_hi = max(img)
  limit_lo = min(img)
ENDIF ELSE BEGIN 
 IF NOT keyword_set(startguess) THEN begin
; If mask is being used, mask out the image
   IF NOT keyword_set(use_sky) THEN begin
    m_im=moment(img,maxmoment=2,/NAN,/double)
    limit_hi = m_im[0] + Nsigma*sqrt(m_im[1])
    limit_lo = m_im[0] - Nsigma*sqrt(m_im[1])
   ENDIF ELSE BEGIN
    IF KEYWORD_SET(mask) THEN BEGIN
       mysky,img,imsky,imskysig,mask=mask,/silent
    ENDIF ELSE mysky,img,imsky,imskysig,/silent
    limit_hi = imsky + Nsigma*imskysig
    limit_lo = imsky - Nsigma*imskysig
   ENDELSE 
 ENDIF ELSE BEGIN
  limit_hi = startguess[0] + Nsigma*startguess[1]
  limit_lo = startguess[0] - Nsigma*startguess[1]
 ENDELSE 
ENDELSE

m_box=dblarr(2)
FOR i=0,n_boxes-1 do BEGIN
  mskcount=0
;  PLOTS,[xcorner[i],xcorner[i]+bxw,xcorner[i]+bxw,xcorner[i]],[ycorner[i],ycorner[i],ycorner[i]+bxw,ycorner[i]+bxw]
  IF KEYWORD_SET(mask) THEN masklist = WHERE(mask[roi[i,*]] EQ 1b,mskcount) 

  IF (mskcount GT (reject_frac)*(bxw^2)) THEN BEGIN
; This region is going to be masked, don't do anything further
    xcorner[i]=0
    ycorner[i]=0
  ENDIF ELSE BEGIN
;    PLOTS,[xcorner[i],xcorner[i]+bxw],[ycorner[i]+bxw,ycorner[i]]       
    IF KEYWORD_SET(use_sky) THEN BEGIN 
;       IF KEYWORD_SET(mask) THEN BEGIN
          mmm2,img[roi[i,*]],boxsky,boxskysig
; Note that mmm's boxsky is a MODE, not a mean
;          mysky,img[roi[i,*]],boxsky,boxskysig,mask=mask,/silent
;       ENDIF ELSE mysky,img[roi[i,*]],boxsky,boxskysig,/silent
;       m_box[0]=boxsky &  m_box[1]=boxskysig
    ENDIF ELSE boxsky=moment(img[roi[i,*]],maxmoment=1,/NAN,/double)
; If you use moment, m_box[1] is sigma squared, but we don't use it
; So, if use_sky is true, the sky level is the mode of the square, and if
; it's false the sky level is the mean?
     
    IF ((boxsky GT limit_hi) OR (boxsky lt limit_lo)) THEN BEGIN
      xcorner[i]=0
      ycorner[i]=0
    ENDIF
  ENDELSE
ENDFOR   

check = where(xcorner,num_boxes)
IF num_boxes EQ 0 THEN BEGIN
 print,"ERROR: No boxes - all excluded due to Nsigma or mask"
 results=[0,0]
 return  
ENDIF 

;---------------------------------------------------------------
;redefine new regions of interest 
xcorner=xcorner(where(xcorner))
ycorner=ycorner(where(ycorner))
n_boxes=n_elements(xcorner)
roi=lonarr(n_boxes,bxw^2)

FOR i=0,n_boxes-1 DO BEGIN 
 roi[i,*]=polyfillv([xcorner[i],xcorner[i]+bxw,xcorner[i]+bxw,xcorner[i]],$
                   [ycorner[i],ycorner[i],ycorner[i]+bxw,ycorner[i]+bxw],$
                   sx,sy)
ENDFOR 

;---------------------------------------------------------------
;draw boxes at the locations defined by corners
;xcorner & ycorner refer to lower left corner
;tv box reguires center position
IF NOT keyword_set(nodisplay) THEN for i=0,n_boxes-1 do tvbox,bxw,$
    xcorner[i]+bxw/2,ycorner[i]+bxw/2,color=!d.n_colors-1

;---------------------------------------------------------------
;compute box2box sky values
meanboxflux  = fltarr(n_boxes)

;mean of each box
for i=0,n_boxes-1 do begin
  IF KEYWORD_SET(use_sky) THEN BEGIN 
    IF KEYWORD_SET(mask) THEN BEGIN
       masklist = WHERE(mask[roi[i,*]] EQ 1b,mskcount)
       IF mskcount GT (reject_frac)*(bxw^2) AND NOT KEYWORD_SET(silent) THEN $
          PRINT,"WARNING in box2boxbg2: roi includes mask ",i
       mmm2,img[roi[i,*]],boxsky,boxskysig
;       mysky,img[roi[i,*]],boxsky,boxskysig,mask=mask,/silent
    ENDIF ELSE mysky,img[roi[i,*]],boxsky,boxskysig,/silent
  ENDIF ELSE boxsky=moment(img[roi[i,*]],maxmoment=1,/NAN,/double)
   meanboxflux[i] = boxsky[0]
endfor 

;box2box mean, std.dev, & std dev of mean
IF n_boxes GE 2 THEN BEGIN
 boxmoments = moment(meanboxflux,/double)
 ambflux    = boxmoments[0]           ; amb  -> avg. mean box
 ambfsigma  = sqrt(boxmoments[1])   
 ambfsdom   = ambfsigma/sqrt(n_boxes) ; sdom -> std. dev of mean
ENDIF ELSE BEGIN
 ambflux    = meanboxflux[0]
 ambfsigma  = 0.0
 ambfsdom   = 0.0
ENDELSE

;place results into an array for return
results=[ambflux,ambfsigma,ambfsdom]
boxinfo=fltarr(3,n_boxes)
boxinfo[0,*]=meanboxflux
boxinfo[1,*]=xcorner+bxw/2
boxinfo[2,*]=ycorner+bxw/2

;---------------------------------------------------------------
;write output
bunit = SXPAR(hdr,'BUNIT',count=count)
IF count NE 1 THEN bunit = 'unknown'

IF NOT keyword_set(outfile) AND keyword_set(silent) THEN outfile = '/dev/null'
IF NOT keyword_set(outfile) THEN outfile = '/dev/tty'

OPENW, outunit, outfile, /GET_LUN, /MORE

printf, outunit, '=================================================='
printf, outunit, 'Box2boxbg: '+ systime(0)
;; printf, outunit, 'Input file: '+file
IF keyword_set(Nsigma) THEN BEGIN 
printf, outunit, 'Pix-to-Pix Exclusion Level: ',Nsigma,' Nsigma'
ENDIF ELSE BEGIN
printf, outunit, 'Pix-to-Pix Exclusion Level: none'
endelse
IF keyword_set(use_sky) THEN BEGIN 
printf, outunit, 'Use_sky: yes'
ENDIF ELSE BEGIN
printf, outunit, 'Use_sky: no'
ENDELSE 
printf, outunit, '=================================================='
printf, outunit, 'Avg. Box-to-Box Mean Sky Per Pixel: '
printf, outunit, 'Units: '+bunit
printf, outunit, '--------------------------------------------------'
printf, outunit
printf, outunit, '  number of boxes: ',n_boxes
printf, outunit, '  box width (pixels):',bxw
printf, outunit
printf, outunit, '  sky   =',ambflux
printf, outunit, '  sigma =',ambfsigma
printf, outunit, '  sdom  =',ambfsdom
printf, outunit

FREE_LUN, outunit

end



