FUNCTION calc_box_sky,boxdata,mask,bxw,Dx,Dy,theta,a,b,boxfile=boxfile
; Input: box data and mask
; Output: sky[0-1], corresponding to Average, SD.
   sz = SIZE(boxdata)
   n_boxes = sz[2]
   rejfrac = 0.01

   boxflag = KEYWORD_SET(boxfile)

   IF boxflag THEN OPENW,unit1,boxfile,/GET_LUN

   delsize = (bxw-1)/2

   good = make_array(n_boxes, /BYTE, VALUE=1b)
;   goodbin = WHERE(good,count)

   FOR ii=0,n_boxes-1 DO BEGIN
     count = 0
     FOR x=(boxdata[1,ii]-delsize),(boxdata[1,ii]+delsize) DO BEGIN
       FOR y=(boxdata[2,ii]-delsize),(boxdata[2,ii]+delsize) DO BEGIN
; mask is 1b at the rejected pixels
         IF (mask[x,y]) THEN count = count + 1
       ENDFOR
     ENDFOR

; If more than a couple of the pixels are masked, throw out the whole box.
     IF (count GT (FLOAT(bxw^2)*rejfrac)) THEN good[ii] = 0b

     IF boxflag THEN BEGIN
       z = calc_z(boxdata[1,ii],boxdata[2,ii],Dx,Dy,theta,a,b)
       PRINTF,unit1,boxdata[1,ii],boxdata[2,ii],z[0],boxdata[0,ii],good[ii]
     ENDIF
   ENDFOR

   goodbin = WHERE(good,count)

   IF boxflag THEN BEGIN
     CLOSE,unit1
     FREE_LUN,unit1
   ENDIF

;   boxflux  = FLTARR(count)
;   FOR ii = 0,(count-1) DO boxflux[ii] = boxdata[0,goodbin[ii]]

   skyres = moment(boxdata[0,goodbin],Maxmoment=2)
; Was /double as well, but we don't NEED double precision for this,
; and it just sucks up disk space.

; moment's second variable is actually variance, so sqrt to get sigma
   skyres[1] = SQRT(skyres[1])

   RETURN,skyres

END
