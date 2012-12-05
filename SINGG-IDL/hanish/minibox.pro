FUNCTION minibox,img,Px,Py,shift

   total = 0.0
   sz = SIZE(img)

; shift is how many pixels correspond to 14 arcsec
   FOR ii = MAX([0,LONG(Px - shift)]), $
            MIN([sz[1],LONG(Px + shift)]) DO BEGIN
      FOR jj = MAX([0,LONG(Py - shift)]), $
               MIN([sz[2],LONG(Py + shift)]) DO BEGIN
         total = total + img[ii,jj]
      ENDFOR     
   ENDFOR

   RETURN,total

END
