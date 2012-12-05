PRO bindata,boxdata,skyres,zmin,zmax,zbinsize,sz,Dx,Dy,a,b,theta,$
   Zbox,Zbin,Skybin

; Outputs are Zbox,Zbin,Skybin

; Now, boxdata is a 3xN array of (mean, X, Y) data
   bsz=SIZE(boxdata)
   n_boxes=bsz[2]

   num_bins = LONG(zmax/zbinsize)+1
   Ztemp=FLTARR(2,num_bins)
   Zbox=FLTARR(n_boxes)
   Skybox=FLTARR(n_boxes)

; Ztemp[0,*] are the number in each bin, Ztemp[1,*] are the sum of the means
   dist_ellipse,ellval,[sz[1],sz[2]],Dx,Dy,(a/b),(theta/!dtor)+90.0
   FOR i=0,n_boxes-1 DO BEGIN
     Zbox[i] = ellval[FIX(boxdata[1,i]),FIX(boxdata[2,i])]/a
   ENDFOR

   FOR i=0,n_boxes-1 DO BEGIN
;     x=boxdata[1,i]
;     y=boxdata[2,i]
;     Zbox[i] = ellval[x,y]/a
;     Zbox[i]=calc_z(x,y,Dx,Dy,theta,a,b)
     Skybox[i]=boxdata[0,i]

     IF (Zbox[i] GT zmin) AND (Zbox[i] LT zmax) THEN BEGIN
       binnum = LONG(Zbox[i]/zbinsize)
       IF (binnum LT num_bins) THEN BEGIN
         Ztemp[0,binnum] = Ztemp[0,binnum] + 1
         Ztemp[1,binnum] = Ztemp[1,binnum] + Skybox[i]
       ENDIF
     ENDIF 
     
   ENDFOR

   Zbin=FLTARR(num_bins)
   Skybin=FLTARR(num_bins)
   FOR jj=0,num_bins-1 DO BEGIN
     Zbin[jj] = FLOAT(jj)*zbinsize
     Skybin[jj] = -100.0
     IF Ztemp[0,jj] GT 0 THEN BEGIN
       Skybin[jj] = Ztemp[1,jj] / Ztemp[0,jj]
     ENDIF
   ENDFOR

END
