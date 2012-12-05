PRO shunt_calcquants, hdrg, keep_class, id, xim, yim, mag, krad, backg, aim, bim, thim, w50, class, $
                      spec_class, xdirect, ydirect, bx_specx, bx_specy, bx_directx, bx_directy, $
                      ra, dec, bx_ra, bx_dec
   ;
   ; select output to keep by what the classification code is,
   ; and calculate position parameters for the kept objects
   ;
   ; G. Meurer 09/2005
   ;
   ; loop through allowed classifications
   nclass = strlen(keep_class)
   nobj   = n_elements(id)
   nkeep  = 0
   FOR ii = 0, nclass-1 DO BEGIN 
      cl     = strmid(keep_class,ii,1)
      kk     = where(strpos(spec_class,cl) GE 0, nkk)
      IF nkk GT 0 THEN BEGIN 
         jj = sort(mag[kk])
         qq = kk[jj]
         IF nkeep EQ 0 THEN ptr = qq ELSE ptr = [ptr, qq]
         nkeep    = nkeep + nkk
      ENDIF 
   ENDFOR 
   ;
   ; save the elements that are to be kept
   id         = id[ptr]
   xim        = xim[ptr]
   yim        = yim[ptr]
   mag        = mag[ptr]
   krad       = krad[ptr]
   backg      = backg[ptr]
   aim        = aim[ptr]
   bim        = bim[ptr]
   thim       = thim[ptr]
   w50        = w50[ptr]
   class      = class[ptr]
   spec_class = spec_class[ptr]
   xdirect    = xdirect[ptr]
   ydirect    = ydirect[ptr]
   bx_specx   = bx_specx[ptr,*]
   bx_specy   = bx_specy[ptr,*]
   bx_directx = bx_directx[ptr,*]
   bx_directy = bx_directy[ptr,*]
   ;
   ; convert coords to RA, Dec
   xyad, hdrg, (xdirect - 1.0), (ydirect - 1.0), ra, dec
   x0         = bx_directx[*,0] - 1.0
   x1         = bx_directx[*,1] - 1.0
   x2         = bx_directx[*,2] - 1.0
   x3         = bx_directx[*,3] - 1.0
   y0         = bx_directy[*,0] - 1.0
   y1         = bx_directy[*,1] - 1.0
   y2         = bx_directy[*,2] - 1.0
   y3         = bx_directy[*,3] - 1.0
   xyad, hdrg, x0, y0, ra0, dec0
   xyad, hdrg, x1, y1, ra1, dec1
   xyad, hdrg, x2, y2, ra2, dec2
   xyad, hdrg, x3, y3, ra3, dec3
   bx_ra      = [[ra0], [ra1], [ra2], [ra3]]
   bx_dec     = [[dec0], [dec1], [dec2], [dec3]]
   ;
END 
