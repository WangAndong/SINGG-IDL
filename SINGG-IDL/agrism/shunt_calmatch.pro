PRO shunt_calmatch, shcat, drcat, drim, growbox, snlimit, matchcat
   ;
   ; Find matches needed to perform squashed grism <-> direct
   ; image coordinate and mag transformation.
   ;
   ; shcat    -> shunt results catalog
   ; drcat    -> direct image catalog (PEARS parsed GOODS format)
   ; drim     -> direct image (needed for WCS)
   ; growbox  -> array of offsets to grow search box from shunt
   ;             [dx1, dx2, dy1, dy2]
   ;             xmin_o = xmin + dx1; xmax_o = xmax + dx2
   ;             ymin_o = ymin + dy1; ymax_o = ymax + dy2
   ;             try growbox = [-10, 0, -3, +3]
   ; snlimit  -> Signal/noise level limit in shunt catalog.
   ;             try snlimit = 5 - 10
   ; matchcat -> name for catalog of matched results
   ;
   ; G. Meurer, 02/2006
   ;
   fmtsh      = '(i,f,f,f,a,x,x,x,x,x,x,x,x,f,f,f,f,f,f,f,f,f,f,f,f,f)'
   fmtdr      = '(f,f,l,f,f,f,f,f,f,f,f,f,f,f,f)'
   fmtm       = '(i5,f8.2,f8.2,f7.2,f7.1,a3, i8,f8.2,f8.2,f7.1,f7.1,f7.1,f7.3,f7.3,f7.3)'
   ;
   ; read in direct catalog
   readcol, drcat, xim, yim, iddr, raw, decw, aim, bim, thim, aw, bw, thw, $
            m435, m606, m775, m850, format=fmtdr
   ;
   ; calculate color
   color1     = m606 - m850
   color2     = m775 - m850
   ;
   ; read shunt cat
   readcol, shcat, idsh, xsq, ysq, magsq, sclass, $
            ra, dec, ra0, dec0, ra1, dec1, ra2, dec2, ra3, dec3, skylev, skysig, sn, $
            format=fmtsh
   ;
   ; find good shunt sources, and throw out the rest
   ; + not sclass = 'o'
   ; + sn => snlimit
   sclass     = strtrim(strupcase(sclass),2)
   good       = where(sclass NE 'O' AND sn GE snlimit, ngood)
   idsh       = idsh[good]
   xsq        = xsq[good]
   ysq        = ysq[good]
   magsq      = magsq[good]
   sclass     = sclass[good]
   ra         = ra[good]
   dec        = dec[good]
   ra0        = ra0[good]
   dec0       = dec0[good]
   ra1        = ra1[good]
   dec1       = dec1[good]
   ra2        = ra2[good]
   dec2       = dec2[good]
   ra3        = ra3[good]
   dec3       = dec3[good]
   skylev     = skylev[good]
   skysig     = skysig[good]
   sn         = sn[good]
   print, 'Number of shunt sources to match: ', ngood
   ;
   ; read in direct image header
   fits_read, drim, imdr, hddr, /header_only
   ;
   ; convert ra, dec of corners into pixel corners
   adxy, hddr, ra0, dec0, xdr0, ydr0
   adxy, hddr, ra1, dec1, xdr1, ydr1
   adxy, hddr, ra2, dec2, xdr2, ydr2
   adxy, hddr, ra3, dec3, xdr3, ydr3
   ;
   ; grow the boxes
   xdr0       = xdr0 + growbox[0]
   ydr0       = ydr0 + growbox[2]
   xdr1       = xdr1 + growbox[1]
   ydr1       = ydr1 + growbox[2]
   xdr2       = xdr2 + growbox[1]
   ydr2       = ydr2 + growbox[3]
   xdr3       = xdr3 + growbox[0]
   ydr3       = ydr3 + growbox[3]
   ;
   ; loop through sources find potential matches
   openw, lu, matchcat, /get_lun
   nmatch     = make_array(ngood, /int, value=0)
   FOR ii = 0, ngood-1 DO BEGIN 
      px      = [xdr0[ii], xdr1[ii], xdr2[ii], xdr3[ii]]
      py      = [ydr0[ii], ydr1[ii], ydr2[ii], ydr3[ii]]
      mtch    = inside(xim, yim, px, py, /index)
      IF mtch[0] NE -1 THEN BEGIN 
         nmatch[ii] = n_elements(mtch)
         FOR jj = 0, nmatch[ii]-1 DO BEGIN 
            kk = mtch[jj]
            printf, lu, idsh[ii], xsq[ii], ysq[ii], magsq[ii], sn[ii], ' '+sclass[ii]+' ', $
                        iddr[kk], xim[kk], yim[kk], aim[kk], bim[kk], thim[kk], $
                        m775[kk], color1[kk], color2[kk], format=fmtm
         ENDFOR            
      ENDIF 
      print, ii, nmatch[ii]
   ENDFOR 
   free_lun, lu
END 
