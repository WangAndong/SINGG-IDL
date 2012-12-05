PRO test_sex_setthresh
   ;
   ; program to determine what THRESHOLD to set in sextractor.
   ; Know that we want at least five connected pixels in 
   ; detection image, and that S/N(peak) > SNLIMIT in filter image
   ;
   snlimit      = 4.0
   ;filtim       = 'f775w_play.fits'
   ;sigfilt      = 14.9
   filtim       = 'f850lp_play.fits'
   sigfilt      = 18.0
   detim        = 'detim_play.fits'
   sigdet       = 12.5
   catdet       = 'detim_play.cat'
   snchlim1     = 4.0
   snchlim2     = 20.0
   ;
   xrange       = [0, 20]
   yrange       = [-0.5, 2.0]
   xtitle       = '!3 Peak S/N in filter image: '+filtim
   ytitle       = '!3 S/N ratio: peak(filter im)/[5th brightest neighbor (det im)]'
   ;
   ; read fits images
   fits_read, filtim, dfilt, hfilt
   fits_read, detim, ddet, hdet
   k            = size(ddet)
   ni           = k[1]
   nj           = k[2]
   ;
   ; read catalog
   readcol, catdet, num, x, y, xp, yp, format='(i,f,f,i,i)'
   ip           = xp - 1
   jp           = yp - 1
   nc           = n_elements(num)
   sn2f         = make_array(nc)
   sn2d         = make_array(nc)
   snpeakf      = dfilt[ip,jp]/sigfilt
   snpeakd      = ddet[ip,jp]/sigdet
   FOR i = 0, nc-1 DO BEGIN 
      ;
      ; find S/N of fifth brightest pixel in 3x3 box
      i1        = max([ip[i]-1,0])
      i2        = min([ip[i]+1,ni-1])
      j1        = max([jp[i]-1,0])
      j2        = min([jp[i]+1,nj-1])
      nv        = (i2 - i1 + 1)*(j2 - j1 + 1)
      valf      = dfilt[i1:i2,j1:j2]
      vald      = ddet[i1:i2,j1:j2]
      kf        = reverse(sort(valf))
      kd        = reverse(sort(vald))
      j         = min([4, nv])               ; fifth brightest pixel if possible
      sn2f[i]   = valf[kf[j]]/sigfilt       
      sn2d[i]   = vald[kd[j]]/sigdet        
   ENDFOR 
   snratfd      = sn2d/snpeakf
   snratff      = sn2f/snpeakf
   plot, snpeakf, snratfd, psym=sym(1), xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, $
    xtitle=xtitle, ytitle=ytitle, charsize=1.2
   ;
   good         = where(snpeakf GE snchlim1 AND snpeakf LE snchlim2, ng)
   IF ng GT 0 THEN grm_avsigclip, snratfd[good], 3.0, 50, mean, nuse, nrej, nit, /verbose
END

