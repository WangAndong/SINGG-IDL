PRO blemaxe_merge_emsource, emcati, emcato, dlam_merge
   ;
   ; Merge results of axe run on blem positions
   ;
   ; Read a catalog of axe emission line sources (at blem positions) 
   ; and merge the measurements
   ; of the detection image sources with more than one emission line 
   ; source.
   ;
   ; emcati     -> name of input emission line source catalog
   ; emcato     -> name of output emission line source catalog
   ; dlam_merge -> sources have to be this close to each other in
   ;               wavelength for them to be considered the same line.
   ;
   ; G. Meurer 11/04
   ;
   setplotcolors
   imdir      = '~/ACS/Grism/HDFN_jun04/Apsis/Work/'
   filsg      = imdir + 'detectionImage_SEGM.fits'
   fildet     = imdir + 'detectionImage.fits'
   fmti       = '(i,f,f,f,f,f,f,f,f,f,f,f,f,i,i,i,f,f,f)'
   hdro       = '#  id    xim     yim    mag     a     b    theta   w50   class    RA[deg]   Dec[deg]  minsn  maxsn nord0 lin qual   cen   width  flux      continuum     EW   linid nknt siglam   RA,Dec [sexigessimal] '
   fmto       = '(i5,1x,f7.1,1x,f7.1,1x,f7.2,1x,f5.1,1x,f5.1,1x,f6.1,1x,f6.2,1x,f6.3,1x,f10.5,1x,f10.5,1x,f6.1,1x,f6.1,1x,i5,1x,i3,1x,i3,1x,f7.1,1x,f6.1,1x,e9.2,1x,e9.2,1x,f7.1,1x,i6,1x,i4,1x,f6.2,1x,a27)'
   ;
   ; read input catalog; get array coords
   readcol, emcati, id, xim, yim, mag, aa, bb, theta, w50im, class, $
    ra, dec, minsn, maxsn, nord0, lin, qual, cen, $
    w50, flux, cont, ew, format=fmti
   nobji = n_elements(id)
   xarr  = fix(xim - 0.5)
   yarr  = fix(yim - 0.5)
   ;
   ; get header of detection image
   fits_read, fildet, dum, hddet, /header_only
   ;
   ; update RA & DEC
   xyad, hddet, xim, yim, ra, dec
   ;
   ; open segmentation image
   fits_read, filsg, imsg, hdsg
   ;
   ; get sexids 
   sexid = imsg[xarr, yarr]
   ;
   ; find unique detection image objects
   kk       = sort(sexid)
   tmp      = sexid[kk]
   kk       = uniq(tmp)
   sexidu   = tmp[kk]
   nkk      = n_elements(sexidu)
   print, 'Unique detection image objects with emission lines     : ', nkk
   ;
   ; determine the number of emission line sources in each objects
   ns       = make_array(nkk,/int,value=0)
   FOR ii = 0, nkk-1 DO BEGIN 
      jj     = where(sexid EQ sexidu[ii], n)
      ns[ii] = n
   ENDFOR 
   jj       = where(ns GT 1,njj)
   print, 'Detection image objects with multiple em line sources  : ', njj
   ;
   ; sort by number of emission line sources
   jj       = reverse(sort(ns))
   sexidu   = temporary(sexidu[jj])
   ns       = temporary(ns[jj])
   ;
   openw, lu, emcato, /get_lun
   printf, lu, hdro
   FOR ii = 0, nkk-1 DO BEGIN 
      ;
      ; have to refind the lines...  nj should = ns[ii]
      jj    = where(sexid EQ sexidu[ii], nj)
      IF ns[ii] GT 1 THEN BEGIN 
         ymean  = mean(yim[jj])
         x      = xim[jj]
         y      = yim[jj] - ymean
         title  = 'Direct image source = '+strtrim(string(sexidu[ii]),2)+' N(em sources) ='+strtrim(string(ns[ii]),2)
         xtitle = 'Cshift [pixels]'
         ytitle = 'yg - mean(yg)'
         ;keywait, 'type something for plot: '
         ;plot, x, y, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, psym=1, $
         ; title=title, xtitle=xtitle, ytitle=ytitle
         ;
         used   = make_array(ns[ii],/int,value=-1)
         nline  = 0
         nloose = ns[ii]
         ptr    = 0
         REPEAT BEGIN 
            IF used[ptr] EQ -1 THEN BEGIN 
               ;
               ; ok found a new line
               lamlin = cen[jj[ptr]]
               siglam = -1.0
               flxlin = flux[jj[ptr]]
               ;
               ; do two iterations of finding sources with similar shifts
               ll     = where(abs(cen[jj] - lamlin) LE dlam_merge, nll)
               IF nll GT 1 THEN BEGIN 
                  mom     = moment(cen[jj[ll]])
                  lamlin  = mom[0]
               ENDIF ELSE BEGIN 
                  lamlin  = cen[jj[ptr]]
               ENDELSE 
               ;
               ; finish 2nd iteration, calculate quantities to output
               ll     = where(abs(cen[jj] - lamlin) LE dlam_merge, nll)
               IF nll GT 1 THEN BEGIN 
                  mom    = moment(cen[jj[ll]])
                  lamlin = mom[0]
                  siglam = sqrt(mom[1])
                  flxlin = total(flux[jj[ll]])
               ENDIF ELSE BEGIN 
                  lamlin = cen[jj[ptr]]
                  siglam = -1.0
                  flxlin = flux[jj[ptr]]
               ENDELSE 
               sexidlin = sexidu[ii]
               ximlin   = mean(xim[jj[ll]])
               yimlin   = mean(yim[jj[ll]])
               aalin    = mean(aa[jj[ll]])
               bblin    = mean(bb[jj[ll]])
               thetalin = mean(theta[jj[ll]])
               w50imlin = mean(w50im[jj[ll]])
               classlin = mean(class[jj[ll]])
               idlin    = id[jj[ptr]]
               minsnlin = min(minsn[jj[ll]])
               maxsnlin = max(maxsn[jj[ll]])
               nord0lin = max(nord0[jj[ll]])
               quallin  = max(qual[jj[ll]])
               maglin   = mag[jj[ptr]]
               w50lin   = mean(w50[jj[ll]])
               contlin  = mean(cont[jj[ll]])
               ewlin    = flxlin/contlin
               xyad, hddet, ximlin, yimlin, ralin, declin         ; derive RA & DEC
               radecstr = adstring(ralin, declin, 2)
               ; print
               printf, lu, sexidlin, ximlin, yimlin, maglin, aalin, bblin, thetalin, w50imlin, classlin, $
                           ralin, declin, minsnlin, maxsnlin, nord0lin, nline+1, quallin, lamlin, w50lin, flxlin, $
                           contlin, ewlin, idlin, nll, siglam, radecstr, format=fmto
               ;
               ; update
               used[ll]   = nline
               nline      = nline + 1
            ENDIF 
            ptr = ptr + 1
            jjj = where(used EQ -1, nloose)
         ENDREP UNTIL (nloose EQ 0 OR ptr GE ns[ii])
      ENDIF ELSE BEGIN 
         ;
         ; just one emission line
         IF nj NE 1 THEN stop, '**** nj ne 1 ?????'
         xyad, hddet, xim[jj], yim[jj], ralin, declin ; derive RA & DEC
         radecstr = adstring(ralin, declin, 2)
         printf, lu, sexid[jj], xim[jj], yim[jj], mag[jj], aa[jj], bb[jj], theta[jj], w50im[jj], class[jj], $
                     ra[jj], dec[jj], minsn[jj], maxsn[jj], nord0[jj], 1, qual[jj], cen[jj], w50[jj], flux[jj], $
                     cont[jj], ew[jj], id[jj], 1, -1.0, radecstr, format=fmto
      ENDELSE 
   ENDFOR 
   free_lun, lu
END 
