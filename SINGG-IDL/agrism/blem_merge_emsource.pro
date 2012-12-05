PRO blem_merge_emsource, emcati, emcato, dxmerge
   ;
   ; Read a catalog of emission line sources and merge the measurements
   ; of the detection image sources with more than one emission line 
   ; source.
   ;
   ; emcati   -> name of input emission line source catalog
   ; emcato   -> name of output emission line source catalog
   ; dxmerge  -> sources have to be this close to each other in
   ;             xshift for them to be considered the same line.
   ;
   ; G. Meurer  08/04
   ; G. Meurer  09/06 list cases of sexid <= -1 seperately at the end.
   ;
   agdef    = -1.0
   bgdef    = -1.0
   fwhmgdef = -1.0
   thetadef = -99.9
   peakdef  = -1.0
   xrange   = [-150.0, 20.0]
   yrange   = [-50.0, 50.0]
   ; setplotcolors
   fmtg     = '(i5,f8.2,f8.2,f5.1,f5.1,f7.2,f7.1,f8.3,f6.2,f8.3,f8.2,f8.2,f8.2,f8.2,f6.2,f7.2,i6)'
   fmto     = '(i5,f8.2,f8.2,f5.1,f5.1,f7.2,f7.1,f8.3,f6.2,f8.3,f8.2,f8.2,f8.2,f9.2,f6.2,f8.2,i6,i5,i5,f8.2)'
   hdro     = '# idg    xg      yg    ag   bg   fwhmg thetag   apeak  aw50   cpeak  cshift   cxp     cyp     cwl    cw50   cflx  sexid linid nknt siglam'
   ;
   readfmt, emcati, fmtg, idg, xg, yg, ag, bg, fwhmg, thetag, apeak, $
                          aw50, cpeak, cshift, cxp, cyp, cwl, cw50, cflx, sexid, skipline=1
   ;
   ; find cases with sexid = -1 (unidentified)
   pp        = where(sexid LE -1, npp)
   print, 'Emission line sources with direct source not cataloged : ', npp
   ;
   ; find unique detection image objects
   jj        = where(sexid GE 0, njj)
   kk        = sort(sexid[jj])
   tmp       = sexid[jj[kk]]
   kk        = uniq(tmp)
   sexidu    = tmp[kk]
   nkk       = n_elements(sexidu)
   print, 'Unique detection image objects with emission lines     : ', nkk
   ;
   ; determine the number of emission line sources in each objects
   ns       = make_array(nkk,/int,value=0)
   FOR i = 0, nkk-1 DO BEGIN 
      j     = where(sexid EQ sexidu[i], n)
      ns[i] = n
   ENDFOR 
   j        = where(ns GT 1,nj)
   print, 'Detection image objects with multiple em line sources  : ', nj
   ;
   ; sort by number of emission line sources
   j        = reverse(sort(ns))
   sexidu   = temporary(sexidu[j])
   ns       = temporary(ns[j])
   ;
   openw, lu, emcato, /get_lun
   printf, lu, hdro
   FOR i = 0, nkk-1 DO BEGIN 
      ;
      ; have to refind the lines...  nj should = ns[i]
      j     = where(sexid EQ sexidu[i], nj)
      IF ns[i] GT 1 THEN BEGIN 
         ymean  = mean(yg[j])
         x      = cshift[j]
         y      = yg[j] - ymean
         title  = 'Direct image source = '+strtrim(string(sexidu[i]),2)+' N(em sources) ='+strtrim(string(ns[i]),2)
         xtitle = 'Cshift [pixels]'
         ytitle = 'yg - mean(yg)'
         ;keywait, 'type something for plot: '
         ;plot, x, y, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, psym=1, $
         ; title=title, xtitle=xtitle, ytitle=ytitle
         ;
         lineid = make_array(ns[i],/int,value=-1)
         nline  = 0
         nloose = ns[i]
         ptr    = 0
         REPEAT BEGIN 
            IF lineid[ptr] EQ -1 THEN BEGIN 
               ;
               ; ok found a new line
               lamlin = cwl[j[ptr]]
               siglam = -1.0
               flxlin = cflx[j[ptr]]
               shlin  = cshift[j[ptr]]
               ;
               ; do two iterations of finding sources with similar shifts
               ll     = where(abs(cshift[j[ptr]] - shlin) LE dxmerge, nll)
               IF nll GT 1 THEN BEGIN 
                  mom    = moment(cshift[j[ll]])
                  shlin  = mom[0]
               ENDIF ELSE BEGIN 
                  shlin  = cshift[j[ptr]]
               ENDELSE 
               ll     = where(abs(cshift[j] - shlin) LE dxmerge, nll)
               ;
               ; finish 2nd iteration, calculate quantities to output
               IF nll GT 1 THEN BEGIN 
                  mom    = moment(cshift[j[ll]])
                  shlin  = mom[0]
                  mom    = moment(cwl[j[ll]])
                  lamlin = mom[0]
                  siglam = sqrt(mom[1])
                  flxlin = total(cflx[j[ll]])
               ENDIF ELSE BEGIN 
                  shlin  = cshift[j[ptr]]
                  lamlin = cwl[j[ptr]]
                  siglam = -1.0
                  flxlin = cflx[j[ptr]]
               ENDELSE 
               xglin   = mean(xg[j[ll]])
               yglin   = mean(yg[j[ll]])
               cxplin  = mean(cxp[j[ll]])
               cyplin  = mean(cyp[j[ll]])
               aw50lin = mean(aw50[j[ll]])
               cw50lin = mean(cw50[j[ll]])
               idlin   = min(idg[j[ll]])
               ;
               ; print
               printf, lu, idlin, xglin, yglin, agdef, bgdef, fwhmgdef,  thetadef, peakdef, $
                           aw50lin, peakdef, shlin, cxplin, cyplin, lamlin, cw50lin, flxlin, sexidu[i], $
                           nline+1, nll, siglam, format=fmto
               ;
               ; update
               lineid[ll] = nline
               nline      = nline + 1
            ENDIF 
            ptr = ptr + 1
            jj  = where(lineid EQ -1, nloose)
         ENDREP UNTIL (nloose EQ 0 OR ptr GE ns[i])
      ENDIF ELSE BEGIN 
         ;
         ; just one emission line
         IF nj NE 1 THEN stop, '**** nj ne 1 ?????'
         printf, lu, idg[j], xg[j], yg[j], ag[j], bg[j], fwhmg[j], thetag[j], apeak[j], $
                     aw50[j], cpeak[j], cshift[j], cxp[j], cyp[j], cwl[j], cw50[j], cflx[j], sexid[j], $
                     1, 1, -1.0, format=fmto
      ENDELSE 
   ENDFOR 
   ;
   ; now list sources with no sexid
   IF npp GT 0 THEN BEGIN 
      FOR jj = 0, npp-1 DO BEGIN 
         ii = pp[jj]
         printf, lu, idg[ii], xg[ii], yg[ii], ag[ii], bg[ii], fwhmg[ii], thetag[ii], apeak[ii], $
                     aw50[ii], cpeak[ii], cshift[ii], cxp[ii], cyp[ii], cwl[ii], cw50[ii], cflx[ii], sexid[ii], $
                     1, 1, -1.0, format=fmto
      ENDFOR 
   ENDIF 
   ;
   free_lun, lu
END 
