PRO blem_sortout, fili, filgood, filstars, filunc, $
                  filseg=filseg, filsgoodd=filsgoodd, abcdef=abcdef, invmat=invmat
   ;
   ; Sort output from blind_emfind into
   ;  + good emission line sources
   ;  + stars
   ;  + uncertain / ambiguous sources
   ;
   ; fili      -> Output file from blind_emfind
   ; scat      -> Source catalog of direct image
   ; filgood   -> Name of good sources catalog to be written
   ; filstars  -> Name of stellar sources catalog to be written
   ; filunc    -> Name of uncertain/ambiguous sources to be written
   ; filseg    -> Input segmentation image of the direct image.  If
   ;              this is passed sexid is derived from this rather than
   ;              fili.
   ; filsgoodd -> Array of file names giving vertices of polygon in
   ;              direct image pixel coords that enclose the good areas
   ;              in the direct image.  If this is passed then 
   ;              apall flag check is redone.  If abcdef is also passed
   ;              then the zero-order check is also re-done
   ; abcdef    -> transformation matrix between zero-order and direct 
   ;              image from mrmatch.
   ; invmat    -> invert transformation matrix
   ;
   ; G. Meurer 08/04
   ; G. Meurer 09/06 added filseg and filsgoodd options
   ;                 resets qualcc=2 for incidences of no ccpeak
   ;                 reports number of unique sexids
   fmt1     = '(i5,f8.2,f8.2,f8.2,f8.3,f5.1,f5.1,f7.1,f7.2,i3,i4,i4)'
   fmt2     = '(72x,f7.1,f7.1,f9.1,f7.1,1x,a3,1x,f8.3,f7.2,f6.2)'
   fmt3     = '(72x,57x,a3,1x,f8.3,f8.2,f6.2,f8.2,f8.2,f8.2,f8.2,f7.2,a8)'
   fmtg     = '(i5,f8.2,f8.2,f5.1,f5.1,f7.2,f7.1,f8.3,f6.2,f8.3,f8.2,f8.2,f8.2,f8.2,f6.2,f7.2,i6)'
   fmts     = '(i5,i5)'
   fmtu     = '(i5,f8.2,f8.2,f5.1,f5.1,f7.2,f7.1,f8.3,f6.2,f8.3,f8.2,f8.2,f8.2,f8.2,f6.2,f7.2,i6,i4,i4,i4,a25)'
   hdrg     = '# idg    xg      yg    ag   bg   fwhmg thetag   apeak  aw50   cpeak  cshift   cxp     cyp     cwl    cw50   cflx  sexid'
   hdrs     = '# sexid n_features'
   hdru     = '# idg    xg      yg    ag   bg   fwhmg thetag   apeak  aw50   cpeak  cshift   cxp     cyp     cwl    cw50   cflx  sexid qcc fla flz reason'
   ;
   ; read blem output file
   readfmt, fili, fmt1, idg, xg, yg, yd, magg, ag, bg, thetag, fwhmg, flagg, apallflag, zordflag, skipline=1
   readfmt, fili, fmt2, pksngf, pksndf, fluxa, skya, qualcut, apeak, ashift, aw50, skipline=1
   readfmt, fili, fmt3, qualcc, cpeak, cshift, cw50, cwl, cprms, cxp, cyp, cflx, csexid, skipline=1
   ;
   nin      = n_elements(idg)
   qualcut  = strtrim(qualcut, 2)
   qualcc   = strtrim(qualcc, 2)
   ;
   ; as the default get sexid from input file
   csexid   = strtrim(csexid, 2)
   sexid    = fix(csexid)
   ;
   ; check for and resolve multiple sexids
   ; only report changes if filseg is not set
   k        = where(strpos(csexid,',') NE -1, nk)
   IF nk GT 0 AND NOT keyword_set(filseg) THEN $
    FOR i = 0, nk-1 DO print, 'WARNING: Multiple SEx ID = ', csexid[k[i]], ' -> Grism ID = ', idg[k[i]], ' --> Adopting SExid = ', sexid[k[i]]
   ;
   ; get sextractor IDs, either from segmentation image
   IF keyword_set(filseg) THEN BEGIN 
      ;
      ; get sexids from segmentation image
      newid    = make_array(nin,/long,value=-1)
      fits_read, filseg, imseg, hdseg
      siz      = size(imseg)
      nx       = siz[1]
      ny       = siz[2]
      ;
      ; get segmentation image values for cases where
      ; direct image position is defined
      kk       = where(cxp GE 1.0 AND cxp LE float(nx) AND cyp GE 1.0 AND cyp LE float(ny), nkk)
      IF nkk GT 0 THEN BEGIN 
         ipix      = fix(cxp[kk] - 0.5)
         jpix      = fix(cyp[kk] - 0.5)
         newid[kk] = imseg[ipix,jpix]
      ENDIF 
      ;
      ; used passed IDs for stars, and cxp,cyp not defined
      kk       = where((cxp LT 1.0 OR cxp GT float(nx) OR cyp LT 1.0 OR cyp GT float(ny)) AND qualcut EQ '3', nkk)
      IF nkk GT 0 THEN newid[kk] = sexid[kk]
      ;
      ; make sure there are no newid = 0 cases, change to -1
      qq        = where(newid EQ 0, nqq)
      ;print, 'Number of cases with segmentation image value = 0      : ', nqq
      IF nqq GT 0 THEN newid[qq] = -1
      ;
      ; report number of cases with changed sexid
      kk       = where(sexid NE newid, nkk)
      print, 'Number of changes between initial sexid and seg image  : ', nkk
      sexid    = newid
   ENDIF 
   ;
   ; fix cases where qualcc should be 2
   qq         = where(qualcut EQ '0' AND qualcc EQ '0' AND cpeak LT 0.0, nqq)
   qualcc[qq] = '2'
   ;
   ; redo check of whether direct source is off frame 
   ; This is only done if filsgoodd is passed
   IF keyword_set(filsgoodd) THEN BEGIN 
      np         =  n_elements(filsgoodd) 
      IF np EQ 1 AND filsgoodd[0] EQ '' THEN BEGIN 
         apallflag   = make_array(nnin, /byte, value=0b)
      ENDIF ELSE BEGIN 
         goodtest   =  make_array(np,nin,/byte)
         ;
         ; Loop through each good area polygon
         FOR jj = 0, np-1 DO BEGIN 
            IF filsgoodd[jj] NE '' THEN BEGIN 
               readcol, filsgoodd[jj], xp, yp, format='(f,f)'
               good           = inside(cxp, cyp, xp, yp)
               goodtest[jj,*] = reform(good, nin)
            ENDIF ELSE BEGIN 
               ;
               ; In this instance we know that more than one file name
               ; was passed.  So if there is an empty string mark test
               ; as failed, otherwise all positions would automatically be 
               ; passed.  We only want to do that if no filenames were 
               ; passed in filsgoodg
               goodtest[j,*] = 0b
            ENDELSE 
         ENDFOR 
         ;
         ; combine results for all good areas
         good       = total(goodtest, 1)
         gd         = where(good GE 1 OR (cxp LT 0.0 AND cyp LT 0.0), ngd)
         apallflag   = make_array(nin, /byte, value=1b)
         IF ngd GT 0 THEN apallflag[gd] = 0b
      ENDELSE 
      ;
      ; check if zero-order check needs to be redone as well
      IF keyword_set(abcdef) THEN BEGIN 
         ;
         ; Invert match transformation matrix if needed
         IF keyword_set(invmat) THEN BEGIN 
            cx     = -1.0*abcdef[0]
            cy     = -1.0*abcdef[3]
            amat   = [[abcdef[1], [abcdef[2]]], [abcdef[4], abcdef[5]]]
            tmat   = float(invert(amat, status, /double))
            IF status NE 0 THEN BEGIN 
               print, '**** ERRROR in BLEM_SORTOUT.  Could not invert transformation matrix'
               print, '**** Status from INVERT : ', status
               return
            ENDIF 
         ENDIF ELSE BEGIN 
            cx     = abcdef[0]
            cy     = abcdef[3]
            tmat   = [[abcdef[1], [abcdef[2]]], [abcdef[4], abcdef[5]]]
         ENDELSE  
         ;
         ; calculate position of sources in direct image if they are
         ; zeroth order images
         xd        =  cx + tmat[0]*xg + tmat[1]*yg
         yd        =  cy + tmat[2]*xg + tmat[3]*yg
         ;
         ; the rest of the test is like the apall test (less commenting here)
         IF np EQ 1 AND filsgoodd[0] EQ '' THEN BEGIN 
            zordflag = make_array(nnin, /byte, value=0b)
         ENDIF ELSE BEGIN 
            goodtest =  make_array(np,nin,/byte)
            ;
            FOR jj = 0, np-1 DO BEGIN 
               IF filsgoodd[jj] NE '' THEN BEGIN 
                  readcol, filsgoodd[jj], xp, yp, format='(f,f)'
                  good           = inside(xd, yd, xp, yp)
                  goodtest[jj,*] = reform(good, nin)
               ENDIF ELSE BEGIN 
                  ;
                  goodtest[j,*] = 0b
               ENDELSE 
            ENDFOR 
            ;
            good       = total(goodtest, 1)
            gd         = where(good GE 1 OR (cxp LT 0.0 AND cyp LT 0.0), ngd)
            zordflag   = make_array(nin, /byte, value=1b)
            IF ngd GT 0 THEN zordflag[gd] = 0b
         ENDELSE 
      ENDIF 
   ENDIF 
   ;
   ; isolate good emission line sources & print
   k        = where(qualcut EQ '0' AND qualcc EQ '0' AND apallflag EQ 0 AND zordflag EQ 0, nk)
   print, 'Number of good emission line sources                   : ', nk 
   openw, lu, filgood, /get_lun
   printf, lu, hdrg
   FOR i = 0, nk-1 DO printf, lu, idg[k[i]], xg[k[i]], yg[k[i]], ag[k[i]], bg[k[i]], fwhmg[k[i]], $
                                  thetag[k[i]], apeak[k[i]], aw50[k[i]], cpeak[k[i]], cshift[k[i]], $
                                  cxp[k[i]], cyp[k[i]], cwl[k[i]], cw50[k[i]], cflx[k[i]], sexid[k[i]], $
                                  format=fmtg
   free_lun, lu
   ;
   ; find unique sexids among emission line sources
   qq       = where(sexid[k] NE -1, nqq)
   IF nqq GT 0 THEN BEGIN 
      check = sexid[k[qq]]
      jj    = sort(check)
      qq    = uniq(check[jj])
      nqq   = n_elements(qq)
   ENDIF 
   print, 'Number of unique emission line galaxies                : ', nqq
   ;
   ; isolate stars, find unique object IDs
   k        = where(qualcut EQ '3' AND sexid NE -1, nk)
   starid   = sexid[k]
   k        = sort(starid)
   staridu  = starid[k]
   k        = uniq(staridu)
   staridu  = temporary(staridu[k])
   ; 
   ; determine number of "features", write Sexid & number of 
   ; features to output file
   nk       = n_elements(staridu)
   print, 'Number of unique stars                                 : ', nk 
   nf       = make_array(nk, /int, value=0)
   openw, lu, filstars, /get_lun
   FOR i = 0, nk-1 DO BEGIN 
      k = where(starid EQ staridu[i], n)
      nf[i] = n
      printf, lu, staridu[i], nf[i], format=fmts
   ENDFOR 
   free_lun, lu
   ;
   ; find uncertain and ambiguous sources
   k1       = where(qualcut EQ '0' AND apallflag EQ 0 AND zordflag EQ 0 AND qualcc EQ '1', nk1)
   k2       = where(qualcut EQ '0' AND apallflag EQ 0 AND zordflag EQ 0 AND qualcc EQ '2', nk2)
   k3       = where(qualcut EQ '0' AND (apallflag NE 0 OR zordflag NE 0), nk3)
   print, 'Number of sources with ambiguous CC peak selection     : ', nk1
   print, 'Number of sources with no apparent direct counterparts : ', nk2
   print, 'Number of sources near edge                            : ', nk3
   openw, lu, filunc, /get_lun
   printf, lu, hdru
   IF nk1 GT 0 THEN FOR i = 0, nk1-1 DO printf, lu, idg[k1[i]], xg[k1[i]], yg[k1[i]], ag[k1[i]], bg[k1[i]], fwhmg[k1[i]], $
                                                thetag[k1[i]], apeak[k1[i]], aw50[k1[i]], cpeak[k1[i]], cshift[k1[i]], $
                                                cxp[k1[i]], cyp[k1[i]], cwl[k1[i]], cw50[k1[i]], cflx[k1[i]], sexid[k1[i]], $
                                                qualcc[k1[i]], apallflag[k1[i]], zordflag[k1[i]], 'Ambiguous CC spec', $
                                                format=fmtu
   IF nk2 GT 0 THEN FOR i = 0, nk2-1 DO printf, lu, idg[k2[i]], xg[k2[i]], yg[k2[i]], ag[k2[i]], bg[k2[i]], fwhmg[k2[i]], $
                                                thetag[k2[i]], apeak[k2[i]], aw50[k2[i]], cpeak[k2[i]], cshift[k2[i]], $
                                                cxp[k2[i]], cyp[k2[i]], cwl[k2[i]], cw50[k2[i]], cflx[k2[i]], sexid[k2[i]], $
                                                qualcc[k2[i]], apallflag[k2[i]], zordflag[k2[i]], 'No direct image source', $
                                                format=fmtu
   IF nk3 GT 0 THEN FOR i = 0, nk3-1 DO printf, lu, idg[k3[i]], xg[k3[i]], yg[k3[i]], ag[k3[i]], bg[k3[i]], fwhmg[k3[i]], $
                                                thetag[k3[i]], apeak[k3[i]], aw50[k3[i]], cpeak[k3[i]], cshift[k3[i]], $
                                                cxp[k3[i]], cyp[k3[i]], cwl[k3[i]], cw50[k3[i]], cflx[k3[i]], sexid[k3[i]], $
                                                qualcc[k3[i]], apallflag[k3[i]], zordflag[k3[i]], 'Near edge of grism image', $
                                                format=fmtu
   free_lun, lu
   ;
END 
