PRO blind_emfind, filgr, filgf, fildr, fildf, filsg, catgf, catdr, filsgoodg, filsgoodd, $
                  xref, yref, dldp_a_0, dldp_a_1, dldp_a_2, dydx_a_0, dydx_a_1, yoff_a, $
                  aper, filsens, extsens, dxmed, abcdef, dxbeam, thresh_rms, apmin, fwag, szstmp, $
                  dzgr, dzgf, dzdr, dzdf, pfx1d, pfxgs, pfxcc, filout, $
                  outdir=outdir, invmat=invmat, awkgrcat=awkgrcat, awkdrcat=awkdrcat, $
                  fil_1stqa=fil_1stqa
   ;
   ; blind emission line finder.  Looks for emission line sources 
   ; in filtered grism image and then attempts to find 
   ; corresponding filtered direct image source.
   ;
   ; filgr      -> raw grism image file.
   ; filgf      -> filtered grism image file.
   ; fildr      -> raw direct image file.
   ; fildf      -> filtered direct image file.
   ; filsg      -> segmentation image file produced by SExtractor.
   ; catgf      -> Catalog of filtered grism image file produced by 
   ;               SExtractor.
   ; catdr      -> direct image catalog
   ; filsgoodg  -> String array of filenames containing good areas
   ;               in grism image.  If set to an empty string
   ;               then the entire area is set to be good.
   ; filsgoodd  -> String array of filenames containing good areas 
   ;               in direct image. If set to an empty string
   ;               then the entire area is set to be good.
   ; xref,yref  -> x,y position reference for aXe 2d function evaluation.
   ; dldp_a_0   -> zeropt element equation for wavelength
   ;               solution from aXe config file.
   ; dldp_a_1   -> dispersion element equation for wavelength
   ;               solution from aXe config file.
   ; dldp_a_2   -> quadratic element equation for wavelength
   ;               solution from aXe config file.
   ; dydx_a_0   -> trace zeroth order (offset) equation coefficients from
   ;               aXe config file.
   ; dydx_a_1   -> trace first order (tilt) equation coefficients from 
   ;               the aXe config file.
   ; yoff_a     -> y offset equation coefficients from 
   ;               the aXe config file.
   ; aper       -> aperture diameter
   ; filsens    -> file with sensitivity curve
   ; extsens    -> extension of sensitivity curve
   ; dxmed      -> number of columns pixels used in median filter.
   ; abcdef     -> transformation matrix from mrmatch.
   ; dxbeam     -> Beam offset for order 1
   ; thresh_rms -> threshold for fitting cros correlation peaks with
   ;               respect to the (clipped) rms in the cross corr.
   ; apmin      -> Minimum aperture in rows.
   ; fwag       -> factor to multiply by a_grism to get extraction width. 
   ; dzgr       -> 2 elem array: display range for raw grism image.
   ; dzgf       -> 2 elem array: display range for filtered grism image.
   ; dzdr       -> 2 elem array: display range for raw direct image.
   ; dzdf       -> 2 elem array: display range for filtered direct image.
   ; pfx1d      -> prefix for output 1d extraction
   ; pfxgs      -> prefix for grayscale png files
   ; pfxcc      -> prefix for cross corr results
   ; filout     -> File for combined results
   ; outdir     -> if set then this is the output directory
   ; invmat     -> If set then invert the transformation matrix before 
   ;               using.
   ; awkgrcat   -> awk file used for parsing grism catalog.  Defaults
   ;               to "blem_rdsex.awk"
   ; awkdrcat   -> awk file used for parsing direct catalog, Defaults
   ;               to "blem_rddirsex.awk"
   ; fil_1stqa  -> file of first QA results for some (or perhaps, all) 
   ;               sources.  File should have four columns
   ;               1 - ID (not used, really)
   ;               2 - X pixel
   ;               3 - Y pixel
   ;               4 - QA code for first QA check.
   ;
   ; G. Meurer  12/2002 - originally written
   ;            06/2004 - renovated
   ;            08/2005 - add fil_1stqa
   COMMON sensmod, lams, sens, esens, hsens
   forward_FUNCTION blem_collapsex
   qq      = ' '
   buf     = 5
   wpos0   = [900, 650]
   wpos1   = [500, 650]
   wpos2   = [0, 650]
   wszct1  = [300, 3*150]
   wszct2  = 1.25*wszct1
   wszcc1  = [300, 2*150]
   wszcc2  = 1.25*wszcc1
   mpk     = 11           ; max peaks + 1
   fnorm   = 1.0e-18
   clstar_min = 0.5
   tol1qa     = 2.0       ; tolerance in pixels for match to prev 1st qa position
   ;
   ; set default output directory
   IF keyword_set(outdir) THEN BEGIN 
      odir = strtrim(outdir,2)
      ns   = strlen(odir)
      IF strmid(odir,ns-1,1) NE '/' THEN odir = odir+'/'
   ENDIF ELSE BEGIN 
      odir = ''
   ENDELSE 
   ;
   ; Open images:
   ; - direct image raw
   ; - direct image filtered
   ; - segmentation of direct image
   ; - grism image, raw
   ; - grism image, filtered & masked
   fits_read, filgr, imgr, hdgr
   fits_read, filgf, imgf, hdgf
   fits_read, fildr, imdr, hddr
   fits_read, fildf, imdf, hddf
   fits_read, filsg, imsg, hdsg
   exptime = sxpar(hdgr, 'EXPTIME')
   imsg    = long(imsg)              ; GRM 17 Jul, 2006
   setsensmod, filsens, extsens
   ;
   ; Determine buffer size around direct image compared to grism image
   szg  = size(imgr)
   szd  = size(imdr)
   dbuf = [szd[1] - szg[1], szd[2] -szg[2]] / 2
   ;
   ; get awk files
   IF NOT keyword_set(awkgrcat) THEN awkgrcat = 'blem_rdsex.awk'
   IF NOT keyword_set(awkdrcat) THEN awkdrcat = 'blem_rddrsex.awk'
   ;
   ; Read SExtractor filtered grism image catalog.
   blem_rdsex, catgf, idg, xg, yg, magg, ag, bg, thetag, fwhmg, flagg, class_star, $
    awkfil=awkgrcat
   nsource = n_elements(idg)
   ; nsource = 1 ; for testing
   ;
   ; Create empty arrays for results
   qualcut = make_array(nsource, value='0', /string)
   apeak   = make_array(nsource, value=-1.0, /float)
   ashift  = make_array(nsource, value=-1.0, /float)
   aw50    = make_array(nsource, value=-1.0, /float)
   cpeak   = make_array(nsource, value=-1.0, /float)
   cshift  = make_array(nsource, value=-1.0, /float)
   cw50    = make_array(nsource, value=-1.0, /float)
   cwl     = make_array(nsource, value=-1.0, /float)
   cprms   = make_array(nsource, value=-1.0, /float)
   cxp     = make_array(nsource, value=-1.0, /float)
   cyp     = make_array(nsource, value=-1.0, /float)
   cflx    = make_array(nsource, value=-1.0, /float)
   csexid  = make_array(nsource, value='-1', /string)
   qualcc  = make_array(nsource, value='9', /string)  ; default changed to 9
   pksngf  = make_array(nsource, value=-1.0, /float)
   pksndf  = make_array(nsource, value=-1.0, /float)
   ;
   ; Invert match transformation matrix if needed
   IF keyword_set(invmat) THEN BEGIN 
      cx     = -1.0*abcdef[0]
      cy     = -1.0*abcdef[3]
      amat   = [[abcdef[1], [abcdef[2]]], [abcdef[4], abcdef[5]]]
      tmat   = float(invert(amat, status, /double))
      IF status NE 0 THEN BEGIN 
         print, '**** ERRROR in BLIND_EMFIND.  Could not invert transformation matrix'
         print, '**** Status from INVERT : ', status
         return
      ENDIF 
   ENDIF ELSE BEGIN 
      cx     = abcdef[0]
      cy     = abcdef[3]
      tmat   = [[abcdef[1], [abcdef[2]]], [abcdef[4], abcdef[5]]]
   ENDELSE  
   print, cx, cy
   print, tmat
   ;
   ; For all remaining sources derive:
   ; - i0,i1: column extraction range grism image
   ; - id0,id1: column extraction range direct image
   ; - xd: approx position of direct image
   ; - row offset between direct and grism image
   ; - extraction aperture size
   ; - extraction aperture rows
   ; - (extraction row weights)
   ic       = fix(xg + 0.5)
   i0       = fix(xg - dxbeam[1] - 0.5*dxmed + 0.5)  > 0 < szg[1]
   i1       = fix(xg - dxbeam[0] + 0.5*dxmed + 0.5)  > 0 < szg[1]
   id0      = i0 + dbuf[1]
   id1      = i1 + dbuf[1]
   xda      = xg - 0.5*float(dxbeam[0] + dxbeam[1]) 
   yda      = yg - yoff_a[0] 
   dydx_a0  = eval_axe_poly(dydx_a_0, xda, yda, xref=xref, yref=yref)
   dydx_a1  = eval_axe_poly(dydx_a_1, xda, yda, xref=xref, yref=yref)
   yoff_aa  = eval_axe_poly(yoff_a, xda, yda, xref=xref, yref=yref)
   dydx     = dydx_a0 + 0.5*float(dxbeam[0] + dxbeam[1])*dydx_a1
   dya      = yoff_aa+dydx
   xda      = xda + dbuf[0] > 0 < szd[1]
   yda      = yda + dbuf[1] > 0 < szd[2]
   xd       = xda
   yd       = yg - dya + dbuf[1] > 0 < szd[2]
   ;
   ; convert ic,i0,i1,id0,id1 to array elements
   ic     = ic - 1
   i0     = i0 - 1
   i1     = i1 - 1
   id0    = id0 - 1
   id1    = id1 - 1
   ;
   thrad     = !dpi*thetag/180.0
   ap        = fwag*sqrt((ag*sin(thrad))^2 + (bg*cos(thrad))^2)
   k         = where(ap LT apmin, nk)
   IF nk GT 0 THEN ap[k] = apmin
   jg0       = fix(yg - 0.5*ap + 0.5) - 1
   jg1       = fix(yg + 0.5*ap + 0.5) - 1
   jd0       = fix(yd - 0.5*ap + 0.5) - 1
   jd1       = jd0 + (jg1 - jg0)
   ; 
   ; check corners of extraction apertures to test if whole aperture is 
   ; within good areas of grism image
   blem_chextrap, xg, yg, ap, dxbeam, dxmed, filsgoodg, apallgood
   apallflag = 1b - apallgood
   ;
   ; check if source could be a zeroth order image of an off field source
   blem_chzordoff, xg, yg, cx, cy, tmat, filsgoodd, zordpass
   zordflag  = 1b - zordpass
   ;
   ; read first QA results if file_1stqa is set.
   IF keyword_set(fil_1stqa) THEN BEGIN 
      readcol, fil_1stqa, id1qa, x1qa, y1qa, qa1qa, format='(i,f,f,a)'
      qa1qa = strupcase(strtrim(qa1qa,2))
      n1qa = long(n_elements(id1qa))
   ENDIF ELSE BEGIN 
      n1qa = 0l 
   ENDELSE 
   ;
   ; make sure extraction apertures are within image limits
   mi = min([szg[1], szd[1]])
   mj = min([szg[2], szd[2]])
   k = where(jg0 LT 0, nk)
   IF nk GT 0 THEN jg0[k] = 0
   k = where(i0 LT 0, nk)
   IF nk GT 0 THEN i0[k] = 0
   k = where(jg1 GT szg[2]-1, nk)
   IF nk GT 0 THEN jg1[k] = szg[1] - 1
   k = where(jd1 GT szd[2]-1, nk)
   IF nk GT 0 THEN jd1[k] = szd[2] - 1
   k = where(i1 GT szg[1], nk)
   IF nk GT 0 THEN i1[k] = szg[1] - 1
   ;
   nrow = max([jd1 - jd0, jg1 - jg0])
   ncol = i1 - i0
   ;
   ; Do aperture phot
   aprad  = 0.5*aper
   skyrad = max(aprad) + 2.0 + [0.0, float(ceil(max(aprad)))]
   phpadu = 1.0
   badpix = [-10000,60000]
   grm_aper,imgf,xg-1.,yg-1.,fluxa,efluxa,skya,eskya,phpadu,aper,skyrad,badpix,/flux
   ; Loop through all sources 
   ;  - use a while loop to allow back tracking
   ;    and other fanangling within loop.
   i = 0
   WHILE (i GE 0 AND i LT nsource) DO BEGIN 
      print, ' '
      print, ' ------------------------------------------- '
      print, ' '
      print, 'BLIND_EMFIND: working on source : ', idg[i]
      ;
      ; output filenames
      idstr    = strtrim(idg[i],2)
      fil1d    = odir+pfx1d + '_'+ idstr +'.dat'
      filgs1   = odir+pfxgs + '_sm_'+idstr +'.png'
      filgs2   = odir+pfxgs + '_lg_'+idstr +'.png'
      filmc    = pfx1d + '_' +idstr +'.ps'
      filcc    = odir+pfxcc + '_'+idstr + '.dat'
      ;
      ; extract postage stamps & ribbons. 
      i2       = min([max([fix(xg[i] - 0.5*szstmp + 0.5) - 1, 0]), szg[1]-1])
      i3       = min([max([fix(xg[i] + 0.5*szstmp + 0.5) - 1, 0]), szg[1]-1])
      j2       = min([max([fix(yg[i] - 0.5*szstmp + 0.5) - 1, 0]), szg[2]-1])
      j3       = min([max([fix(yg[i] + 0.5*szstmp + 0.5) - 1, 0]), szg[2]-1])
      j4       = min([max([fix(yd[i] - 0.5*szstmp + 0.5) - 1, 0]), szd[2]-1])
      j5       = min([max([fix(yd[i] + 0.5*szstmp + 0.5) - 1, 0]), szd[2]-1])
      bstmp_gr = bytscl(-1.0*imgr[i2:i3,j2:j3], min=min(-1.0*dzgr), max=max(-1.0*dzgr))
      bstmp_gf = bytscl(-1.0*imgf[i2:i3,j2:j3], min=min(-1.0*dzgf), max=max(-1.0*dzgf))
      brbnl_dr = bytscl(-1.0*imdr[id0[i]:id1[i],j4:j5],  min=min(-1.0*dzdr), max=max(-1.0*dzdr))
      brbnl_df = bytscl(-1.0*imdf[id0[i]:id1[i],j4:j5],  min=min(-1.0*dzdf), max=max(-1.0*dzdf))
      brbns_gr = bytscl(-1.0*imgr[i0[i]:i1[i],jg0[i]:jg1[i]], min=min(-1.0*dzgr), max=max(-1.0*dzgr))
      brbns_gf = bytscl(-1.0*imgf[i0[i]:i1[i],jg0[i]:jg1[i]], min=min(-1.0*dzgf), max=max(-1.0*dzgf))
      brbns_dr = bytscl(-1.0*imdr[id0[i]:id1[i],jd0[i]:jd1[i]], min=min(-1.0*dzdr), max=max(-1.0*dzdr))
      brbns_df = bytscl(-1.0*imdf[id0[i]:id1[i],jd0[i]:jd1[i]], min=min(-1.0*dzdf), max=max(-1.0*dzdf))
      ;
      ; make grayscale plots.  Output png.
      loadct,0
      blem_multigray, buf, 0, 1, bstmp_gr, bstmp_gf, brbnl_dr, brbnl_df, $
       brbns_gr, brbns_gf, brbns_dr, brbns_df, outpng=filgs1, wpos=wpos0
      blem_multigray, buf, 0, 2, bstmp_gr, bstmp_gf, brbnl_dr, brbnl_df, $
       brbns_gr, brbns_gf, brbns_dr, brbns_df, outpng=filgs2, wpos=wpos0
      ;
      ; Collapse 2D cuts to 1D
      spgf     = blem_collapsex(imgf[i0[i]:i1[i],jg0[i]:jg1[i]])
      spgr     = blem_collapsex(imgr[i0[i]:i1[i],jg0[i]:jg1[i]])
      spdf     = blem_collapsex(imdf[id0[i]:id1[i],jd0[i]:jd1[i]])
      spdr     = blem_collapsex(imdr[id0[i]:id1[i],jd0[i]:jd1[i]])
      ;
      ; Output ascii 1D cuts
      ni = i1[i] - i0[i] + 1
      openw, lu, fil1d, /get_lun
      printf,lu,'# column    grism_raw   direct_raw   grism_filt  direct_filt'
      FOR ii = 0, ni-1 DO printf,lu,ii+i0[i],spgr[ii],spdr[ii],spgf[ii],spdf[ii]
      free_lun,lu
      ;
      ; Create normalized "spectra" to correlate.  
      ; - For the filtered grism image this is zero except for a median 
      ;   window sized box centered on the suspected line.
      ; - Normalize by clipped rms
      ikp0      = fix(xg[i] - 0.5*dxmed + 0.5) - 1 - i0[i] > 0 < szg[1]
      ikp1      = fix(xg[i] + 0.5*dxmed + 0.5) - 1 - i0[i] > 0 < szg[2]
      spgf2     = 0.0*spgf
      spgf2[ikp0:ikp1] = spgf[ikp0:ikp1]
      grm_avsigclip, spgf, 5.0, 3, meangf, rmsgf, nuse, nrej, nit
      grm_avsigclip, spdf, 5.0, 3, meandf, rmsdf, nuse, nrej, nit
      spgf2     = (spgf2 - meangf)/rmsgf
      spdf2     = (spdf - meandf)/rmsdf
      pksngf[i] = max(spgf2)
      ;
      ; Determine likely ID of source
      rbns_sg   = imsg[id0[i]:id1[i],jd0[i]:jd1[i]]
      blem_sexids, rbns_sg, nid, id, imin, imax
      ;
      ; Make multiple plot of 1D cuts.
      x       = i0[i] + indgen(i1[i] - i0[i] + 1)
      dxoff   = id0[i] - id1[i]
      blem_multicut_hard, filmc, x, ic[i], spgr, spgf, spgf2, spdr, spdf, spdf2, idstr, $
       nid, id, imin, imax, outdir=outdir, dxoff=dxoff
      blem_multicut, 1, wszct2, x, ic[i], spgr, spgf, spgf2, spdr, spdf, spdf2, idstr, $
       nid, id, imin, imax, wpos=wpos1, dxoff=dxoff
      ;
      ; if fil_1stqa is set (n1qa > 0) then check and set previous 
      ; quality code
      qa1done = 0b 
      IF n1qa GT 0 THEN BEGIN 
         r1qa = sqrt((x1qa - xg[i])^2 + (y1qa - yg[i])^2)
         kk   = sort(r1qa)
         IF r1qa[kk[0]] LE tol1qa THEN BEGIN
            qset       = qa1qa[kk[0]]
            blem_qualcut, qq, lstep, go_on, qset=qset
            qualcut[i] = qq
            qa1done    = 1b
         ENDIF 
      ENDIF 
      ;
      ; get cut quality, unless source is already marked as a star
      IF NOT qa1done THEN BEGIN 
         IF qualcut[i] NE '3' THEN BEGIN 
            blem_qualcut, qq, lstep, go_on
            IF lstep LT -1 THEN lstep = nsource+1
            qualcut[i] = qq
         ENDIF ELSE BEGIN 
            print, 'BLIND_EMFIND: Skipping source already marked as star with id = '+csexid[i]
            go_on      = 0b
         ENDELSE 
      ENDIF 
      IF go_on THEN BEGIN 
         ;
         ; loop until a valid peak is selected
         REPEAT BEGIN 
            peak     = make_array(mpk, /float, value=-1.0)
            shift    = make_array(mpk, /float, value=-1.0)
            w50      = make_array(mpk, /float, value=-1.0)
            prms     = make_array(mpk, /float, value=-1.0)
            wl       = make_array(mpk, /float, value=-1.0)
            xp       = make_array(mpk, /float, value=-1.0)
            yp       = make_array(mpk, /float, value=-1.0)
            flx      = make_array(mpk, /float, value=-1.0)
            psexid   = make_array(mpk, /string, value='-1')
            ;
            ; Auto correlate & fit.
            icen     = ic[i] - i0[i]
            ;
            window,2,xsize=wszcc2[0],ysize=wszcc2[1],xpos=wpos2[0],ypos=wpos2[1]
            !P.MULTI = [0,1,2]
            afunc    = grm_cfunc(spgf2, spgf2)
            na       = n_elements(afunc)
            anorm    = abs(max(afunc)/20.0)
            title    = 'Grism auto correlation. Grism cat : '+idstr
            grm_cpeak_fit, afunc, anorm, afit, icen, lev, pk, sh, w, window=float(2.0*dxmed)
            npk      = 1
            apeak[i] = pk
            ashift[i]= sh 
            aw50[i]  = w 
            peak[0]  = apeak[i]
            shift[0] = ashift[i]
            w50[0]   = aw50[i]
            prms[0]  = apeak[i]/anorm
            xp[0]    = xg[i]
            yp[0]    = yg[i]
            blem_ccplot, icen, afunc, afit, 0.0, anorm, title=title
            ;
            ; Cross correlate & fit
            cfunc    = grm_cfunc(spdf2, spgf2)
            nc       = n_elements(cfunc)
            cfit_tot = 0.0*cfunc
            cresid   = cfunc - cfit_tot
            grm_avsigclip, cresid, thresh_rms, 3, cmean, crms, nuse, nrej, nit
            kk       = where((cresid-cmean) GT thresh_rms*crms, nhigh)
            doafit   = nhigh GT 0
            WHILE (doafit GT 0 AND npk LT mpk) DO BEGIN 
               doafit     = 0b
               grm_cpeak_fit, cresid, crms, cfit, icen, lev, pk, sh, w, $
                window=float(3.0*dxmed)
               peak[npk]  = pk 
               shift[npk] = sh
               w50[npk]   = w
               npk        = npk + 1
               cfit_tot   = cfit_tot + cfit
               cresido    = cfunc - cfit_tot
               grm_avsigclip, cresido, thresh_rms, 3, cmean, crms, nuse, nrej, nit
               kk       = where((cresido-cmean) GT thresh_rms*crms, nhigh)
               IF nhigh GT 0 THEN BEGIN 
                  blem_ccplot, icen, cresid, cfit, cmean, crms, title='Fit to residuals'
                  doafit = blem_addgauss()
                  blem_ccplot, icen, afunc, afit, 0.0, anorm, title=title
               ENDIF 
               cresid   = cresido
            ENDWHILE
            good   = where(cresid+cfit_tot-cmean LE thresh_rms*crms, ngood)
            IF ngood GT 0 THEN grm_avsigclip, cfunc[good], thresh_rms, 3, cmean, crms, nuse, nrej, nit 
            title  = 'Grism - direct cross-correlation. Grism cat : '+idstr
            blem_ccplot, icen, cfunc, cfit_tot, cmean, crms, title=title
            ;
            ; Query acceptability of cross correlation, ID
            IF npk GT 1 THEN BEGIN 
               nn           = npk - 1
               prms[1:nn]   = peak[1:nn]/crms
               shh          = shift[1:nn]
               xpp          = xg[i] + shh + dbuf[0]
               ypp          = yd[i]
               dxx          = -1.0*shift[1:nn]
               wll          = axe_wl(dxx, xpp, ypp, dldp_a_0, dldp_a_1, dldp_a_2, xref=xref, yref=yref)
               dwll         = axe_wl(dxx+1, xpp, ypp, dldp_a_0, dldp_a_1, dldp_a_2, xref=xref, yref=yref) - wll
               sxx          = blem_assign_sexid(shh, icen, nid, id, imin, imax)
               ; slam         = interpol(sens,lams,wll)*dwll*exptime*fnorm
               slam         = interpol(sens,lams,wll)*exptime*fnorm

               flx[1:nn]    = fluxa[i]/slam
               xp[1:nn]     = xpp
               yp[1:nn]     = ypp
               wl[1:nn]     = wll
               psexid[1:nn] = sxx
            ENDIF 
            ; 
            ; show user cross correlation results
            blem_ccout, npk, xp, yp, peak, shift, w50, wl, prms, psexid
            ;
            ; select peak, store results
            selmeth   = ''
            ipk       = blem_selpeak(npk, selmeth=selmeth)
         ENDREP UNTIL ipk LE npk-1 
         IF ipk LT 0 THEN BEGIN
            ;
            ; user wants to repeat plots of cuts
            lstep = 0
         ENDIF ELSE BEGIN 
            IF ipk GT 0 THEN BEGIN 
               ;
               ; store results for selected peak 
               cpeak[i]   = peak[ipk]
               cshift[i]  = shift[ipk]
               cw50[i]    = w50[ipk]
               cwl[i]     = wl[ipk]
               cprms[i]   = prms[ipk]
               cxp[i]     = xp[ipk]
               cyp[i]     = yp[ipk]
               cflx[i]    = flx[ipk]
               csexid[i]  = psexid[ipk]
               ;
               ; find S/N peak in filtered direct image centered near 
               ; source center
               jkp0       = max([min([ikp0 + fix(cshift[i]+0.5),i1[i] - i0[i]]),0])
               jkp1       = max([min([ikp1 + fix(cshift[i]+0.5),i1[i] - i0[i]]),0])
               pksndf[i]  = max(spdf2[jkp0:jkp1])
            ENDIF 
            ;
            ; set cross cor quality flag
            killcc    = 0b
            qualcc[i] = blem_qualcc(npk, killcc=killcc)
            IF killcc THEN npk = 1
            ;
            ; write output, unless user requests source be redone
            IF qualcc[i] EQ 'R' THEN lstep = 0 ELSE $
             blem_ccout, npk, xp, yp, peak, shift, w50, wl, prms, psexid, $
             file=filcc, selmeth=selmeth, selccid=ipk, qualcc=qualcc[i]
         ENDELSE 
      ENDIF ELSE BEGIN 
         ;qualcc[i] = '9'
         ;
         ; check if the last qualcut was a star and previously 
         ; unidentified.  If so then identify star and 
         ; grism sources associated with it.
         IF qq EQ '3' AND csexid[i] EQ '-1' THEN BEGIN 
            blem_fndstar, id0[i], id1[i], jd0[i], jd1[i], catdr, idstar, xstar, ystar, $
             clstar_min=clstar_min, awkfil=awkdrcat
            ;
            ; proceed if one star found
            IF idstar[0] GE 0 THEN $
             blem_flgstars, xstar, ystar, idstar, yd[i]-yg[i], ap[i], dxbeam, $
                            xg, yg, qualcut, csexid, xd, yd, nflg, dxdg=dbuf[1]
         ENDIF 
      ENDELSE 
      i = max([i + lstep,0])
   ENDWHILE 
   ;
   ; Output results database
   fmt0 = '(a)'
   fmt1 = 'i5,f8.2,f8.2,f8.2,f8.3,f5.1,f5.1,f7.1,f7.2,i3,i4,i4'
   fmt2 = 'f7.1,f7.1,f9.1,f7.1,1x,a3,1x,f8.3,f7.2,f6.2'
   fmt3 = '1x,a3,1x,f8.3,f8.2,f6.2,f8.2,f8.2,f8.2,f8.2,f7.2,a8'
   fmt  = '('+fmt1+','+fmt2+','+fmt3+')'
   hdr  = '# idg    xg      yg      yd     magg   ag   bg  thetag  fwhmg flg fla flz '+$
          'pksngf pksndf  fluxa    skya qcut   apeak  ashift aw50  '+$
          'qcc   cpeak  cshift  cw50   cwl     cprms   cxp     cyp     cflx  csexid'
   openw,lu,odir+filout,/get_lun
   printf, lu, hdr, format=fmt0
   printf, -1, hdr, format=fmt0
   FOR i = 0, nsource-1 DO BEGIN 
      printf,lu,idg[i],xg[i],yg[i],yd[i],magg[i],ag[i],bg[i],thetag[i],fwhmg[i],flagg[i],apallflag[i],zordflag[i],$
       pksngf[i],pksndf[i],fluxa[i],skya[i],qualcut[i],apeak[i],ashift[i],aw50[i],$
       qualcc[i],cpeak[i],cshift[i],cw50[i],cwl[i],cprms[i],cxp[i],cyp[i],$
       cflx[i],csexid[i],$
       format=fmt
      printf,-1,idg[i],xg[i],yg[i],yd[i],magg[i],ag[i],bg[i],thetag[i],fwhmg[i],flagg[i],apallflag[i],zordflag[i],$
       pksngf[i],pksndf[i],fluxa[i],skya[i],qualcut[i],apeak[i],ashift[i],aw50[i],$
       qualcc[i],cpeak[i],cshift[i],cw50[i],cwl[i],cprms[i],cxp[i],cyp[i],$
       cflx[i],csexid[i],$
       format=fmt
   ENDFOR 
   free_lun,lu
END
