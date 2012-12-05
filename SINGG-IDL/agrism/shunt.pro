PRO shunt, fgrism, mask_files, pfxo, zrange, verbose=verbose, title=title, $
           fdirect=fdirect, zrange_direct=zrange_direct, nobreak=nobreak, notpears=notpears, $
           direct_crpix_offset=direct_crpix_offset, direct_cat=direct_cat, $
           snmin=snmin, mmax=mmax
   ;
   ; Supernova Hunt
   ;
   ; Search for supernova in 2d grism image
   ;
   ; G. Meurer 09/2005
   ; G. Meurer ~12/2005  added dybuf to shunt_setup
   ; G. Meurer 02/2006 remove xoffset, replace with parx, pary giving 
   ;                   coordinate transformation coefficients
   ;                   add 'D' (for default) spectral class, snmin, mmax
   fmtdr      = '(f,f,l,f,f,f,f,f,f,f,f,f,f,f,f)'
   shdir = '/home/meurer/ACS/Grism/PEARS/SHUNT_setup/'
   ;
   ; setup parameters and copy files for shunt
   shunt_setup, shdir, xbin, dxrange, dyrbn, dyspec, dybuf, dxcrcut, dxstamp, dystamp, awkstr, $
                parx, pary, exdirct, eydirct, keep_class, keep_color, verbose=verbose
   IF n_elements(zrange_direct) EQ 2 THEN zrange_d = zrange_direct ELSE zrange_d = zrange
   zrange_g = zrange
   IF NOT keyword_set(title) THEN title = 'SHUNT classifications of field : '+pfxo
   ;
   ; set some default parameters if not passed
   IF keyword_set(snmin) THEN minsn = snmin ELSE minsn = 0.0
   IF keyword_set(mmax) THEN maxm = mmax ELSE maxm = 0.0
   ;
   ; Prepare images for cataloging
   IF keyword_set(notpears) THEN $
    shunt_prep, fgrism, mask_files, xbin, pfxo, fgsci, fgtrim, fgmask, fgsquash, fgsqmask, $
                exptime, nobreak=nobreak, verbose=verbose $
    ELSE $
    shunt_prep_pears, fgrism, fdirect, mask_files, xbin, pfxo, zrange_g, zrange_d, $
                      fgsci, fgtrim, fgmask, fgsquash, fgsqmask, fdsci, verbose=verbose
   zrange_sq = float(xbin)*zrange_g
   ;
   ; fix pixel offset in direct image
   IF n_elements(direct_crpix_offset) EQ 2 THEN BEGIN 
      IF keyword_set(verbose) THEN PRINT, 'SHUNT: fixing CRPIX1,2 in image : '+fdsci
      fits_read, fdsci, imd, hdd
      crpix1 = sxpar(hdd, 'crpix1')+direct_crpix_offset[0]
      crpix2 = sxpar(hdd, 'crpix2')+direct_crpix_offset[1]
      sxaddpar, hdd, 'crpix1', crpix1, /savecomment
      sxaddpar, hdd, 'crpix2', crpix2, /savecomment
      fits_write, fdsci, imd, hdd
   ENDIF 
   ;
   ; Catalog images
   shunt_cat, fgsquash, awkstr, fcat, fecat, ffilt, fsegm, fback, verbose=verbose
   ;
   ; read input catalog
   shunt_readcat, fecat, nobj, id, xim, yim, mag, krad, backg, aim, bim, thim, w50, class, verbose=verbose
   ;
   ; make arrays to store classifications, and other key parameters
   spec_class =  make_array(nobj, /string, value='N')
   ii         =  0
   cl         = ''
   xdirect    =  make_array(nobj, /float, value=-1.0)
   ydirect    =  make_array(nobj, /float, value=-1.0)
   bx_specx   =  make_array(nobj, 4, /long, value=-1l)
   bx_specy   =  make_array(nobj, 4, /long, value=-1l)
   bx_directx =  make_array(nobj, 4, /float, value=-1.0)
   bx_directy =  make_array(nobj, 4, /float, value=-1.0)
   sky_lev    =  make_array(nobj, /float, value=-1.0)
   sky_sig    =  make_array(nobj, /float, value=-1.0)
   max_sn     =  make_array(nobj, /float, value=-1.0)
   ;
   ; read in grism image, and squashed grism image
   IF keyword_set(verbose) THEN print, 'SHUNT: reading grism and squashed grism images. '
   fits_read, fgsci, im_grism, hdrg
   fits_read, fgsquash, im_squash, hdrs
   ;
   ; Loop through sources, brightest first
   kk    = sort(mag)
   REPEAT BEGIN 
      jj    = kk[ii]
      xsq   = xim[jj]
      ysq   = yim[jj]
      iid   = id[jj]
      fps   = pfxo+'_shunt_'+strtrim(string(iid),2)+'.ps'
      fjpg  = pfxo+'_shunt_'+strtrim(string(iid),2)+'.jpg'
      print, 'Print working on spectrum id#: '+strtrim(string(iid),2)+' ('+strtrim(string(ii+1),2)+'/'+strtrim(string(nobj),2)+')'
      ;
      ; extract cutouts
      shunt_extract, im_grism, im_squash, xbin, xsq, ysq, dxrange, $
       dyrbn, dyspec, dybuf, dxcrcut, dxstamp, dystamp, $
       gr_ribbon, gr_spec, gr_sky, gr_crcut, sq_stamp, $
       lim_ribbon, lim_spec, lim_crcut, lim_stamp, $
       skylev, skysig, maxsn
      ;
      ; make plots for classification
      gr_sky2 = float(dyspec)*gr_sky
      shunt_plot, iid, xsq, ysq, gr_ribbon, gr_spec, gr_sky2, gr_crcut, sq_stamp, $
                  lim_ribbon, lim_spec, lim_crcut, lim_stamp, zrange_g, zrange_sq
      im = tvrd(true=3)
      WRITE_JPEG,fjpg,im,TRUE=3,QUALITY=100
      shunt_plot, iid, xsq, ysq, gr_ribbon, gr_spec, gr_sky2, gr_crcut, sq_stamp, $
                  lim_ribbon, lim_spec, lim_crcut, lim_stamp, zrange_g, zrange_sq, $
                  hardfile=fps
      ;
      ; classify
      IF maxsn GE minsn AND mag[jj] LE maxm THEN cl  = shunt_classify() ELSE cl = 'D'
      ;
      ; fill up arrays
      spec_class[jj]   = cl
      xdirect[jj]      = parx[0] + parx[1]*xsq + parx[2]*ysq
      ydirect[jj]      = pary[0] + pary[1]*xsq + pary[2]*ysq
      bx_specx[jj,0]   = lim_spec[0]
      bx_specx[jj,1]   = lim_spec[1]
      bx_specx[jj,2]   = lim_spec[1]
      bx_specx[jj,3]   = lim_spec[0]
      bx_specy[jj,0]   = lim_spec[2]
      bx_specy[jj,1]   = lim_spec[2]
      bx_specy[jj,2]   = lim_spec[3]
      bx_specy[jj,3]   = lim_spec[3]
      bx_directx[jj,0] = xdirect[jj] - 0.5*exdirct
      bx_directx[jj,1] = xdirect[jj] + 0.5*exdirct
      bx_directx[jj,2] = xdirect[jj] + 0.5*exdirct
      bx_directx[jj,3] = xdirect[jj] - 0.5*exdirct
      bx_directy[jj,0] = ydirect[jj] - 0.5*eydirct
      bx_directy[jj,1] = ydirect[jj] - 0.5*eydirct
      bx_directy[jj,2] = ydirect[jj] + 0.5*eydirct
      bx_directy[jj,3] = ydirect[jj] + 0.5*eydirct
      sky_lev[jj]      = skylev
      sky_sig[jj]      = skysig
      max_sn[jj]       = maxsn
      ;
      ii = ii + 1
   ENDREP UNTIL ii EQ nobj-1 OR cl EQ 'Q'      
   ;
   ; keep only useful entries & calculate RA & Decs
   shunt_calcquants, hdrg, keep_class, id, xim, yim, mag, krad, backg, aim, bim, thim, w50, class, $
                     spec_class, xdirect, ydirect, bx_specx, bx_specy, bx_directx, bx_directy, $
                     ra, dec, bx_ra, bx_dec
   nobj = n_elements(id)
   ;
   ; write output catalog
   kk = strpos(fcat, '.cat')
   filo = strmid(fcat, 0, kk) + '_shunt.cat'
   shunt_catout, filo, id, xim, yim, mag, spec_class, bx_specx, bx_specy, ra, dec, bx_ra, bx_dec, $
                 sky_lev, sky_sig, max_sn
   ;
   ; write regions file
   freg = strmid(fcat, 0, kk) + '_shunt.reg'
   shunt_regout, freg, keep_class, keep_color, id, spec_class, bx_ra, bx_dec
   ;
   ; read in direct image, if available, then make direct image stamps
   IF keyword_set(fdirect) THEN BEGIN 
      IF keyword_set(verbose) THEN print, 'SHUNT: reading direct image. '
      fits_read, fdsci, im_direct, hdrd
      pfx_dstamp = pfxo+'_shunt_direct_stamp'
      FOR ii = 0, nobj-1 DO BEGIN 
         rad    = ra[ii]
         decd   = dec[ii]
         idd    = id[ii]
         bxra   = bx_ra[ii,*]
         bxdec  = bx_dec[ii,*]
         fstamp = pfx_dstamp+'_'+strtrim(string(idd),2)+'.jpg'
         fhard  = pfx_dstamp+'_'+strtrim(string(idd),2)+'.ps'
         shunt_direct_stamp, im_direct, hdrd, dxstamp, dystamp, zrange_d, idd, rad, decd, bxra, bxdec
         im     = tvrd(true=3)
         WRITE_JPEG,fstamp,im,TRUE=3,QUALITY=100
         shunt_direct_stamp, im_direct, hdrd, dxstamp, dystamp, zrange_d, idd, rad, decd, bxra, bxdec, $
                             hardfile=fhard
      ENDFOR 
   ENDIF ELSE BEGIN 
      pfx_dstamp = 0b
   ENDELSE  
   ;
   ; if passed read in direct catalog
   nmtch = make_array(nobj, /int, value=0)
   IF keyword_set(direct_cat) THEN BEGIN 
      readcol, direct_cat, ximd, yimd, iddrd, rawd, decwd, aimd, bimd, thimd, awd, bwd, thwd, $
       m435d, m606d, m775d, m850d, format=fmtdr
      ;
      ; go through objects and check for matches
      FOR ii = 0, nobj-1 DO BEGIN 
         bxra  = reform(bx_ra[ii,*],4)
         bxdec = reform(bx_dec[ii,*],4)
         qq    = inside(rawd, decwd, bxra, bxdec, /index)
         IF qq[0] EQ -1 THEN nmtch[ii] = 0 ELSE nmtch[ii] = n_elements(qq)
      ENDFOR 
      ;
      ; find cases where there are no matches, and spectral clas NE 'O'
      qq = where(nmtch LE 0 AND spec_class NE 'O', nqq)
      IF nqq GT 0 THEN BEGIN 
         print, 'Cases with no direct image source in error box:'
         forprint, id[qq], ' '+spec_class[qq]+' ', qq
      ENDIF ELSE print, nqq
   ENDIF 
   ;
   ; write html output
   fhtml = strmid(fcat, 0, kk) + '_shunt.html'
   pfx_plots = pfxo+'_shunt'
   shunt_html_out, fhtml, filo, title, id, xim, yim, spec_class, mag, ra, dec, $
                    pfx_plots, pfx_dstamp=pfx_dstamp
END 
