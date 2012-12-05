PRO hdfn_collate_all
   ;
   ; collate the GOODSN photometry and Hawaii redshifts matched to
   ; the objects in the IDT detection image.  Put the compiled results 
   ; in an IDL database.
   ;
   ; G. Meurer 08/2004
   ;
   ; setup
   filif    = '/home/meurer/ACS/Grism/HDFN_jun04/Apsis/Work/Match/table4_fly99.dat'
   filic    = '/home/meurer/ACS/Grism/HDFN_jun04/Apsis/Work/hdf.Rcnt.photz.cat'
   fmtf     = '(i4,15x,a12,1x,a12,1x,a13,1x,f6.2,6x,f6.3,i2)'
   fmtc     = '(l,d,d,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f)'
   wd       = '/home/meurer/ACS/Grism/HDFN_jun04/Apsis/Work/'
   detcat   = 'detectionImage.cat'
   detim    = 'detectionImage.fits'
   bpzcat   = 'goodsn_idt_phot.bpz'
   fmtd     = '(l,f,f,f,f,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,f,x,'+ $
               'x,x,x,x,x,d,d,x,x,x,x,x,x,f,f,x,x,f,f,x,x,x,x,x,x,x,x,x,x,'+ $
               'x,x,x,f,x,x,x,x,x,x,x,x,l,f)'
   goodscat = 'goodsn_idt_mast.cat'
   mccat    = 'multicolor.cat'
   zpdet    = 36.41         ; zeropoint detection image
   pixsz    = 0.05          ; pixel scale in arcsec
   rmatch   = 0.20          ; match radius in arcsec
   defp     = -99.00        ; default value for positions
   defm     = -99.0         ; default value for photometry
   defz     =  -1.0         ; set undefined redshifts to this
   deff     =   0b          ; set undefined flags to this
   defs     =  -1           ; set undefined sources to this
   deft     =  -1.0         ; set undefined BPZ templates to this
   dmagimag =  1.70         ; detection image mag - imag should be near this for match
   ;
   ; definition of fields for structure and items for db
   item      =  ['id', 'ra', 'dec', 'x', 'y', 'kron_rad', 'a_image', 'b_image', $
                 'theta_image', 'theta_world', 'fwhm_image', 'flags', 'class', $
                 'det_mag', 'i_mag', 'err_i_mag', 'z_mag', 'err_z_mag', $
                 'iau_g', 'id_g', 'ra_g', 'dec_g', 'x_g', 'y_g', $
                 'z_mag_g', 'err_z_mag_g', 'i_mag_g', 'err_i_mag_g', 'v_mag_g', $
                 'err_v_mag_g', 'b_mag_g', 'err_b_mag_g', 'class_g', $
                 'z_b_1', 'z_b_min_1', 'z_b_max_1', 't_b_1', 'odds_1', $
                 'z_b_2', 'z_b_min_2', 'z_b_max_2', 't_b_2', 'odds_2', $
                 'z_b_3', 'z_b_min_3', 'z_b_max_3', 't_b_3', 'odds_3', $
                 'z_ml', 't_ml', 'chi_squared', 'm_0', $
                 'ra_h', 'dec_h', 'x_h', 'y_h', 'z_spec_h', 'flag_z_spec_h', $
                 'source_z_spec_h', 'z_phot_h', 'flag_z_phot_h', 'z_phot_odds_h', $
                 'hk_mag_h', 'z_mag_h', 'i_mag_h', 'r_mag_h', 'v_mag_h', 'b_mag_h', 'u_mag_h', $
                 'id_f', 'ra_f', 'dec_f', 'x_f', 'y_f', 'z_phot_f', 't_f', 'i_mag_f', 'name_hdfn_f', $
                 'id_c', 'ra_c', 'dec_c', 'x_c', 'y_c', 'z_b_c', 'z_b_min_c', 'z_b_max_c', 't_b_c', $
                 'odds_c', 'z_ml_c', 't_ml_c', 'chi_c', 'u_mag_c', 'b_mag_c', 'v_mag_c', 'r_mag_c', $
                 'i_mag_c', 'z_mag_c', 'hk_mag_c']
   tout      = ['I', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'I', 'R', 'R', 'R', 'R', 'R', 'R', $
                'C', 'I', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', $
                'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', $
                'R', 'R', 'R', 'R', 'R', 'B', 'I', 'R', 'B', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', $
                'I', 'R', 'R', 'R', 'R', 'R', 'I', 'R', 'C', $
                'I', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R']
   tlen      = [4, 8, 8, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, $
                0, 4, 8, 8, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, $
                4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, $
                8, 8, 4, 4, 4, 1, 2, 4, 1, 4, 4, 4, 4, 4, 4, 4, 4, $
                2, 8, 8, 4, 4, 4, 2, 4, 0, $
                4, 8, 8, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4]
   dstring   = 'j,d,d,f,f,f,f,f,f,f,f,j,f,f,f,f,f,f,'+ $
               'a,j,d,d,f,f,f,f,f,f,f,f,f,f,f,'+ $
               'f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,'+$
               'd,d,f,f,f,b,i,f,b,f,f,f,f,f,f,f,f,'+$
               'i,d,d,f,f,f,i,f,a,'+$
               'j,d,d,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f'
   descript  = ['ID', 'RA (J2000) [deg]', 'Dec (J2000) [deg]', 'X position [pixels]', 'Y position [pixels]', $
                'Kron radius ', 'A (semi-major axis size) [pixels]', 'B (semi-minor axis size) [pixels]', $
                'PA (image) [deg]', 'PA (world) [deg]', 'FWHM [pixels]', 'Sextractor flags', 'Stellaricity', $
                'detectionImage mag [ABmag]', 'I mag (BPZ) [ABmag]', 'Error in I mag [ABmag]', 'Z mag (BPZ) [ABmag]', 'Error in Z mag [ABmag]', $
                'IAU name - GOODS', 'ID - GOODS', 'RA - GOODS (J2000) [deg]', 'Dec - GOODS (J2000) [deg]', $
                'X position - GOODS [pixels]', 'Y position - GOODS [pixels]', 'z mag - GOODS [ABmag]', 'Error in z mag - GOODS [ABmag]', $
                'i mag - GOODS [ABmag]', 'Error in i mag - GOODS [ABmag]', 'V mag - GOODS [ABmag]', 'Error in V mag - GOODS [ABmag]', $
                'B mag - GOODS [ABmag]', 'Error in B mag - GOODS [ABmag]', 'Stellaricity - GOODS', $
                'Phot redshift (peak 1)', 'Min(phot redshift) (peak 1)', 'Max(phot redshift) (peak 1)', 'Template (peak 1)', 'Odds (peak 1)', $
                'Phot redshift (peak 2)', 'Min(phot redshift) (peak 2)', 'Max(phot redshift) (peak 2)', 'Template (peak 2)', 'Odds (peak 2)', $
                'Phot redshift (peak 3)', 'Min(phot redshift) (peak 3)', 'Max(phot redshift) (peak 3)', 'Template (peak 3)', 'Odds (peak 3)', $
                'Phot redshift - max liklihood', 'Template - max liklihood', 'Chi squared', 'Fiducial mag', $
                'RA - Hawaii (J2000) [deg]', 'Dec - Hawaii (J2000) [deg]', 'X position - Hawaii [pixels]', 'Y position - Hawaii [pixels]', $
                'Spectroscopic redshift - Hawaii', 'Spec redshift flag - Hawaii', 'Spec redshift source - Hawaii', $
                'Photometric redshift - Hawaii', 'Photometric redshift flag - Hawaii', 'Photometric redshift odds - Hawaii', $
                'HK mag - Hawaii [ABmag]', 'z mag - Hawaii [ABmag]', 'i mag - Hawaii [ABmag]', 'r mag - Hawaii [ABmag]', $
                'v mag - Hawaii [ABmag]', 'B mag - Hawaii [ABmag]', 'U mag - Hawaii [ABmag]', $
                'ID - FLY99', 'RA - FLY99 (J2000) [deg]', 'Dec - FLY99 (J2000) [deg]', 'X position - FLY99 [pixels]', 'Y position - FLY99 [pixels]', $
                'Photometric redshift - FLY99', 'Photometric type - FLY99', 'I_814 mag - FLY99 [ABmag]', 'Name in original HDFN cat (from FLY99)', $
                'ID - Capak', 'RA - Capak (J2000) [deg]', 'Dec - Capak (J2000) [deg]', 'X position - Capak [pixels]', 'Y position - Capak [pixels]', $
                'Best phot redshift - Capak', 'min(phot redshift) - Capak', 'max(phot redshift) - Capak', 'Template - Capak', 'Odds - Capak', $
                'Phot redshift: max liklihood - Capak', 'Template:max liklihood - Capak', 'Chi - Capak', $
                'HK mag - Capak [ABmag]', 'z mag - Capak [ABmag]', 'i mag - Capak [ABmag]', 'r mag - Capak [ABmag]', $
                'v mag - Capak [ABmag]', 'B mag - Capak [ABmag]', 'U mag - Capak [ABmag]']
   dbtitle   = 'HDF-N IDT collated data'
   filo      = 'hdfn_idt'
   indblk    = ['id', 'ra', 'dec', 'x', 'y', 'det_mag', 'z_spec_h', 'source_z_spec_h', 'z_b_c', $
                'id_g', 'i_mag_g', 'z_mag_g', 'v_mag_g', 'b_mag_g', 'hk_mag_c', 'u_mag_c', 'z_phot_f', 't_f']
   ;
   ; trans structure for hawaii catalog
   ah        = -6.507098039    
   bh        =  1.000231223     
   ch        =  0.001695950     
   dh        =  9.200322721     
   eh        = -0.001646151    
   fh        =  1.000310652     
   ;
   ; trans structure for GOODS catalog
   ag        = -6.494578686
   bg        =  1.000268634
   cg        =  0.001649286
   dg        =  9.207672369
   eg        = -0.001638957
   fg        =  1.000303173 
   ;
   ; trans structure for FLY99 catalog
   af        = -2.030026187
   bf        =  0.999551219
   cf        =  0.002221419
   df        =  7.322390192
   ef        = -0.002895114
   ff        =  1.000134379
   ;
   ; trans structure for capak catalog
   ac        =  -5.767623626    
   bc        =   1.000759887     
   cc        =   0.001336394     
   dc        =   5.397664976     
   ec        =  -0.002130544    
   fc        =   1.000337764 
   ;
   cd, wd, current=cwd
   ;
   ; read in header of detection image
   fits_read, detim, data, hdr, /header_only
   naxis1    = sxpar(hdr, 'naxis1')
   naxis2    = sxpar(hdr, 'naxis1')
   ;
   ;  calculate match radius in pixels and x,y pixel range
   ;  for good objects
   rmatchp   = rmatch/pixsz
   x1        = -1.0*rmatchp       
   x2        = float(naxis1+rmatchp)
   y1        = -1.0*rmatchp       
   y2        = float(naxis2+rmatchp)
   ;
   ; read in detection image catalog, everything will
   ; be collated to this.
   readcol, detcat, id, x, y, det_mag, errmag, kron_rad, ra, dec, a_image, b_image, theta_image, theta_world, fwhm_image, flags, class, $
    format=fmtd
   det_mag   = zpdet + det_mag
   rmatchp2  = (0.5*fwhm_image+rmatchp)^2
   nid       = n_elements(id)
   ;
   ; plot positions
   setplotcolors
   plot, x, y, xrange=[x1,x2], yrange=[y1,y2], xstyle=1, ystyle=1, psym=1, $
    xtitle='X [pixels]', ytitle='Y [pixels]'
   ;
   ; make arrays to store multicolor info
   i_mag      = make_array(nid, /float, value=defm)
   err_i_mag  = make_array(nid, /float, value=defm)
   z_mag      = make_array(nid, /float, value=defm)
   err_z_mag  = make_array(nid, /float, value=defm)
   ;
   ; read in multicolor catalog
   readcol, mccat, idm, imagm, eimagm, zmagm, ezmagm, $
    format='(l,x,x,x,x,x,x,f,f,x,x,x,f,f)'
   nm        = n_elements(idm)
   indm      = make_array(nid, /long, value=-1l)
   ;
   ; match the multicat entries to the detection Image cat.
   FOR i = 0, nm-1 DO BEGIN 
      j = where(id EQ idm[i], nj)
      IF nj EQ 0 THEN BEGIN 
         print, 'No match for for multicat source : ', idm[i]
         stop
      ENDIF ELSE BEGIN 
         IF nj GT 1 THEN BEGIN 
            jj  = sort(abs(det_mag[j] - imagm[k[i]] - dmagimag))
            j   = temporary(j[jj])
            print, 'Multimatch, n = '+strtrim(nj,2)+' source =  ', idm[i]
            stop
         ENDIF 
         indm[i] = j[0]
      ENDELSE 
   ENDFOR 
   ;
   ; populate relevant arrays
   j      = where(indm GE 0l, nj)
   i_mag[indm[j]]     =  imagm[j]
   z_mag[indm[j]]     =  zmagm[j]
   err_i_mag[indm[j]] =  eimagm[j]
   err_z_mag[indm[j]] =  ezmagm[j]
   ;
   print, 'Number of matches to multicolor catalog: ', nj, ' / ', nid
   ;
   ; plot matches
   oplot, x[indm[j]], y[indm[j]], psym=sym(6), color=!black
   ;
   ; make arrays to store matched goods results
   iau_g       = make_array(nid, /string, value='')
   ra_g        = make_array(nid, /float, value=defp)
   dec_g       = make_array(nid, /float, value=defp)
   x_g         = make_array(nid, /float, value=defp)
   y_g         = make_array(nid, /float, value=defp)
   z_mag_g     = make_array(nid, /float, value=defm)
   i_mag_g     = make_array(nid, /float, value=defm)
   v_mag_g     = make_array(nid, /float, value=defm)
   b_mag_g     = make_array(nid, /float, value=defm)
   err_z_mag_g = make_array(nid, /float, value=defm)
   err_i_mag_g = make_array(nid, /float, value=defm)
   err_v_mag_g = make_array(nid, /float, value=defm)
   err_b_mag_g = make_array(nid, /float, value=defm)
   id_g        = make_array(nid, /long, value=-1l)
   class_g     = make_array(nid, /float, value=defp)
   ;
   ; read in GOODS catalog
   readcol, goodscat, ind, xidtg, yidtg, zmagg, ezmagg, bmagg, ebmagg, vmagg, evmagg, imagg, emagg, $
            idiaug, rag, decg, sectg, areag, theta_imageg, kron_radg, fradg, fwhmg, classg, a_imageg, b_imageg, idg, $
            format='(l,f,f,f,f,f,f,f,f,f,f,a,d,d,i,f,f,f,f,f,f,f,f,l)'
   ng        = n_elements(idg)
   indg      = make_array(ng, /long, value=-1l)
   ;
   ; (re)calculate positions in detection Image
   adxy, hdr, rag, decg, xidt, yidt
   xidt      = xidt + 1.0
   yidt      = yidt + 1.0
   xidtg     = ag + bg*xidt + cg*yidt
   yidtg     = dg + eg*xidt + fg*yidt
   ;
   ; find GOODS sources that may be in ACS FOV
   k         = where(xidtg GE x1 AND xidtg LE x2 AND yidtg GE y1 AND yidtg LE y2, nk)
   print, 'Max number of Hawaii sources, that may match: ', nk
   ;
   ;
   ; loop through those sources and try to find matches
   IF nk GT 0 THEN BEGIN 
      FOR i = 0, nk-1 DO BEGIN 
         r2  = (x - xidtg[k[i]])^2 + (y - yidtg[k[i]])^2
         j   = where(r2 LT rmatchp2, nj)
         IF nj EQ 0 THEN BEGIN 
            print, 'No match for GOODS source at expected pos: ', xidtg[k[i]], yidtg[k[i]]
         ENDIF ELSE BEGIN 
            str = 'Match : ' 
            IF nj GT 1 THEN BEGIN 
               jj  = sort(abs(det_mag[j] - imagg[k[i]] - dmagimag))
               j   = temporary(j[jj])
               str = 'Multimatch, n = '+strtrim(nj,2)+' : '
            ENDIF 
            indg[k[i]] = j[0]
            print, str+'GOODS pos,mag: ', xidtg[k[i]],yidtg[k[i]],imagg[k[i]], ' IDT pos,mag: ',x[j[0]], y[j[0]], det_mag[j[0]], imagg[k[i]]-det_mag[j[0]]
         ENDELSE 
      ENDFOR 
      ;
      ; populate relevant arrays
      j      = where(indg GE 0l, nj)
      iau_g[indg[j]]       =  idiaug[j]
      ra_g[indg[j]]        =  rag[j]
      dec_g[indg[j]]       =  decg[j]
      x_g[indg[j]]         =  xidtg[j]
      y_g[indg[j]]         =  yidtg[j]
      z_mag_g[indg[j]]     =  zmagg[j]
      i_mag_g[indg[j]]     =  imagg[j]
      v_mag_g[indg[j]]     =  vmagg[j]
      b_mag_g[indg[j]]     =  bmagg[j]
      err_z_mag_g[indg[j]] =  ezmagg[j]
      err_i_mag_g[indg[j]] =  ebmagg[j]
      err_v_mag_g[indg[j]] =  evmagg[j]
      err_b_mag_g[indg[j]] =  ebmagg[j]
      id_g[indg[j]]        =  idg[j]
      class_g[indg[j]]     =  classg[j]
      ;
      print, 'Number of matches to GOODS catalog: ', nj
      ;
      ; plot possible matches, and final matches
      oplot, xidtg[k], yidtg[k], psym=7, color=!purple
      oplot, xidtg[j], yidtg[j], psym=7, color=!red
      ;
      k                  = where(det_mag GE 0.0 AND i_mag_g GE 0.0 AND ra_g GE 0.0)
      dmag               = det_mag[k] - i_mag_g[k]
      grm_avsigclip, dmag, 3.0, 50, mean, sigma, nuse, nrej, nit, /verbose
   ENDIF 
   ;
   ; Make arrays for BPZ results
   z_b_1           = make_array(nid,/float,value=defz)
   z_b_min_1       = make_array(nid,/float,value=defz)
   z_b_max_1       = make_array(nid,/float,value=defz)
   t_b_1           = make_array(nid,/float,value=deft)
   odds_1          = make_array(nid,/float,value=defz)
   z_b_2           = make_array(nid,/float,value=defz)
   z_b_min_2       = make_array(nid,/float,value=defz)
   z_b_max_2       = make_array(nid,/float,value=defz)
   t_b_2           = make_array(nid,/float,value=deft)
   odds_2          = make_array(nid,/float,value=defz)
   z_b_3           = make_array(nid,/float,value=defz)
   z_b_min_3       = make_array(nid,/float,value=defz)
   z_b_max_3       = make_array(nid,/float,value=defz)
   t_b_3           = make_array(nid,/float,value=deft)
   odds_3          = make_array(nid,/float,value=defz)
   z_ml            = make_array(nid,/float,value=defz)
   t_ml            = make_array(nid,/float,value=deft)
   chi_squared     = make_array(nid,/float,value=defz)
   m_0             = make_array(nid,/float,value=defm)
   ;
   ; read in Txitxo's BPZ catalog
   readcol, bpzcat, idb, zb1b, zbmin1b, zbmax1b, tb1b, odds1b, zb2b, zbmin2b, zbmax2b, tb2b, odds2b, $
                    zb3b, zbmin3b, zbmax3b, tb3b, odds3b, zmlb, tmlb, chisqb, m0b, iaub, $
                    format='(i,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,a)'
   nb              = n_elements(iaub)
   indb            = make_array(nid,/long,value=-1L)
   ;
   ; find GOODS sources that can be matched to BPZ catalog
   k               = where(iau_g NE '', nk)
   ;
   ; loop through those sources and match to BPZ cat
   IF nk GT 0 THEN BEGIN 
      FOR i = 0, nk-1 DO BEGIN 
         j         = where(iaub EQ iau_g[k[i]], nj)
         IF nj EQ 0 THEN BEGIN 
            print, 'No BPZ sources match GOODS source : '+iau_g[k[i]]
         ENDIF ELSE BEGIN 
            IF nj EQ 1 THEN BEGIN 
               indb[k[i]] = j[0]
            ENDIF ELSE BEGIN 
               stop, '**** Multiple matches for source : '+iau_g[k[i]]
            ENDELSE 
         ENDELSE 
      ENDFOR 
   ENDIF 
   ;
   ; populate arrays
   j               = where(indb GE 0L, nj)
   z_b_1[j]        = zb1b[indb[j]]
   z_b_min_1[j]    = zbmin1b[indb[j]]
   z_b_max_1[j]    = zbmax1b[indb[j]]
   t_b_1[j]        = tb1b[indb[j]]
   odds_1[j]       = odds1b[indb[j]]
   z_b_2[j]        = zb2b[indb[j]]
   z_b_min_2[j]    = zbmin2b[indb[j]]
   z_b_max_2[j]    = zbmax2b[indb[j]]
   t_b_2[j]        = tb2b[indb[j]]
   odds_2[j]       = odds2b[indb[j]]
   z_b_3[j]        = zb3b[indb[j]]
   z_b_min_3[j]    = zbmin3b[indb[j]]
   z_b_max_3[j]    = zbmax3b[indb[j]]
   t_b_3[j]        = tb3b[indb[j]]
   odds_3[j]       = odds3b[indb[j]]
   z_ml[j]         = zmlb[indb[j]]
   t_ml[j]         = tmlb[indb[j]]
   chi_squared[j]  = chisqb[indb[j]]
   m_0[j]          = m0b[indb[j]]
   ;
   ; make arrays for things from Hawaii cat to go in.
   ra_h            = make_array(nid,/float,value=defp)
   dec_h           = make_array(nid,/float,value=defp)
   hk_mag_h        = make_array(nid,/float,value=defm)
   z_mag_h         = make_array(nid,/float,value=defm)
   i_mag_h         = make_array(nid,/float,value=defm)
   r_mag_h         = make_array(nid,/float,value=defm)
   v_mag_h         = make_array(nid,/float,value=defm)
   b_mag_h         = make_array(nid,/float,value=defm)
   u_mag_h         = make_array(nid,/float,value=defm)
   z_spec_h        = make_array(nid,/float,value=defz)
   flag_z_spec_h   = make_array(nid,/byte,value=deff)
   source_z_spec_h = make_array(nid,/int,value=defs)
   z_phot_h        = make_array(nid,/float,value=defz)
   flag_z_phot_h   = make_array(nid,/byte,value=deff)
   z_phot_odds_h   = make_array(nid,/float,value=defz)
   x_h             = make_array(nid,/float,value=defp)
   y_h             = make_array(nid,/float,value=defp)
   dmag_h          = make_array(nid,/float,value=defm)
   ;
   ; read in hawaii spectro-z catalog
   read_cbhcs04, rah, dech, hkmagh, zmagh, imagh, rmagh, vmagh, bmagh, umagh, $
                 zsph, flag_zsph, src_zsph, zphh, flag_zphh, zphoddsh   
   nh        = n_elements(rah)
   indh      = make_array(nh,/long,value=-1l)
   ;
   ; calculate positions in detection image
   adxy, hdr, rah, dech, xidt, yidt
   xidt      = xidt + 1.0
   yidt      = yidt + 1.0
   xidth     = ah + bh*xidt + ch*yidt
   yidth     = dh + eh*xidt + fh*yidt
   ;
   ; find hawaii sources that may be in ACS FOV
   k         = where(xidth GE x1 AND xidth LE x2 AND yidth GE y1 AND yidth LE y2, nk)
   print, 'Max number of Hawaii sources, that may match: ', nk
   ;
   ; loop through those sources and try to find matches
   IF nk GT 0 THEN BEGIN 
      FOR i = 0, nk-1 DO BEGIN 
         r2  = (x - xidth[k[i]])^2 + (y - yidth[k[i]])^2
         j   = where(r2 LT rmatchp2, nj)
         IF nj EQ 0 THEN BEGIN 
            print, 'No match for Hawaii source at expected pos: ', xidth[k[i]], yidth[k[i]]
         ENDIF ELSE BEGIN 
            str = 'Match : ' 
            IF nj GT 1 THEN BEGIN 
               jj  = sort(abs(det_mag[j] - imagh[k[i]] - dmagimag))
               j   = temporary(j[jj])
               str = 'Multimatch, n = '+strtrim(nj,2)+' : '
            ENDIF 
            indh[k[i]] = j[0]
            print, str+'Hawaii pos,mag: ', xidth[k[i]],yidth[k[i]],imagh[k[i]], ' IDT pos,mag: ',x[j[0]], y[j[0]], det_mag[j[0]], imagh[k[i]]-det_mag[j[0]]
         ENDELSE 
      ENDFOR 
      ;
      ; populate relevant arrays
      j      = where(indh GE 0l, nj)
      ra_h[indh[j]]            = rah[j]
      dec_h[indh[j]]           = dech[j]
      hk_mag_h[indh[j]]        = hkmagh[j]
      z_mag_h[indh[j]]         = zmagh[j]
      i_mag_h[indh[j]]         = imagh[j]
      r_mag_h[indh[j]]         = rmagh[j]
      v_mag_h[indh[j]]         = vmagh[j]
      b_mag_h[indh[j]]         = bmagh[j]
      u_mag_h[indh[j]]         = umagh[j]
      z_spec_h[indh[j]]        = zsph[j]
      flag_z_spec_h[indh[j]]   = flag_zsph[j]
      source_z_spec_h[indh[j]] = src_zsph[j]
      z_phot_h[indh[j]]        = zphh[j]
      flag_z_phot_h[indh[j]]   = flag_zphh[j]
      z_phot_odds_h[indh[j]]   = zphoddsh[j]
      x_h[indh[j]]             = xidth[j]
      y_h[indh[j]]             = yidth[j]
      ;
      print, 'Number of matches to Hawaii catalog: ', nj
      ;
      ; plot possible matches, and final matches
      oplot, xidth[k], yidth[k], psym=sym(1), color=!blue
      oplot, xidth[j], yidth[j], psym=sym(1), color=!green
      ;
      k                  = where(det_mag GE 0.0 AND i_mag_h GE 0.0 AND ra_h GE 0.0)
      dmag               = det_mag[k] - i_mag_h[k]
      grm_avsigclip, dmag, 3.0, 50, mean, sigma, nuse, nrej, nit, /verbose
   ENDIF 
   ;
   ; Make arrays for holding FLY99 stuff
   id_f        = make_array(nid, /long, value=-1l)
   ra_f        = make_array(nid, /float, value=defp)
   dec_f       = make_array(nid, /float, value=defp)
   x_f         = make_array(nid, /float, value=defp)
   y_f         = make_array(nid, /float, value=defp)
   i_mag_f     = make_array(nid, /float, value=defm)
   name_hdfn_f = make_array(nid, /string, value='')
   z_phot_f    = make_array(nid, /float, value=defz)
   t_f         = make_array(nid, /int, value=-1)
   ;
   ; read fly99
   readfmt, filif, fmtf, idf, rastr, decstr, idhdfnf, imagf, zfly, tfly
   nf        = n_elements(idf)
   indf      = make_array(nf,/long,value=-1l)
   ;
   ; compress name
   idhdfnf   = strtrim(idhdfnf,2)
   ;
   ; convert positions to decimal degrees
   raf       = 15.0d0*sexideg(rastr, delim=' ')
   decf      = sexideg('+'+strtrim(decstr,2), delim=' ')
   ;
   ; calculate position in detection image
   adxy, hdr, raf, decf, xidt, yidt
   xidt      = xidt + 1.0
   yidt      = yidt + 1.0
   xidtf     = af + bf*xidt + cf*yidt
   yidtf     = df + ef*xidt + ff*yidt
   ;
   ; find fly99 sources that may be in ACS FOV
   k         = where(xidtf GE x1 AND xidtf LE x2 AND yidtf GE y1 AND yidtf LE y2, nk)
   print, 'Max number of FLY99 sources, that may match: ', nk
   ;
   ; loop through those sources and try to find matches
   IF nk GT 0 THEN BEGIN 
      FOR i = 0, nk-1 DO BEGIN 
         r2  = (x - xidtf[k[i]])^2 + (y - yidtf[k[i]])^2
         j   = where(r2 LT rmatchp2, nj)
         IF nj EQ 0 THEN BEGIN 
            print, 'No match for FLY99 source at expected pos: ', xidtf[k[i]], yidtf[k[i]]
         ENDIF ELSE BEGIN 
            str = 'Match : ' 
            IF nj GT 1 THEN BEGIN 
               jj  = sort(abs(det_mag[j] - imagf[k[i]] - dmagimag))
               j   = temporary(j[jj])
               str = 'Multimatch, n = '+strtrim(nj,2)+' : '
            ENDIF 
            indf[k[i]] = j[0]
            print, str+'BPZ pos,mag: ', xidtf[k[i]],yidtf[k[i]],imagf[k[i]], ' IDT pos,mag: ',x[j[0]], y[j[0]], det_mag[j[0]], imagf[k[i]]-det_mag[j[0]]
         ENDELSE 
      ENDFOR 
      ;
      ; populate relevant arrays
      j      = where(indf GE 0l, nj)
      ra_f[indf[j]]            = raf[j]
      dec_f[indf[j]]           = decf[j]
      x_f[indf[j]]             = xidtf[j]
      y_f[indf[j]]             = yidtf[j]
      i_mag_f[indf[j]]         = imagf[j]
      name_hdfn_f[indf[j]]     = idhdfnf[j]
      z_phot_f[indf[j]]        = zfly[j]
      t_f[indf[j]]             = tfly[j]
      ;
      print, 'Number of matches to FLY99 catalog: ', nj
      ;
      ; plot possible matches, and final matches
      oplot, xidtf[k], yidtf[k], psym=sym(8), color=!magenta
      oplot, xidtf[j], yidtf[j], psym=sym(3), color=!orange
      ;
      k                  = where(det_mag GE 0.0 AND i_mag_h GE 0.0 AND ra_h GE 0.0)
      dmag               = det_mag[k] - i_mag_h[k]
      grm_avsigclip, dmag, 3.0, 50, mean, sigma, nuse, nrej, nit, /verbose
   ENDIF 
   ;
   ; Make arrays to hold Capak stuff
   id_c        = make_array(nid, /float, value=-1l)
   ra_c        = make_array(nid, /float, value=defp)
   dec_c       = make_array(nid, /float, value=defp)
   x_c         = make_array(nid, /float, value=defp)
   y_c         = make_array(nid, /float, value=defp)
   u_mag_c     = make_array(nid, /float, value=defm)
   b_mag_c     = make_array(nid, /float, value=defm)
   v_mag_c     = make_array(nid, /float, value=defm)
   r_mag_c     = make_array(nid, /float, value=defm)
   i_mag_c     = make_array(nid, /float, value=defm)
   z_mag_c     = make_array(nid, /float, value=defm)
   hk_mag_c    = make_array(nid, /float, value=defp)
   z_b_c       = make_array(nid, /float, value=defz)
   z_b_min_c   = make_array(nid, /float, value=defz)
   z_b_max_c   = make_array(nid, /float, value=defz)
   t_b_c       = make_array(nid, /float, value=defz)
   odds_c      = make_array(nid, /float, value=defz)
   z_ml_c      = make_array(nid, /float, value=defz)
   t_ml_c      = make_array(nid, /float, value=defz)
   chi_c       = make_array(nid, /float, value=defz)
   ;
   ; read in Capak photometry & spectro-z catalog
   readcol, filic, idc, rac, decc, umagc, bmagc, vmagc, rmagc, imagc, zmagc, hkmagc, $
            zbc, z0c, z1c, tbc, oc, zmlc, tmlc, chic, format=fmtc
   nc        = n_elements(rac)
   indc      = make_array(nc,/long,value=-1l)
   ;
   ; calculate positions in detection image
   adxy, hdr, rac, decc, xidt, yidt
   xidt      = xidt + 1.0
   yidt      = yidt + 1.0
   xidtc     = ac + bc*xidt + cc*yidt
   yidtc     = dc + ec*xidt + fc*yidt
   ;
   ; find Capak sources that may be in ACS FOV
   k         = where(xidtc GE x1 AND xidtc LE x2 AND yidtc GE y1 AND yidtc LE y2, nk)
   print, 'Max number of Capak sources, that may match: ', nk
   ;
   ; loop through those sources and try to find matches
   IF nk GT 0 THEN BEGIN 
      FOR i = 0, nk-1 DO BEGIN 
         r2  = (x - xidtc[k[i]])^2 + (y - yidtc[k[i]])^2
         j   = where(r2 LT rmatchp2, nj)
         IF nj EQ 0 THEN BEGIN 
            print, 'No match for Capak source at expected pos: ', xidtc[k[i]], yidtc[k[i]]
         ENDIF ELSE BEGIN 
            str = 'Match : ' 
            IF nj GT 1 THEN BEGIN 
               jj  = sort(abs(det_mag[j] - imagc[k[i]] - dmagimag))
               j   = temporary(j[jj])
               str = 'Multimatch, n = '+strtrim(nj,2)+' : '
            ENDIF 
            indc[k[i]] = j[0]
            print, str+'Capak pos,mag: ', xidtc[k[i]],yidtc[k[i]],imagc[k[i]], ' IDT pos,mag: ',x[j[0]], y[j[0]], det_mag[j[0]], imagc[k[i]]-det_mag[j[0]]
         ENDELSE 
      ENDFOR 
      ;
      ; populate relevant arrays
      j                        = where(indc GE 0l, nj)
      id_c[indc[j]]            = idc[j]
      ra_c[indc[j]]            = rac[j]
      dec_c[indc[j]]           = decc[j]
      x_c[indc[j]]             = xidtc[j]
      y_c[indc[j]]             = yidtc[j]
      u_mag_c[indc[j]]         = umagc[j]
      b_mag_c[indc[j]]         = bmagc[j]
      v_mag_c[indc[j]]         = vmagc[j]
      r_mag_c[indc[j]]         = rmagc[j]
      i_mag_c[indc[j]]         = imagc[j]
      z_mag_c[indc[j]]         = zmagc[j]
      hk_mag_c[indc[j]]        = hkmagc[j]
      z_b_c[indc[j]]           = zbc[j]
      z_b_min_c[indc[j]]       = z0c[j]
      z_b_max_c[indc[j]]       = z1c[j]
      t_b_c[indc[j]]           = tbc[j]
      odds_c[indc[j]]          = oc[j]
      z_ml_c[indc[j]]          = zmlc[j]
      t_ml_c[indc[j]]          = tmlc[j]
      chi_c[indc[j]]           = chic[j]
      ;
      print, 'Number of matches to Capak catalog: ', nj
      ;
      ; plot possible matches, and final matches
      oplot, xidtc[k], yidtc[k], psym=sym(9), color=!dyellow
      oplot, xidtc[j], yidtc[j], psym=sym(15), color=!cyan
      ;
      k                  = where(det_mag GE 0.0 AND i_mag_h GE 0.0 AND ra_h GE 0.0)
      dmag               = det_mag[k] - i_mag_h[k]
      grm_avsigclip, dmag, 3.0, 50, mean, sigma, nuse, nrej, nit, /verbose
   ENDIF 
   ;
   ; DB creation stuff
   ;
   ; set lengths of character items
   k       = where(tlen EQ 0,nk)
   IF nk GT 0 THEN FOR i = 0,nk-1 DO result = execute('tlen[k[i]] = max(strlen('+item[k[i]]+'))') $
              ELSE stop
   ;
   ; create structure
   create_struct2, hdfn, 'hdfn', item, dstring, dimen=nid
   ;
   ; open dbd file, write top of the file
   ;
   cd, cwd
   fildbd  = filo + '.dbd'
   openw, lu, fildbd, /get_lun
   printf, lu, '#title'
   printf, lu, dbtitle
   printf, lu, ''
   printf, lu, '#maxentries'
   printf, lu, strtrim(string(nid),2)
   printf, lu, ''
   printf, lu, '#items'
   ;
   ; Fill hdfn structure, write item lines of dbd file
   mc     = max(strlen(item))
   pritem = ljust(strupcase(item), mc)
   ni     = n_elements(item)
   FOR i = 0, ni-1 DO BEGIN 
      cmd = "hdfn."+strtrim(item[i],2)+" = "+strtrim(item[i],2)
      result = execute(cmd)
      typstr = ljust(tout[i]+'*'+strtrim(string(tlen[i]),2),5)
      printf,lu,pritem[i]+'   '+typstr+'   "'+descript[i]+'"'
   ENDFOR 
   ;
   ; write index blocks of dbd file and close
   printf,lu,'  '
   printf,lu,'#index'
   ni = n_elements(indblk)
   FOR i = 0, ni-1 DO BEGIN 
      printf,lu,ljust(strupcase(strtrim(indblk[i],2)),mc)+'   sort'
   ENDFOR 
   free_lun,lu   
   ;
   ; write database
   !PRIV = 2
   dbcreate, filo, 1, 1, /external
   dbopen, filo, 1
   dbbuildstruct, hdfn
   dbclose,dummy
END 
