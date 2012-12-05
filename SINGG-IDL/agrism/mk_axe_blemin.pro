PRO mk_axe_blemin
   ;
   ; Make input catalog for axe to run on sources found by BLEM.
   ; 
   ; G. Meurer 09/2004
   ;
   fili        = '/home/meurer/ACS/Grism/HDFN_jun04/Apsis/Work/BLEM_aug04a/hdfn_blem_emsource.dat'
   filo        = 'hdfn_blem_axein.cat'
   detim       = 'detectionImage.fits'
   fmtg        = '(i5,f8.2,f8.2,f5.1,f5.1,f7.2,f7.1,f8.3,f6.2,f8.3,f8.2,f8.2,f8.2,f8.2,f6.2,f7.2,i6)'
   fmto        = '(i5,1x,f9.2,1x,f9.2,1x,f12.8,1x,f12.8,1x,f7.2,1x,f6.2,1x,f6.2,1x,f6.1,1x,e12.5,1x,e12.5,1x,f6.1,1x,f6.2,1x,e12.5,1x,f8.4)'
   idmerge1    = [91, 132, 240]
   idmerge2    = [92, 133, 241]
   ;
   ; read emission line sources from BLEM output
   readfmt, fili, fmtg, idg, xg, yg, ag, bg, fwhmg, thetag, apeak, $
                  aw50, cpeak, cshift, cxp, cyp, cwl, cw50, cflx, sexid, skipline=1
   ng          = n_elements(idg)
   ;
   ; merge object positions as needed
   nmerge      = n_elements(idmerge1)
   keep        = make_array(ng, /int, value=1b)
   FOR jj = 0, nmerge-1 DO BEGIN 
      kk1       = where(idg EQ idmerge1[jj], n1)
      kk2       = where(idg EQ idmerge2[jj], n2)
      IF n1 NE 1 OR n2 NE 1 THEN stop
      kk1       = kk1[0]
      kk2       = kk2[0]
      cxp[kk1]  = 0.5*(cxp[kk1]+cxp[kk2])
      cyp[kk1]  = 0.5*(cyp[kk1]+cyp[kk2])
      keep[kk2] = 0b
   ENDFOR  
   ;
   ; save sources to keep
   gg          = where(keep EQ 1b, ng)
   idg         = idg[gg]
   cxp         = cxp[gg]
   cyp         = cyp[gg]
   ;
   ; read header of detection Image
   fits_read, detim, data, hdr, /header_only
   ;
   ; transform pixel positions to RA, Dec.
   xyad, hdr, cxp, cyp, ra, dec
   cd11        = sxpar(hdr,'cd1_1')
   cd12        = sxpar(hdr,'cd1_2')
   cd21        = sxpar(hdr,'cd2_1')
   cd22        = sxpar(hdr,'cd2_2')
   scale       = 0.5*(sqrt(cd11^2 + cd12^2) + sqrt(cd21^2 + cd22^2))
   ;
   ; set up some default output arrays
   a_image     = make_array(ng, /float, value=2.0)
   b_image     = make_array(ng, /float, value=2.0)
   theta_image = make_array(ng, /float, value=90.0)
   fwhm_image  = make_array(ng, /float, value=2.0)
   class_star  = make_array(ng, /float, value=0.5)
   mag_auto    = make_array(ng, /float, value=25.00)
   ;
   a_world     = scale*a_image
   b_world     = scale*b_image
   theta_world = theta_image
   fwhm_world  = scale*fwhm_image
   ;
   ; write output file
   openw, lu, filo, /get_lun
   printf,lu,'#   1 NUMBER          Running object number'
   printf,lu,'#   2 X_IMAGE         Object position along x                       [pixel]'
   printf,lu,'#   3 Y_IMAGE         Object position along y                       [pixel]'
   printf,lu,'#   4 X_WORLD         Barycenter position along world x axis        [deg]'
   printf,lu,'#   5 Y_WORLD         Barycenter position along world y axis        [deg]'
   printf,lu,'#   6 MAG_AUTO        Kron-like elliptical aperture magnitude       [mag]'
   printf,lu,'#   7 A_IMAGE         Profile RMS along major axis                  [pixel]'
   printf,lu,'#   8 B_IMAGE         Profile RMS along minor axis                  [pixel]'
   printf,lu,'#   9 THETA_IMAGE     Position angle (CCW/x)                        [deg]'
   printf,lu,'#  10 A_WORLD         Profile RMS along major axis (world units)    [deg]'
   printf,lu,'#  11 B_WORLD         Profile RMS along minor axis (world units)    [deg]'
   printf,lu,'#  12 THETA_WORLD     Position angle (CCW/world-x)                  [deg]'
   printf,lu,'#  13 FWHM_IMAGE      FWHM assuming a gaussian core                 [pixel]'
   printf,lu,'#  14 FWHM_WORLD      FWHM assuming a gaussian core                 [deg]'
   printf,lu,'#  15 CLASS_STAR      S/G classifier output'
   FOR i = 0, ng-1 DO printf, lu, idg[i], cxp[i], cyp[i], ra[i], dec[i], mag_auto[i], $
    a_image[i], b_image[i], theta_image[i], a_world[i], b_world[i], theta_world[i], $
    fwhm_image[i], fwhm_world[i], class_star[i], $
    format=fmto
   free_lun, lu
END 
