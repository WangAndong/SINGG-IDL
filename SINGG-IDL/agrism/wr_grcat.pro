PRO wr_grcat, filo, id, xim, yim, magauto, aim, bim, thetaim, w50, class, $
              radeg, decdeg, rdstr
   ;
   ; Write output grism catalog
   ;
   ; filo    -> output file name
   ; id      -> id # of objects
   ; xim     -> x pixel pos
   ; yim     -> y pixel pos
   ; magauto -> magauto
   ; aim     -> semi major axis length [pixels]
   ; bim     -> semi-minor axis length [pixels]
   ; thetaim -> position angle of major axis
   ; w50     -> FWHM
   ; class   -> star/galaxy calssification
   ; radeg   -> RA [degrees]
   ; decdeg  -> Dec [Degrees]
   ; rdstr   -> RA Dec [sexigessimal string]
   ;
   ; G.R. Meurer 10/02
   fmt='(i8,f10.2,f9.2,f9.2,f10.3,f10.3,f9.1,f9.2,f8.3,f13.5,f13.5,4x,a)'
   ;
   openw,lu,filo,/get_lun
   printf,lu,'# 1 NUMBER Running object number '
   printf,lu,'# 2 X_IMAGE Object position along x [pixel] '
   printf,lu,'# 3 Y_IMAGE Object position along y [pixel] '
   printf,lu,'# 4 MAG_AUTO Kron-like elliptical aperture magnitude [mag] '
   printf,lu,'# 5 A_IMAGE Profile RMS along major axis [pixel] '
   printf,lu,'# 6 B_IMAGE Profile RMS along minor axis [pixel] '
   printf,lu,'# 7 THETA_IMAGE Position angle (CCW/x) [deg] '
   printf,lu,'# 8 FWHM_IMAGE FWHM assuming a gaussian core [pixel] '
   printf,lu,'# 9 CLASS_STAR S/G classifier output '
   printf,lu,'# 10 X_WORLD Barycenter position along world x axis [deg] '
   printf,lu,'# 11 Y_WORLD Barycenter position along world y axis [deg] '
   printf,lu,'# 12 RA_H Hour portion OF right ascension [Hour]'
   printf,lu,'# 13 RA_M Minute portion OF right ascension [min time]'
   printf,lu,'# 14 RA_S Second portion OF right ascension [sec time]'
   printf,lu,'# 15 DEC_D Degree portion OF declination [deg]'
   printf,lu,'# 16 DEC_M Minute portion OF declination [min]'
   printf,lu,'# 16 DEC_S Second portion OF declination [sec]'
   FOR i = 0, n_elements(id)-1 DO $
    printf,lu,id[i],xim[i],yim[i],magauto[i],aim[i],bim[i],thetaim[i],w50[i],class[i], $
    radeg[i],decdeg[i],rdstr[i],format=fmt
   ;
   free_lun,lu
END 
