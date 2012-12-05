PRO aXeCat, objCat, outCat, mag_bright=mag_bright, mag_faint=mag_faint, a_min=a_min, b_min=b_min

; read object calalog
  fmt = 'I,F5.2,F5.2,F10.2,F10.2,F5.2,F5.2,F10.2,F5.2,F5.2,F5.2,F5.2,F5.2,F5.2,F5.2'
  nskip = 15
  readcol, objCat, NUM, X_IMAGE,Y_IMAGE,X_WORLD,Y_WORLD,MAG_AUTO,$
    A_IMAGE,B_IMAGE,THETA_IMAGE,A_WORLD,B_WORLD,THETA_WORLD,$
    FWHM_IMAGE,FWHM_WORLD,CLASS_STAR, $
    skipline = nskip, format = fmt 

; set header
  hdrCat = strarr(15)
  hdrCat[0]  = '#   1 NUMBER          Running object number'
  hdrCat[1]  = '#   2 X_IMAGE         Object position along x                       [pixel]'
  hdrCat[2]  = '#   3 Y_IMAGE         Object position along y                       [pixel]'
  hdrCat[3]  = '#   4 X_WORLD         Barycenter position along world x axis        [deg]'
  hdrCat[4]  = '#   5 Y_WORLD         Barycenter position along world y axis        [deg]'
  hdrCat[5]  = '#   6 MAG_AUTO        Kron-like elliptical aperture magnitude       [mag]'
  hdrCat[6]  = '#   7 A_IMAGE         Profile RMS along major axis                  [pixel]'
  hdrCat[7]  = '#   8 B_IMAGE         Profile RMS along minor axis                  [pixel]'
  hdrCat[8]  = '#   9 THETA_IMAGE     Position angle (CCW/x)                        [deg]'
  hdrCat[9]  = '#  10 A_WORLD         Profile RMS along major axis (world units)    [deg]'
  hdrCat[10] = '#  11 B_WORLD         Profile RMS along minor axis (world units)    [deg]'
  hdrCat[11] = '#  12 THETA_WORLD     Position angle (CCW/world-x)                  [deg]'
  hdrCat[12] = '#  13 FWHM_IMAGE      FWHM assuming a gaussian core                 [pixel]'
  hdrCat[13] = '#  14 FWHM_WORLD      FWHM assuming a gaussian core                 [deg]'
  hdrCat[14] = '#  15 CLASS_STAR      S/G classifier output'


; set default parameter values
  IF NOT keyword_set(mag_bright) THEN mag_bright = 15.0
  IF NOT keyword_set(mag_faint) THEN mag_faint = 27.0
  IF NOT keyword_set(a_min)   THEN a_min = 1.25
  IF NOT keyword_set(b_min)   THEN b_min = 0.8

; select bright and faint objects
  j_bright = where (MAG_AUTO LE mag_bright)
  j_faint  = where (MAG_AUTO GE mag_faint)

; exclude probable CRs
  j_CR = where (B_IMAGE GT b_min)
  NUM         =  NUM[j_CR]
  X_IMAGE     =  X_IMAGE[j_CR]     
  Y_IMAGE     =  Y_IMAGE[j_CR]     
  X_WORLD     =  X_WORLD[j_CR]     
  Y_WORLD     =  Y_WORLD[j_CR]     
  MAG_AUTO    =  MAG_AUTO[j_CR]    
  A_IMAGE     =  A_IMAGE[j_CR]     
  B_IMAGE     =  B_IMAGE[j_CR]     
  THETA_IMAGE =  THETA_IMAGE[j_CR] 
  A_WORLD     =  A_WORLD[j_CR]     
  B_WORLD     =  B_WORLD[j_CR]     
  THETA_WORLD =  THETA_WORLD[j_CR] 
  FWHM_IMAGE  =  FWHM_IMAGE[j_CR]  
  FWHM_WORLD  =  FWHM_WORLD[j_CR]  
  CLASS_STAR  =  CLASS_STAR[j_CR]  

; select small objects
  j_small_obj = where (A_IMAGE LE a_min)

; set orientation and size for small objects
  THETA_IMAGE[j_small_obj] = -90.0
  A_IMAGE[j_small_obj] = 2.0
;  B_IMAGE[j_small_obj] = 2.0
  A_WORLD[j_small_obj] = A_IMAGE[j_small_obj] * 0.05 / 3600.
;  B_WORLD[j_small_obj] = B_IMAGE[j_small_obj] * 0.05 / 3600.

; set orientation and size for faint objects
  THETA_IMAGE[j_faint] = -90.0
  A_IMAGE[j_faint] = 2.0
;  B_IMAGE[j_faint] = 2.0
  A_WORLD[j_faint] = A_IMAGE[j_faint] * 0.05 / 3600.
;  B_WORLD[j_faint] = B_IMAGE[j_faint] * 0.05 / 3600.

; write output catalog
  colwrite, outCat, NUM, X_IMAGE,Y_IMAGE,X_WORLD,Y_WORLD,MAG_AUTO,$
     A_IMAGE,B_IMAGE,THETA_IMAGE,A_WORLD,B_WORLD,THETA_WORLD,$
     FWHM_IMAGE,FWHM_WORLD,CLASS_STAR, header=hdrCat
END
