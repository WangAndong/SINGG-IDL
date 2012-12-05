PRO test_mkjpg
  wd = '~/SUNGG/OUPIPE/J0506-31'
  band = ['R', 'Halpha', 'NUV', 'FUV']
  fili = 'J0506-31_aligned_'+band+'.fits'
  ebv  = 0.0144
  band = strupcase(band)
  ;
  cd, wd, current=cwd
  ;
  fits_read, fili[0], img, hd, /header_only
  nx   = sxpar(hd, 'naxis1')
  ny   = sxpar(hd, 'naxis2')
  imgc = make_array(nx,ny,4,/float,value=0.0)
  phfl = make_array(4, /float, value=0.0)
  phpl = make_array(4, /float, value=0.0)
  ;
  FOR ii = 0, 3 DO BEGIN
     fits_read, fili[ii], img, hd
     imgc[*,*,ii] = img
     IF ii EQ 1 THEN phfl[ii] = sxpar(hd, 'photflux') ELSE phfl[ii] = sxpar(hd, 'photflam')
     phpl[ii] = sxpar(hd, 'photplam')
  ENDFOR
  openw, ll, 'test.log', /get_lun
  ;
  filo = 'test_'+['hnf','hrf','hrn','rnf']+'.jpg'
  ;
  ssoup_mkjpg, ll, imgc, band, phfl, phpl, filo, /highcut, ebv=10.0*ebv
  ;
  free_lun, ll
  cd, cwd
  stop
END 
