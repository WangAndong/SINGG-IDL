pro test_poisson_sky
  wd    = '/Volumes/data/meurer/SUNGG/OUPIPE/J0005-28'
  fili = 'GI1_009016_HPJ0005m28_0001-fd-int.fits'
  maski = 'J0005-28_uv_mask.fits'
  i1    =  1776
  i2    =  1875
  j1    =  1846
  j2    =  1945
  ;
  i1    =  1860
  i2    =  1869
  j1    =  1850
  j2    =  1859
  ;
  cd, wd, current=cwd
  ;
  fits_read, fili, img, hdr
  exptime = sxpar(hdr, 'exptime')
  fits_read, maski, msk, mhdr
  img0    = exptime*img[i1:i2,j1:j2]
  msk0    = msk[i1:i2,j1:j2]
  print, 'testing poison_sky without mask '
  poisson_sky, img0, skylev, eskylev
  PRINT, 'testing poisson_sky with mask '
  poisson_sky, img0, skylev, eskylev, mask=msk0
  print, 'testing poisson_sky with circlerad'
  poisson_sky, img0, skylev, eskylev, mask=msk0, circlerad=1
  poisson_sky, img0, skylev, eskylev, mask=msk0, circlerad=7
  i1    =  2101
  i2    =  2200
  j1    =  1331
  j2    =  1431
  img0    = exptime*img[i1:i2,j1:j2]
  msk0    = msk[i1:i2,j1:j2]
  print, 'testing poison_sky without mask '
  poisson_sky, img0, skylev, eskylev
  PRINT, 'testing poisson_sky with mask '
  poisson_sky, img0, skylev, eskylev, mask=msk0
  print, 'testing poisson_sky with circlerad'
  poisson_sky, img0, skylev, eskylev, mask=msk0, circlerad=1
  poisson_sky, img0, skylev, eskylev, mask=msk0, circlerad=30

end 
