pro test_askyfit
  hname     = 'J0406-52'
  wd        = '~/SUNGG/OUPIPE/'+hname
  fimg      = hname+'_aligned_Halpha.fits'
  fsmask    = hname+'_aligned_skymask.fits'
  ll        = -1
  order     =  1
  boxsize   = -1
  ;
  for ii = 0, 3 do begin 
     order  = ii
     fimgo  = hname+'_aligned_Halpha_'+numstr(order)+'.fits'
     ;
     cd, wd, current=cwd
     fits_read, fimg, img, hdi
     fits_read, fsmask, masks, hdm
     ;
     imgo   = img
     ;
     ssoup_askyfit, ll, imgo, masks, order, skypar, eskypar, skysig, skysigbox, boxsize, $
                    boxdata=boxdata, /subtract, /verbose
     ;
     fits_write, fimgo, imgo, hdi
     ;
  endfor 
  ;
  cd, cwd
end
