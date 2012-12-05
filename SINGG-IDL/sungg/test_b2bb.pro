PRO test_b2bb
  ;
  ; test box2boxbg3,4 on galex images
  ;
  wd       = '~/SUNGG/OUPIPE/J0331-51/'
  fili     = 'GI1_009040_IC1954-fd-int.fits'
  film     = 'J0331-51_uv_mask.fits'
  frej     = 0.1
  bxw      = 15
  ;
  cd,wd,current=cwd
  fits_read, fili, imi, hdi
  fits_read, film, imm, hdm
  expt     = sxpar(hdi, 'exptime')
  imi      = float(imi)*float(expt)
  imi      = imi[801:2600,801:2600]
  imm      = imm[801:2600,801:2600]
  ;
  print, 'Running box2boxbg3 with /use_sky'
  box2boxbg3, imi, bxw, nsigma=3, mask=imm, /use_sky, /nodisplay, reject_frac=frej, results=res3a, boxinfo=bi3a
  print, 'Running box2boxbg3 without /use_sky'
  box2boxbg3, imi, bxw, nsigma=3, mask=imm, results=res3b, /nodisplay, reject_frac=frej, boxinfo=bi3b
  print, 'Running box2boxbg4 with /use_sky'
  box2boxbg4, imi, bxw, nsigma=3, mask=imm, /use_sky, /poisson, results=res4a, /nodisplay, reject_frac=frej, boxinfo=bi4a
  print, 'Running box2boxbg4 without /use_sky'
  box2boxbg4, imi, bxw, nsigma=3, /poisson, results=res4b, /nodisplay, reject_frac=frej, boxinfo=bi4b
  ;
  forprint, res3a, res3b, res4a, res4b
  ;
  stop
  cd,cwd
END 
