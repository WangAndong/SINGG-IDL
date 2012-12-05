PRO test_epears_pipe
   ; 
   fsci    = 'new_cdf-n-pears-1-gpa050_drz.fits'
   fwht    = 'cdf-n-pears-1-gpa050_wht.fits'
   fscio   = 'cdfnp1_g050.fits'
   frms    = 'cdfnp1_g050_rms.fits'
   fsmooth = 'cdfnp1_g050_smooth.fits'
   fsharp  = 'cdfnp1_g050_sharp.fits'
   fcat    = 'cdfnp1_g050_sharp.cat'
   fcheck  = 'cdfnp1_g050_sharp_segm.fits'
   freg    = 'cdfnp1_g050_sharp.reg'
   ;fmsk    = 'pears_goodarea.3.dat'
   ;fmsk    = 'mask.fits'
   fmsk    = 'pears_goodarea_4'
   epears_pipe, fsci, fwht, fscio, frms, fsmooth, fsharp, fcat, fcheck, freg, fmsk=fmsk
   fsci    = 'new_cdf-n-pears-1-gpa065_drz.fits'
   fwht    = 'cdf-n-pears-1-gpa065_wht.fits'
   fscio   = 'cdfnp1_g065.fits'
   frms    = 'cdfnp1_g065_rms.fits'
   fsmooth = 'cdfnp1_g065_smooth.fits'
   fsharp  = 'cdfnp1_g065_sharp.fits'
   fcat    = 'cdfnp1_g065_sharp.cat'
   fcheck  = 'cdfnp1_g065_sharp_segm.fits'
   freg    = 'cdfnp1_g065_sharp.reg'
   epears_pipe, fsci, fwht, fscio, frms, fsmooth, fsharp, fcat, fcheck, freg, fmsk=fmsk
   fsci    = 'new_cdf-n-pears-1-gpa134_drz.fits'
   fwht    = 'cdf-n-pears-1-gpa134_wht.fits'
   fscio   = 'cdfnp1_g134.fits'
   frms    = 'cdfnp1_g134_rms.fits'
   fsmooth = 'cdfnp1_g134_smooth.fits'
   fsharp  = 'cdfnp1_g134_sharp.fits'
   fcat    = 'cdfnp1_g134_sharp.cat'
   fcheck  = 'cdfnp1_g134_sharp_segm.fits'
   freg    = 'cdfnp1_g134_sharp.reg'
   epears_pipe, fsci, fwht, fscio, frms, fsmooth, fsharp, fcat, fcheck, freg, fmsk=fmsk
END 
