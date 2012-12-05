PRO test_epears_dpipe
   root = 'cdfnp1_'+['i050', 'i065', 'i134', 'z050', 'z065', 'z134']
   ;
   nr   = n_elements(root)
   FOR jj = 0, nr-1 DO BEGIN 
      fsci    = root[jj]+'_cr.fits'
      fscio   = root[jj]+'.fits'
      cmd     = 'mv -f '+fscio+' '+fsci
      spawn, cmd
      fsmooth = root[jj]+'_smooth.fits'
      fsharp  = root[jj]+'_sharp.fits'
      fcat    = root[jj]+'_sharp.cat'
      fcheck  = root[jj]+'_sharp_segm.fits'
      freg    = root[jj]+'_sharp.reg'
      epears_dpipe, fsci, fscio, fsmooth, fsharp, fcat, fcheck, freg
   ENDFOR 
END 
