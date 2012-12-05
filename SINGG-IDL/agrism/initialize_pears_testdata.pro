PRO initialize_pears_testdata
   fgood_grism  = ['grism_ccd1.dat', 'grism_ccd2.dat']
   fgood_direct = ['direct_ccd1.dat', 'direct_ccd2.dat']
   fgrism       = 'sn_epoch1_g800l_drz.fits'
   fdirect      = 'sn_epoch1_zi_drz.fits'
   pfxg         = 'grism'
   pfxd         = 'direct'
   t            = [-105.673035610, 1.002578038, -0.003091192, -0.353078513, 0.000706450, 0.999755280]
   fluxrat      = 4.15
   grcut        = 61.0
   grow         = 3.0
   verbose      = 1b
   ;
   pears_init_images, fdirect, pfxd, fgood_direct, verbose=verbose
   pears_init_images, fgrism, pfxg, fgood_grism, verbose=verbose
   ;
   fdrct        = pfxd+'_edgemsk.fits'
   fdsharp      = pfxd+'_sharp.fits'
   fgsharp      = pfxg+'_sharp.fits'
   fgsharp1     = pfxg+'_sharp_msk1.fits'
   fgsharp2     = pfxg+'_sharp_msk2.fits'
   print, 'INITIALIZE_PEARS_DATA: making first zero order masked image: '+systime()
   zero_ord_mask, fdrct, fgsharp, fgsharp1, t, fluxrat, grcut, grow=grow
   print, 'INITIALIZE_PEARS_DATA: making second zero order masked image: '+systime()
   zero_ord_mask, fdsharp, fgsharp, fgsharp2, t, fluxrat, grcut, grow=grow
   print, 'INITIALIZE_PEARS_DATA: finished: '+systime()
END 
