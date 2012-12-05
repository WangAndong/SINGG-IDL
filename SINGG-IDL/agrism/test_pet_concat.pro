PRO test_pet_concat
   ext0 = 97
   ext1 = 353
   wd = '/data3/acs27/meurer/grism/hdfn_new/axe_all/'
   pet = 'j8eb01g800l_drz_sci_1.PET.fits'
   fout = 'output/test_concat_1.PET.fits'
   cd,wd
   lup = fxposit(pet, ext0)
   bintab0 = mrdfits(lup, 0, hdr0)
   free_lun,lup
   hprint,hdr0
   help,bintab0
   ;keywait,'press any key to continue : '
   lup = fxposit(pet, ext1)
   bintab1 = mrdfits(lup, 0, hdr1)
   free_lun,lup
   hprint,hdr1
   ;keywait,'press any key to continue : '
   help,bintab1

   bintab2 = create_struct('ID',1l, $
                           'N', bintab0.N+bintab1.N, $
                           'P_X', [bintab0.P_X,bintab1.P_X], $
                           'P_Y', [bintab0.P_Y,bintab1.P_Y], $
                           'X', [bintab0.X,bintab1.X], $
                           'Y', [bintab0.Y,bintab1.Y], $
                           'DIST', [bintab0.DIST,bintab1.DIST], $
                           'XS', [bintab0.XS,bintab1.XS], $
                           'XI', [bintab0.XI,bintab1.XI], $
                           'LAMBDA', [bintab0.LAMBDA,bintab1.LAMBDA], $
                           'DLAMBDA', [bintab0.DLAMBDA,bintab1.DLAMBDA], $
                           'COUNT', [bintab0.COUNT,bintab1.COUNT], $
                           'ERROR', [bintab0.ERROR,bintab1.ERROR], $
                           'CONTAM', [bintab0.CONTAM,bintab1.CONTAM])

   hdr2 = hdr0
   sxaddpar, hdr2, 'extname', bintab2.id
   sxaddpar, hdr2, 'objectid', bintab2.id
   sxaddpar, hdr2, 'exptime', 6700.0
   ;
   ; write output table
   mwrfits,bintab2,fout,hdr2
   ;
   ; check that it worked
   lup = fxposit(fout, 1)
   bintab3 = mrdfits(lup, 0, hdr3)
   free_lun,lup
   hprint,hdr3
   help,bintab3,/struct
END 
