PRO singg_genweb_all
   ;sampfil  = 'sample_ravsort.dat'
   sampfil   = 'sample2_ravsort.dat'
   nhd_samp  = 5
   nhd_ext   = 4
   colobs    = 89
   nmax      = 45
   exfile    = 'extras.dat'
   obslis    = 'observed.lis'
   obsfil    = 'observed.dat'
   shistpl   = 'masshist.png'
   shisttab  = 'masshist.dat'
   obspage   = 'observed.html'
   samptab   = 'index.html'
   updatefil = 'sample2_ravsort.update'
;
; update observed galaxies.
;
   print,' Running singg_make_observed '
   singg_make_observed,sampfil,nhd_samp,colobs,obslis,obsfil,updatefil
   ; singg_make_observed,sampfil,nhd_samp,colobs,exfile,nhd_ext,obslis,obsfil,updatefil
   print,' Running singg_observed_table '
   singg_observed_table,obsfil,samptab,obspage
;
; make mass histograms
;
   print,' making mass histograms '
   setplotcolors
   setbgfg,!white,!black

   singg_masshist, updatefil, 0.2, 6.0, 11.5, 0, nmax, pfile=shisttab
   makepng,shistpl,/color
;
; Update web pages
;
   print,' Running singg_genweb '
   singg_genweb,updatefil,obspage,shistpl,shisttab
;
; update the observed galaxies page
;
END 



