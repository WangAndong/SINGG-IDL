PRO singg_genweb_v3
   sampfil   = 'sample3_ravsort.dat'
   nhd_samp  = 5
   nhd_ext   = 4
   colobs    = 91
   nmax      = 45
   obslis    = 'observed_long.lis'
   obsfil    = 'observed.dat'
   shistpl   = 'masshist.png'
   shisttab  = 'masshist.dat'
   obspage   = 'observed.html'
   samptab   = 'index.html'
   updatefil = 'sample3_ravsort.update'
   title     = 'SINGG: Sample V03 (Oct 2002)'
   subtitle  = ''
   imgdir    = '/data3/acs27/meurer/SINGG/Sample/V03b_oct02/CADC'
;
; update observed galaxies.
;
   print,' Running singg_make_observed '
   singg_make_observed,sampfil,nhd_samp,colobs,obslis,obsfil,updatefil
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
   singg_genweb,updatefil,obspage,shistpl,shisttab,title,subtitle,imgdir=imgdir
;
; update the observed galaxies page
;
END 



