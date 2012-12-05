PRO test_ssoup_plothafuv
   sname  = 'J0240-08'
   ;
   wd     = '~/SUNGG/OUPIPE/'+sname
   ll     = -1
   ;
   dcorr  = 1b
   fsprof = sname+'_aligned_sprof.dat'
   fplot1 = 'test_'+sname+'_hafuv.jpg'
   fplot2 = 'test_'+sname+'_hafuv.eps'
   ;dcorr  = 1b
   ;fsprof = sname+'_aligned_sprof0.dat'
   ;fplot1 = 'test_'+sname+'_hafuv0.jpg'
   ;fplot2 = 'test_'+sname+'_hafuv0.eps'
   ;
   cd,wd,current=cwd
   ;ssoup_plothafuv, ll, sname, fsprof, fplot1, /kline, dcorr=dcorr
   ;ssoup_plothafuv, ll, sname, fsprof, fplot2, /kline, dcorr=dcorr
   ssoup_plothafuv, ll, sname, fsprof, fplot1, dcorr=dcorr
   ssoup_plothafuv, ll, sname, fsprof, fplot2, dcorr=dcorr
   ;
   cd,cwd
END 
