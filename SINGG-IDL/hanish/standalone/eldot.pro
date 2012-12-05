PRO eldot

  cd,'/data1/acs22/hanish/'

  obj = ['J0039-14a','J0205-55a','J0209-10','J0246-30', $
         'J0443-05','J0504-16','J1026-19','J1051-17',$
         'J1059-09','J1159-19','J1250-20','J1408-21']
  run = ['Run06','Run12','Run02','Run15',$
         'Run08','Run08','Run13','Run13',$
         'Run08','Run08','Run13','Run13']

  FOR ii = 0,N_ELEMENTS(obj)-1 DO BEGIN

    spawn,'cp -f '+run[ii]+'/Proc4/'+obj[ii]+'/J*_isophote.profile eldot'
    spawn,'cp -f '+run[ii]+'/Proc3/'+obj[ii]+'/J*.pl.fits.gz eldot'

  ENDFOR

  cd,'/data1/acs22/hanish/eldot'
  spawn,'tar cvf profile.tar *.profile'

  FOR ii = 0,N_ELEMENTS(obj)-1 DO BEGIN

    spawn,'tar cvf '+obj[ii]+'.pl.tar '+obj[ii]+'*.pl.fits.gz'

  ENDFOR

  RETURN

END
