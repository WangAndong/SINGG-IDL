PRO test_ssoup_calprof
  ;wd      = '~/SUNGG/OUPIPE/J0039-14a/'
  wd      = '~/SUNGG/OUPIPE/J1300-13/'
  cd, wd, current=cwd
  ;hname   = 'J0039-14a'
  hname   = 'J1300-13'
  band    = ['R', 'Halpha', 'NUV', 'FUV']
  ffits   = hname+'_aligned_'+band+'.fits'
  fprofs  = hname+'_aligned_'+band+'.profile'
  band    = strupcase(band)
  dbopen, 'singg_sample'
  list    = dbmatch('NAME',HNAME)
  fscprof  = 'test_scalprof.dat'
  ffcprof  = 'test_fcalprof.dat'
  fscprof0 = 'test_scalprof0.dat'
  ffcprof0 = 'test_fcalprof0.dat'
  dbext,list,'EBV',ebv
  dbclose
  ebv     = ebv[0]
  nim     = n_elements(ffits)
  ll      = -1
  photplam = make_array(nim, /float, value=0.0)
  FOR ii = 0, nim-1 DO BEGIN 
     fits_read, ffits[ii], im, hd, /header_only
     photplam[ii] = sxpar(hd, 'photplam')
  ENDFOR 
  ;
  ssoup_calprof, ll, band, photplam, ebv, fprofs, fscprof, ffcprof, fscprof0, ffcprof0
  ;
  cd, cwd
END 
