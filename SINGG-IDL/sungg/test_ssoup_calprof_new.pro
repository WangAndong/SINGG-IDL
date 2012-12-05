PRO test_ssoup_calprof_new
  ;
  ; G. Meurer  04/2010
  wd          = '~/SUNGG/OUPIPE/J0249-02/'
  flog        = 'test.log'
  band        = ['R', 'HALPHA', 'NUV', 'FUV']
  photplam    = [6507.46, 6575.48, 2315.7, 1538.6]
  ebvg        = 0.1
  sname       = 'J0249-02'
  fprofs      = 'J0249-02_aligned_'+['R', 'Halpha', 'NUV', 'FUV']+'.profile'
  fscalprof   = 'test_smprof.dat'
  ffcalprof   = 'test_fmprof.dat'
  fscalprof0  = 'test_smprof0.dat'
  ffcalprof0  = 'test_fmprof0.dat'
  fplotj1     = 'test_sm.jpg'
  fplotj2     = 'test_hafuv.jpg'
  fplotj3     = 'test_hafuv0.jpg'
  ;
  cd, wd, current=cwd
  openw, ll, flog, /get_lun
  ssoup_calprof_new, ll, band, photplam, ebvg, fprofs, fscalprof, ffcalprof, fscalprof0, ffcalprof0
  ;
  ssoup_plotsprofs_new, ll, sname, fscalprof, fscalprof0, fplotj1
  ;
  ssoup_plothafuv_new, ll, sname, fscalprof, fplotj2
  ssoup_plothafuv_new, ll, sname, fscalprof0, fplotj2, /dcorr
  ;
  free_lun,ll
  cd, cwd
END 
