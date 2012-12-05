PRO test_ssoup_plotsprofs
  wd      = '~/SUNGG/OUPIPE/J0039-14a/'
  ll = -1
  sname = 'J0039-14a'
  fsprof = 'test_scalprof.dat'
  fsprof0 = 'test_scalprof0.dat'
  fjpg = 'test_plotsprofs.jpg'
  feps = 'test_plotsprofs.eps'
  cd, wd, current=cwd
  ssoup_plotsprofs, ll, sname, fsprof, fsprof0, fjpg
  ssoup_plotsprofs, ll, sname, fsprof, fsprof0, feps
  cd, cwd
END 
