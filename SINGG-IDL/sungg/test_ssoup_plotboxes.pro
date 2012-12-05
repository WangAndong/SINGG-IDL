PRO test_ssoup_plotboxes
  ;wd      = '~/SUNGG/OUPIPE/J0249-02_play2/'
  wd      = '~/SUNGG/OUPIPE/J1321-31/'
  ;wd      = '~/SUNGG/OUPIPE/J1300-13/'
  flog    = 'test_ssoup_plotboxes.log'
  ;sname   = 'J0249-02'
  sname   = 'J1321-31'
  ;sname   = 'J1300-13'
  ;bname   = 'R'
  bname   = ['R', 'HALPHA', 'NUV', 'FUV']
  fbox    = sname+'_aligned_box_'+bname+'.dat'
  fbplot1 = sname+'_aligned_box_'+bname+'.jpg'
  fbplot2 = sname+'_aligned_box_'+bname+'.eps'
  bxsiz   = 15
  nb      = n_elements(bname)
  ;
  openw, ll, flog, /get_lun
  cd, wd, current=cwd
  FOR ii = 0, nb-1 DO BEGIN 
     ssoup_plotboxes, ll, bxsiz, sname, bname[ii], fbox[ii], fbplot1[ii]
     ssoup_plotboxes, ll, bxsiz, sname, bname[ii], fbox[ii], fbplot2[ii]
  ENDFOR
  free_lun, ll
  cd, cwd
END 
