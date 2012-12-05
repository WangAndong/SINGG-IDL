PRO patch_phot_all

  runlist = ['Run01','Run02','Run03','Run04','Run04s','Run05','Run06','Run07','Run08','Run09','Run10','Run11','Run12','Run13','Run15','Run17']

  spawn,'pwd',cdir

  FOR ii = 0,N_ELEMENTS(runlist)-1 DO BEGIN
    rundir = cdir[0]+'/'+STRTRIM(runlist[ii],2)+'/'
    p3dir = rundir+'Proc3/'
    p4dir = rundir+'Proc4/'
    PRINT,'Patching photometry for ',runlist[ii]

    read_catalog,p3dir+STRTRIM(runlist[ii])+'.catalog',run_struct,object, $
               filter,Rfile,Nfile,Sfile,ellipse,refnum,Rmask,Nmask,nsig,/SILENT

    FOR jj = 0,N_ELEMENTS(object)-1 DO BEGIN
      PRINT,'    '+object[jj]
      patch_phot,p4dir+STRTRIM(object[jj],2)+'/'
    ENDFOR

  ENDFOR

  RETURN

END
