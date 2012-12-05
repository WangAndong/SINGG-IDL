PRO run_flux_all,RUNLIST=runlist

  IF NOT KEYWORD_SET(runlist) THEN $
     runlist = ['Run01','Run02','Run03','Run04','Run04s','Run05','Run06','Run07','Run08','Run09','Run10','Run11','Run12','Run13','Run15','Run17']

  spawn,'pwd',cdir

  FOR ii = 0,N_ELEMENTS(runlist)-1 DO BEGIN
    dir=cdir[0]+'/'+runlist[ii]

    IF FILE_TEST(dir) THEN BEGIN
      PRINT,'Measuring fluxes for '+runlist[ii]
      run_flux,dir=dir,/skip,/fast
    ENDIF ELSE BEGIN
      PRINT,runlist[ii]+' not in directory.'
    ENDELSE
  ENDFOR

  RETURN
END
