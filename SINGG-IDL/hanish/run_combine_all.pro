PRO run_combine_all,RUNLIST=runlist,FORCE=force

  IF NOT KEYWORD_SET(runlist) THEN $
     runlist = ['Run01','Run02','Run03','Run04','Run04s','Run05','Run06','Run07','Run08','Run09','Run10','Run11','Run12','Run13','Run15','Run17']

  spawn,'pwd',cdir

  FOR ii = 0,N_ELEMENTS(runlist)-1 DO BEGIN
    dir = cdir[0]+'/'+runlist[ii]+'/'

    IF FILE_TEST(dir) THEN BEGIN
      PRINT,'Combining '+runlist[ii]
      catfile = dir+'/Proc3/'+STRTRIM(runlist[ii],2)+'.catalog'
      IF FILE_TEST(catfile) THEN BEGIN
        IF KEYWORD_SET(force) THEN BEGIN
          run_combine,dir=dir,CATALOG=catfile,/output,/force,buffer=150
        ENDIF ELSE BEGIN
          run_combine,dir=dir,CATALOG=catfile,/output,buffer=150,/skip
        ENDELSE
      ENDIF ELSE BEGIN
        IF KEYWORD_SET(force) THEN BEGIN
          run_combine,dir=dir,/output,/force,buffer=150
        ENDIF ELSE BEGIN
          run_combine,dir=dir,/output,buffer=150,/skip
        ENDELSE
      ENDELSE
    ENDIF ELSE BEGIN
      PRINT,runlist[ii]+' not in directory.'
    ENDELSE
  ENDFOR

  RETURN
END
