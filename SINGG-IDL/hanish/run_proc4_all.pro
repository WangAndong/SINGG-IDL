PRO run_proc4_all,RUNLIST=runlist,SKIP=skip,FAST=fast

  IF NOT KEYWORD_SET(runlist) THEN $
     runlist = ['Run01','Run02','Run03','Run04','Run04s','Run05','Run06','Run07','Run08','Run09','Run10','Run11','Run12','Run13','Run15','Run17']

  spawn,'pwd',cdir

  FOR ii = 0,N_ELEMENTS(runlist)-1 DO BEGIN
    dir = cdir[0]+'/'+runlist[ii]+'/'
    catfile = dir+'/Proc3/'+STRTRIM(runlist[ii],2)+'.catalog'
    IF FILE_TEST(dir) THEN BEGIN
      PRINT,'Creating masks for '+runlist[ii]
      IF KEYWORD_SET(skip) THEN BEGIN
        IF KEYWORD_SET(fast) THEN BEGIN
          run_proc4,dir=dir,catalog=catfile,/skip,/fast
        ENDIF ELSE BEGIN
          run_proc4,dir=dir,catalog=catfile,/skip
        ENDELSE
      ENDIF ELSE BEGIN
        IF KEYWORD_SET(fast) THEN BEGIN
          run_proc4,dir=dir,catalog=catfile,/fast
        ENDIF ELSE BEGIN
          run_proc4,dir=dir,catalog=catfile
        ENDELSE
      ENDELSE
    ENDIF ELSE BEGIN
      PRINT,runlist[ii]+' not in directory.'
    ENDELSE
  ENDFOR

  RETURN
END
