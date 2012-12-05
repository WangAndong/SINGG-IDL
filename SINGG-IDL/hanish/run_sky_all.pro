PRO run_sky_all,RUNLIST=runlist,FAST=fast

  IF NOT KEYWORD_SET(runlist) THEN $
     runlist = ['Run01','Run02','Run03','Run04','Run04s','Run05','Run06','Run07','Run08','Run09','Run10','Run11','Run12','Run13','Run15','Run17']

  spawn,'pwd',cdir

  FOR ii = 0,N_ELEMENTS(runlist)-1 DO BEGIN
    dir = cdir[0]+'/'+runlist[ii]+'/'

    IF FILE_TEST(dir) THEN BEGIN
      PRINT,'Sky-subtracting '+runlist[ii]
      IF KEYWORD_SET(fast) THEN BEGIN
        run_sky,dir=dir,output=runlist[ii]+'_sky.out',/fast
      ENDIF ELSE BEGIN
        run_sky,dir=dir,output=runlist[ii]+'_sky.out',/skip
      ENDELSE
    ENDIF ELSE BEGIN
      PRINT,runlist[ii]+' not in directory.'
    ENDELSE
  ENDFOR

  RETURN
END
