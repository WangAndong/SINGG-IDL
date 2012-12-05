PRO sbplot_all,RUNLIST=runlist,FORCE=force

  IF NOT KEYWORD_SET(runlist) THEN runlist = ['Run01','Run02','Run03','Run04','Run04s','Run05','Run06','Run07','Run08','Run09','Run10','Run11','Run12','Run13','Run15','Run17']

  FOR ii = 0,N_ELEMENTS(runlist)-1 DO BEGIN
    PRINT,runlist[ii]
    catfile = '/data1/acs22/hanish/'+STRTRIM(runlist[ii],2)+'/Proc3/'+STRTRIM(runlist[ii],2)+'.catalog'

    read_catalog,catfile,run_struct,object,filter,Rfile,Nfile,Sfile, $
                         ellipse,refnum,Rmask,Nmask,nsig,/SILENT

    FOR jj = 0,N_ELEMENTS(object)-1 DO BEGIN
      PRINT,'  '+object[jj]
      dir = '/data1/acs22/hanish/'+STRTRIM(runlist[ii],2)+'/Proc4/'+STRTRIM(object[jj],2)+'/'
      Rssfile = STRMID(Rfile[jj],0,STRLEN(Rfile[jj])-5)+"_ss.fits"
      Nssfile = STRMID(Nfile[jj],0,STRLEN(Nfile[jj])-5)+"_ss.fits"
      Sssfile = STRMID(Sfile[jj],0,STRLEN(Sfile[jj])-5)+"_ss.fits"

      IF KEYWORD_SET(force) THEN sbplot,Rssfile,Nssfile,Sssfile,DIR=dir,/FORCE $
                            ELSE sbplot,Rssfile,Nssfile,Sssfile,DIR=dir
    ENDFOR
  ENDFOR

  RETURN
END
