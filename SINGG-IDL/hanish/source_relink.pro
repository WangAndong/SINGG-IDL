PRO source_relink
; Remakes all of the Proc3/4 symbolic links in the Sources directory

  runlist = ['Run01','Run02','Run03','Run04','Run04s','Run05','Run06','Run07','Run08','Run09','Run10','Run11','Run12','Run13','Run15','Run17']
  n_runs = N_ELEMENTS(runlist)

  spawn,'pwd',cdir
  sdir = cdir[0]+'/Sources/'

  FOR ii = 0,n_runs-1 DO BEGIN
    runval = STRTRIM(runlist[ii],2)
    PRINT,'Relinking '+runval
    proc3dir = cdir[0]+'/'+runval+'/Proc3/'
    proc4dir = cdir[0]+'/'+runval+'/Proc4/'
    catfile = proc3dir+runval+'.catalog'
    IF NOT FILE_TEST(catfile) THEN BEGIN
      PRINT,'ERROR in source_relink: missing catalog file ',catfile
      RETURN
    ENDIF
    read_catalog,catfile,run_struct,object,filter,Rfile,Nfile,Sfile, $
                         ellipse,refnum,Rmask,Nmask,nsig,/SILENT

    FOR jj = 0,N_ELEMENTS(object)-1 DO BEGIN
      target = STRTRIM(object[jj],2)
      PRINT,'  '+target
      sourcedir = sdir+target+'/'
      proc3targdir = proc3dir+target+'/'
      proc4targdir = proc4dir+target+'/'

      spawn,'/bin/rm -f '+sourcedir+'J*'+runval+'*'

; Trim the '.fits' off the ends
      Rshort = STRMID(Rfile[jj],0,STRLEN(Rfile[jj])-5)
      Nshort = STRMID(Nfile[jj],0,STRLEN(Nfile[jj])-5)
      Sshort = STRMID(Sfile[jj],0,STRLEN(Sfile[jj])-5)

      ellfile = target+'_ellipse.dat'
      spawn,'ln -s '+proc4targdir+ellfile+' '+sourcedir+target+'_'+runval+'_ellipse.dat'

      spawn,'ln -s '+proc3targdir+Rshort+'.fits '+sourcedir+Rshort+'_'+runval+'.fits'
      spawn,'ln -s '+proc4targdir+Rshort+'_ss.fits '+sourcedir+Rshort+'_'+runval+'_ss.fits'
      spawn,'ln -s '+proc4targdir+Rshort+'_ss_brightness.profile '+sourcedir+Rshort+'_'+runval+'_ss_brightness.profile'
      spawn,'ln -s '+proc4targdir+Rshort+'_ss_isophote.profile '+sourcedir+Rshort+'_'+runval+'_ss_isophote.profile'

      spawn,'ln -s '+proc3targdir+Nshort+'.fits '+sourcedir+Nshort+'_'+runval+'.fits'
      spawn,'ln -s '+proc4targdir+Nshort+'_ss.fits '+sourcedir+Nshort+'_'+runval+'_ss.fits'
      spawn,'ln -s '+proc4targdir+Nshort+'_ss_brightness.profile '+sourcedir+Nshort+'_'+runval+'_ss_brightness.profile'
      spawn,'ln -s '+proc4targdir+Nshort+'_ss_isophote.profile '+sourcedir+Nshort+'_'+runval+'_ss_isophote.profile'

      spawn,'ln -s '+proc3targdir+Sshort+'.fits '+sourcedir+Sshort+'_'+runval+'.fits'
      spawn,'ln -s '+proc4targdir+Sshort+'_ss.fits '+sourcedir+Sshort+'_'+runval+'_ss.fits'
      spawn,'ln -s '+proc4targdir+Sshort+'_ss_brightness.profile '+sourcedir+Sshort+'_'+runval+'_ss_brightness.profile'
      spawn,'ln -s '+proc4targdir+Sshort+'_ss_isophote.profile '+sourcedir+Sshort+'_'+runval+'_ss_isophote.profile'
      spawn,'ln -s '+proc4targdir+Sshort+'_mask.fits '+sourcedir+Sshort+'_'+runval+'_mask.fits'

      spawn,'ln -s '+proc4targdir+target+'_mask.fits '+sourcedir+target+'_'+runval+'_mask.fits'
      spawn,'ln -s '+proc4targdir+target+'_mask.seg.fits '+sourcedir+target+'_'+runval+'_mask.seg.fits'

    ENDFOR

  ENDFOR

  RETURN
END
