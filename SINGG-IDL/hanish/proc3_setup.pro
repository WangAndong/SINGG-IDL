PRO proc3_setup,RUNLIST=runlist,FORCE=force
; Sets up all of the files and directories needed for our later
; processing steps.
; Replaces: singg_setup, make_catalog, fluxlist
; OPTIONAL INPUTS
;   runlist         List of runs to be processed
;   /force          Create the files, even if we think they might not
;                     need it.  Note that in some cases we just don't
;                     know, so this flag should be used after any
;                     major changes; minor stuff you can do by hand.

  forceflag = KEYWORD_SET(force)
  spawn,'pwd',cdir
  filt_rddbfnames,'filter',fnamarr

  IF NOT KEYWORD_SET(runlist) THEN $
    runlist = ['Run01','Run02','Run03','Run04','Run04s','Run05','Run06','Run07','Run08','Run09','Run10','Run11','Run12','Run13','Run15','Run17']
  n_runs = N_ELEMENTS(runlist)

  spawn,'ls '+!singgdir+'filtcoef/J*_filtcoef.dat',coefflist
  longlist = STRMID(coefflist,STRLEN(!singgdir+'filtcoef/'),30)
  FOR ii = 0,N_ELEMENTS(coefflist)-1 DO BEGIN
    underpos = STRPOS(longlist[ii],'_')
    longlist[ii] = STRMID(longlist[ii],0,underpos)
  ENDFOR

  contlist = ['R','cont','V','I'] ; list name abbreviations
  contlist2 = ['R','C','V','I'] ; filename abbreviations

  hk_opendbs,sampstr

  FOR ii = 0,n_runs-1 DO BEGIN
    runval = STRTRIM(runlist[ii],2)

    IF forceflag THEN BEGIN
      spawn,'/bin/rm -f '+runval+'/Proc3/J*/obj_*.lis'
      spawn,'/bin/rm -f '+runval+'/Proc2/Obj/obj_*.lis'
      spawn,'/bin/rm -f '+runval+'/Proc3/*.catalog'
    ENDIF

    PRINT,'Setting up Proc3/4 for '+runval

    proc2dir = cdir[0]+'/'+runval+'/Proc2/'
; The Proc3 and Proc4 directories would already be in place, from
; hk_setup, but we'll check just to be safe.
    proc3dir = cdir[0]+'/'+runval+'/Proc3/'
    IF NOT FILE_TEST(proc3dir) THEN spawn,'mkdir '+proc3dir
    proc4dir = cdir[0]+'/'+runval+'/Proc4/'
    IF NOT FILE_TEST(proc4dir) THEN spawn,'mkdir '+proc4dir

    fits_read,proc2dir+'Ref/basic_mask.fits',bimg,bhd,/header_only

    catfile = proc3dir+runval+'.catalog'
    catflag = forceflag OR NOT FILE_TEST(catfile)
    IF catflag THEN BEGIN
; We want to keep this separate from the other list files because
; there's a chance this file was created previously, and edited.
      OPENW,unit,catfile,/GET_LUN

      PRINTF,unit,'# RUNID = '+runval
      PRINTF,unit,'# OBSERVAT = '+STRTRIM(SXPAR(bhd,'OBSERVAT'),2)
      PRINTF,unit,'# TELESCOP = '+STRTRIM(SXPAR(bhd,'TELESCOP'),2)
      PRINTF,unit,'# OBSERVER = '+STRTRIM(SXPAR(bhd,'OBSERVER'),2)
      PRINTF,unit,'# PIXSIZE = '+STRTRIM(SXPAR(bhd,'XPIXSIZE'),2)
      PRINTF,unit,'# object filt R-image         narrow-image       subtracted-image   ellipse file         pos.ref. R-mask      narrow-mask    nsig'

      CLOSE,unit
      FREE_LUN,unit
    ENDIF

    listfile1 = proc4dir+runval+'.lis'
    IF FILE_TEST(listfile1) THEN spawn,'/bin/rm -f '+listfile1
    spawn,'echo "# image         directory photcal" > '+listfile1

    listfile2 = proc4dir+runval+'_net.lis'
    IF FILE_TEST(listfile2) THEN spawn,'/bin/rm -f '+listfile2
    spawn,'echo "# subtracted-image directory filterfile photcal niirat" > '+listfile2

    spawn,'ls '+proc2dir+'Obj/obj???????.fits '+proc2dir+'Obj/obj???????.fits.gz',imlist
    IF NOT FILE_TEST(imlist[0]) THEN BEGIN
      PRINT,'ERROR in proc3_setup: cannot find valid Source files'
      RETURN
    ENDIF

    n_images = N_ELEMENTS(imlist)

    PRINT,' Assembling filter lists'

    objs = STRARR(100) ; all objects observed in this run
    num_objs = 0

    filts = STRARR(22) ; all filters used in this run
    num_filts = 0

    FOR jj = 0,n_images-1 DO BEGIN
;;      PRINT,'  '+imlist[jj]

      junk = readfits(imlist[jj],hd,/SILENT)

      patch_target,hd ; If it had multiple targets, we want to make sure the script uses the right one.

      target = STRTRIM(SXPAR(hd,'TARGET'),2)
      targtype = STRTRIM(SXPAR(hd,'TARGTYPE'),2)
      filter = STRTRIM(SXPAR(hd,'FILTNAME'),2)

      IF targtype EQ 'SINGG' OR targtype EQ 'REJECT' THEN BEGIN
        filt2 = singg_filtnam(fnamarr,filter,pos,/SILENT) ; we just want 'pos'
        filt = STRTRIM(fnamarr[pos[0],1],2)
        IF filt EQ '6850' THEN filt = 'cont'
        ind = WHERE(STRTRIM(filts,2) EQ filt,count)
        IF count EQ 0 THEN BEGIN
          filts[num_filts] = filt
          num_filts = num_filts + 1
        ENDIF

        ind = WHERE(STRTRIM(objs,2) EQ target,count)
        IF count EQ 0 THEN BEGIN
          objs[num_objs] = target
          num_objs = num_objs + 1
        ENDIF

        IF STRPOS(imlist[jj],'.gz') GT 0 THEN BEGIN
; We want the non-gzipped name.
          objfile = STRMID(imlist[jj],STRLEN(imlist[jj])-18,15) ; 'obj???????.fits'
        ENDIF ELSE BEGIN
          objfile = STRMID(imlist[jj],STRLEN(imlist[jj])-15,15) ; 'obj???????.fits'
        ENDELSE

; Make sure the Proc3 and Proc4 target subdirectories are in place.
; Obviously, the mkdir's will only trigger on the first image for this
; target.
        proc3targdir = proc3dir+target+'/'
        IF NOT FILE_TEST(proc3targdir) THEN spawn,'mkdir '+proc3targdir
        proc4targdir = proc4dir+target+'/'
        IF NOT FILE_TEST(proc4targdir) THEN spawn,'mkdir '+proc4targdir

; Add to the appropriate .lis files.
; The results will be obj_R.lis, obj_cont.lis, or obj_6???.lis (or
; obj_V.lis and obj_I.lis for the one run that included those)

; First, add to the big Proc2 lists.
        outlis = cdir[0]+'/'+runval+'/Proc2/Obj/obj_'+filt+'.lis'
        IF FILE_TEST(outlis) THEN spawn,'echo "'+objfile+'" >> '+outlis $
                             ELSE spawn,'echo "'+objfile+'" > '+outlis
; Then, add to the appropriate Proc3 list.
        outlis = proc3targdir+'obj_'+filt+'.lis'
; print,'  --> ',outlis,' ',filt,' '
        IF FILE_TEST(outlis) THEN spawn,'echo "'+objfile+'" >> '+outlis $
                             ELSE spawn,'echo "'+objfile+'" > '+outlis

      ENDIF
    ENDFOR

; Now that all of the filter list files are in place, create the
; filter-specific mask files.  If you added a bunch of new files,
; rerun this by hand.

    filtmaskflag = forceflag
    PRINT,' Creating filter-specific mask files: '
    FOR jj = 0,num_filts-1 DO BEGIN
      maskfile = proc2dir+'/Ref/mask_'+filts[jj]+'.fits'
      filtmaskflag = filtmaskflag OR NOT FILE_TEST(maskfile)
    ENDFOR

; That is, if we're in /force mode, or if ANY of the filter-specific
; masks are missing, rebuild the set.
    IF filtmaskflag THEN BEGIN
      PRINT,'  Creating filter-specific mask files for '+runval
      make_filter_masks,indir=proc2dir
    ENDIF ELSE BEGIN
      PRINT,'  All filter masks created, skipping '+runval
    ENDELSE
    
; Now that we have the directories in place, start linking combined
; images.  Of course, the files linked to won't be in place yet.
    PRINT,' Setting up links'
    FOR jj = 0,num_objs-1 DO BEGIN
      target = STRTRIM(objs[jj],2)
      sourcedir = cdir[0]+'/Sources/'+target+'/'
      proc3targdir = proc3dir+target+'/'
      proc4targdir = proc4dir+target+'/'

      PRINT,'  '+runval+' '+target
      Rcheck = LONARR(N_ELEMENTS(contlist))
      contfiles = proc3targdir+'obj_'+STRTRIM(contlist,2)+'.lis'
      FOR kk = 0,N_ELEMENTS(contlist)-1 DO BEGIN
        Rcheck[kk] = LONG(FILE_TEST(contfiles[kk]))
      ENDFOR
      Rind = WHERE(Rcheck EQ 1,num_R)
      IF num_R EQ 0 THEN BEGIN
        PRINT,"WARNING in proc3_setup: missing continuum image lists ",target
        PRINT,"  R: ",Rcheck[0]
        PRINT,"  cont: ",Rcheck[1]
        PRINT,"  V/I: ",(Rcheck[2] + Rcheck[3])
      ENDIF

      spawn,'ls '+proc3targdir+'obj_6???.lis',narrowlist
      Ncheck = FILE_TEST(narrowlist[0])
      IF NOT Ncheck THEN BEGIN
        PRINT,"WARNING in proc3_setup: missing Narrow-band image lists ",target
        num_narrow = 0
      ENDIF ELSE BEGIN
        num_narrow = N_ELEMENTS(narrowlist)
      ENDELSE

; The image names will depend on how many filter combos existed for
; this galaxy.
      IF num_R GT 0 THEN BEGIN
        Rfilt = contlist2[Rind]
        Rfile = target+'_'+Rfilt
        Rmask = 'mask_'+contlist[Rind]+'.fits'
      ENDIF
 
      IF Ncheck THEN BEGIN
        Nfilt = STRMID(narrowlist,STRLEN(proc3targdir)+4,4)
        Nfile = target+'_'+Nfilt
        Nmask = 'mask_'+Nfilt+'.fits'
      ENDIF

      IF num_R GT 0 AND num_narrow GT 0 THEN BEGIN
        Sfile = STRARR(num_R,num_narrow)
        Sval = LONG(num_R GT 1)*10 + LONG(num_narrow GT 1)

        CASE Sval OF
          0:  BEGIN
                Sfile[0,0] = target+'_'+Rfilt+'sub'
              END
          1:  BEGIN
                Sfile[0,*] = target+'_'+Nfilt+'_'+Rfilt[0]+'sub'
              END
          10: BEGIN
                Sfile[*,0] = target+'_'+Nfilt[0]+'_'+Rfilt+'sub'
              END
          11: BEGIN
                FOR ll = 0,num_narrow-1 DO BEGIN
                  Sfile[*,ll] = target+'_'+Nfilt[ll]+'_'+Rfilt+'sub'
                ENDFOR
              END
          ELSE:  BEGIN
                PRINT,"ERROR in proc3_setup: invalid CASE value ",Sval
                RETURN
              END
        ENDCASE
      ENDIF

; Clear out the existing links.  This one COULD be tied to the /force
; option, but they're just links, it'd take more time to check for
; their existence then it'd take to just remake them.
    spawn,'/bin/rm -f '+sourcedir+'J*'+runval+'*'

; The continuum .fits images:
      FOR kk = 0,num_R-1 DO BEGIN
        spawn,'ln -s '+proc3targdir+Rfile[kk]+'.fits '+sourcedir+Rfile[kk]+'_'+runval+'.fits'
        spawn,'ln -s '+proc4targdir+Rfile[kk]+'_ss.fits '+sourcedir+Rfile[kk]+'_'+runval+'_ss.fits'
        spawn,'ln -s '+proc4targdir+Rfile[kk]+'_ss_brightness.profile '+sourcedir+Rfile[kk]+'_'+runval+'_ss_brightness.profile'
        spawn,'ln -s '+proc4targdir+Rfile[kk]+'_ss_isophote.profile '+sourcedir+Rfile[kk]+'_'+runval+'_ss_isophote.profile'
      ENDFOR
; The narrow-band .fits images:
      FOR ll = 0,num_narrow-1 DO BEGIN
        spawn,'ln -s '+proc3targdir+Nfile[ll]+'.fits '+sourcedir+Nfile[ll]+'_'+runval+'.fits'
        spawn,'ln -s '+proc4targdir+Nfile[ll]+'_ss.fits '+sourcedir+Nfile[ll]+'_'+runval+'_ss.fits'
        spawn,'ln -s '+proc4targdir+Nfile[ll]+'_ss_brightness.profile '+sourcedir+Nfile[ll]+'_'+runval+'_ss_brightness.profile'
        spawn,'ln -s '+proc4targdir+Nfile[ll]+'_ss_isophote.profile '+sourcedir+Nfile[ll]+'_'+runval+'_ss_isophote.profile'
      ENDFOR
; The subtracted .fits images:
      FOR kk = 0,num_R-1 DO BEGIN
        FOR ll = 0,num_narrow-1 DO BEGIN
          spawn,'ln -s '+proc3targdir+Sfile[kk,ll]+'.fits '+sourcedir+Sfile[kk,ll]+'_'+runval+'.fits'
          spawn,'ln -s '+proc4targdir+Sfile[kk,ll]+'_ss.fits '+sourcedir+Sfile[kk,ll]+'_'+runval+'_ss.fits'
          spawn,'ln -s '+proc4targdir+Sfile[kk,ll]+'_ss_brightness.profile '+sourcedir+Sfile[kk,ll]+'_'+runval+'_ss_brightness.profile'
          spawn,'ln -s '+proc4targdir+Sfile[kk,ll]+'_ss_isophote.profile '+sourcedir+Sfile[kk,ll]+'_'+runval+'_ss_isophote.profile'
          spawn,'ln -s '+proc4targdir+Sfile[kk,ll]+'_mask.fits '+sourcedir+Sfile[kk,ll]+'_'+runval+'_mask.fits'
        ENDFOR
      ENDFOR

; The ellipse file
      ellfile = target+'_ellipse.dat'
      spawn,'ln -s '+proc4targdir+ellfile+' '+sourcedir+target+'_'+runval+'_ellipse.dat'

; The mask files
      spawn,'ln -s '+proc4targdir+target+'_mask.fits '+sourcedir+target+'_'+runval+'_mask.fits'
      spawn,'ln -s '+proc4targdir+target+'_mask.seg.fits '+sourcedir+target+'_'+runval+'_mask.seg.fits'

; Find posref.  This should be the centrally-located R-band image; if
; there are no R-band, use 6850-band.
; If there are no R or 6850 images, we won't be putting a line in the
; catalog for combining anyway.
      IF num_R GT 0 THEN BEGIN
; Use whichever continuum list comes first: R, 6850, V, or I.
        listfile = 'obj_'+contlist[Rind[0]]+'.lis'
        readcol_new,proc3targdir+listfile,Rlist,COMMENT='#',FORMAT='(A)',/SILENT
        posfile = find_middle(proc2dir+'Obj/'+Rlist)
        posref = STRMID(posfile,STRLEN(proc2dir)+7,7)
      ENDIF ELSE BEGIN
; No R-band image
        posfile = find_middle(proc2dir+'Obj/'+narrowlist)
        posref = STRMID(posfile,STRLEN(proc2dir)+7,7)
      ENDELSE

; Now, assemble the catalog file line entry for each galaxy/filter combo.
      catfile = proc3dir+runval+'.catalog'
      FOR kk = 0,num_R-1 DO BEGIN
        FOR ll = 0,num_narrow-1 DO BEGIN
; Note that it'll only put in lines for valid R/narrow combos; if one
; of our checks failed before, it won't write the line.
          IF catflag THEN BEGIN
            inline = target+' '+Nfilt[ll]+' '+Rfile[kk]+'.fits '+Nfile[ll]+ $
                     '.fits '+Sfile[kk,ll]+'.fits '+ellfile+' '+posref+' '+ $
                     Rmask[kk]+' '+Nmask[ll]+' 30'
; append a line to the catalog file
            spawn,'echo "'+inline+'" >> '+catfile
          ENDIF
        ENDFOR
      ENDFOR

    ENDFOR

  ENDFOR

; Now that all the run-specific stuff is sorta done, go into the STD
; directory and set up the list files needed.
  PRINT,'Managing Standard images'
  stddir = cdir[0]+'/STD/'
  spawn,'/bin/rm -f '+stddir+'obj*.lis'
  spawn,'ls '+stddir+'obj???????.fits '+stddir+'obj???????.fits.gz',stdlist
  IF NOT FILE_TEST(stdlist[0]) THEN BEGIN
    PRINT,'ERROR in proc3_setup: cannot find valid Standard files'
    RETURN
  ENDIF

  FOR ii = 0,N_ELEMENTS(stdlist)-1 DO BEGIN
    IF STRPOS(stdlist[ii],'.gz') GT 0 THEN BEGIN
      img = readfits(stdlist[ii],hd,/SILENT)
      stdfile = STRMID(stdlist[ii],STRLEN(stdlist[ii])-18,15)
    ENDIF ELSE BEGIN
      fits_read,stdlist[ii],img,hd,/header_only
      stdfile = STRMID(stdlist[ii],STRLEN(stdlist[ii])-15,15)
    ENDELSE

    filtname = SXPAR(hd,'FILTNAME')
    filt2 = singg_filtnam(fnamarr,filter,pos,/SILENT) ; we just want 'pos'
    filt = STRTRIM(fnamarr[pos[0],1],2)

    outlis = stddir+'obj_'+filt+'.lis'
    IF FILE_TEST(outlis) THEN spawn,'echo '+stdfile+' >> '+outlis $
                         ELSE spawn,'echo '+stdfile+' > '+outlis
  ENDFOR

; And then dump the contents of the Unknown directory into a text file.
  PRINT,'Managing Unknown images'
  unkdir = cdir[0]+'/Unknown/'
  spawn,'ls '+unkdir+'obj???????.fits '+unkdir+'obj???????.fits.gz',unklist
  IF NOT FILE_TEST(unklist[0]) THEN BEGIN
    PRINT,'ERROR in proc3_setup: cannot find valid Unknown files'
    RETURN
  ENDIF

  unkout = unkdir+'Unknown.lis'
  IF FILE_TEST(unkout) THEN spawn,'/bin/rm -f '+unkout
  spawn,'echo "# Image        Object" > '+unkout
  FOR ii = 0,N_ELEMENTS(unklist)-1 DO BEGIN
    IF STRPOS(unklist[ii],'.gz') GT 0 THEN BEGIN
      img = readfits(unklist[ii],hd,/SILENT)
      unkfile = STRMID(unklist[ii],STRLEN(unklist[ii])-18,15)
    ENDIF ELSE BEGIN
      fits_read,unklist[ii],img,hd,/header_only
      unkfile = STRMID(unklist[ii],STRLEN(unklist[ii])-15,15)
    ENDELSE

    spawn,'echo "'+unkfile+' '+SXPAR(hd,'OBJECT')+'" >> '+unkout
  ENDFOR

  RETURN
END
