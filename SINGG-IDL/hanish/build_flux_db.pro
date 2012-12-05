PRO build_flux_db,RUNLIST=runlist
; OPTIONAL INPUTS
; runlist    List of runs to include in DB

  IF NOT KEYWORD_SET(runlist) THEN $
    runlist = ['Run01','Run02','Run03','Run04','Run04s','Run05','Run06','Run07','Run08','Run09','Run10','Run11','Run12','Run13','Run15','Run17']
  n_runs = N_ELEMENTS(runlist)

  refdb = 'singg_sample'
  headerdb = "proc3_header"
  fluxdb = "singg_flux"

  spawn,"/bin/rm -f "+!singgdir+""+fluxdb+".dbf"
  spawn,"/bin/rm -f "+!singgdir+""+fluxdb+".dbh"
  spawn,"/bin/rm -f "+!singgdir+""+fluxdb+".dbx"

  dbopen,headerdb,0
  dbext,-1,"FILENAME,FILTNAME,IMTYPE,RUNID,TARGTYPE,PHOTMTRC", $
            filename,hfilter, imtype,runid,targtype,photmtrc
  dbclose,dummy

  dbopen,refdb,0
  dbext,-1,'NAME,RA,DEC,W50,VHEL', $
            refname,hra,hdec,w50,vhel

  dbopen,'hicat_feb04',0
  dbext,-1,'HIPASS_NAME,COMMENT,CFSD_FLG,EXT_FLG', $
            hiname,     hcom,  cfsd_flg,ext_flg
  dbclose

  filt_rddbfnames,'filter',fnamarr

  voptfile = !singgdir+'sr2_voptid_03.dat'
  readcol_new,voptfile,vname,vent,vid,junk1,junk2,junk3,vbest,vref,vsid,vsv, $
              v2id,v2nam,v2v,v2sq,v6id,v6nam,v6v,v6sq,vnv,vnerr,vnref, $
              COMMENT='#',/SILENT, $
;;              FORMAT='A,I,A,A,I,F,I,A,A,I,L,A,I,I,L,A,I,I,I,I,A'
              FORMAT='A,I,A,A,I,F,I,A,A,I,L,A,I,I,L,A,I,I,I,I,A'

  readcol_new,!singgdir+'singg_multiple.dat',mobj,mfile,mrun,mpr, $
              COMMENT='#',FORMAT='(A,A,A,I)',/SILENT

  !PRIV=2
  dbcreate,fluxdb,1,1,/EXTERNAL
  spawn,"mv "+fluxdb+".db* "+!singgdir

; STEP 1: Open all of the .profile files, pull the header information
; out, and put it into the arrays.
  FOR ii = 0,n_runs-1 DO BEGIN
    PRINT,"Processing "+runlist[ii]

    spawn,'ls '+STRTRIM(runlist[ii],2)+'/Proc3/Run*.catalog',catlist

    IF N_ELEMENTS(catlist) NE 1 OR NOT FILE_TEST(catlist[0]) THEN BEGIN
      PRINT,"ERROR in build_flux_db: Need one and only one catalog file in the directory",N_ELEMENTS(catlist)
      RETURN
    ENDIF
    read_catalog,catlist[0],run_struct,object_old,filter,Rfile,Nfile,Sfile, $
                         ellipse,refnum,Rmask,Nmask,nsig,/SILENT
object = update_name(object_old)
    pixsize = run_struct.pixsize

    FOR jj = 0,N_ELEMENTS(object)-1 DO BEGIN
      PRINT,"  Processing object "+object[jj]
      idir = STRTRIM(runlist[ii],2)+'/Proc4/'+STRTRIM(object_old[jj],2)+'/'

      imlist = STRARR(3)
      n_files = N_ELEMENTS(imlist)

      imlist[0] = STRMID(Rfile[jj],0,STRLEN(Rfile[jj])-5)+"_ss.fits"
      imlist[1] = STRMID(Nfile[jj],0,STRLEN(Nfile[jj])-5)+"_ss.fits"
      imlist[2] = STRMID(Sfile[jj],0,STRLEN(Sfile[jj])-5)+"_ss.fits"

      Risofile = idir+STRMID(STRTRIM(Rfile[jj],2),0,STRLEN(Rfile[jj])-5)+ $
                 "_ss_isophote.profile"
      Nisofile = idir+STRMID(STRTRIM(Nfile[jj],2),0,STRLEN(Nfile[jj])-5)+ $
                 "_ss_isophote.profile"
      Sisofile = idir+STRMID(STRTRIM(Sfile[jj],2),0,STRLEN(Sfile[jj])-5)+ $
                 "_ss_isophote.profile"
      Rbrtfile = idir+STRMID(STRTRIM(Rfile[jj],2),0,STRLEN(Rfile[jj])-5)+ $
                 "_ss_brightness.profile"
      Nbrtfile = idir+STRMID(STRTRIM(Nfile[jj],2),0,STRLEN(Nfile[jj])-5)+ $
                 "_ss_brightness.profile"
      Sbrtfile = idir+STRMID(STRTRIM(Sfile[jj],2),0,STRLEN(Sfile[jj])-5)+ $
                 "_ss_brightness.profile"

;; Need a better way to do this.
IF FILE_TEST(idir+imlist[0]) AND FILE_TEST(idir+imlist[1]) AND FILE_TEST(idir+imlist[2]) AND $
FILE_TEST(Risofile) AND FILE_TEST(Rbrtfile) AND $
FILE_TEST(Nisofile) AND FILE_TEST(Nbrtfile) AND $
FILE_TEST(Sisofile) AND FILE_TEST(Sbrtfile) THEN BEGIN

      ew50i_f = calc_ew(Risofile,Nisofile,Sisofile,err_ew50i_f,ew50radi_f,PIXSIZE=pixsize)
      ew50b_f = calc_ew(Rbrtfile,Nisofile,Sbrtfile,err_ew50b_f,ew50radb_f,PIXSIZE=pixsize)
      ew50i_t = calc_ew(Risofile,Nisofile,Sisofile,err_ew50i_t,ew50radi_t,PIXSIZE=pixsize,/TOTAL)
      ew50b_t = calc_ew(Rbrtfile,Nisofile,Sbrtfile,err_ew50b_t,ew50radb_t,PIXSIZE=pixsize,/TOTAL)

      FOR kk = 0,n_files-1 DO BEGIN
; Set the basic variables, the ones that are independent of the number
; of galaxies in the image.

        brtfile = idir+STRMID(imlist[kk],0,STRLEN(imlist[kk])-5)+ $
                  "_brightness.profile"
        IF NOT FILE_TEST(brtfile) THEN BEGIN
          PRINT,"Missing brightness-peak profile: ",brtfile
          RETURN
        ENDIF
        isofile = idir+STRMID(imlist[kk],0,STRLEN(imlist[kk])-5)+ $
                  "_isophote.profile"
        IF NOT FILE_TEST(isofile) THEN BEGIN
          PRINT,"Missing isophote-centered profile: ",isofile
          RETURN
        ENDIF

; Read the header of the .fits image, just to get the size of each
; pixel, the RA/DEC, and the photometry patching.
        fits_read,idir+imlist[kk],img,hd,/header_only

        photflag = STRTRIM(SXPAR(hd,'PHOTMTRC'),2)
        delsky = SXPAR(hd,'DELSKY')
        delsub = STRTRIM(SXPAR(hd,'DELSUB'),2)
        IF delsub EQ 'Y' THEN delsky = 0.0 ; already been corrected
        IF photflag EQ 'P' THEN BEGIN
; If this was one of those galaxies where we patched the photometry,
; the value stored in the flux profiles might not be right.
          photval = SXPAR(hd,'PHOTFLUX',count=count)
          IF count GT 0 THEN scale = photval ELSE scale = SXPAR(hd,'PHOTFLAM')
          merge_profile_headers,brtfile,isofile,dbs,DELSKY=delsky,SCALE=scale
        ENDIF ELSE BEGIN
          merge_profile_headers,brtfile,isofile,dbs,DELSKY=delsky
        ENDELSE

; Next, pull some basic information from the header DB.  We COULD
; extract some of this from the actual header we just read in, but
; this is probably safer in the long run.
        good = 1b
        index = WHERE(STRTRIM(filename,2) EQ STRTRIM(imlist[kk],2) AND $
                      'Run'+STRTRIM(runid,2) EQ runlist[ii],ncount)
        IF ncount NE 1 THEN BEGIN
          PRINT,"ERROR in build_flux_db: incorrect number of header matches"
          PRINT,"  Filename: ",imlist[kk]," Run: ",runlist[ii], ncount
          RETURN
        ENDIF

        FOR ll = 0,dbs[0].numgals-1 DO BEGIN
; Now, add all the variables that weren't handled in the automatic way.
          dbs[ll].runid = STRTRIM(runlist[ii],2)
          dbs[ll].object = STRTRIM(object[jj],2) ;;update_name(object[jj])
          dbs[ll].filter = STRTRIM(hfilter[index[0]],2)
          dbs[ll].imtype = STRTRIM(imtype[index[0]],2)
          dbs[ll].photmtrc = photmtrc[index[0]]

          xyad,hd,dbs[ll].xcenter_brt,dbs[ll].ycenter_brt,temp_ra,temp_dec
; RA, DEC are actually RA_BRT and DEC_BRT.
          dbs[ll].ra = temp_ra
          dbs[ll].dec = temp_dec

          xyad,hd,dbs[ll].xcenter_iso,dbs[ll].ycenter_iso,temp_ra,temp_dec
          dbs[ll].ra_iso = temp_ra
          dbs[ll].dec_iso = temp_dec

          dbs[ll].ew50_f_brt = ew50b_f[ll]
          dbs[ll].err_ew50_f_brt = err_ew50b_f[ll]
          dbs[ll].r50_ew_f_brt = ew50radb_f[ll]

          dbs[ll].ew50_t_brt = ew50b_t[ll]
          dbs[ll].err_ew50_t_brt = err_ew50b_t[ll]
          dbs[ll].r50_ew_t_brt = ew50radb_t[ll]

          dbs[ll].ew50_f_iso = ew50i_f[ll]
          dbs[ll].err_ew50_f_iso = err_ew50i_f[ll]
          dbs[ll].r50_ew_f_iso = ew50radi_f[ll]

          dbs[ll].ew50_t_iso = ew50i_t[ll]
          dbs[ll].err_ew50_t_iso = err_ew50i_t[ll]
          dbs[ll].r50_ew_t_iso = ew50radi_t[ll]

          IF LONG(dbs[ll].fluxcal) NE 1 THEN BEGIN
; Flux routines were never run, and it shouldn't be stored in the
; database.
;;            PRINT,dbs[ll].fluxcal,targtype[index[0]]
            PRINT,"ERROR in build_flux_db: image has not been converted to physical units ",imlist[kk]
;;            RETURN
            good = 0b
            dbs[ll].fluxcal = 'F'
          ENDIF ELSE BEGIN
            dbs[ll].fluxcal = 'T'
          ENDELSE

; In the few cases where multiple HIPASS targets fell into a single
; field, we need to make sure the right names are used.
          objtemp = dbs[ll].object
          galtemp = dbs[ll].galindex
          numtemp = dbs[ll].numgals

          update_object,objtemp,galtemp,numtemp

          dbs[ll].object = objtemp
          dbs[ll].galindex = galtemp
          dbs[ll].numgals = numtemp

; Now, do the HICAT matching.
          hindex = WHERE(STRUPCASE(STRTRIM(hiname,2)) EQ STRUPCASE(STRTRIM(objtemp,2)),hicount)
          IF hicount GT 0 THEN BEGIN
            dbs[ll].cfsd_flg = cfsd_flg[hindex[0]]
            dbs[ll].ext_flg = ext_flg[hindex[0]]
            dbs[ll].hcat_comm = LONG(hcom[hindex[0]] GT 0)
          ENDIF ELSE BEGIN
            dbs[ll].cfsd_flg = -999
            dbs[ll].ext_flg = -999
            dbs[ll].hcat_comm = -999
          ENDELSE

; Since we split some sources, have to do refindex in here.
          refindex = WHERE(STRTRIM(refname,2) EQ STRTRIM(objtemp,2),refcount)
          IF refcount GT 1 THEN BEGIN
; In the reference database there should be one entry per HIPASS object
            PRINT,"ERROR in build_flux_db: multiple reference matches ",fobject[netindex[ii]],refcount
            RETURN
          ENDIF
;;print,objtemp,hicount,refcount
          IF refcount EQ 1 THEN BEGIN
            dbs[ll].vel_match = 0
            vindex = WHERE(STRTRIM(vname,2) EQ objtemp+':S'+STRTRIM(STRING(ll+1),2),vcount)
            IF vcount EQ 0 THEN BEGIN
              dbs[ll].vel_match = 1
            ENDIF ELSE BEGIN
              dvel = ABS(vhel[refindex[0]] - vbest[vindex[0]])
              IF dvel GT 500.0 THEN BEGIN
                dbs[ll].vel_match = 3
              ENDIF ELSE BEGIN
                IF dvel GT 0.5*W50[refindex[0]] THEN dbs[ll].vel_match = 2
              ENDELSE
            ENDELSE
            gcirc,1,(dbs[ll].ra/15.0),dbs[ll].dec,(hra[refindex[0]]/15.0),hdec[refindex[0]],ang
            dbs[ll].sep_hipass = ang/60.0
          ENDIF ELSE BEGIN
            dbs[ll].vel_match = -999
            dbs[ll].sep_hipass = -999.0
          ENDELSE

;;          dbs[ll].edge_flg = 
;;          dbs[ll].overlap_flg = 
;;          dbs[ll].manycr_flg = 
;;          dbs[ll].hump_flg = 

          mind = WHERE(STRTRIM(mobj,2) EQ objtemp AND $
                       STRTRIM(mrun,2) EQ runlist[ii],mcount)
          IF mcount GT 1 THEN BEGIN
            mpmin = 999
            CASE dbs[ll].imtype OF
              'cont': BEGIN
                  IF STRPOS(dbs[ll].filter,'R') GE 0 THEN contchr = 'R' $
                                                     ELSE contchr = 'C'
                  mind2 = WHERE(STRPOS(mfile[mind],'_'+contchr+'sub') GT 0,mcount2)
                  mpmin = MIN(mpr[mind[mind2]])
                END
              'onband': BEGIN
                  nind = WHERE(STRTRIM(fnamarr[*,0],2) EQ dbs[ll].filter,ncount)
                  nbname = fnamarr[nind[0],1]
                  mind2 = WHERE((STRPOS(mfile[mind],nbname) GT 0 OR (STRPOS(mfile[mind],'_6') LT 0)),mcount2)
                  mpmin = MIN(mpr[mind[mind2]])
                END
              'net': BEGIN
                  mind2 = WHERE(mfile[mind] EQ dbs[ll].filename,mcount2)
                  mpmin = mpr[mind[mind2[0]]]
                END
              ELSE: BEGIN
                  PRINT,'ERROR in build_flux_db: invalid image type: ',dbs[ll].imtype
                  RETURN
                END
            ENDCASE
            dbs[ll].mult_priority = mpmin
          ENDIF ELSE BEGIN
            IF mcount EQ 1 THEN dbs[ll].mult_priority = mpr[mind[0]] $
                           ELSE dbs[ll].mult_priority = 1
          ENDELSE

        ENDFOR
; Can't just use filename to match, since each file contains multiple
; sources to be added.

        IF good THEN singg_dbupdate,fluxdb,["FILENAME","OBJECT","GALINDEX","RUNID"], $
                        [[dbs.filename],[dbs.object],[STRING(dbs.galindex)],[dbs.runid]],dbs

      ENDFOR

endif else begin
 print,'    ERROR in build_flux_db: missing files: ',object[jj]
endelse

    ENDFOR

  ENDFOR

  dbopen,fluxdb,1
  dbindex
  dbclose,dummy

END
