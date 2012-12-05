PRO build_source_db,RUNLIST=runlist

  IF NOT KEYWORD_SET(runlist) THEN runlist = ["Run01","Run02","Run03","Run06"]
  n_runs = N_ELEMENTS(runlist)

  headerdb = !singgdir+"/proc3_header"
  refdb = !singgdir+"/singg_sample" ; not used
  fluxdb = !singgdir+"/singg_source"

  spawn,"rm "+fluxdb+".dbf"
  spawn,"rm "+fluxdb+".dbh"

  dbopen,headerdb,0
  dbext,-1,"FILENAME,FILTNAME,IMTYPE",filename,filter,imtype
  dbclose,dummy

  !PRIV=2
  dbcreate,fluxdb,1,1,/EXTERNAL

  dbfile = fluxdb+".dbd"

; dbs is the database structure
  dbs = create_db_struct(dbfile,VNAME=varname,VTYPE=vartype)

; STEP 1: Open all of the isophote.profile files, pull the header information
; out, and put it into the arrays.
  FOR ii = 0,n_runs-1 DO BEGIN
    CD,runlist[ii]

    PRINT,"Processing "+runlist[ii]
    spawn,"ls -d j*",dirlist

    FOR jj = 0,N_ELEMENTS(dirlist)-1 DO BEGIN
      CD,dirlist[jj]

      id = "J"+STRMID(dirlist[jj],1,STRLEN(dirlist[jj])-1)
      PRINT,"  Processing object "+dirlist[jj]
      spawn,"ls "+id+"*_ss.fits",imlist
      n_files = N_ELEMENTS(imlist)
      IF n_files EQ 0 THEN BEGIN
        PRINT,"ERROR in build_source_db: you forgot to unzip the .fits files ", $
              dirlist[jj]
        RETURN
      ENDIF

      FOR kk = 0,n_files-1 DO BEGIN
;; Temporary fix: clear out the junk in the array
        clear_struct,dbs,vartype

;; Redo it so that the structure is still only for one galaxy at a time.
        
; Set the basic variables, the ones that are independent of the number
; of galaxies in the image.
        dbs.object = id
        dbs.runname = runlist[ii]
        dbs.profileb = STRMID(imlist[kk],0,STRLEN(imlist[kk])-5)+"_brightness.profile"
        dbs.profilei = STRMID(imlist[kk],0,STRLEN(imlist[kk])-5)+"_isophote.profile"
        dbs.filename = imlist[kk]

; Read the header of the .fits image, just to get the size of each pixel.
        fits_read,imlist[kk],img,hd,/header_only
        getrot,hd,rot,cdelt 
        cdelt  = abs(cdelt)*3600.
        as_pix = cdelt[0]

; Next, pull some basic information from the header DB.  We COULD
; extract some of this from the actual header we just read in, but
; this is probably safer in the long run.
        index = WHERE(STRTRIM(filename,2) EQ STRTRIM(imlist[kk],2),count)
        IF count NE 1 THEN BEGIN
          PRINT,"ERROR in build_source_db: incorrect number of header matches"
          PRINT,"  Filename: ",imlist[kk]," Run: ",runlist[ii], count
          RETURN
        ENDIF

        dbs.filter = filter[index[0]]
        dbs.imtype = imtype[index[0]]

; Now, check to see if the two files exist, and read them.
        IF FILE_TEST(dbs.profileb) THEN BEGIN
          readcol_new,dbs.profileb,keyword,keyval,comment1,comment2, $
                      FORMAT="X,A,X,F,X,A,A",SKIPLINE=2,/SILENT

          bxcenter = keyval[WHERE(keyword EQ "XCENTER")] ; 0
          bycenter = keyval[WHERE(keyword EQ "YCENTER")] ; 1
          baxerat = keyval[WHERE(keyword EQ "AXERAT")] ; 2
          bposang = keyval[WHERE(keyword EQ "POSANG")] ; 3
          bskylev = keyval[WHERE(keyword EQ "SKYLEV")] ; 4,5
          bskysigpx = keyval[WHERE(keyword EQ "SKYSIGPX")] ; 6,7
          bfluxrad = keyval[WHERE(keyword EQ "FLUXRAD")]*as_pix ; 10
          bflux = keyval[WHERE(keyword EQ "FLUX")] ; 11,12
          bhlradius = keyval[WHERE(keyword EQ "HLRADIUS")]*as_pix ; 13
          bhlcrcflx = keyval[WHERE(keyword EQ "HLCRCFLX")]/(as_pix)^2.0 ; 15,16
        ENDIF ELSE BEGIN
          PRINT,"Missing brightness-peak profile: ",dbs.profileb
          RETURN
        ENDELSE

        IF FILE_TEST(dbs.profilei) THEN BEGIN
          readcol_new,dbs.profilei,keyword,keyval,comment1,comment2, $
                      FORMAT="X,A,X,F,X,A,A",SKIPLINE=2,/SILENT

          ixcenter = keyval[WHERE(keyword EQ "XCENTER")] ; 0
          iycenter = keyval[WHERE(keyword EQ "YCENTER")] ; 1
          iaxerat = keyval[WHERE(keyword EQ "AXERAT")] ; 2
          iposang = keyval[WHERE(keyword EQ "POSANG")] ; 3
          iskylev = keyval[WHERE(keyword EQ "SKYLEV")] ; 4,5
          iskysigpx = keyval[WHERE(keyword EQ "SKYSIGPX")] ; 6,7
          ifluxrad = keyval[WHERE(keyword EQ "FLUXRAD")]*as_pix ; 10
          iflux = keyval[WHERE(keyword EQ "FLUX")] ; 11,12
          ihlradius = keyval[WHERE(keyword EQ "HLRADIUS")]*as_pix ; 13
          ihlcrcflx = keyval[WHERE(keyword EQ "HLCRCFLX")]/(as_pix)^2.0 ; 15,16
        ENDIF ELSE BEGIN
          PRINT,"Missing isophote-centered profile: ",dbs.profilei
          RETURN
        ENDELSE


; First, figure out how many galaxies are in this image
        num_gals = N_ELEMENTS(bxcenter)
        num_gals_i = N_ELEMENTS(ixcenter)
        IF num_gals NE num_gals_i THEN BEGIN
          PRINT,"ERROR in build_source_db: isophote and brightness profiles have"
          PRINT,"  different number of galaxies."
          PRINT,filename
          RETURN
        ENDIF

        bfactor = (N_ELEMENTS(bflux) / N_ELEMENTS(bxcenter))
        ifactor = (N_ELEMENTS(iflux) / N_ELEMENTS(ixcenter))

        IF bfactor EQ 1 OR ifactor EQ 1 THEN BEGIN
; If factor=1, then the flux routines were never run, and it shouldn't
; be stored in the database.  I'm making the later logic more
; flexible, on the offhand chance that we'd be okay with this.
          PRINT,"ERROR in build_source_db: image has not been converted to physical units ",imlist[kk]
          RETURN
        ENDIF

        bindex2 = (INDGEN(num_gals) + 1) * bfactor - 1
        iindex2 = (INDGEN(num_gals) + 1) * ifactor - 1
; that is, index2 will be [0,1,2] if the units haven't been set,
; and [1,3,5] if they have.

        FOR ll = 0,num_gals-1 DO BEGIN
          dbs.galindex = ll

          dbs.xcenterb = bxcenter[ll]
          dbs.ycenterb = bycenter[ll]
          xyad,hd,dbs.xcenterb,dbs.ycenterb,dbs.ra_brt,dbs.dec_brt
          dbs.axeratb = baxerat[ll]
          dbs.posangb = bposang[ll]
          dbs.skylevb = bskylev[bindex2[ll]]
          dbs.skysigpb = bskysigpx[bindex2[ll]]
          dbs.fluxradb = bfluxrad[ll]
          dbs.fluxb = bflux[bindex2[ll]]
          dbs.hlradiub = bhlradius[ll]
          dbs.hlcrcflb = bhlcrcflx[bindex2[ll]]

          dbs.xcenteri = ixcenter[ll]
          dbs.ycenteri = iycenter[ll]
          xyad,hd,dbs.xcenteri,dbs.ycenteri,dbs.ra_iso,dbs.dec_iso
          dbs.axerati = iaxerat[ll]
          dbs.posangi = iposang[ll]
          dbs.skylevi = iskylev[iindex2[ll]]
          dbs.skysigpi = iskysigpx[iindex2[ll]]
          dbs.fluxradi = ifluxrad[ll]
          dbs.fluxi = iflux[iindex2[ll]]
          dbs.hlradiui = ihlradius[ll]
          dbs.hlcrcfli = ihlcrcflx[iindex2[ll]]

;; Want to add the following to the db as well:
;; error in flux (continuum error and sky error)
;; name of box2box file

          dbopen,fluxdb,1
          dbbuildstruct,dbs,/silent
          dbclose,dummy

        ENDFOR

      ENDFOR

      CD,".."
    ENDFOR

    CD,".."
  ENDFOR

  dbopen,fluxdb,1
  dbindex
  dbclose,dummy

END
