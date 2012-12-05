PRO build_test_header,RUNLIST=runlist,VERBOSE=verbose
; Sets up the combined-image proc3 header database with values for all runs.
; OPTIONAL INPUTS
; runlist     List of runs to be processed.  Within each run, all
;             directories starting with a "j" will be used.
; /verbose    Print lots of text debug messages to the screen.

  headername = "test_header"
  headerfile = !singgdir+""+headername+".dbd"

; Cleanup, to make sure we're not appending the same galaxies again
  spawn,"/bin/rm -f "+!singgdir+""+headername+".dbf"
  spawn,"/bin/rm -f "+!singgdir+""+headername+".dbh"
  spawn,"/bin/rm -f "+!singgdir+""+headername+".dbx"

  !PRIV=2
  dbcreate,headername,1,1,/EXTERNAL
  spawn,"mv -f "+headername+".db* "+!singgdir

  IF NOT KEYWORD_SET(runlist) THEN runlist = ['Run01','Run02','Run03','Run04','Run04s','Run05','Run06','Run07','Run08','Run09','Run10','Run11','Run12','Run13','Run15','Run17']
  n_runs = N_ELEMENTS(runlist)

; This one structure, 'header', will be overwritten with new data for each run
  header = create_db_struct(headerfile,TITLE="proc3header",VNAME=varname,VTYPE=vartype)
  num_vars = N_ELEMENTS(varname)

; Read the block of filter aliases
  filt_rddbfnames,"filter",fnamarr

  FOR irun = 0,n_runs-1 DO BEGIN
    PRINT,"Starting "+runlist[irun]

    spawn,'ls -d '+runlist[irun]+'/Proc3/J*',dirlist
    n_objects = N_ELEMENTS(dirlist)

    FOR ii = 0,n_objects-1 DO BEGIN
      object = STRTRIM(STRMID(dirlist[ii],STRLEN(runlist[irun])+7,15),2)
      IF KEYWORD_SET(verbose) THEN $
          PRINT," Adding database entries for object: ",object

      spawn,'ls '+STRTRIM(dirlist[ii],2)+'/'+object+'*.fits',imlist
      n_files = N_ELEMENTS(imlist)
      IF NOT FILE_TEST(imlist[0]) THEN n_files = 0

; Anything from the header that matches the keywords will be added
; automatically.  If there's something else you want to add (like imtype), it's
; not important exactly WHERE you add it; the database will still add things in
; the order they're listed in the .dbd file.

      FOR jj = 0,n_files-1 DO BEGIN
; First, clear the structure and reset the basic variables.
        clear_struct,header,vartype

        header.runid = runlist[irun]
        header.object = update_name(object)

; Determine what type of image it is.
;        imlen = STRLEN(imlist[jj])
;        IF STRMID(imlist[jj],(imlen-8),3) EQ "sub" THEN BEGIN
        IF STRPOS(imlist[jj],'sub.fits') GT 0 THEN BEGIN
          header.imtype="net"
        ENDIF ELSE BEGIN
;          IF STRMID(imlist[jj],(imlen-10),2) EQ "_6" THEN BEGIN
          IF STRPOS(imlist[jj],'_6') GT 0 THEN BEGIN
            header.imtype="onband"
          ENDIF ELSE BEGIN
;            tempstr = STRUPCASE(STRMID(imlist[jj],(imlen-6),1))
;            IF tempstr EQ "R" OR tempstr EQ "C" THEN BEGIN
            IF STRPOS(imlist[jj],'_R.') GT 0 OR STRPOS(imlist[jj],'_C.') GT 0 $
               OR STRPOS(imlist[jj],'_I.') GT 0 OR STRPOS(imlist[jj],'_V.') GT 0 THEN BEGIN
              header.imtype="cont"
            ENDIF ELSE BEGIN
              PRINT,"ERROR in build_header_db: invalid image type for image ",imlist[jj]
              PRINT,"  for object ",object
              RETURN
            ENDELSE
          ENDELSE
        ENDELSE
print,imlist[jj]
        fits_read,imlist[jj],img,hd,/header_only

        FOR kk = 0,num_vars-1 DO BEGIN
; Next, test to see if varname[kk] is in the header
          tempval = SXPAR(hd,varname[kk],count=count)

          IF count GT 0 THEN BEGIN
; We want to make sure that if it's a string, it trims it.
            IF STRMID(vartype[kk],0,1) EQ "A" THEN tempval = STRTRIM(tempval,2)

; Patch for RA and DEC:
            IF varname[kk] EQ "RA" THEN BEGIN
              header.(kk) = sexideg(tempval)*15.d0 ; convert to decimal degrees
            ENDIF ELSE BEGIN
              IF varname[kk] EQ "DEC" THEN BEGIN
                header.(kk) = sexideg(tempval) ; convert to decimal degrees
              ENDIF ELSE BEGIN
                header.(kk) = tempval
              ENDELSE
            ENDELSE
          ENDIF
        ENDFOR ; loop over variables

; Filter is a special case
        filtname = STRTRIM(SXPAR(hd,'FILTNAME'),2)
;        filtname = STRTRIM(SXPAR(hd,"FILTER1"),2)
;        IF filtname EQ "dia" OR filtname EQ "cb" THEN filtname = STRTRIM(SXPAR(hd,"FILTER2"),2)
        filt = singg_filtnam(fnamarr,filtname,pos,/SILENT)
        header.filtname = filt[0]

        spawn,"date",datestring
        header.update = STRTRIM(datestring,2)

; Add this image to the header database
        dbopen,headername,1
        dbbuildstruct,header,/silent
        dbclose,dummy

      ENDFOR ; loop over files

    ENDFOR ; loop over objects

  ENDFOR ; loop over runs

; Now that the DB has been completed, index the whole thing.
  dbopen,headername,1
  dbindex
  dbclose,dummy

  RETURN
END
