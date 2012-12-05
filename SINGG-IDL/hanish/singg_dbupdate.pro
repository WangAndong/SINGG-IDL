PRO singg_dbupdate,dbname,keywords,keyvals,struct
; Wrapper for DB_BUILDSTRUCT with the new "update" logic in place. 
; If the file has never been created, run DBCREATE.  Checks to see
; if the database already includes data for the specified filename.

; INPUTS
; dbname      Name of database, no paths or extensions.  Note that it
;             must be located in !singgdir.
; keywords    Array of keywords to specify a line in the database to
;             be edited.  Typically, FILENAME is used.
; keyvals     Array of values to match to the database, matching 1:1
;             to the "keywords" argument.
; struct      Structure containing all the variables to be added to
;             the database.  All entries must be scalars (strings,
;             integers, etc., but NO ARRAYS) corresponding to the
;             values for a single input file.  The structure as a
;             whole can be an array (struct[ii].varname), though.

  dbfilename = !singgdir+""+dbname+".dbf"
  count = 0
  !PRIV=2
; Find out how many elements:
  n_sources = N_ELEMENTS(struct)
  n_keywords = N_ELEMENTS(keywords)
  sz_vals = SIZE(keyvals)
  IF (sz_vals[0] GT 1) AND (sz_vals[1] NE n_sources OR sz_vals[2] NE n_keywords) THEN BEGIN
    PRINT,"ERROR in SINGG_DBUPDATE: Numbers of sources, keywords, values must match ",n_sources,n_keywords
    PRINT,sz_vals
    RETURN
  ENDIF
  varnames = TAG_NAMES(struct)

; Check to see if the existing database has the "update" element.  If
; so, reset the value to the current date and time.
  updateind = WHERE(STRUPCASE(STRTRIM(varnames,2)) EQ "UPDATE",updcount)
  spawn,"date",datestring

  newflag = NOT FILE_TEST(dbfilename)
  IF newflag THEN BEGIN
    PRINT,"Creating new DB ",dbname
    dbcreate,dbname,1,1,/EXTERNAL
    spawn,"mv "+dbname+".db* "+!singgdir
  ENDIF ELSE BEGIN
    teststr = ''
    testvals = ''
    FOR jj = 0,n_keywords-1 DO BEGIN
      IF jj GT 0 THEN BEGIN
        teststr = teststr + ','
        testvals = testvals + ','
      ENDIF
      teststr = teststr + STRTRIM(keywords[jj],2)
      testvals = testvals + 'v'+STRTRIM(STRING(jj),2)
    ENDFOR

    dbopen,dbname,0
    test = execute("dbext,-1,'"+teststr+"',"+testvals)
    dbclose,dummy

    IF N_ELEMENTS(v0) EQ 0 THEN newflag = 1b
    IF test NE 1 AND NOT newflag THEN BEGIN
      PRINT,"ERROR in SINGG_DBUPDATE: failed DBEXT command ",teststr+","+testvals
      RETURN
    ENDIF
  ENDELSE

; Add this image to the database
  FOR ii = 0,n_sources-1 DO BEGIN

; First, see if there's already a line in the database with the right
; set of keywords.
    count = 0
    IF NOT newflag THEN BEGIN
      testwhere = ""
      FOR jj = 0,n_keywords-1 DO BEGIN
        IF n_sources EQ 1 THEN testval = STRTRIM(keyvals[jj],2) $
                          ELSE testval = STRTRIM(keyvals[ii,jj],2)
        IF jj GT 0 THEN testwhere = testwhere + " AND "
        testwhere = testwhere + "STRTRIM(v"+STRTRIM(STRING(jj),2)+",2) EQ '"+testval+"'"
      ENDFOR
      test = execute("list = WHERE("+STRTRIM(testwhere,2)+",count)")

      IF count GT 1 OR test NE 1 THEN BEGIN
        PRINT,"ERROR in SINGG_DBUPDATE: failed command: ",test,count,testwhere
        RETURN
      ENDIF
    ENDIF

    IF updcount GT 0 THEN struct[ii].update = STRTRIM(datestring,2)

    dbopen,dbname,1
; Add this image to the database
    IF count EQ 0 THEN BEGIN
; Either it's a new DB or there was no match in the existing one, so add it.
      dbbuildstruct,struct[ii],/silent
    ENDIF ELSE BEGIN
; This object was already in the existing DB, so overwrite the old values.
      dbbuildstruct,struct[ii],rownum=(list[0]+1),/silent
    ENDELSE
    dbclose,dummy
  ENDFOR

  RETURN

END
