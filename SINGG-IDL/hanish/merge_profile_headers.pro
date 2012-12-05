PRO merge_profile_headers,bfile,ifile,mergestr,DELSKY=delsky,SCALE=scale

  IF NOT KEYWORD_SET(delsky) THEN delsky = 0.0

  IF KEYWORD_SET(scale) THEN BEGIN
    read_profile_header,bfile,bstr,/silent,DELSKY=delsky,SCALE=scale
    read_profile_header,ifile,istr,/silent,DELSKY=delsky,SCALE=scale
  ENDIF ELSE BEGIN
    read_profile_header,bfile,bstr,DELSKY=delsky,/silent
    read_profile_header,ifile,istr,DELSKY=delsky,/silent
  ENDELSE

  IF bstr[0].numgals NE istr[0].numgals THEN BEGIN
    PRINT,"ERROR in merge_profile_headers: isophote and brightness profiles have"
    PRINT,"  different number of galaxies."
    PRINT,bfile,ifile,bstr[0].numgals,istr[0].numgals
    RETURN
  ENDIF

  dbfile = !singgdir+"/singg_flux.dbd"
  tempmergestr = create_db_struct(dbfile,TITLE="merge",VNAME=varname,VTYPE=vartype)
  clear_struct,tempmergestr,vartype
  mergestr = REPLICATE(tempmergestr,bstr[0].numgals)

  bpos = STRPOS(varname,"_BRT")
  ipos = STRPOS(varname,"_ISO")

  bkeyword = TAG_NAMES(bstr[0])
  ikeyword = TAG_NAMES(istr[0])

  FOR ii = 0,N_ELEMENTS(varname)-1 DO BEGIN
    IF bpos[ii] LT 0 AND ipos[ii] LT 0 THEN BEGIN
; A variable common to both files.  Make sure they match.
      bindex = WHERE(STRTRIM(bkeyword,2) EQ STRTRIM(varname[ii],2),bcount)
      iindex = WHERE(STRTRIM(ikeyword,2) EQ STRTRIM(varname[ii],2),icount)
      IF bcount NE 1 OR icount NE 1 THEN BEGIN
        PRINT,"ERROR in merge_profile_headers: array size mismatch ",varname[ii],bcount,icount
        RETURN
      ENDIF ELSE BEGIN
; If it's not a variable used in the profile files, it's up to the
; user to fill it in later.
        bval = bstr.(bindex[0])
        ival = istr.(iindex[0])
        badval = 0b
        CASE STRMID(vartype[ii],0,1) OF
          "A": BEGIN
                 badval = (bval NE ival)
                 dumpval = bval
               END
          "B": BEGIN
                 badval = (bval NE ival)
                 dumpval = bval
               END
          "I": BEGIN
                 bval = FIX(bval)
                 ival = FIX(ival)
                 badval = (bval NE ival)
                 dumpval = (bval+ival)/2
               END
          "J": BEGIN
                 bval = LONG(bval)
                 ival = LONG(ival)
                 badval = (bval NE ival)
                 dumpval = (bval+ival)/2
               END
          "F": BEGIN
                 bval = FLOAT(bval)
                 ival = FLOAT(ival)
                 badval = ABS(1.0 - ival/bval) GT 0.001
                 dumpval = (bval+ival)/2.0
               END
          "D": BEGIN
                 bval = DOUBLE(bval)
                 ival = DOUBLE(ival)
                 badval = ABS(1.0 - ival/bval) GT 0.001
                 dumpval = (bval+ival)/2.0
               END
          ELSE: BEGIN
                 PRINT,"ERROR in merge_profile_headers: unknown type"
                 RETURN
               END
        ENDCASE
        IF MAX(badval) THEN BEGIN
          PRINT,"ERROR in merge_profile_headers: bad match on common variable ",varname[ii],bfile,bval,ifile,ival
          RETURN
        ENDIF
        FOR jj = 0,bstr[0].numgals-1 DO BEGIN
          mergestr[jj].(ii) = dumpval[jj]
        ENDFOR
      ENDELSE
    ENDIF ELSE BEGIN
; It's a variable distinct to one file.  No matching needed, just type
; it and dump.  Just make sure you've got the right variable name.
      IF bpos[ii] GT 0 THEN BEGIN
        tempname = STRMID(varname[ii],0,bpos[ii])
        bindex = WHERE(STRTRIM(bkeyword,2) EQ STRTRIM(tempname,2),count)
        IF count GT 0 THEN val = bstr.(bindex[0])
      ENDIF ELSE BEGIN
; ipos must be the one
        tempname = STRMID(varname[ii],0,ipos[ii])
        iindex = WHERE(STRTRIM(ikeyword,2) EQ STRTRIM(tempname,2),count)
        IF count GT 0 THEN val = istr.(iindex[0])
      ENDELSE
      IF count GT 0 THEN BEGIN
        CASE STRMID(vartype[ii],0,1) OF
          "A": dumpval = val
          "B": dumpval = BYTE(val)
          "I": dumpval = FIX(val)
          "J": dumpval = LONG(val)
          "F": dumpval = FLOAT(val)
          "D": dumpval = DOUBLE(val)
          ELSE: BEGIN
                 PRINT,"ERROR in merge_profile_headers: unknown type",tempname,vartype[ii]
                 RETURN
               END
        ENDCASE
        FOR jj = 0,bstr[0].numgals-1 DO BEGIN
          mergestr[jj].(ii) = dumpval[jj]
        ENDFOR
      ENDIF ELSE BEGIN
        PRINT,"MISSING: ",varname[ii]," ",tempname
      ENDELSE
    ENDELSE

  ENDFOR

; Patch the structure a bit.
  FOR jj = 0,bstr[0].numgals-1 DO BEGIN
    mergestr[jj].profile_brt = bfile
    mergestr[jj].profile_iso = ifile

    IF mergestr[jj].pa GT 180.0 THEN mergestr[jj].pa = mergestr[jj].pa - 180.0

    IF ABS(mergestr[jj].flux_o) GT 1E-20 THEN mergestr[jj].flag_o = 'T' $
                                         ELSE mergestr[jj].flag_o = 'F'
  ENDFOR

; Variables still missing must be handled by the calling routine.
; These include:
; RUNID,OBJECT,UPDATE,FILTER,IMTYPE,RA,DEC,and the six EW50 vars.

  RETURN
END
