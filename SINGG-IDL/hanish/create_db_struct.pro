FUNCTION create_db_struct,dbfile,TITLE=title,SIZE=size, $
                                 VNAME=vname,VTYPE=vtype,VUNIT=vunit
; Creates a structure based on the format of a database definition
; file, for use in database construction and/or parsing.

; INPUT
; dbfile          Name of database definition (.dbd) file
; OPTIONAL INPUT
; title           Optional structure title
; size            Number of elements to allocate for each variable
; OPTIONAL OUTPUTS
; vname           Name of each variable
; vtype           Type of each variable

  readcol_new,dbfile,varname,vartype,varcomment, $
              FORMAT="A,A,A",comment='#',SKIPLINE=6,/SILENT,/QUOTES
  good = WHERE(STRLEN(varcomment) GT 0, num_vars)

  vartype2 = STRARR(num_vars)

  IF KEYWORD_SET(size) THEN sizestring = "("+STRTRIM(STRING(size),2)+")" $
                       ELSE sizestring = ""

  IF NOT KEYWORD_SET(title) THEN title = "Structure"

  FOR ii = 0,num_vars-1 DO BEGIN
    varshort = STRMID(vartype[good[ii]],0,1)
    IF STRLEN(vartype[good[ii]]) GE 2 THEN $
      varbyte = STRMID(vartype[good[ii]],2,1)
    CASE varshort OF
      "C": vartype2[ii] = "A"+sizestring
      "B": vartype2[ii] = "B"+sizestring
      "I": IF varbyte GE 4 THEN vartype2[ii] = "J"+sizestring $
                           ELSE vartype2[ii] = "I"+sizestring
      "R": IF varbyte GE 8 THEN vartype2[ii] = "D"+sizestring $
                           ELSE vartype2[ii] = "F"+sizestring
      ELSE: PRINT,"WARNING: invalid variable type: ",vartype,varbyte
    ENDCASE 
  ENDFOR

  create_struct2,structure,title,varname[good],vartype2

; Used to use KEYWORD_SET, but it gives 0 if the variable hasn't been
; set yet.  You could use ARG_PRESENT, but it doesn't cost much time
; to just set them.

  vname = varname[good]
  vtype = vartype2
  vunit = STRARR(num_vars)
  upos = STRPOS(varcomment,"[",/REVERSE_SEARCH)
  FOR ii = 0,num_vars-1 DO BEGIN
    IF upos[ii] GE 0 THEN BEGIN
      vunit[ii] = STRMID(varcomment[good[ii]],upos[good[ii]],STRLEN(varcomment[good[ii]])-upos[good[ii]])
; This leaves both brackets on.
    ENDIF ELSE BEGIN
      vunit[ii] = "N/A"
    ENDELSE
  ENDFOR

  RETURN,structure

END
