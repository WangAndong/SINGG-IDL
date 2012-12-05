PRO read_catalog,catfile,run_struct,object,filter,Rfile,Nfile,Sfile, $
                 ellipse,refnum,Rmask,Nmask,nsig,SILENT=silent
; Reads the Run*.catalog file, both its header and the table.

  silentflag = KEYWORD_SET(silent)

  readcol_new,catfile,char1,keyword,char2,keyval,FORMAT='A,A,A,A',/SILENT,/QUOTES
  headind = WHERE(char1 EQ '#' AND char2 EQ '=',headcount)

  varnames = ['RUNID','OBSERVAT','TELESCOP','OBSERVER','PIXSIZE']
  vartypes = ['A','A','A','A','F']

  create_struct2,run_struct,'Run_keywords',varnames,vartypes
  FOR ii = 0,N_ELEMENTS(varnames)-1 DO BEGIN
    ind = WHERE(STRTRIM(keyword[headind],2) EQ varnames[ii],count)
    IF count EQ 1 THEN BEGIN
      run_struct.(ii) = keyval[headind[ind[0]]]
    ENDIF ELSE BEGIN
      IF NOT silentflag THEN PRINT,'WARNING in read_catalog: no match for keyword '+varnames[ii]+' in file '+catfile
    ENDELSE
  ENDFOR

; Now, read the rest of the file.
  IF silentflag THEN BEGIN
    readcol_new,catfile,object,filter,Rfile,Nfile,Sfile,ellipse,refnum, $
                        Rmask,Nmask,nsig,FORMAT='A,A,A,A,A,A,A,A,A,A', $
                        COMMENT='#',SKIPLINE=headcount,/SILENT
  ENDIF ELSE BEGIN
    readcol_new,catfile,object,filter,Rfile,Nfile,Sfile,ellipse,refnum, $
                        Rmask,Nmask,nsig,FORMAT='A,A,A,A,A,A,A,A,A,A', $
                        COMMENT='#',SKIPLINE=headcount
  ENDELSE

  RETURN
END
