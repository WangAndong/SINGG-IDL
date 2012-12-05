PRO convert_db,oldname

  newname = STRTRIM(oldname,2)+"_ieee"

  oldfile = STRTRIM(oldname,2)+".dbd"
  newfile = STRTRIM(newname,2)+".dbd"

  readcol_new,oldfile,varname,vartype,varcomment, $
              FORMAT="A,A,A",comment='#',SKIPLINE=6
  num_vars = N_ELEMENTS(varname)
  vartype2 = STRARR(num_vars)

; test for size
  dbopen,oldname,0
  dbext,-1,varname[0],temparr
  dbclose,dummy

  num_gals = N_ELEMENTS(temparr)

  FOR ii = 0,num_vars-1 DO BEGIN
    CASE STRMID(vartype[ii],0,1) OF
      "C": vartype2[ii] = "A("+STRTRIM(STRING(num_gals),2)+")"
      "I": vartype2[ii] = "I("+STRTRIM(STRING(num_gals),2)+")"
      "R": vartype2[ii] = "F("+STRTRIM(STRING(num_gals),2)+")"
    ENDCASE
  ENDFOR

  create_struct,structure,oldname,varname,vartype2

  dbopen,oldname,0
  FOR ii = 0,num_vars-1 DO BEGIN
    dbext,-1,varname[ii],temparr
    structure.(ii) = temparr[0:num_gals-1]
  ENDFOR
  dbclose,dummy

; Now, write the outputs:

; Copy the .dbd to the new name
  spawn,"cp "+oldfile+" "+newfile

  !PRIV=2
  dbcreate,newname,1,1,/EXTERNAL
  dbopen,newname,1
  dbbuildstruct,structure
  dbclose,dummy

END
