PRO build_sr1

  derived = create_db_struct(!singgdir+"/singg_derived.dbd",TITLE="SR1", $
                             VNAME=varname,VTYPE=vartype,VUNIT=varunit)
  num_vars = N_ELEMENTS(varname)

  dbopen,"singg_derived",0
  derind = good_derived2(/TRIM)+1
;;  derind = good_derived()
  n_good = N_ELEMENTS(derind)
  struct = REPLICATE(derived,n_good)

  FOR ii = 0,num_vars-1 DO BEGIN
    dbext,derind,varname[ii],arr
    struct[*].(ii) = arr
  ENDFOR
  dbclose

  newdb = "sr1"

  spawn,"/bin/rm -f "+!singgdir+""+newdb+".dbf"
  spawn,"/bin/rm -f "+!singgdir+""+newdb+".dbh"
  spawn,"/bin/rm -f "+!singgdir+""+newdb+".dbx"
  !PRIV=2
  dbcreate,newdb,1,1,/EXTERNAL
  spawn,"mv -f "+newdb+".db* "+!singgdir

  FOR ii = 0,n_good-1 DO BEGIN
    singg_dbupdate,newdb,["NAME","FILTER_R","FILTER_N"],[[struct[ii].name],[struct[ii].filter_r],[struct[ii].filter_n]],struct[ii]
  ENDFOR

  PRINT,"Finished creating SR1 database"

  dbopen,newdb,1
  dbindex
  dbclose,dummy

END
