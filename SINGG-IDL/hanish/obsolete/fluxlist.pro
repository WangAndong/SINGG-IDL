PRO fluxlist,CATALOG=catalog
; Makes the two list files needed for photflux.
; Run it in the directory containing the catalog file.

  IF KEYWORD_SET(catalog) THEN BEGIN
    IF NOT FILE_TEST(catalog) THEN BEGIN
      PRINT,"ERROR in fluxlist: specified catalog file does not exist"
      RETURN
    ENDIF
  ENDIF ELSE BEGIN
    spawn,"ls Run*.catalog",catlist

    IF N_ELEMENTS(catlist) NE 1 THEN BEGIN
      PRINT,"ERROR in fluxlist: Need one and only one catalog file in the directory",N_ELEMENTS(catlist)
      RETURN
    ENDIF
    catalog = catlist[0]
  ENDELSE

  read_catalog,catalog,run_struct,object,filter,Rfile,Nfile,Sfile, $
                       ellipse,refnum,Rmask,Nmask,/SILENT
  runname = run_struct.runid

  OPENW,unit1,!singgdir+"filtcoef/"+runname+".lis",/GET_LUN

  OPENW,unit2,!singgdir+"filtcoef/"+runname+"_net.lis",/GET_LUN

; Note that all paths are absolute.  You don't need to be in any specific
; directory for this to work.

  FOR ii = 0,N_ELEMENTS(object)-1 DO BEGIN
    Rssfile = object[ii]+"/"+STRMID(Rfile[ii],0,STRLEN(Rfile[ii])-5)+"_ss.fits"
    Nssfile = object[ii]+"/"+STRMID(Nfile[ii],0,STRLEN(Nfile[ii])-5)+"_ss.fits"
    Sssfile = object[ii]+"/"+STRMID(Sfile[ii],0,STRLEN(Sfile[ii])-5)+"_ss.fits"

    PRINTF,unit1,Rssfile+" "+object[ii]+" Y"
    PRINTF,unit1,Nssfile+" "+object[ii]+" Y"

    coeffdat = !singgdir+"filtcoef/J"+STRMID(object[ii],1,STRLEN(object[ii])-1)+"_filtcoef.dat"
; Note that this usually won't be the right filtcoef file, since they use the longer J6-4 name
    PRINTF,unit2,Sssfile+" "+coeffdat+" Y 0.35"

  ENDFOR 

  CLOSE,unit1
  FREE_LUN,unit1
  CLOSE,unit2
  FREE_LUN,unit2

END
