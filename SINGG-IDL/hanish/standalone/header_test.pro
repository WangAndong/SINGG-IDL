PRO header_test,CATALOG=catalog
; Within a given run, open every .fits file and read the header.  That way, we
; can see which ones give the EXTENSION error.

  IF KEYWORD_SET(catalog) THEN BEGIN
    IF NOT FILE_TEST(catalog) THEN BEGIN
      PRINT,"ERROR in run_combine: specified catalog file does not exist"
      RETURN
    ENDIF
  ENDIF ELSE BEGIN
    spawn,"ls run??.catalog",catlist

    IF N_ELEMENTS(catlist) NE 1 THEN BEGIN
      PRINT,"ERROR in run_combine: Need one and only one catalog file in the directory",N_ELEMENTS(catlist)
      RETURN
    ENDIF
    catalog = catlist[0]
  ENDELSE

  readcol_new,catalog,object,filter,Rfile,Cfile,Sfile, $
                      ellipse,refnum,Rmask,Cmask, $
                      FORMAT="A,A,A,A,A,A,A,A,A",comment='#'

  FOR ii = 0,n_elements(object)-1 DO BEGIN
    CD,object[ii]

    PRINT,"Checking galaxy: ",object[ii]

    spawn,"ls *.fits",imlist

    FOR jj = 0, N_ELEMENTS(imlist)-1 DO BEGIN
      PRINT,"  Verifying image: ",imlist[jj]
      fits_read,imlist[jj],img,hd

    ENDFOR

    CD,".."
  ENDFOR

END
