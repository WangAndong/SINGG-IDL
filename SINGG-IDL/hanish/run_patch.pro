PRO run_patch,CATALOG=catalog,BUFFER=buffer,IMSIZE=imsize

; Run from the run's root directory, where the catalog files are.
  IF KEYWORD_SET(catalog) THEN BEGIN
    IF NOT FILE_TEST(catalog) THEN BEGIN
      PRINT,"ERROR in run_combine: specified catalog file does not exist"
      RETURN
    ENDIF
  ENDIF ELSE BEGIN
    spawn,"ls Run*.catalog",catlist

    IF N_ELEMENTS(catlist) NE 1 THEN BEGIN
      PRINT,"ERROR in run_combine: Need one and only one catalog file in the directory",N_ELEMENTS(catlist)
      RETURN
    ENDIF
    catalog = catlist[0]
  ENDELSE

  read_catalog,catalog,run_struct,object,filter,Rfile,Nfile,Sfile, $
                       ellipse,refnum,Rmask,Nmask,nsig,/SILENT

  IF NOT KEYWORD_SET(buffer) THEN buffer=0
  IF NOT KEYWORD_SET(imsize) THEN BEGIN
    imsize = [2048,2048]
  ENDIF ELSE BEGIN
    IF N_ELEMENTS(imsize) EQ 1 THEN imsize = imsize*[1,1]
  ENDELSE
  
  FOR ii = 0,N_ELEMENTS(object)-1 DO BEGIN
    PRINT,"Combining images for object: ",object[ii]

    CD,object[ii],current=rootdir

    IF KEYWORD_SET(imsize) THEN tempsize = imsize ELSE tempsize = 2048

    patch_cntrat,rfile=Rfile[ii],nfile=Nfile[ii],sfile=Sfile[ii], $
                 buffer=buffer,imsize=imsize,filter=filter[ii]

    CD,rootdir

  ENDFOR

  CLOSE,/ALL

END
