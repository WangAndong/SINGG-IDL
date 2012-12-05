PRO redo_radec,CATALOG=catalog
; Redoes the RA/Dec calculation for all combined and sky-subtracted images within a run.

; Run from the run's root directory, where the catalog files are.
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

  FOR ii = 0,N_ELEMENTS(object)-1 DO BEGIN

    PRINT,"Fixing RA/Dec header information for object ",object[ii]

    CD,object[ii]

    fits_read,Rfile[ii],Rimg,Rhd
    fits_read,Cfile[ii],Cimg,Chd
    fits_read,Sfile[ii],Simg,Shd

    fix_radec,Rhd,Chd,Shd

    fits_write,Rfile[ii],Rimg,Rhd
    fits_write,Cfile[ii],Cimg,Chd
    fits_write,Sfile[ii],Simg,Shd

; Now, the sky-subtracted ones
    Rssfile = STRMID(Rfile[ii],0,STRLEN(Rfile[ii])-5)+"_ss.fits"
    Cssfile = STRMID(Cfile[ii],0,STRLEN(Cfile[ii])-5)+"_ss.fits"
    Sssfile = STRMID(Sfile[ii],0,STRLEN(Sfile[ii])-5)+"_ss.fits"    

    fits_read,Rssfile,Rssimg,Rsshd
    fits_read,Cssfile,Cssimg,Csshd
    fits_read,Sssfile,Sssimg,Ssshd

    fix_radec,Rsshd,Csshd,Ssshd

    fits_write,Rssfile,Rssimg,Rsshd
    fits_write,Cssfile,Cssimg,Csshd
    fits_write,Sssfile,Sssimg,Ssshd

    CD,".."

  ENDFOR

END
