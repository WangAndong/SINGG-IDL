PRO file_check
; Checks for files, duh.

  spawn,"ls -d j*",dirlist
  
  num_dirs = N_ELEMENTS(dirlist)

  FOR ii = 0,num_dirs-1 DO BEGIN
;;    PRINT,"Entering directory ",dirlist[ii]
    CD,dirlist[ii]
    id = "J"+STRMID(dirlist[ii],1,STRLEN(dirlist[ii])-1)
    rcount = 0
    ncount = 0

    IF FILE_TEST("obj_R.lis.gz") THEN BEGIN
      IF NOT FILE_TEST(id+"_R.fits.gz") THEN PRINT, $
          "Object "+id+" missing R image"
      IF NOT FILE_TEST(id+"_R_ss.fits.gz") THEN PRINT, $
          "Object "+id+" missing R_ss image"
      rcount = rcount + 1
    ENDIF

    IF FILE_TEST("obj_cont.lis.gz") THEN BEGIN
      IF NOT FILE_TEST(id+"_C.fits.gz") THEN PRINT, $
          "Object "+id+" missing C image"
      IF NOT FILE_TEST(id+"_C_ss.fits.gz") THEN PRINT, $
          "Object "+id+" missing C_ss image"
      rcount = rcount + 2
    ENDIF

    spawn,"ls obj_6???.lis.gz",narrowlist
    ncount = N_ELEMENTS(narrowlist)
    FOR jj = 0,ncount-1 DO BEGIN
      filter = STRMID(narrowlist[jj],4,4)
      IF NOT FILE_TEST(id+"_"+filter+".fits.gz") THEN PRINT, $
          "Object "+id+" missing "+filter+" image"
      IF NOT FILE_TEST(id+"_"+filter+"_ss.fits.gz") THEN PRINT, $
          "Object "+id+" missing "+filter+"_ss image"

; Now check for Rsub images
      IF rcount EQ 1 OR rcount EQ 3 THEN BEGIN
        IF ncount EQ 1 THEN BEGIN
          IF NOT FILE_TEST(id+"_Rsub.fits.gz") THEN PRINT, $
              "Object "+id+" missing Rsub image"
          IF NOT FILE_TEST(id+"_Rsub_ss.fits.gz") THEN PRINT, $
              "Object "+id+" missing Rsub_ss image"
        ENDIF ELSE BEGIN
          IF NOT FILE_TEST(id+"_"+filter+"_Rsub.fits.gz") THEN PRINT, $
              "Object "+id+" missing "+filter+"_Rsub image"
          IF NOT FILE_TEST(id+"_"+filter+"_Rsub_ss.fits.gz") THEN PRINT, $
              "Object "+id+" missing "+filter+"_Rsub_ss image"
        ENDELSE
      ENDIF

; and Csub
      IF rcount EQ 2 OR rcount EQ 3 THEN BEGIN
        IF ncount EQ 1 THEN BEGIN
          IF NOT FILE_TEST(id+"_Csub.fits.gz") THEN PRINT, $
              "Object "+id+" missing Csub image"
          IF NOT FILE_TEST(id+"_Csub_ss.fits.gz") THEN PRINT, $
              "Object "+id+" missing Csub_ss image"
        ENDIF ELSE BEGIN
          IF NOT FILE_TEST(id+"_"+filter+"_Csub.fits.gz") THEN PRINT, $
              "Object "+id+" missing "+filter+"_Csub image"
          IF NOT FILE_TEST(id+"_"+filter+"_Csub_ss.fits.gz") THEN PRINT, $
              "Object "+id+" missing "+filter+"_Csub_ss image"
        ENDELSE
      ENDIF
    ENDFOR

    CD,".."
  ENDFOR

END
