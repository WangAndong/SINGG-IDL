PRO check_gals
; Quick program that compares the morphology file to the database to
; ensure that the right number of sources is used for each image.

  listfile = !singgdir+"newmorph.in"

  readcol_new,listfile,name1,name2,dum1,dum2,dum3,dum4,nsource,dum5,dum6,dum7,$
              FORMAT='A,A,F,F,F,F,I,I,I,I',/SILENT,COMMENT='#'

  listname = update_name(name2)

  fluxdb = "singg_flux"

  dbopen,fluxdb,0
  dbext,-1,"OBJECT,NUMGALS,IMTYPE",fobj,fnum,fimtype
  dbclose,dummy
  fobj = update_name(fobj)

  FOR ii = 0,N_ELEMENTS(fobj)-1 DO BEGIN
    IF STRTRIM(fimtype[ii],2) EQ "net" THEN BEGIN
      index = WHERE(STRTRIM(listname,2) EQ STRTRIM(fobj[ii],2),count)
      IF count EQ 0 THEN BEGIN
        PRINT,"ERROR: object not found ",fobj[ii]
;;        RETURN
      ENDIF ELSE BEGIN
        IF fnum[ii] NE nsource[index[0]] THEN BEGIN
          PRINT,"MISMATCH: object ",STRTRIM(fobj[ii],2)," lists ", $
                 STRTRIM(STRING(nsource[index[0]]),2)," objects but has ", $
                 STRTRIM(STRING(fnum[ii]),2)," in ellipse file"
        ENDIF
      ENDELSE
    ENDIF
  ENDFOR

  RETURN
END
