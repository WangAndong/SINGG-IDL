PRO calc_ecntrat
; Assembles a table of continuum scaling ratios, for determining
; uncertainties.  One entry per filter combination.

  cntratfile = !singgdir+"/ecntrat.dat"
  readcol_new,cntratfile,nfilt,rfilt,efrac,note, $
              COMMENT='#',FORMAT='(A,A,F,A)',/SILENT

  headerdb = "proc3_header"

  dbopen,headerdb,0
  dbext,-1,"RUNID,TARGET,IMTYPE,FILENAME,FILTNAME,CNTRAT1,EXPTIME,FLUXREF", $
            runid,target,imtype,filename,filtname,cntrat, exptime,fluxref
  dbclose,dummy

  Slist = WHERE(STRTRIM(imtype,2) EQ 'net',Scount)

  rfiltname = STRARR(Scount)+"R_Harris"
  rfiltname[WHERE(STRPOS(filename[Slist],'_Csub_ss') GE 0)] = '6850/95'

  Rexptime = INTARR(Scount)
  Nexptime = INTARR(Scount)
  path = STRARR(Scount)

  FOR ii = 0,Scount-1 DO BEGIN
    id = "j"+STRTRIM(STRMID(target[Slist[ii]],1,STRLEN(target[Slist[ii]])-1),2)
    path[ii] = "/home/hanish/data/"+STRTRIM(runid[Slist[ii]],2)+"/"+id+"/"
    fname = path[ii]+STRTRIM(filename[Slist[ii]],2)

    index = WHERE(STRTRIM(Rfilt,2) EQ STRTRIM(rfiltname[ii],2) AND $
                  STRTRIM(Nfilt,2) EQ STRTRIM(filtname[Slist[ii]],2),count)

    IF count NE 1 THEN BEGIN
      PRINT,"ERROR in calc_ecntrat: cannot match filter combination ",rfiltname[ii],filtname[Slist[ii]]
      RETURN
    ENDIF

    IF NOT FILE_TEST(fname) THEN BEGIN
      PRINT,"ERROR in calc_ecntrat: missing file",fname
    ENDIF

    fits_read,fname,img,hd
    SXADDPAR,hd,'ECNTRAT1',efrac[index[0]]*cntrat[Slist[ii]],' Continuum ratio RMS (kernel)'
    fits_write,fname,img,hd

  ENDFOR

  RETURN
END
