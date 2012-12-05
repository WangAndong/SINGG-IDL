PRO build_standard_db
; Sets up the standards database with values for all runs.

  stdname = "standards"
  stdbfile = !singgdir+""+stdname+".dbd"

; Cleanup, to make sure we're not appending the same galaxies again
  spawn,"/bin/rm -f "+!singgdir+""+stdname+".dbf"
  spawn,"/bin/rm -f "+!singgdir+""+stdname+".dbh"
  spawn,"/bin/rm -f "+!singgdir+""+stdname+".dbx"

  !PRIV=2
  dbcreate,stdname,1,1,/EXTERNAL
  spawn,"mv -f "+stdname+".db* "+!singgdir

  stdb = create_db_struct(stdbfile,TITLE="standarddb",VNAME=varname,VTYPE=vartype)
  num_vars = N_ELEMENTS(varname)

; Read the block of filter aliases
  filt_rddbfnames,"filter",fnamarr

; Get the list of inputs.  The .tab files are easier to use than the
; .dats, since there are other .dat files, and there are no .dat files
; for the hires data.
;;  spawn,"ls *.tab",imlist
  readcol_new,!singgdir+"std.tab",imlist,COMMENT='#',FORMAT='(A)',/SILENT
  n_stds = N_ELEMENTS(imlist)

  readcol_new,!singgdir+"/standards.dat",name,ra1,ra2,ra3,dec1,dec2,dec3, $
              vmag,best,sources,COMMENT='#',FORMAT='(A,I,I,F,I,I,F,F,A,A)', $
              DELIMITER=' ',/SILENT
  n_entries = N_ELEMENTS(name)
  patchname = repchr(STRUPCASE(name),'+','P')
  patchname = repchr(patchname,'-','M')

  rastr = STRTRIM(STRING(ra1),2)+":"+STRTRIM(STRING(ra2),2)+":"+ $
          STRTRIM(STRING(ra3),2)
  decstr = STRTRIM(STRING(dec1),2)+":"+STRTRIM(STRING(dec2),2)+":"+ $
           STRTRIM(STRING(dec3),2)

  abtab = read_stdtab(!singgdir+"/standards_abmag.dat",fname,abname,abref)
;  n_filts = N_ELEMENTS(fname)
;  n_abs = N_ELEMENTS(abname)

  patchabname = repchr(STRUPCASE(abname),'+','P')
  patchabname = repchr(patchabname,'-','M')

; List of data sources, and the abbreviations to use
  namelist = [['ham','HIRES','Hamuy_highres'], $
              ['ham','ESO','Hamuy_(92+94)'], $
              ['mas','ING','Massey_(88)'], $
              ['oke','ESO','Oke_(90)'], $
              ['oke','ING','Oke_(90)'], $
              ['oke','ING','Oke_(74)'], $
              ['sto','ING','Stone_(77)']]

  FOR ii = 0,n_stds-1 DO BEGIN

;; PATCH
;;IF imlist[ii] EQ 'hz15_eso_ing.tab' THEN imlist[ii] = 'hz15_sto_ing.tab'
;; /PATCH

    clear_struct,stdb,vartype

    und = STRPOS(imlist[ii],'_')
    obj = STRMID(imlist[ii],0,und)
    remain = STRMID(imlist[ii],und+1,STRLEN(imlist[ii])-und+1)
    und = STRPOS(remain,'_')
    datas = STRMID(remain,0,und)
    remain = STRMID(remain,und+1,STRLEN(remain)-und+1)
    und = STRPOS(remain,'.')
    webs = STRUPCASE(STRMID(remain,0,und))
    remain = STRMID(remain,und+1,STRLEN(remain)-und+1)

; First, parse the name to set up other variables
    object = STRUPCASE(obj)

; Match to the RA/DEC table
    rdind = WHERE(STRTRIM(patchname,2) EQ STRTRIM(object,2),rdcount)
    IF rdcount NE 1 THEN BEGIN
      PRINT,"ERROR in build_standard_db: incorrect RA/Dec matches ",rdcount,imlist[ii]
      RETURN
    ENDIF

    ind = WHERE(namelist[0,*] EQ datas AND namelist[1,*] EQ webs,count)
    longname = namelist[2,ind]+'[' + namelist[1,ind] + ']'
    srclist = STRSPLIT(sources[rdind[0]],',',/EXTRACT)

    IF count EQ 0 THEN BEGIN
      PRINT,"ERROR in build_standard_db: no match for file ",imlist[ii],datas+' '+webs
      RETURN
    ENDIF
    IF count EQ 1 THEN BEGIN
      arrpos = 0
    ENDIF ELSE BEGIN
      arrpos = -1
      FOR jj = 0,count-1 DO BEGIN
        IF STRPOS(sources[rdind[0]],longname[jj]) GE 0 THEN BEGIN
          IF arrpos GT 0 THEN BEGIN
            PRINT,"ERROR in build_standard_db: multiple matches for file ", $
                  imlist[ii],namelist[2,ind[jj]],namelist[2,ind[arrpos]]
          ENDIF
          arrpos = jj
        ENDIF
      ENDFOR
      IF arrpos LT 0 THEN BEGIN
        PRINT,"ERROR in build_standard_db: screwed-up logic!!!"
        PRINT,srclist[rdind[0]]
        FORPRINT,' '+longname
        RETURN
      ENDIF
    ENDELSE

    nameind = WHERE(srclist EQ longname[arrpos],namecount)
    IF namecount NE 1 THEN BEGIN
      PRINT,"ERROR in build_standard_db: incorrect source name matches ",namecount
      PRINT,longname[arrpos]
      FORPRINT,' '+srclist
      RETURN
    ENDIF
    stdb.datasource = namelist[2,ind[arrpos]]
    stdb.priority = nameind[0] + 1

    stdb.websource = webs ; ESO, ING, or HIRES
    stdb.datafile = STRMID(imlist[ii],0,STRLEN(imlist[ii])-4)+'.dat'

; Match to the ABmag table
    abind = WHERE(STRTRIM(patchabname,2) EQ STRTRIM(object,2) AND $
                  STRTRIM(STRUPCASE(abref),2) EQ STRUPCASE(STRTRIM(stdb.datasource,2)),abcount)
    IF abcount NE 1 THEN BEGIN
      PRINT,"ERROR in build_standard_db: incorrect ABmag matches ",abcount," "+imlist[ii]
      PRINT,object+' '+stdb.datasource
      FORPRINT,' '+patchabname+' '+abref
      RETURN
    ENDIF

    PRINT," Adding database entries for standard: ",object

    stdb.name = STRTRIM(name[rdind[0]],2)

    stdb.ra = sexideg(rastr[rdind[0]])*15.d0
    stdb.dec = sexideg(decstr[rdind[0]])
    stdb.vmag = vmag[rdind[0]]
    stdb.best = STRTRIM(best[rdind[0]],2)

;; change this whole list to be more dynamic?  Parse the fname argument?

    stdb.ab_6563      = abtab[abind[0],0]
    stdb.ab_6568_f035 = abtab[abind[0],16]
    stdb.ab_6568_f075 = abtab[abind[0],17]
    stdb.ab_6568_f130 = abtab[abind[0],18]
    stdb.ab_6573      = abtab[abind[0],12]
    stdb.ab_6595_f075 = abtab[abind[0],20]
    stdb.ab_6600      = abtab[abind[0],1]
    stdb.ab_6605_f035 = abtab[abind[0],21]
    stdb.ab_6605_f075 = abtab[abind[0],22]
    stdb.ab_6605_f130 = abtab[abind[0],23]
    stdb.ab_6606      = abtab[abind[0],2]
    stdb.ab_6619      = abtab[abind[0],13]
    stdb.ab_6628_f035 = abtab[abind[0],24]
    stdb.ab_6628_f075 = abtab[abind[0],25]
    stdb.ab_6628_f130 = abtab[abind[0],26]
    stdb.ab_6649      = abtab[abind[0],3]
    stdb.ab_6653      = abtab[abind[0],14]
    stdb.ab_6696      = abtab[abind[0],4]
    stdb.ab_6709      = abtab[abind[0],15]
    stdb.ab_6737      = abtab[abind[0],5]
    stdb.ab_6738      = abtab[abind[0],6]
    stdb.ab_6781      = abtab[abind[0],7]
    stdb.ab_6826      = abtab[abind[0],8]
    stdb.ab_6850      = abtab[abind[0],19]
    stdb.ab_R         = abtab[abind[0],10]
    stdb.ab_V         = abtab[abind[0],11]
    stdb.ab_I         = abtab[abind[0],9]

; Add this entry to the database
    spawn,"date",datestring
    stdb.update = STRTRIM(datestring,2)

    dbopen,stdname,1
    dbbuildstruct,stdb,/silent
    dbclose,dummy

  ENDFOR ; loop over data files

; Now that the DB has been completed, index the whole thing.
  dbopen,stdname,1
  dbindex
  dbclose,dummy

END
