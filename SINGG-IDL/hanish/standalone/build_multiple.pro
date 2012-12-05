PRO build_multiple
; Creates the 'singg_multiple.dat' file used to determine precedence
; when an object was observed multiple times.

  dbopen,'singg_flux',0
  dbext,-1,'OBJECT,RUNID,FILENAME,FILTER,IMTYPE,GALINDEX,NUMGALS', $
            fobj,  frun, ffile,   ffilt, ftype, fgal,    fnum
  dbclose

  netind = WHERE(STRTRIM(ftype,2) EQ 'net')

  fobj = STRTRIM(fobj,2)
  frun = STRTRIM(frun,2)
  ffile = STRTRIM(ffile,2)
  ffilt = STRTRIM(ffilt,2)
  ftype = STRTRIM(ftype,2)
  fgal = STRTRIM(fgal,2)
  fnum = STRTRIM(fnum,2)

  flist = UNIQ(fobj[netind],SORT(fobj[netind]))
  n_objs = N_ELEMENTS(flist)

  dbopen,'proc3_header',0
  dbext,-1,'TARGET,RUNID,PHOTQUAL,FILTNAME,IMTYPE,FILENAME', $
            hobj,hrunid,hpq,hfilt,himtype,hfile
  dbclose
  hrun = 'Run'+hrunid

  datfile = !singgdir+'singg_multiple.dat'
  spawn,'/bin/rm -f '+datfile
  OPENW,unit,datfile,/GET_LUN
  PRINTF,unit,'# Object    File                        Run   Priority'

  FOR ii = 0,n_objs-1 DO BEGIN
    object = fobj[netind[flist[ii]]]
    ind = WHERE(fobj EQ object AND ftype EQ 'net' AND fgal EQ 1,count)
    IF count GT 1 THEN BEGIN
; For whatever reason, there are multiple net-image entries.  Could be
; multiple filters, could be multiple runs.  Either way, we're going
; to add it to the list.
;;      priority = INDGEN(count)+1
      
print,object
;forprint,'  '+ffile[ind]+' '+frun[ind]+' '+ffilt[ind]
      pq = FLTARR(count)
      FOR jj = 0,count-1 DO BEGIN

;test = WHERE(strtrim(hobj,2) eq object,count3)
;if count3 ge 1 then forprint,hobj[test],hfile[test]

        hind = WHERE(STRTRIM(hfile,2) EQ ffile[ind[jj]] AND $
                     STRTRIM(hrun,2) EQ frun[ind[jj]],hcount)
        IF hcount NE 1 THEN BEGIN
          PRINT,'ERROR in build_multiple: no header match! ',ffile[ind[jj]],hcount
          RETURN
        ENDIF
        pq[jj] = hpq[hind[0]]
      ENDFOR
      kk = SORT(pq)
      priority = kk+1

      FOR jj = 0,count-1 DO BEGIN
        PRINTF,unit,object,ffile[ind[jj]],frun[ind[jj]],priority[jj], $
               FORMAT='(A10,A28,A7,"  ",I2)'
      ENDFOR
    ENDIF
  ENDFOR

  CLOSE,unit
  FREE_LUN,unit

  RETURN
END
