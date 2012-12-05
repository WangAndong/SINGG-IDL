FUNCTION good_flux,TRIM=trim,PHOT=phot,FULL=full
; Reads the singg_multiple.dat file and uses it to eliminate any entry
; in the flux database not corresponding to our best sets.
; OPTIONAL INPUT
;   /trim      Instead of keeping only a single set for each object,
;                 only throw out the data flagged as unusable.
;   /phot      Don't use any entries with "N" in the PHOTMTRC field.
;   /full      Include the nondetection (900+) entries, as long as
;                 they're not 999s.

; 'singg_flux' must be open when this routine is called.

  dbext,-1,'OBJECT,RUNID,FILENAME,FILTER,IMTYPE,GALINDEX,NUMGALS,PHOTMTRC', $
            fobj,  frun, ffile,   ffilt, ftype, fgal,    fnum,   photmtrc

  photflag = KEYWORD_SET(phot)
  fullflag = KEYWORD_SET(full)

  fobj = STRTRIM(fobj,2)
  frun = STRTRIM(frun,2)
  ffile = STRTRIM(ffile,2)
  ffilt = STRTRIM(ffilt,2)
  ftype = STRTRIM(ftype,2)
  fgal = STRTRIM(fgal,2)
  fnum = STRTRIM(fnum,2)

  n_flux = N_ELEMENTS(fobj)
  pr = INTARR(n_flux)
  IF photflag THEN BEGIN
    pind = WHERE(STRTRIM(photmtrc,2) EQ 'N',count)
    IF count GT 0 THEN pr[pind] = 999
  ENDIF
  bestind = INDGEN(n_flux) ; placeholder

  readcol_new,!singgdir+'singg_multiple.dat',mobj,mfile,mrun,mpr, $
              COMMENT='#',FORMAT='(A,A,A,I)',/SILENT

  FOR ii = 0,n_flux-1 DO BEGIN
    ind = WHERE(STRTRIM(mobj,2) EQ fobj[ii] AND $
                STRTRIM(mrun,2) EQ frun[ii],count)

    CASE count OF
      0: BEGIN
; It's not in the multiple file, so there's no problem.
        IF pr[ii] EQ 0 THEN pr[ii] = 1
      END
      1: BEGIN
; It's in the multiple file, but there's only one entry for this
; object/run, which means it wasn't a filter issue.
        IF pr[ii] EQ 0 THEN pr[ii] = mpr[ind[0]]
      END
      ELSE: BEGIN
; It's in the multiple file, and there are multiple entries for this
; object/run, which means it IS a filter issue and now we have to
; figure out which images to toss out.
;print,fobj[ii]+'  '+ffile[ii]+' '+frun[ii]+' '+STRTRIM(ftype[ii],2)
        CASE ftype[ii] OF
          'net': BEGIN
; Easiest case.
            ind2 = WHERE(STRTRIM(mfile[ind],2) EQ ffile[ii],count2)
            IF count2 NE 1 THEN BEGIN
              PRINT,'ERROR in good_flux: cannot match net file ',ffile[ii],count2
              RETURN,bestind
            ENDIF
            IF pr[ii] EQ 0 THEN pr[ii] = mpr[ind[ind2[0]]]
          END
          'cont': BEGIN
            pos = STRPOS(ffile[ii],'_ss.fits')
            fshort = STRMID(ffile[ii],pos-1,1)
            nmatch = 0
            minpr = 999
            FOR jj = 0,count-1 DO BEGIN
              IF STRPOS(mfile[ind[jj]],fshort+'sub') GE 0 THEN BEGIN
                nmatch = nmatch+1
                IF mpr[ind[jj]] LT minpr THEN minpr = mpr[ind[jj]]
              ENDIF
            ENDFOR
            IF pr[ii] EQ 0 THEN pr[ii] = minpr
          END
          'onband': BEGIN
            pos = STRPOS(ffile[ii],'_6')
            fshort = STRMID(ffile[ii],pos+1,4)
            nmatch = 0
            minpr = 999
            FOR jj = 0,count-1 DO BEGIN
              IF STRPOS(mfile[ind[jj]],fshort) GE 0 THEN BEGIN
                nmatch = nmatch+1
                IF mpr[ind[jj]] LT minpr THEN minpr = mpr[ind[jj]]
              ENDIF
            ENDFOR
            IF pr[ii] EQ 0 THEN pr[ii] = minpr
          END
        ENDCASE
      END
    ENDCASE
  ENDFOR

  IF KEYWORD_SET(trim) THEN BEGIN
; Only remove the "bad" entries.
    IF fullflag THEN bestind = WHERE(pr LT 990) $
                ELSE bestind = WHERE(pr LT 900)
  ENDIF ELSE BEGIN
; Remove anything that's not the "best" entry.
    IF fullflag THEN bestind = WHERE(pr EQ 1 OR pr EQ 901) $
                ELSE bestind = WHERE(pr EQ 1)
  ENDELSE

  RETURN,bestind
END
