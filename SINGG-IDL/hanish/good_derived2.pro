FUNCTION good_derived2,TRIM=trim,PHOT=phot,FULL=full
; Reads the singg_multiple.dat file and uses it to eliminate any entry
; in the derived database not corresponding to our best sets.
; OPTIONAL INPUT
;   /trim      Instead of keeping only a single set for each object,
;                 only throw out the data flagged as unusable.
;   /phot      Don't use any entries with "N" in the PHOTMTRC field.
;   /full      Include the nondetection (900+) entries, as long as
;                 they're not 999s.

; 'singg_derived' must be open when this routine is called.

  dbext,-1,'OBJECT,RUNID,FILTER_R,FILTER_N,PHOTMTRC', $
            dobj, drun, drfilt,  dnfilt,  photmtrc

  photflag = KEYWORD_SET(phot)
  fullflag = KEYWORD_SET(full)

  dobj = STRTRIM(dobj,2)
  drun = STRTRIM(drun,2)
  drfilt = STRTRIM(drfilt,2)
  dnfilt = STRTRIM(dnfilt,2)

  n_der = N_ELEMENTS(dobj)
  pr = INTARR(n_der)
  IF photflag THEN BEGIN
    pind = WHERE(STRTRIM(photmtrc,2) EQ 'N',count)
    IF count GT 0 THEN pr[pind] = 999
  ENDIF
  bestind = INDGEN(n_der) ; placeholder

  readcol_new,!singgdir+'singg_multiple.dat',mobj,mfile,mrun,mpr, $
              COMMENT='#',FORMAT='(A,A,A,I)',/SILENT

  FOR ii = 0,n_der-1 DO BEGIN
    ind = WHERE(STRTRIM(mobj,2) EQ dobj[ii] AND $
                STRTRIM(mrun,2) EQ drun[ii],count)

    CASE count OF
      0: BEGIN
; It's not in the multiple file, so there's no problem, unless we
; already flagged it as nonphotometric.
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

; Get the cont and narrow filter names:
        contchr = STRMID(drfilt[ii],0,1)
        IF contchr EQ '6' THEN contchr = 'C'
        narrow = STRMID(dnfilt[ii],0,4)
        IF narrow EQ '6693' THEN narrow = '6696'

        filext = narrow+'_'+contchr+'sub_ss.fits'
        ind2 = WHERE(STRPOS(mfile[ind],filext) GE 0,count2)
        IF count2 NE 1 THEN BEGIN
          PRINT,'ERROR in good_derived: cannot match file in multiple file ',filext,count2
          STOP
        ENDIF
        IF pr[ii] EQ 0 THEN pr[ii] = mpr[ind[ind2[0]]]
;        print,pr[ii]
      ENDELSE
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
