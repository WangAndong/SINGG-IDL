PRO dbplothist,dbname,keyword,xmin,xmax,binsz, $
               filter,filtcol,runname, $
               Xtitle,Title,LOG=log,VERBOSE=verbose

; This is a wrapper for the plothist routine, extracting information from the
; SINGG database.  It's only for when you're extracting a single keyword and
; dumping a histogram of it.
; INPUTS:
;    dbname       Name of database archive (no extensions)
;    keyword      Keyword in database to parse for
;    xmin         Minimum value to plot
;    xmax         Maximum value to plot
;    binsz        Bin size of histogram
;    filter   A list of the filter names to parse for within the database
;    filtcol    ...and a list of colors for each filter.
;    runname      Name of the run in question
;    Title, Xtitle are passed directly to the plot routine
; OPTIONAL INPUTS:
;    log          scale logarithmically?

  loudflag = KEYWORD_SET(verbose)

  totval = FLTARR(1000)/0.0
; This fills totval with NAN entries, which are filtered out by plothist

  n_filters = N_ELEMENTS(filter)
  nindex = INTARR(n_filters+1)
  dummy = FLTARR(2)

  dbopen,dbname,0
  dbext,-1,keyword+',runid,filtname,photflam',val,runid,filtname,photflam
  dbclose,dummy

  FOR jj = 0,n_filters-1 DO BEGIN
    IF loudflag THEN PRINT,"Extracting indices for filter ",filter[jj]
; We only want non-net images, so for now we use PHOTFLAM>0.0 to sort
    index = WHERE(STRTRIM(runid,2) EQ STRTRIM(runname,2) AND $
                  STRTRIM(filtname,2) EQ STRTRIM(filter[jj],2) AND $
                  photflam GT 0.0,count)
; Old logic.  The problem was, while we could STRTRIM the values to
; compare to the DB, we couldn't do the same for the entries within
; the DB.  So, we extract first, and compare that way.  Faster, too. 
;    dbopen,dbname,0
;    index=dbfind("runid="+STRTRIM(runname,2)+ $
;                ",filtname="+STRTRIM(filter[jj],2)+ $
;                ",photflam>0.0",count=count) - 1
;    dbclose,dummy

    nindex[jj+1] = nindex[jj]+count
    IF count GT 0 THEN totval[nindex[jj]:(nindex[jj+1]-1)] = val[index]
  ENDFOR

  IF loudflag THEN PRINT,"Values range from "+STRING(MIN(totval))+" to "+STRING(MAX(totval))
  IF MIN(totval) LT xmin OR MAX(totval) GT xmax THEN BEGIN
    PRINT,'WARNING in dbplothist: limit mismatch '
    PRINT,MIN(totval),xmin
    PRINT,MAX(totval),xmax
  ENDIF

  yscale = 5
  ymax = LONG(yscale)*(1+LONG((1.0+MAX( $
        HISTOGRAM(totval,BINSIZE=binsz,MAX=xmax,MIN=xmin)))/FLOAT(yscale)))

  IF KEYWORD_SET(log) THEN BEGIN
    PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[0,ymax], $
         XSTYLE=1,YSTYLE=1,COLOR=!black,CHARSIZE=2.0,/XLOG, $
         XTITLE=Xtitle,YTITLE='Number of images',TITLE=Title
  ENDIF ELSE BEGIN
    PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[0,ymax], $
         XSTYLE=1,YSTYLE=1,COLOR=!black,CHARSIZE=2.0, $
         XTITLE=Xtitle,YTITLE='Number of images',TITLE=Title
  ENDELSE

  FOR jj = 1,n_filters DO BEGIN
; Count backwards
    kk = n_filters-jj ; kk goes from 0 to n_filters-1, in reverse
    npoints = nindex[kk+1] - nindex[kk]

; Note that the logic fails if plothist is fed two equal values.
    IF KEYWORD_SET(log) THEN BEGIN
      IF npoints GT 2 THEN plothist,totval[0:nindex[kk+1]-1],/fill, $
              bin=binsz,/overplot,FCOLOR=filtcol[kk],PSYM=10,/NAN,COLOR=!black, $
              /XLOG
    ENDIF ELSE BEGIN
      IF npoints GT 2 THEN plothist,totval[0:nindex[kk+1]-1],/fill, $
              bin=binsz,/overplot,FCOLOR=filtcol[kk],PSYM=10,/NAN,COLOR=!black
    ENDELSE
  ENDFOR

  RETURN
END
