PRO makeplot_db,RUNLIST=runlist,PS=ps,PNG=png,VERBOSE=verbose
; OPTIONAL INPUTS:
; runlist  Optional override to only process certain runs
; /ps      Replaces the normal window output with a postscript dump
; /png     Write a .png file of the output window
; NOTE: makepng uses the background color of the xterm that spawned it, I think

  psflag = KEYWORD_SET(ps)
  pngflag = KEYWORD_SET(png)
  loudflag = KEYWORD_SET(verbose)

  IF NOT KEYWORD_SET(runlist) THEN runlist = ['Run01','Run02','Run03','Run04','Run05','Run06','Run07','Run08','Run09','Run10','Run11','Run12','Run13','Run15','Run17']
  n_runs = N_ELEMENTS(runlist)
  rshort = STRMID(runlist,3,2)

;  dbname = !singgdir+'/proc3_header'
  dbname = 'proc3_header'

  as_pix = 0.432
; arcsec per pixel.  We COULD read this from headers, but why bother?  Until 
; the runs where we don't use the 1.5m, it'll stay constant.
  pi = ACOS(-1.0)
  dummy = FLTARR(2)
  yscale = 5

  IF NOT psflag THEN BEGIN
;    SET_PLOT,'X'
;    HELP,/device
    DEVICE, RETAIN=2, DECOMPOSED=0
    WINDOW,XSIZE=1200,YSIZE=1100
  ENDIF
  IF psflag THEN set_plot,'PS'
  n_cols = CEIL(n_runs / 6.0)
  n_rows = CEIL(FLOAT(n_runs)/n_cols)
  !P.MULTI=[0,n_cols,n_rows,0,0]

  setplotcolors

  filtnam = ['R_Harris','6850/95', $
             '6563/75', $
             '6568/28', $
             '6595/36', $
             '6600/75', $
             '6605/32', $
             '6606/75', $
             '6619/73', $
             '6628/33', $
             '6653/68', $
             '6693/76', $
             '6709/71', $
             '6738/50', $
             '6781/78', $
             '6826/78']
; For some reason, the full names (with the /) don't work.  But that's okay,
; since it only matches substrings anyway.
  filtcol = [!black,!lgray, $
             !purple, $
             !dmagenta, $
             !magenta, $
             !dblue, $
             !blue, $
             !dcyan, $
             !cyan, $
             !dgreen, $
             !green, $
             !orange, $
             !dorange, $
             !red, $
             !pink, $
             !dgray]

  n_filters = N_ELEMENTS(filtnam)

; Now that the arrays are set up, it's time for histograms!
  PRINT,"OPTIONS"
  PRINT,"(1) SEEING histogram"
  PRINT,"(2) SKYLEV ABmag histogram (R, narrow)"
  PRINT,"(3) SKYSIG ABmag histogram (R, narrow)"
  PRINT,"(4) SKYSIGBX ABmag histogram (R, narrow)"
  PRINT,"(5) SKYSIG histogram (net images)"
  PRINT,"(6) SKYSIGBX histogram (net images)"
  PRINT,"(7) Point-source detection limit (5 sigmas, R, narrow)
  PRINT,"(8) Point-source detection limit (5 sigmas, net images)
  PRINT,"(9) PHOTQUAL histogram"
  PRINT,"(0) MAGZPT1 histogram (BROKEN)"
  PRINT,"Enter selection: "
  option = FIX(GET_KBRD(1))

; Once the database is fixed to where it's one large DB for multiple runs, use
; this logic.  Read it all once, then calculate as needed.
  dbopen,dbname,0
  dbext,-1,"RUNID,IMTYPE,FILTNAME,SEEING,SKYLEV,SKYSIG,SKYSIGBX,PHOTQUAL,PHOTFLAM,PHOTFLUX,MAGZPT1",$
            runid,imtype,filtname,seeing,skylev,skysig,skysigbx,photqual,photflam,photflux,magzpt1
  dbclose,dummy1

  CASE option OF

  1: BEGIN
; PLOT 1: Seeing
  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,/color,bits_per_pixel=8,FILENAME='seeing.ps'
  ENDIF

; Extract the data from the databases

  FOR ii = 0,n_runs-1 DO BEGIN
    PRINT,"Extracting seeing for ",runlist[ii]

    dbplothist,dbname,'SEEING',0.4,4.0,0.2, $
               filtnam,filtcol,rshort[ii], $
               'Seeing [arcsec]',"Seeing for "+runlist[ii]
  ENDFOR

  IF NOT psflag AND pngflag THEN BEGIN
    makepng,/color,"seeing.png"
    PRINT,"Press any key to continue"
    key = GET_KBRD(1)
  ENDIF
  END ; Option 1

  2: BEGIN
; PLOT 2: Sky level
  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,/color,bits_per_pixel=8,FILENAME='skylev.ps'
  ENDIF

; Extract the data from the databases

  FOR ii = 0,n_runs-1 DO BEGIN
    totval = FLTARR(1000)/0.0
; This fills totval with NAN entries, which are filtered out by plothist
    nindex = INTARR(n_filters+1)

    PRINT,"Extracting sky level for ",runlist[ii]
    FOR jj = 0,n_filters-1 DO BEGIN
      IF loudflag THEN PRINT,"Extracting indices for filter ",filtnam[jj]
      index = WHERE(STRTRIM(runid,2) EQ STRTRIM(rshort[ii],2) AND $
                    STRTRIM(filtname,2) EQ STRTRIM(filtnam[jj],2) AND $
                    photflam GT 0.0,count)
; We need to convert to ABMag
      nindex[jj+1] = nindex[jj]+count
      IF count GT 0 THEN totval[nindex[jj]:nindex[jj+1]-1] = magzpt1[index] - $
                2.5*ALOG10(as_pix^(-2.0)*skylev[index])
    ENDFOR

    IF loudflag THEN PRINT,"Values range from "+STRING(MIN(totval))+" to "+STRING(MAX(totval))
    xmin = 18.0
    xmax = 22.0
    binsz = 0.25
    ymax = LONG(yscale)*(1+LONG((1.0+MAX( $
        HISTOGRAM(totval,BINSIZE=binsz,MAX=xmax,MIN=xmin)))/FLOAT(yscale)))

    PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[0,ymax], $
         XSTYLE=1,YSTYLE=1,COLOR=!black, CHARSIZE=2.0, $
         XTITLE='Sky level [ABmag/arcsec!U2!N]',YTITLE='Number of images', $
         TITLE="Sky level for "+runlist[ii]

    FOR jj = 1,n_filters DO BEGIN
; Count backwards
      kk = n_filters-jj ; kk goes from 0 to n_filters-1, in reverse
      npoints = nindex[kk+1] - nindex[kk]
      IF npoints GT 2 THEN plothist,totval[0:nindex[kk+1]-1],/fill, $
              bin=binsz,/overplot,FCOLOR=filtcol[kk],PSYM=10,/NAN
    ENDFOR
  ENDFOR

  IF NOT psflag AND pngflag THEN BEGIN
    makepng,/color,"skylev.png"
    PRINT,"Press any key to continue"
    key = GET_KBRD(1)
  ENDIF
  END ; Option 2

  3: BEGIN
; PLOT 3: Sky sigma (pixel-to-pixel) for R and narrow-band images
  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,/color,bits_per_pixel=8,FILENAME='skysig.ps'
  ENDIF

; Extract the data from the databases

  FOR ii = 0,n_runs-1 DO BEGIN
    totval = FLTARR(1000)/0.0
; This fills totval with NAN entries, which are filtered out by plothist
    nindex = INTARR(n_filters+1)

    PRINT,"Extracting sky sigma for ",runlist[ii]

    FOR jj = 0,n_filters-1 DO BEGIN
      IF loudflag THEN PRINT,"Extracting indices for filter ",filtnam[jj]
      index = WHERE(STRTRIM(runid,2) EQ STRTRIM(rshort[ii],2) AND $
                    STRTRIM(filtname,2) EQ STRTRIM(filtnam[jj],2) AND $
                    skysig GT 0.0 AND photflam GT 0.0,count)

; We need to convert to ABMag
      nindex[jj+1] = nindex[jj]+count
      IF count GT 0 THEN totval[nindex[jj]:nindex[jj+1]-1] = magzpt1[index] - $
                2.5*ALOG10(as_pix^(-2.0)*skysig[index])
    ENDFOR

    IF loudflag THEN PRINT,"Values range from "+STRING(MIN(totval))+" to "+STRING(MAX(totval))
    xmin = 21.0
    xmax = 26.0
    binsz = 0.25
    ymax = LONG(yscale)*(1+LONG((1.0+MAX( $
        HISTOGRAM(totval,BINSIZE=binsz,MAX=xmax,MIN=xmin)))/FLOAT(yscale)))

    PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[0,ymax], $
         XSTYLE=1,YSTYLE=1,COLOR=!black, CHARSIZE=2.0, $
         XTITLE='Sky sigma [ABmag/arcsec!U2!N]',YTITLE='Number of images', $
         TITLE="Sky sigma for "+runlist[ii]

    FOR jj = 1,n_filters DO BEGIN
; Count backwards
      kk = n_filters-jj ; kk goes from 0 to n_filters-1, in reverse
      npoints = nindex[kk+1] - nindex[kk]

      IF npoints GT 2 THEN plothist,totval[0:nindex[kk+1]-1],/fill, $
              bin=binsz,/overplot,FCOLOR=filtcol[kk],PSYM=10,/NAN,COLOR=!black
    ENDFOR
  ENDFOR

  IF NOT psflag AND pngflag THEN BEGIN
    makepng,/color,"skysig.png"
    PRINT,"Press any key to continue"
    key = GET_KBRD(1)
  ENDIF
  END ; Option 3

  4: BEGIN
; PLOT 4: Sky sigma (box-to-box) for R and narrow-band images
  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,/color,bits_per_pixel=8,FILENAME='skysigbx.ps'
  ENDIF

; Extract the data from the databases

  FOR ii = 0,n_runs-1 DO BEGIN
    totval = FLTARR(1000)/0.0
; This fills totval with NAN entries, which are filtered out by plothist
    nindex = INTARR(n_filters+1)

    PRINT,"Extracting box sky sigma for ",runlist[ii]

    FOR jj = 0,n_filters-1 DO BEGIN
      IF loudflag THEN PRINT,"Extracting indices for filter ",filtnam[jj]
      index = WHERE(STRTRIM(runid,2) EQ STRTRIM(rshort[ii],2) AND $
                    STRTRIM(filtname,2) EQ STRTRIM(filtnam[jj],2) AND $
                    skysigbx GT 0.0 AND photflam GT 0.0,count)

; We need to convert to ABMag
      nindex[jj+1] = nindex[jj]+count
      IF count GT 0 THEN totval[nindex[jj]:nindex[jj+1]-1] = magzpt1[index] - $
                2.5*ALOG10(as_pix^(-2.0)*skysigbx[index])
    ENDFOR

    IF loudflag THEN PRINT,"Values range from "+STRING(MIN(totval))+" to "+STRING(MAX(totval))
    xmin = 22.0
    xmax = 28.0
    binsz = 0.25
    ymax = LONG(yscale)*(1+LONG((1.0+MAX( $
        HISTOGRAM(totval,BINSIZE=binsz,MAX=xmax,MIN=xmin)))/FLOAT(yscale)))

    PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[0,ymax], $
         XSTYLE=1,YSTYLE=1,COLOR=!black, CHARSIZE=2.0, $
         XTITLE='Sky box sigma [ABmag/arcsec!U2!N]',YTITLE='Number of images', $
         TITLE="Sky box sigma for "+runlist[ii]

    FOR jj = 1,n_filters DO BEGIN
; Count backwards
      kk = n_filters-jj ; kk goes from 0 to n_filters-1, in reverse
      npoints = nindex[kk+1] - nindex[kk]

      IF npoints GT 2 THEN plothist,totval[0:nindex[kk+1]-1],/fill, $
              bin=binsz,/overplot,FCOLOR=filtcol[kk],PSYM=10,/NAN,COLOR=!black
    ENDFOR
  ENDFOR

  IF NOT psflag AND pngflag THEN BEGIN
    makepng,/color,"skysigbx.png"
    PRINT,"Press any key to continue"
    key = GET_KBRD(1)
  ENDIF
  END ; Option 4

  5: BEGIN
; PLOT 5: Sky sigma (pixel-to-pixel) for net images
  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,/color,bits_per_pixel=8,FILENAME='skysig2.ps'
  ENDIF

; Extract the data from the databases

  FOR ii = 0,n_runs-1 DO BEGIN
    totval = FLTARR(1000)/0.0
; This fills totval with NAN entries, which are filtered out by plothist
    nindex = INTARR(n_filters+1)

    PRINT,"Extracting sky sigma for ",runlist[ii]

    FOR jj = 0,n_filters-1 DO BEGIN
      IF loudflag THEN PRINT,"Extracting indices for filter ",filtnam[jj]
      index = WHERE(STRTRIM(runid,2) EQ STRTRIM(rshort[ii],2) AND $
                    STRTRIM(filtname,2) EQ STRTRIM(filtnam[jj],2) AND $
                    skysig GT 0.0 AND STRTRIM(imtype,2) EQ 'net',count)

; We need to convert to erg/cm^2/s/arcsec^2
      nindex[jj+1] = nindex[jj]+count
      IF count GT 0 THEN totval[nindex[jj]:nindex[jj+1]-1] = $
             ALOG10(photflux[index]*skysig[index]/(as_pix)^2.0)
    ENDFOR

    IF loudflag THEN PRINT,"Values range from "+STRING(MIN(totval))+" to "+STRING(MAX(totval))
    xmin = -17.0
    xmax = -15.5
    binsz = 0.05
    ymax = LONG(yscale)*(1+LONG((1.0+MAX( $
        HISTOGRAM(totval,BINSIZE=binsz,MAX=xmax,MIN=xmin)))/FLOAT(yscale)))

    PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[0,ymax], $
         XSTYLE=1,YSTYLE=1,COLOR=!black, CHARSIZE=2.0, $
         XTITLE='LOG(Sky sigma [erg/cm!U2!N/s/arcsec!U2!N])',YTITLE='Number of images', $
         TITLE="Net Image Sky sigma for "+runlist[ii]

    FOR jj = 1,n_filters DO BEGIN
; Count backwards
      kk = n_filters-jj ; kk goes from 0 to n_filters-1, in reverse
      npoints = nindex[kk+1] - nindex[kk]

      IF npoints GT 2 THEN plothist,totval[0:nindex[kk+1]-1],/fill, $
              bin=binsz,/overplot,FCOLOR=filtcol[kk],PSYM=10,/NAN,COLOR=!black
    ENDFOR
  ENDFOR

  IF NOT psflag AND pngflag THEN BEGIN
    makepng,/color,"skysig2.png"
    PRINT,"Press any key to continue"
    key = GET_KBRD(1)
  ENDIF
  END ; Option 5

  6: BEGIN
; PLOT 6: Sky sigma (box-to-box) for net images
  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,/color,bits_per_pixel=8,FILENAME='skysigbx2.ps'
  ENDIF

; Extract the data from the databases

  FOR ii = 0,n_runs-1 DO BEGIN
    totval = FLTARR(1000)/0.0
; This fills totval with NAN entries, which are filtered out by plothist
    nindex = INTARR(n_filters+1)

    PRINT,"Extracting box sky sigma for ",runlist[ii]

    FOR jj = 0,n_filters-1 DO BEGIN
      IF loudflag THEN PRINT,"Extracting indices for filter ",filtnam[jj]
      index = WHERE(STRTRIM(runid,2) EQ STRTRIM(rshort[ii],2) AND $
                    STRTRIM(filtname,2) EQ STRTRIM(filtnam[jj],2) AND $
                    skysigbx GT 0.0 AND STRTRIM(imtype,2) EQ 'net',count)

; We need to convert to ABMag
      nindex[jj+1] = nindex[jj]+count
      IF count GT 0 THEN totval[nindex[jj]:nindex[jj+1]-1] = $
             ALOG10(photflux[index]*skysigbx[index]/(as_pix)^2.0)
    ENDFOR

    IF loudflag THEN PRINT,"Values range from "+STRING(MIN(totval))+" to "+STRING(MAX(totval))
    xmin = -18.5
    xmax = -16.5
    binsz = 0.05
    ymax = LONG(yscale)*(1+LONG((1.0+MAX( $
        HISTOGRAM(totval,BINSIZE=binsz,MAX=xmax,MIN=xmin)))/FLOAT(yscale)))

    PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[0,ymax], $
         XSTYLE=1,YSTYLE=1,COLOR=!black, CHARSIZE=2.0, $
         XTITLE='LOG(Sky box sigma [erg/cm!U2!N/s/arcsec!U2!N])',YTITLE='Number of images', $
         TITLE="Net Image sky box sigma for "+runlist[ii]

    FOR jj = 1,n_filters DO BEGIN
; Count backwards
      kk = n_filters-jj ; kk goes from 0 to n_filters-1, in reverse
      npoints = nindex[kk+1] - nindex[kk]

      IF npoints GT 2 THEN plothist,totval[0:nindex[kk+1]-1],/fill, $
              bin=binsz,/overplot,FCOLOR=filtcol[kk],PSYM=10,/NAN,COLOR=!black
    ENDFOR
  ENDFOR

  IF NOT psflag AND pngflag THEN BEGIN
    makepng,/color,"skysigbx2.png"
    PRINT,"Press any key to continue"
    key = GET_KBRD(1)
  ENDIF
  END ; Option 6

  7: BEGIN
; PLOT 7: Point-source detection limit for R and narrow-band images
  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,/color,bits_per_pixel=8,FILENAME='psdlimit.ps'
  ENDIF

; Extract the data from the databases

  FOR ii = 0,n_runs-1 DO BEGIN
    totval = FLTARR(1000)/0.0
; This fills totval with NAN entries, which are filtered out by plothist
    nindex = INTARR(n_filters+1)

    PRINT,"Extracting sky sigma for ",runlist[ii]

    FOR jj = 0,n_filters-1 DO BEGIN
      IF loudflag THEN PRINT,"Extracting indices for filter ",filtnam[jj]
      index = WHERE(STRTRIM(runid,2) EQ STRTRIM(rshort[ii],2) AND $
                    STRTRIM(filtname,2) EQ STRTRIM(filtnam[jj],2) AND $
                    skysig GT 0.0 AND photflam GT 0.0,count)

; We need to convert to ABMag
      nindex[jj+1] = nindex[jj]+count
      IF count GT 0 THEN totval[nindex[jj]:nindex[jj+1]-1] = magzpt1[index] - $
                2.5*ALOG10(5.0*pi*((seeing[index]/2.0)/as_pix)^2.0*skysig[index])
    ENDFOR

    IF loudflag THEN PRINT,"Values range from "+STRING(MIN(totval))+" to "+STRING(MAX(totval))
    xmin = 18.0
    xmax = 24.0
    binsz = 0.2
    ymax = LONG(yscale)*(1+LONG((1.0+MAX( $
        HISTOGRAM(totval,BINSIZE=binsz,MAX=xmax,MIN=xmin)))/FLOAT(yscale)))

    PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[0,ymax], $
         XSTYLE=1,YSTYLE=1,COLOR=!black, CHARSIZE=2.0, $
         XTITLE='Magnitude [ABmag]',YTITLE='Number of images', $
         TITLE="Point-Source Detection Limit for "+runlist[ii]

    FOR jj = 1,n_filters DO BEGIN
; Count backwards
      kk = n_filters-jj ; kk goes from 0 to n_filters-1, in reverse
      npoints = nindex[kk+1] - nindex[kk]

      IF npoints GT 2 THEN plothist,totval[0:nindex[kk+1]-1],/fill, $
              bin=binsz,/overplot,FCOLOR=filtcol[kk],PSYM=10,/NAN,COLOR=!black
    ENDFOR
  ENDFOR

  IF NOT psflag AND pngflag THEN BEGIN
    makepng,/color,"psdlimit.png"
    PRINT,"Press any key to continue"
    key = GET_KBRD(1)
  ENDIF
  END ; Option 7

  8: BEGIN
; PLOT 8: Point-source detection limit for net images
  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,/color,bits_per_pixel=8,FILENAME='psdlimit2.ps'
  ENDIF

; Extract the data from the databases

  FOR ii = 0,n_runs-1 DO BEGIN
    totval = FLTARR(1000)/0.0
; This fills totval with NAN entries, which are filtered out by plothist
    nindex = INTARR(n_filters+1)

    PRINT,"Extracting sky sigma for ",runlist[ii]

    FOR jj = 0,n_filters-1 DO BEGIN
      IF loudflag THEN PRINT,"Extracting indices for filter ",filtnam[jj]
      index = WHERE(STRTRIM(runid,2) EQ STRTRIM(rshort[ii],2) AND $
                    STRTRIM(filtname,2) EQ STRTRIM(filtnam[jj],2) AND $
                    skysig GT 0.0 AND STRTRIM(imtype,2) EQ 'net',count)

; We need to convert to ABMag
      nindex[jj+1] = nindex[jj]+count
      IF count GT 0 THEN totval[nindex[jj]:nindex[jj+1]-1] = $
        ALOG10((5.0*pi*((seeing[index]/2.0)/as_pix)^2.0*photflux[index]*skysig[index]))
    ENDFOR

    IF loudflag THEN PRINT,"Values range from "+STRING(MIN(totval))+" to "+STRING(MAX(totval))
    xmin = -16.0
    xmax = -14.5
    binsz = 0.05
    ymax = LONG(yscale)*(1+LONG((1.0+MAX( $
        HISTOGRAM(totval,BINSIZE=binsz,MAX=xmax,MIN=xmin)))/FLOAT(yscale)))

    PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[0,ymax], $
         XSTYLE=1,YSTYLE=1,COLOR=!black, CHARSIZE=2.0, $
         XTITLE='LOG(Flux [erg/cm!U2!N/s])',YTITLE='Number of images', $
         TITLE="Point-Source Detection Limit for net images for "+runlist[ii]

    FOR jj = 1,n_filters DO BEGIN
; Count backwards
      kk = n_filters-jj ; kk goes from 0 to n_filters-1, in reverse
      npoints = nindex[kk+1] - nindex[kk]

      IF npoints GT 2 THEN plothist,totval[0:nindex[kk+1]-1],/fill, $
              bin=binsz,/overplot,FCOLOR=filtcol[kk],PSYM=10,/NAN,COLOR=!black
    ENDFOR
  ENDFOR

  IF NOT psflag AND pngflag THEN BEGIN
    makepng,/color,"psdlimit2.png"
    PRINT,"Press any key to continue"
    key = GET_KBRD(1)
  ENDIF
  END ; Option 8

  9: BEGIN
; PLOT 9: PHOTQUAL
  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,/color,bits_per_pixel=8,FILENAME='photqual.ps'
  ENDIF

; Extract the data from the databases

  FOR ii = 0,n_runs-1 DO BEGIN
    PRINT,"Extracting PHOTQUAL for ",runlist[ii]

    dbplothist,dbname,'PHOTQUAL',0.001,0.10,0.005, $
               filtnam,filtcol,rshort[ii], $
               'RMS of time-corrected scaling factor', $
               "Photometric quality for "+runlist[ii] ; ,/LOG
  ENDFOR

  IF NOT psflag AND pngflag THEN BEGIN
    makepng,/color,"photqual.png"
    PRINT,"Press any key to continue"
    key = GET_KBRD(1)
  ENDIF
  END ; Option 9

  0: BEGIN
; PLOT 0: MAGZPT1
  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,/color,bits_per_pixel=8,FILENAME='magzpt1.ps'
  ENDIF

; Extract the data from the databases

  FOR ii = 0,n_runs-1 DO BEGIN
    PRINT,"Extracting MAGZPT1 for ",runlist[ii]

    dbplothist,dbname,'MAGZPT1',19.0,25.0,0.025, $
               filtnam,filtcol,rshort[ii], $
               'Magnitude [ABmag]',"Photometric zeropoint for "+runlist[ii]
  ENDFOR

  IF NOT psflag AND pngflag THEN BEGIN
    makepng,"magzpt1.png"
    PRINT,"Press any key to continue"
    key = GET_KBRD(1)
  ENDIF
  END ; Option 0

;---------------
  ELSE: BEGIN
    PRINT,'ERROR in makeplot_db: invalid option ',option
    RETURN
  END ; Else

  ENDCASE

  RETURN
END

