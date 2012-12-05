PRO makeplot2,ps=ps,FILTFILE=filtfile

; /ps replaces the normal window output with a postscript dump

  psflag = KEYWORD_SET(ps)
  IF NOT KEYWORD_SET(filtfile) THEN filtfile = !singgdir+"/filtfile.dat"
  IF NOT FILE_TEST(filtfile) THEN BEGIN
    PRINT,"ERROR in singg_combine: filter file not found ",filtfile
    RETURN
  ENDIF
; Load filter table:
  singg_rdfiltfile,filtfile,fnamarr

  runlist = ["run01","run02","run03","run06"]
  n_runs = N_ELEMENTS(runlist)
  max_objs = 50

  n_objs = INTARR(n_runs)
  object = STRARR(n_runs,max_objs)
  Rfile = STRARR(n_runs,max_objs)
  Cfile = STRARR(n_runs,max_objs)
  Sfile = STRARR(n_runs,max_objs)

  FOR ii = 0,n_runs-1 DO BEGIN
    catalog = runlist[ii]+".catalog"

    readcol_new,catalog,objtemp,filter,Rtemp,Ctemp,Stemp, $
                        ellipse,refnum,Rmask,Cmask, $
                        FORMAT="A,A,A,A,A,A,A,A,A",comment='#'

    n_objs[ii] = N_ELEMENTS(objtemp)

    FOR jj = 0,n_objs[ii]-1 DO BEGIN
      object[ii,jj] = objtemp[jj]
      Rfile[ii,jj] = Rtemp[jj]
      Cfile[ii,jj] = Ctemp[jj]
      Sfile[ii,jj] = Stemp[jj]
    ENDFOR

  ENDFOR

  as_pix = 0.432
; arcsec per pixel.  We COULD read this from headers, but why bother?

; SEEING
  Rseeing = FLTARR(n_runs,max_objs)
  Cseeing = FLTARR(n_runs,max_objs)
  Sseeing = FLTARR(n_runs,max_objs)

; PHOTQUAL (RMS of scaling, corrected for exposure time)
  Rphotqual = FLTARR(n_runs,max_objs)
  Cphotqual = FLTARR(n_runs,max_objs)
  Sphotqual = FLTARR(n_runs,max_objs)

; SKY (skylev, skysigpix, skysigbox)
  Rsky = FLTARR(n_runs,max_objs,3)
  Csky = FLTARR(n_runs,max_objs,3)
  Ssky = FLTARR(n_runs,max_objs,3)

; Point-source detection limit
  Rpsdlim = FLTARR(n_runs,max_objs)
  Cpsdlim = FLTARR(n_runs,max_objs)
  Spsdlim = FLTARR(n_runs,max_objs)

; Zero-point magnitude (MAGZPT1)
  Rmagzpt = FLTARR(n_runs,max_objs)
  Cmagzpt = FLTARR(n_runs,max_objs)
  Smagzpt = FLTARR(n_runs,max_objs)

; Now that the arrays are set up, start reading info.
  FOR ii = 0,n_runs-1 DO BEGIN
    FOR jj = 0,n_objs[ii]-1 DO BEGIN

      fits_read,Rfile[ii,jj],img,Rhd,/header_only
      fits_read,Cfile[ii,jj],img,Chd,/header_only
      fits_read,Sfile[ii,jj],img,Shd,/header_only

; Read the filter names, and convert to standard naming:
      Rfilter = singg_filtnam(fnamarr,SXPAR(Rhd,"FILTER1"),Rpos)
      Cfilter = singg_filtnam(fnamarr,SXPAR(Chd,"FILTER1"),Cpos)

; We just want the "short" name.
      Rfilt[ii,jj] = fnamarr[Rpos,1]
      Cfilt[ii,jj] = fnamarr[Cpos,1]

; Store the raw stuff

      Rseeing[ii,jj] = SXPAR(Rhd,"SEEING")
      Cseeing[ii,jj] = SXPAR(Chd,"SEEING")
      Sseeing[ii,jj] = SXPAR(Shd,"SEEING") ; Not used

      Rphotqual[ii,jj] = SXPAR(Rhd,"PHOTQUAL")
      Cphotqual[ii,jj] = SXPAR(Chd,"PHOTQUAL")
      Sphotqual[ii,jj] = SXPAR(Shd,"PHOTQUAL") ; Not used

      Rmagzpt[ii,jj] = SXPAR(Rhd,"MAGZPT1")
      Cmagzpt[ii,jj] = SXPAR(Chd,"MAGZPT1")
      Smagzpt[ii,jj] = SXPAR(Shd,"MAGZPT1")
    
      Rsky[ii,jj,0] = SXPAR(Rhd,"SKYLEV")
      Rsky[ii,jj,1] = SXPAR(Rhd,"SKYSIG")
      Rsky[ii,jj,2] = SXPAR(Rhd,"SKYSIGBX")

      Csky[ii,jj,0] = SXPAR(Chd,"SKYLEV")
      Csky[ii,jj,1] = SXPAR(Chd,"SKYSIG")
      Csky[ii,jj,2] = SXPAR(Chd,"SKYSIGBX")

      Ssky[ii,jj,0] = SXPAR(Shd,"SKYLEV") ; Meaningless number
      Ssky[ii,jj,1] = SXPAR(Shd,"SKYSIG") 
      Ssky[ii,jj,2] = SXPAR(Shd,"SKYSIGBX")

    ENDFOR
  ENDFOR

; Now that the arrays are set up, it's time for histograms!

  IF NOT psflag THEN BEGIN
;    SET_PLOT,'X'
;    HELP,/device
    DEVICE, RETAIN=2, DECOMPOSED=0
    WINDOW,XSIZE=800,YSIZE=800
  ENDIF
  IF psflag THEN set_plot,'PS'
  !P.MULTI=[0,0,4,0,0]

  setplotcolors
; was 4,1,5,!blue,6,!red
  run_sym = [1,2,3,6]
  R_clr = !red
  C_clr = !orange
  6568_clr = !blue
  6600_clr = !dblue
  6605_clr = !dgreen
  6618_clr = !green
  6653_clr = !magenta

; PLOT 1: Seeing
  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,/color,bits_per_pixel=8,FILENAME='seeing.ps'
  ENDIF

  FOR ii = 0,n_runs-1 DO BEGIN
    PLOT,HISTOGRAM(Rseeing[ii,*],BINSIZE=0.1),SYM=run_sym[ii],COLOR=R_clr
    OPLOT,HISTOGRAM(Cseeing[ii,*],BINSIZE=0.1),SYM=run_sym[ii],COLOR=6568_clr
  ENDFOR

  IF NOT psflag THEN BEGIN
    makepng,"seeing.png"
    PRINT,"Press any key to continue"
    key = GET_KBRD(1)
  ENDIF

; PLOT 2: PHOTQUAL

  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,/color,bits_per_pixel=8,FILENAME='photqual.ps'
  ENDIF

  FOR ii = 0,n_runs-1 DO BEGIN
    PLOT,HISTOGRAM(Rphotqual[ii,*],BINSIZE=0.002),SYM=run_sym[ii],COLOR=R_clr
    OPLOT,HISTOGRAM(Cphotqual[ii,*],BINSIZE=0.002),SYM=run_sym[ii],COLOR=6568_clr
  ENDFOR

  IF NOT psflag THEN BEGIN
    makepng,"photqual.png"
    PRINT,"Press any key to continue"
    key = GET_KBRD(1)
  ENDIF

; PLOT 3: SKYLEV

  FOR ii = 0,n_runs-1 DO BEGIN
    PLOT,HISTOGRAM(Rsky[ii,*,0],BINSIZE=0.002),SYM=run_sym[ii],COLOR=R_clr
    OPLOT,HISTOGRAM(Csky[ii,*,0],BINSIZE=0.002),SYM=run_sym[ii],COLOR=6568_clr
  ENDFOR

  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,/color,bits_per_pixel=8,FILENAME='skylev.ps'
  ENDIF

;moo

  IF NOT psflag THEN BEGIN
    makepng,"skylev.png"
    PRINT,"Press any key to continue"
    key = GET_KBRD(1)
  ENDIF

; PLOT 4: SKYSIG

  FOR ii = 0,n_runs-1 DO BEGIN
    PLOT,HISTOGRAM(Rsky[ii,*,1],BINSIZE=0.002),SYM=run_sym[ii],COLOR=R_clr
    OPLOT,HISTOGRAM(Csky[ii,*,1],BINSIZE=0.002),SYM=run_sym[ii],COLOR=6568_clr
  ENDFOR

  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,bits_per_pixel=8,/color,FILENAME='skysig.ps'
    sz=1.0
  ENDIF

;moo

  IF NOT psflag THEN makepng,"skysig.png" ELSE DEVICE,/CLOSE

END

; PLOT 5: SKYSIGBX

  FOR ii = 0,n_runs-1 DO BEGIN
    PLOT,HISTOGRAM(Rsky[ii,*,2],BINSIZE=0.002),SYM=run_sym[ii],COLOR=R_clr
    OPLOT,HISTOGRAM(Csky[ii,*,2],BINSIZE=0.002),SYM=run_sym[ii],COLOR=6568_clr
  ENDFOR

  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,bits_per_pixel=8,/color,FILENAME='skysigbx.ps'
    sz=1.0
  ENDIF

;moo

  IF NOT psflag THEN makepng,"skysigbx.png" ELSE DEVICE,/CLOSE

; PLOT 6: MAGZPT1

  IF psflag THEN BEGIN
    DEVICE,/CLOSE
;    set_plot,'PS'
    DEVICE,bits_per_pixel=8,/color,FILENAME='magzpt1.ps'
    sz=1.0
  ENDIF

;moo

  IF NOT psflag THEN makepng,"magzpt1.png" ELSE DEVICE,/CLOSE

END

