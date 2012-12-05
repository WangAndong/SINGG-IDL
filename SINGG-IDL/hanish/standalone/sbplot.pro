PRO sbplot,PS=ps
; Does a simple Surface Brightness vs MHI plot

  psflag = KEYWORD_SET(ps)

  headerdb = "proc3_header"
  refdb = "singg_sample"
  fluxdb = "singg_source"

  dbopen,refdb,0
  dbext,-1,"NAME,   LOGMHI", $
            refname,logMHI
  dbclose,dummy

  dbopen,fluxdb,0
  dbext,-1,"OBJECT,IMTYPE,HLCRCFLI", $
            flobj, fltype,flsb
  dbclose,dummy

  refname = update_name(refname)
  flobj = update_name(flobj)

  Sindex = WHERE(STRTRIM(fltype,2) EQ "net",scount)
  mhi = FLTARR(scount)

  FOR ii = 0,scount-1 DO BEGIN
    refindex = WHERE(STRTRIM(refname,2) EQ STRTRIM(flobj[Sindex[ii]],2),refcount)
    IF refcount GT 1 THEN BEGIN
      PRINT,"ERROR!!! multiple matches ",flobj[Sindex[ii]]
      RETURN
    ENDIF

    IF refcount EQ 0 THEN BEGIN
      PRINT,"No matches! ",flobj[Sindex[ii]]
      mhi[ii] = 0.0
    ENDIF ELSE BEGIN
      mhi[ii] = 10.0^logMHI[refindex[0]]
    ENDELSE
  ENDFOR

  setplotcolors

  IF NOT psflag THEN BEGIN
    set_plot,'X'
    setbgfg,!white,!black
    DEVICE, RETAIN=2, DECOMPOSED=0
    WINDOW,XSIZE=800,YSIZE=600
    !P.MULTI=[0,0,1,0,0]

    charsz = 2.0
  ENDIF ELSE BEGIN
; We'll write .eps files
    set_plot,'PS'
    xs = 6.5
    ys = 0.75*xs
    xoff = 1.2
    yoff = 3.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
           bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,0,1,0,0]

    charsz = 1.5
  ENDELSE

  ymin = -18.0
  ymax = -13.0

  IF psflag THEN title="" ELSE title="Surface Brightness"

  PLOT,ALOG10(mhi),ALOG10(flsb[Sindex]),XRANGE=[7.0,11.0],YRANGE=[ymin,ymax], $
       XTITLE="log!D10!N(M!DHI!N / M!Dsolar!N)",PSYM=1,CHARSIZE=charsz,COLOR=1, $
       YTITLE="log!D10!N(Surface Brightness [erg/cm!E2!N/sec/arcsec!E2!N])",LINESTYLE=1, $
       TITLE=title

  IF psflag THEN BEGIN
    !p.multi   = 0
    !p.noerase = 0
    psend,!outdir+"sbmhi.eps",/noprint,/clobber
  ENDIF

END
