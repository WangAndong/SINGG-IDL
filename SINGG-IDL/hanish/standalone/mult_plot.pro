PRO mult_plot,PS=ps,ALL=all

  psflag = KEYWORD_SET(ps)

  al10 = ALOG(10.0)

  dbopen,'singg_flux',0
  flind = good_flux(/TRIM)+1
  dbext,flind,'OBJECT,RUNID,FILENAME,IMTYPE,FLUX_T_ISO,ERR_FLUX_T_SKY_ISO,ERR_FLUX_T_CONT_ISO,FLUX_SCALE,PHOTMTRC,GALINDEX,NUMGALS', $
               object,runid,filename,imtype,flux_t_iso,err_flux_t_sky_iso,err_flux_t_cont_iso,flux_scale,photmtrc,galindex,numgals
  dbext,flind,'FLUXRAD_F_ISO,AXERAT,PA', $
               rmax,axerat,pa
  dbclose

  dbopen,'proc3_header',0
  dbext,-1,'TARGET,FILENAME,IMTYPE,MAGZPT1,RUNID', $
            target,hfile,   htype, magzpt1,hrun
  dbclose
  hrun = 'Run'+STRTRIM(hrun,2)

  dummy = FLTARR(2)

  readcol_new,!singgdir+'singg_multiple.dat',mobj,mfile,mrun,mpr, $
              COMMENT='#',FORMAT='(A,A,A,I)',/SILENT
  mobj = STRTRIM(mobj,2)
  mfile = STRTRIM(mfile,2)
  mrun = STRTRIM(mrun,2)
  mlist = UNIQ(mobj,SORT(mobj))
  n_mult = N_ELEMENTS(mobj)
  n_sources = N_ELEMENTS(mlist)

  Rfile = STRARR(n_mult)
;;  Nfile = STRARR(n_mult)
  FOR ii = 0,n_mult-1 DO BEGIN
    lpos = STRPOS(mfile[ii],'_')
    subpos = STRPOS(mfile[ii],'sub')
    cont = STRMID(mfile[ii],(subpos-1),1)
    obj = STRMID(mfile[ii],0,lpos)
    Rfile[ii] = obj+'_'+cont+'_ss.fits'
  ENDFOR

  runlist = ['Run01','Run02','Run03','Run04','Run05','Run06','Run07','Run08','Run09','Run10','Run11','Run12','Run13','Run15','Run17']
  n_runs = N_ELEMENTS(runlist)

  IF NOT psflag THEN BEGIN
    set_plot,'X'
    setplotcolors
    setbgfg,!white,!black
    DEVICE, RETAIN=2, DECOMPOSED=0
    WINDOW,XSIZE=1400,YSIZE=1100
    !P.MULTI=[0,4,4,0,0]
    charsz = 3.0
    symsz = 1.2
    thick = 1.0
  ENDIF ELSE BEGIN
    set_plot,'PS'
    setplotcolors
    xs = 10.5
    ys = 6.5
    xoff = 1.2
    yoff = 3.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
           bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,3,2,0,0]
    charsz = 2.0
    symsz = 1.2
    thick = 2.0
  ENDELSE

  xpos = FINDGEN(n_runs)-1.0 ; use 1.0 if angle is 90

; Plot flux vs runID for multiple-image galaxies
  PLOT,dummy,dummy,XRANGE=[0,n_runs+1],YRANGE=[1E-15,1E-10],XSTYLE=1,YSTYLE=1, $
       COLOR=!black,XTITLE='Run ID',YTITLE='Flux [erg cm!E-2!N s!E-1!N]', $
       CHARSIZE=charsz,THICK=thick,/YLOG,XCHARSIZE=0.0001

  ypos = FLTARR(n_runs)+7E-17 ; use 17.8 if angle is 90
  XYOUTS,xpos,ypos,runlist,ALIGNMENT=0.0,COLOR=!black,CHARSIZE=charsz/2.5,/NOCLIP,ORIENTATION=45.0

  FOR ii = 0,n_sources-1 DO BEGIN
    mind = WHERE(mobj EQ mobj[mlist[ii]] AND mrun NE 'Run04s',mcount)
    junk = MIN(mpr[mind],mref)
    IF mcount GT 1 THEN BEGIN
; Don't bother if mcount=1, because that usually means a rejection.
      flux = FLTARR(mcount)
      frun = INTARR(mcount)
      fphot = BYTARR(mcount)
      fplot = FLTARR(mcount-1)
      FOR jj = 0,mcount-1 DO BEGIN
        ind = WHERE(STRTRIM(object,2) EQ mobj[mlist[ii]] AND $
                    STRTRIM(runid,2) EQ mrun[mind[jj]] AND $
                    STRTRIM(filename,2) EQ mfile[mind[jj]],count)
        IF count GE 1 THEN BEGIN
          IF count NE numgals[ind[0]] THEN BEGIN
            PRINT,'ERROR in mult_plot: number of galaxies does not match ',count,numgals[ind[0]], mobj[mlist[ii]]
            RETURN
          ENDIF

          flux[jj] = TOTAL(flux_t_iso[ind])
          rind = WHERE(STRTRIM(runlist,2) EQ STRTRIM(runid[ind[0]],2),count)
          IF count GT 0 THEN frun[jj] = rind[0]+1 ELSE frun[jj] = -999
;          frun[jj] = LONG(STRMID(runid[ind[0]],3,2))

          fphot[jj] = (STRTRIM(photmtrc[ind[0]],2) EQ 'Y')
;;          PRINT,'   ',flux[jj],' ',frun[jj],' ',fphot[jj]
        ENDIF
      ENDFOR

      IF mref GT 0 THEN fplot[0:mref-1] = flux[0:mref-1]-flux[mref]
      IF mref LT (mcount-1) THEN fplot[mref:mcount-2] = flux[mref+1:mcount-1]-flux[mref]
      good = WHERE(frun GT 0,count)

      IF count GT 0 THEN BEGIN
        PLOTS,frun[good],flux[good],THICK=thick,COLOR=!ddgray

        bind = WHERE(NOT fphot[good],bcount)
        IF bcount GT 0 THEN OPLOT,frun[good[bind]],flux[good[bind]],COLOR=!dred,PSYM=sym(1)
        gind = WHERE(fphot,gcount)
        IF gcount GT 0 THEN OPLOT,frun[good[gind]],flux[good[gind]],COLOR=!dblue,PSYM=sym(1)
      ENDIF
    ENDIF
  ENDFOR

; Plot rmax vs R magnitude
  PLOT,dummy,dummy,XRANGE=[8.0,21.0],YRANGE=[4.0,600.0],XSTYLE=1,YSTYLE=1, $
       COLOR=!black,XTITLE='H!7a!6 magnitude [ABmag]',YTITLE='r!Dmax!N [arcsec]', $
       CHARSIZE=charsz,THICK=thick,/YLOG

  FOR ii = 0,n_sources-1 DO BEGIN
    mind = WHERE(mobj EQ mobj[mlist[ii]] AND mrun NE 'Run04s',mcount)
    junk = MIN(mpr[mind],mref)
    IF mcount GT 1 THEN BEGIN
; Don't bother if mcount=1, because that usually means a rejection.
      testind = WHERE(STRTRIM(object,2) EQ mobj[mlist[ii]] AND $
                      STRTRIM(filename,2) EQ mfile[mlist[ii]],testcount)
      IF testcount GE 1 THEN BEGIN
       FOR kk = 0,numgals[testind[0]]-1 DO BEGIN
        fmag = FLTARR(mcount)
        frad = FLTARR(mcount)
        fphot = BYTARR(mcount)
        fplot = FLTARR(mcount-1)
        FOR jj = 0,mcount-1 DO BEGIN
          ind = WHERE(STRTRIM(object,2) EQ mobj[mlist[ii]] AND $
                      STRTRIM(runid,2) EQ mrun[mind[jj]] AND $
                      galindex EQ kk+1 AND $
                      STRTRIM(filename,2) EQ mfile[mind[jj]],count)
          IF count GE 1 THEN BEGIN
            hind = WHERE(STRMID(target,0,8) EQ STRMID(mobj[mlist[ii]],0,8) AND $
                         hrun EQ mrun[mind[jj]] AND $
                         STRTRIM(hfile,2) EQ mfile[mind[jj]],hcount)
            IF hcount EQ 0 THEN BEGIN
              PRINT,'ERROR in mult_plot: no header match for object ',mobj[mlist[ii]],' ',mrun[mind[jj]],' ',mfile[mind[jj]]
              RETURN
            ENDIF ELSE BEGIN
              fmag[jj] = magzpt1[hind[0]] - 2.5 * ALOG10(TOTAL(flux_t_iso[ind]/flux_scale[ind]))
              fphot[jj] = (STRTRIM(photmtrc[ind[0]],2) EQ 'Y')
              frad[jj] = rmax[ind[0]]
            ENDELSE
          ENDIF ELSE BEGIN
;;            PRINT,'ERROR in mult_plot: no flux matches for object ',mobj[mlist[ii]],' ',mrun[mind[jj]],' ',mfile[mind[jj]],' ',kk
;;            RETURN
          ENDELSE
        ENDFOR

        IF mref GT 0 THEN fplot[0:mref-1] = fmag[0:mref-1]-fmag[mref]
        IF mref LT (mcount-1) THEN fplot[mref:mcount-2] = fmag[mref+1:mcount-1]-fmag[mref]

        goodind = WHERE(fmag GT 1.0,count)
        IF count GT 0 THEN OPLOT,fmag[goodind],frad[goodind],COLOR=!blue, $
                                 PSYM=-SYM(1),SYMSIZE=symsz
        badind = WHERE(fmag GT 1.0 AND NOT fphot,count)
        IF count GT 0 THEN OPLOT,fmag[badind],frad[badind],COLOR=!red, $
                                 PSYM=SYM(1),SYMSIZE=symsz*0.75
       ENDFOR
      ENDIF
    ENDIF
  ENDFOR

; Plot theta vs. a/b  ypos = FLTARR(n_runs)+18.0
  PLOT,dummy,dummy,XRANGE=[1.0,4.0],YRANGE=[0.1,20.0],XSTYLE=1,YSTYLE=1, $
       COLOR=!black,XTITLE='Axial ratio (a/b)',YTITLE='!7D!6pa [deg]', $
       CHARSIZE=charsz,THICK=thick ; ,/YLOG

  FOR ii = 0,n_sources-1 DO BEGIN
    mind = WHERE(mobj EQ mobj[mlist[ii]] AND mrun NE 'Run04s',mcount)
    junk = MIN(mpr[mind],mref)
    IF mcount GT 1 THEN BEGIN
; Don't bother if mcount=1, because that usually means a rejection.
      testind = WHERE(STRTRIM(object,2) EQ mobj[mlist[ii]] AND $
                      STRTRIM(filename,2) EQ mfile[mlist[ii]],testcount)
      IF testcount GE 1 THEN BEGIN
       FOR kk = 0,numgals[testind[0]]-1 DO BEGIN
        fpa = FLTARR(mcount)
        faxerat = FLTARR(mcount)
        fphot = BYTARR(mcount)
        fplot = FLTARR(mcount-1)
        FOR jj = 0,mcount-1 DO BEGIN
          ind = WHERE(STRTRIM(object,2) EQ mobj[mlist[ii]] AND $
                      STRTRIM(runid,2) EQ mrun[mind[jj]] AND $
                      galindex EQ kk+1 AND $
                      STRTRIM(filename,2) EQ mfile[mind[jj]],count)
          IF count GE 1 THEN BEGIN
            fpa[jj] = ((pa[ind[0]]+90.0) MOD 180.0)-90.0
            faxerat[jj] = axerat[ind[0]]
            fphot[jj] = (STRTRIM(photmtrc[ind[0]],2) EQ 'Y')
          ENDIF ELSE BEGIN
;;            PRINT,'ERROR in mult_plot: no flux matches for object ',mobj[mlist[ii]],' ',mrun[mind[jj]],' ',mfile[mind[jj]],' ',kk
;;            RETURN
          ENDELSE
        ENDFOR

        IF mref GT 0 THEN fplot[0:mref-1] = fpa[0:mref-1]-fpa[mref]
        IF mref LT (mcount-1) THEN fplot[mref:mcount-2] = fpa[mref+1:mcount-1]-fpa[mref]

        goodind = WHERE(faxerat GT 1.0 AND ABS(fpa-fpa[mref]) GT 0.01,count)
ymean = fpa[mref]
;        ymean = MEAN(fpa[goodind])
        IF count GT 0 THEN OPLOT,faxerat[goodind],ABS(fpa[goodind]-ymean),COLOR=!blue, $
                                 PSYM=-SYM(1),SYMSIZE=symsz
        badind = WHERE(faxerat GT 1.0 AND NOT fphot,count)
        IF count GT 0 THEN OPLOT,faxerat[badind],ABS(fpa[badind]-ymean),COLOR=!red, $
                                 PSYM=SYM(1),SYMSIZE=symsz*0.75
       ENDFOR
      ENDIF
    ENDIF
  ENDFOR

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
  n_filts = N_ELEMENTS(filtnam)
  fgood = BYTARR(n_runs,n_filts)
  ext = FLTARR(n_runs)
  zpt = FLTARR(n_runs,n_filts)

  FOR ii = 0,n_runs-1 DO BEGIN
    p4dir = '/data1/acs22/hanish/'+STRTRIM(runlist[ii],2)+'/Proc4/'
    parfile = p4dir+STRTRIM(runlist[ii],2)+'_all.par'

    readcol_new,parfile,fname,zeropt,ext_term,rms,ngood,nrej, $
                FORMAT='(A,F,F,F,I,I)',/SILENT,COMMENT='#'
    ext[ii] = MEAN(ext_term)

    CASE STRTRIM(runlist[ii],2) OF
      'Run04s': BEGIN
          zeropt = zeropt + 2.5*ALOG10(25.0/4.0)
        END
      'Run15': BEGIN
;          zeropt = zeropt + 2.5*ALOG10(25.0/9.0)
        END
      'Run17': BEGIN
;          zeropt = zeropt + 2.5*ALOG10(25.0/9.0)
        END
      ELSE: BEGIN
        END
    ENDCASE

    FOR jj = 0,N_ELEMENTS(fname)-1 DO BEGIN
      ind = WHERE(filtnam EQ STRTRIM(fname[jj],2),count)
      IF count GT 0 THEN BEGIN
        fgood[ii,ind[0]] = 1b
        zpt[ii,ind[0]] = zeropt[jj]
      ENDIF
    ENDFOR

  ENDFOR

  PLOT,dummy,dummy,XRANGE=[0,n_runs+1],YRANGE=[19.0,25.0],XSTYLE=1,YSTYLE=1, $
       COLOR=!black,XTITLE='Run ID',YTITLE='Zero-point magnitude', $
       CHARSIZE=charsz,THICK=thick,XCHARSIZE=0.0001

  ypos = FLTARR(n_runs)+17.6 ; use 17.8 if angle is 90
  XYOUTS,xpos,ypos,runlist,ALIGNMENT=0.0,COLOR=!black,CHARSIZE=charsz/2.5,/NOCLIP,ORIENTATION=45.0

  FOR ii = 0,n_filts-1 DO BEGIN
    ind = WHERE(fgood[*,ii],count)
    IF count GE 1 THEN BEGIN
      PLOTS,ind+1,zpt[ind,ii],COLOR=filtcol[ii],PSYM=-SYM(1),SYMSIZE=symsz
    ENDIF
  ENDFOR

  PLOT,dummy,dummy,XRANGE=[0,n_runs+1],YRANGE=[-0.15,0.0],XSTYLE=1,YSTYLE=1, $
       COLOR=!black,XTITLE='Run ID',YTITLE='Extinction term', $
       CHARSIZE=charsz,THICK=thick,XCHARSIZE=0.0001

  ypos = FLTARR(n_runs)-0.185
  XYOUTS,xpos,ypos,runlist,ALIGNMENT=0.0,COLOR=!black,CHARSIZE=charsz/2.5,/NOCLIP,ORIENTATION=45.0

  PLOTS,INDGEN(n_runs)+1,ext,COLOR=!blue,PSYM=-SYM(1),SYMSIZE=symsz

  IF psflag THEN BEGIN
    psend,!outdir+'mult1.eps',/noprint,/clobber
spawn,'display '+!outdir+'mult1.eps &'
    set_plot,'PS'
    charsz = 1.5
    xs = 10.5
    ys = 4.5
    xoff = 1.2
    yoff = 3.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
           bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,2,1,0,0]
  ENDIF

; Plot M_R and logFHa vs their discrepancies.
  max_m = 1000
  obj = STRARR(max_m)
  gal = INTARR(max_m)
  run = STRARR(max_m)
  phot = BYTARR(max_m)

  lfr = FLTARR(max_m)
  dfr = FLTARR(max_m)
  dfr2 = FLTARR(max_m)
  dellfr = FLTARR(max_m)
  mr = FLTARR(max_m)
  delmr = FLTARR(max_m)
  dmr = FLTARR(max_m)
  lfha = FLTARR(max_m)
  dfha = FLTARR(max_m)
  dfha2 = FLTARR(max_m)
  dellfha = FLTARR(max_m)
  lrmax = FLTARR(max_m)
  dellrmax = FLTARR(max_m)
  posang = FLTARR(max_m)
  delposang = FLTARR(max_m)
  ab = FLTARR(max_m)
  delab = FLTARR(max_m)

  zz = 0

  FOR ii = 0,n_sources-1 DO BEGIN
    mind = WHERE(mobj EQ mobj[mlist[ii]] AND mrun NE 'Run04s',mcount)
    junk = MIN(mpr[mind],mref)

    IF mcount GT 1 THEN BEGIN
; Don't bother if mcount=1, because that usually means a rejection.
      testind = WHERE(STRTRIM(object,2) EQ mobj[mlist[ii]] AND $
                      STRTRIM(filename,2) EQ mfile[mlist[ii]],testcount)
      IF testcount GE 1 THEN BEGIN
       FOR kk = 0,MAX(numgals[testind])-1 DO BEGIN
        zold = zz
        FOR jj = 0,mcount-1 DO BEGIN
          IF jj EQ mref THEN zref = zz
          Rind = WHERE(STRTRIM(object,2) EQ mobj[mlist[ii]] AND $
                       STRTRIM(runid,2) EQ mrun[mind[jj]] AND $
                       galindex EQ kk+1 AND $
                       STRTRIM(imtype,2) EQ 'cont' AND $
                       STRTRIM(filename,2) EQ Rfile[mind[jj]],Rcount)
          Nind = WHERE(STRTRIM(object,2) EQ mobj[mlist[ii]] AND $
                       STRTRIM(runid,2) EQ mrun[mind[jj]] AND $
                       galindex EQ kk+1 AND $
                       STRTRIM(imtype,2) EQ 'net' AND $
                       STRTRIM(filename,2) EQ mfile[mind[jj]],Ncount)

;if strmid(mobj[mlist[ii]],0,8) eq 'J0406-21' then begin
; print,mcount,testcount,Rcount,Ncount
;endif

          IF Rcount GT 0 AND Ncount GT 0 THEN BEGIN
            hind = WHERE(STRMID(target,0,8) EQ STRMID(mobj[mlist[ii]],0,8) AND $
                         hrun EQ mrun[mind[jj]] AND $
                         STRTRIM(hfile,2) EQ Rfile[mind[jj]],hcount)

            obj[zz] = mobj[mlist[ii]]
            gal[zz] = kk+1
            run[zz] = mrun[mind[jj]]
            phot[zz] = (STRTRIM(photmtrc[Rind[0]],2) EQ 'Y' AND STRTRIM(photmtrc[Nind[0]],2) EQ 'Y')

            mr[zz] = magzpt1[hind[0]] - 2.5*ALOG10(flux_t_iso[Rind[0]]/flux_scale[Rind[0]])
            lfr[zz] = ALOG10(flux_t_iso[Rind[0]]) > (-999)
;;print,mr[zz],lfr[zz],mr[zz]+2.5*lfr[zz]
            lfha[zz] = ALOG10(flux_t_iso[Nind[0]]) > (-999)
            dfr[zz] = err_flux_t_sky_iso[Rind[0]]
            dfha[zz] = SQRT(err_flux_t_sky_iso[Nind[0]]^2 + err_flux_t_cont_iso[Nind[0]]^2)
            lrmax[zz] = ALOG10(rmax[Rind[0]])
            posang[zz] = pa[Rind[0]]
            ab[zz] = axerat[Rind[0]]
;if strmid(mobj[mlist[ii]],0,8) eq 'J0406-21' then begin
; print,obj[zz],gal[zz],run[zz],lfr[zz],lrmax[zz],lfha[zz]
;endif
            zz = zz + 1
          ENDIF
        ENDFOR

        IF zz GT zold THEN BEGIN
          delmr[zold:zz-1] = mr[zold:zz-1] - mr[zref]
          dellfr[zold:zz-1] = lfr[zold:zz-1] - lfr[zref]
          dellfha[zold:zz-1] = lfha[zold:zz-1] - lfha[zref]
          dfr2[zold:zz-1] = (SQRT(dfr[zold:zz-1]^2 + dfr[zref]^2)/(10.0^lfr[zref]))/al10
          dfha2[zold:zz-1] = (SQRT(dfha[zold:zz-1]^2 + dfha[zref]^2)/(10.0^lfha[zref]))/al10
          dellrmax[zold:zz-1] = lrmax[zold:zz-1] - lrmax[zref]
          ptemp = posang[zold:zz-1] - posang[zref]
          delposang[zold:zz-1] = ((ptemp+270.0) MOD 180.0) - 90.0
          delab[zold:zz-1] = ab[zold:zz-1] - ab[zref]
        ENDIF
       
       ENDFOR
      ENDIF
    ENDIF
  ENDFOR

  IF KEYWORD_SET(all) THEN BEGIN
    good = INDGEN(zz)
    pcount = zz
  ENDIF ELSE BEGIN
    good = WHERE(phot NE 0b,pcount)
  ENDELSE

;  forprint,obj,run,phot,lfha

  dmr = dfr2*2.5

; clip any entries with signal-to-noise ratios less than 2.
  IF KEYWORD_SET(all) THEN BEGIN
    rgood = WHERE((dfr2*al10) LT 0.5 AND lfr GT -900,rcount)
  ENDIF ELSE BEGIN
    rgood = WHERE((dfr2*al10) LT 0.5 AND phot NE 0b AND lfr GT -900,rcount)
  ENDELSE

  IF NOT psflag THEN BEGIN
    mult_plot_one,lfr[rgood],dellfr[rgood],[-16.0,-12.0],[-0.4,0.4],!dblue, $
                  'log(F!DR!N [erg cm!E-2!N s!E-1!N A!E-1!N])','!7D!6log(F!DR!N)',psflag,ERR=dfr2[rgood]
  ENDIF

  mult_plot_one,mr[rgood],delmr[rgood],[19.0,9.0],[-1.0,1.0],!dblue, $
                'm!DR!N [ABmag]','!7D!6m!DR!N [ABmag]',psflag,ERR=dmr[rgood],LABEL='(a)'

; clip any entries with signal-to-noise ratios less than 2.
  IF KEYWORD_SET(all) THEN BEGIN
    ngood = WHERE((dfha2*al10) LT 0.5 AND lfha GT -900,ncount)
  ENDIF ELSE BEGIN
    ngood = WHERE((dfha2*al10) LT 0.5 AND phot NE 0b AND lfha GT -900,ncount)
  ENDELSE

print,pcount,rcount,ncount

  mult_plot_one,lfha[ngood],dellfha[ngood],[-14.0,-10.0],[-0.4,0.4],!purple, $
                'log(F!DH!7a!6!N [erg cm!E-2!N s!E-1!N])','!7D!6log(F!DH!7a!6!N)',psflag,ERR=dfha2[ngood],LABEL='(b)'

  IF psflag THEN BEGIN
    psend,!outdir+'mult_flux.eps',/noprint,/clobber
spawn,'display '+!outdir+'mult_flux.eps &'
    set_plot,'PS'
    charsz = 1.5
    xs = 10.5
    ys = 8.5
    xoff = 1.2
    yoff = 3.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
           bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,2,2,0,0]
  ENDIF

  mult_plot_one,lrmax[good],dellrmax[good],[0.5,3.0],[-0.5,0.5],!green, $
                'log(r!Dmax!N [arcsec])','!7D!6log(r!Dmax!N)',psflag,LABEL='(a)'

  mult_plot_one,mr[rgood],dellrmax[rgood],[19.0,9.0],[-0.5,0.5],!blue, $
                'm!DR!N [ABmag]','!7D!6log(r!Dmax!N)',psflag,LABEL='(b)'

  IF NOT psflag THEN BEGIN
    mult_plot_one,posang[good],delposang[good],[0,180],[-90,90],!red, $
                  'position angle [deg]','!7D!6pa [deg]',psflag
  ENDIF

  mult_plot_one,ALOG10(ab[good]),delposang[good],[0,0.6],[-90,90],!orange, $
                'log(a/b)','!7D!6pa [deg]',psflag,LOW=0.1,LABEL='(c)'

  mult_plot_one,ALOG10(ab[good]),(delab[good]/ab[good])/al10,[0,0.6],[-0.2,0.2],!orange, $
                'log(a/b)','!7D!6log(a/b)',psflag,LABEL='(d)'

  IF psflag THEN BEGIN
    psend,!outdir+'mult_other.eps',/noprint,/clobber
spawn,'display '+!outdir+'mult_other.eps &'
;    set_plot,'PS'
;    xs = 10.5
;    ys = 4.5
;    xoff = 1.2
;    yoff = 3.0
;    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
;           bits_per_pixel=8,/encapsulated
;    !P.MULTI=[0,4,1,0,0]
  ENDIF ELSE BEGIN
    mult_plot_one,mr[rgood],delposang[rgood],[19.0,9.0],[-90,90],!dblue, $
                  'm!DR!N [ABmag]','!7D!6pa[deg]',psflag

    mult_plot_one,mr[rgood],(delab[rgood]/ab[rgood])/al10,[19.0,9.0],[-0.3,0.3],!blue, $
                  'm!DR!N [ABmag]','!7D!6log(a/b)',psflag
  ENDELSE

  RETURN

END
