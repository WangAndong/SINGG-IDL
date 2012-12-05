PRO sbplot,Rfile,Nfile,Sfile,DIR=dir,PS=ps,NODEL=nodel,FORCE=force,DEBUG=debug,RUNID=runid,OBJ=obj,NOFILE=nofile

  delflag = 1b-KEYWORD_SET(nodel)
  forceflag = KEYWORD_SET(force) OR KEYWORD_SET(debug)
  debugflag = KEYWORD_SET(debug)
  fileflag = 1b-KEYWORD_SET(nofile)

; Input files must already be sky-subtracted (*_ss.fits).

  IF KEYWORD_SET(runid) AND KEYWORD_SET(obj) THEN BEGIN
    dir = '/data1/acs22/hanish/'+STRTRIM(runid,2)+'/Proc4/'+STRTRIM(obj,2)+'/'
    dlen = STRLEN(dir)
    spawn,'ls '+dir+'J*_?_ss.fits',Rlist
    spawn,'ls '+dir+'J*_6???_ss.fits',Nlist
    spawn,'ls '+dir+'J*_?sub_ss.fits',Slist
    IF N_ELEMENTS(Rlist) NE 1 OR N_ELEMENTS(Nlist) NE 1 OR N_ELEMENTS(Slist) NE 1 THEN BEGIN
      PRINT,'ERROR in sbplot: cannot find unique image ',N_ELEMENTS(Rlist),N_ELEMENTS(Nlist),N_ELEMENTS(Slist)
      RETURN
    ENDIF
    Rfile = STRMID(Rlist[0],dlen,STRLEN(Rlist[0])-dlen)
    Nfile = STRMID(Nlist[0],dlen,STRLEN(Nlist[0])-dlen)
    Sfile = STRMID(Slist[0],dlen,STRLEN(Slist[0])-dlen)
    IF NOT FILE_TEST(dir+Rfile) OR NOT FILE_TEST(dir+Nfile) OR NOT FILE_TEST(dir+Sfile) THEN BEGIN
      PRINT,'ERROR in sbplot: cannot find images ',Rfile,' ',Nfile,' ',Sfile
      RETURN
    ENDIF
    forceflag = 1b
  ENDIF

  psflag = KEYWORD_SET(ps)
  dummy = DBLARR(2)
  al10 = ALOG(10.0)
  bxw = 35

  file = [Rfile,Nfile,Sfile]

  dbopen,'proc3_header',0
  dbext,-1,'FILENAME,RUNID,MAGZPT1,SKYSIGBX,TARGET',filename,hrun,magzpt1,skysigbx,htarg
  dbclose

; dir should be Run*/Proc4/Jwhatever
  runname = ''
  IF KEYWORD_SET(dir) THEN BEGIN
    dir = STRTRIM(dir,2)
    runpos = STRPOS(dir,'Run')
    IF runpos GE 0 THEN BEGIN
      short = STRMID(dir,runpos,9)
      slashpos = STRPOS(short,'/')
      IF slashpos LT 0 THEN runname = STRMID(short,3,2) $
                       ELSE runname = STRMID(short,3,slashpos-3)
    ENDIF
  ENDIF ELSE BEGIN
    dir = './'
    runname = ''
  ENDELSE
; If it doesn't end in a slash, add one.
  IF STRMID(dir,0,1,/reverse_offset) NE '/' THEN dir = dir+'/'

  spawn,'ls '+dir+'/*ellipse.dat',elllist
  IF FILE_TEST(elllist[0]) THEN BEGIN
    read_ellipse_file,elllist[0],n_ellipses,refimage,Dx,Dy,Px,Py,pa, $
                      a_i,b_i,z_s,z_f,z_c
; We COULD use the flux radius, but what we care about is the
; inaccuracy of the sky model, so use z_s.
    a = a_i * z_s
    b = b_i * z_s
    theta = (pa-90.0)*!dtor
    ratio = a/b
  ENDIF ELSE BEGIN
    PRINT,'ERROR in sbplot: no ellipse file ',Rfile
    RETURN
  ENDELSE

  isofile = STRARR(3)
  brtfile = STRARR(3)

  isofile[0] = dir+STRMID(STRTRIM(Rfile,2),0,STRLEN(Rfile)-5)+ $
             "_isophote.profile"
  isofile[1] = dir+STRMID(STRTRIM(Nfile,2),0,STRLEN(Nfile)-5)+ $
             "_isophote.profile"
  isofile[2] = dir+STRMID(STRTRIM(Sfile,2),0,STRLEN(Sfile)-5)+ $
             "_isophote.profile"
  brtfile[0] = dir+STRMID(STRTRIM(Rfile,2),0,STRLEN(Rfile)-5)+ $
             "_brightness.profile"
  brtfile[1] = dir+STRMID(STRTRIM(Nfile,2),0,STRLEN(Nfile)-5)+ $
             "_brightness.profile"
  brtfile[2] = dir+STRMID(STRTRIM(Sfile,2),0,STRLEN(Sfile)-5)+ $
             "_brightness.profile"

  IF NOT FILE_TEST(isofile[0]) OR NOT FILE_TEST(isofile[1]) OR NOT FILE_TEST(isofile[2]) OR $
     NOT FILE_TEST(brtfile[0]) OR NOT FILE_TEST(brtfile[1]) OR NOT FILE_TEST(brtfile[2]) THEN BEGIN
    PRINT,'Cannot find files ',dir
    RETURN
  ENDIF
 
  read_profile_header,isofile[0],teststr,/silent
  numgals = teststr[0].numgals
;  keyword = TAG_NAMES(teststr[0])

  readcol_new,isofile[0],keyword,keyval,FORMAT="X,A,X,A",SKIPLINE=1,/SILENT
  lstart = (WHERE(keyword EQ "all" AND keyval EQ "and"))+INDGEN(numgals)+5
  lend = INTARR(numgals)
  endcheck = WHERE(keyword EQ "GALINDEX")
  FOR jj = 0,numgals-2 DO BEGIN
    lend[jj] = endcheck[jj+1]+jj+3
  ENDFOR
  lend[numgals-1] = N_ELEMENTS(keyword)+numgals+1

  magzpt = FLTARR(3)
  skysig = FLTARR(3)
  delsky = FLTARR(3)
  delsub = STRARR(3)
  FOR ii = 0,2 DO BEGIN
    IF STRLEN(runname) GT 0 THEN BEGIN 
      ind = WHERE(STRTRIM(filename,2) EQ STRTRIM(file[ii],2) AND $
                  STRTRIM(hrun,2) EQ STRTRIM(runname,2),count)
    ENDIF ELSE BEGIN
      ind = WHERE(STRTRIM(filename,2) EQ STRTRIM(file[ii],2),count)
    ENDELSE
    IF count EQ 0 THEN BEGIN
      PRINT,'ERROR in sbplot: cannot match file ',file[ii],count
      RETURN
    ENDIF
; For now, we'll just use the first match.
    magzpt[ii] = magzpt1[ind[0]]
    skysig[ii] = skysigbx[ind[0]]
    target = htarg[ind[0]]

    IF delflag THEN BEGIN
      fits_read,dir+file[ii],img,hdr
      getrot,hdr,rot,cdelt 
      cdelt  = abs(cdelt)*3600.
      as_pix = cdelt[0]
      delsky[ii] = SXPAR(hdr,'DELSKY',count=count)
      delsub[ii] = STRTRIM(SXPAR(hdr,'DELSUB',count=count2),2)
      IF delsub[ii] EQ 'Y' THEN delsky[ii] = 0.0

      IF count NE 1 OR count2 NE 1 OR delsub[ii] NE 'Y' OR forceflag THEN BEGIN
        IF debugflag THEN PRINT,'    Processing '+dir+file[ii]

        sz = SIZE(img)
        buffer = SXPAR(hdr,'BUFFER')
        skysigpx = SXPAR(hdr,'SKYSIG')
        sspos = STRPOS(file[ii],'_ss')
        linepos = STRPOS(STRMID(file[ii],0,sspos),'_',/REVERSE_SEARCH)
        IF ii EQ 2 THEN BEGIN
          maskfile = dir+STRMID(file[ii],0,sspos)+'_mask.fits'
        ENDIF ELSE BEGIN
          maskfile = dir+STRMID(file[ii],0,linepos)+'_mask.fits'
          IF NOT FILE_TEST(maskfile) THEN BEGIN
            spawn,'ls '+dir+STRMID(file[ii],0,linepos)+'_?_mask.fits',masklist
            maskfile = masklist[0]
          ENDIF
        ENDELSE
        fits_read,maskfile,mimg,junk,/data_only

        procpos = STRPOS(dir,'Proc4')
        p3dir = STRMID(dir,0,procpos)+'Proc3/'+STRMID(dir,procpos+6,STRLEN(dir)-procpos-6)
        plfile = p3dir+STRMID(file[ii],0,STRLEN(file[ii])-8)+'.pl.fits.gz'
        plimg = readfits(plfile,/SILENT)
;maxpl = MAX(plimg) < 3
        IF ii EQ 2 THEN maxpl = 2 ELSE maxpl = (MAX(plimg) < 3)
        edgemask = BYTARR(sz[1],sz[2])+1b
        edge = 100
        edgemask[buffer+edge:sz[1]-buffer-1-edge, $
                 buffer+edge:sz[2]-buffer-1-edge] = 0b
;;good = (mimg LT 0.5) AND (plimg GE maxpl)
good = (1b-calc_outer_mask((plimg LT maxpl),(-1*edge))) AND (mimg LT 0.5)
;good = (mimg LT 0.5) AND (plimg GE maxpl) AND (edgemask EQ 0b)

        FOR jj = 0,n_ellipses-1 DO BEGIN
          diamaj = a[jj] * as_pix
          diamin = b[jj] * as_pix
          dist_ellipse,adist,[sz[1],sz[2]],Dx[jj]+buffer,Dy[jj]+buffer,(a[jj]/b[jj]),pa[jj],/DOUBLE
          zdist = adist / a[jj]
          good = good AND (zdist GE 2.0 OR (adist/SQRT(ratio[jj])) GE 900.0)
        ENDFOR

        skyind = WHERE(good,skycount)
        skyres = box_sky(img,(1b-good),[0.0,skysigpx],bxw,boxdata,REJFRAC=0.1)
        delsky[ii] = MEAN(img[skyind]) - skyres[0]
        sxaddpar,hdr,'DELSKY',delsky[ii],' Sky small-scale structure'

        fits_write,dir+file[ii],img,hdr
      ENDIF
    ENDIF
  ENDFOR

  IF NOT psflag THEN BEGIN
    set_plot,'X'
    setplotcolors
    setbgfg,!white,!black
    DEVICE, RETAIN=2, DECOMPOSED=0
    WINDOW,XSIZE=800,YSIZE=1100
    !P.MULTI=[0,0,3,0,0]

    charsz = 3.0
    symsz = 1.2
    thick = 1.0
  ENDIF ELSE BEGIN
; We'll write .eps files
    set_plot,'PS'
    setplotcolors
    xs = 5.5
    ys = 8.0
    xoff = 1.2
    yoff = 3.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
           bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,0,3,0,0]

    charsz = 1.5
    symsz = 0.8
    thick = 2.0
  ENDELSE

  ytstr = ['!7l!3!Dcontinuum!N [ABmag arcsec!E-2!N]', $
           '!7l!3!Dnarrowband!N [ABmag arcsec!E-2!N]', $
           'log(!7R!3!DH!7a!N!3 [erg cm!E-2!N sec!E-1!N arcsec!E-2!N])']

  IF fileflag THEN BEGIN
; Now, create a SB profile file.
    sbfile = dir+STRMID(STRTRIM(Sfile,2),0,STRLEN(Sfile)-8)+'_SB.profile'
    OPENW,unit,sbfile,/GET_LUN
    PRINTF,unit,'# RUNID = '+runname
    PRINTF,unit,'# NUMGALS = '+STRTRIM(STRING(numgals),2)
  ENDIF

  FOR jj = 0,numgals-1 DO BEGIN
    skymult = (0.233 + (21.6/(as_pix*SQRT(a[jj]*b[jj])))^3.0 ) < 2.0
    IF fileflag THEN BEGIN
      PRINTF,unit,'#'
      PRINTF,unit,'# GALINDEX = '+STRTRIM(STRING(jj+1),2)
      PRINTF,unit,'#    a   SB(R)     sigSB(R)  SB(NB)    sigSB(NB) SB(Ha)  sigSBsky(Ha) sigSBcnt(Ha)'
      max_ent = 1000
      sma = FLTARR(max_ent)
      sbR = FLTARR(max_ent)
      sigR = FLTARR(max_ent)
      sbN = FLTARR(max_ent)
      sigN = FLTARR(max_ent)
      sbS = FLTARR(max_ent)
      sigScnt = FLTARR(max_ent)
      sigSsky = FLTARR(max_ent)
    ENDIF
    FOR ii = 0,2 DO BEGIN
      read_profile_header,isofile[ii],Istr,/silent
      read_profile_header,brtfile[ii],Bstr,/silent
      netflag = (STRPOS(isofile[ii],"sub") GT 0)
;      netflag = (ii EQ 2)

      Iferrclip = 0.0
      pfplt_extractprof,isofile[ii],netflag,Isma,Ifint,Idfint,Ingood,Inbad, $
                        Isb,Iesb,Idfraw,Iefintsk,Iefintcn, $
                        pixsize=Istr[jj].pixsize,fscale=Istr[jj].flux_scale, $
                        ferrclip=Iferrclip,lstart=lstart[jj],lend=lend[jj]

      Bferrclip = 0.0
      pfplt_extractprof,brtfile[ii],netflag,Bsma,Bfint,Bdfint,Bngood,Bnbad, $
                        Bsb,Besb,Bdfraw,Befintsk,Befintcn, $
                        pixsize=Bstr[jj].pixsize,fscale=Bstr[jj].flux_scale, $
                        ferrclip=Bferrclip,lstart=lstart[jj],lend=lend[jj]
      n_Idists = N_ELEMENTS(Isma)
      n_Bdists = N_ELEMENTS(Bsma)

      Iskyerr = FLTARR(n_Idists) + Iefintsk[0]/Ingood[0]/skymult
      Bskyerr = FLTARR(n_Bdists) + Befintsk[0]/Bngood[0]/skymult
      Icnterr = FLTARR(n_Idists) + Iefintcn[0]/Ingood[0]
      Bcnterr = FLTARR(n_Bdists) + Befintcn[0]/Bngood[0]
      Iskyerr[1:n_Idists-1] = (Iefintsk[1:n_Idists-1] - Iefintsk[0:n_Idists-2])/Ingood[1:n_Idists-1]/skymult
      Bskyerr[1:n_Bdists-1] = (Befintsk[1:n_Bdists-1] - Befintsk[0:n_Bdists-2])/Bngood[1:n_Bdists-1]/skymult

      Icnterr[1:n_Idists-1] = (Iefintcn[1:n_Idists-1] - Iefintcn[0:n_Idists-2])/Ingood[1:n_Idists-1]
      Bcnterr[1:n_Bdists-1] = (Befintcn[1:n_Bdists-1] - Befintcn[0:n_Bdists-2])/Bngood[1:n_Bdists-1]

      Ierr = SQRT(Iskyerr^2 + Icnterr^2) / (Istr[jj].pixsize^2.0)
      Berr = SQRT(Bskyerr^2 + Bcnterr^2) / (Bstr[jj].pixsize^2.0)

      IF debugflag THEN xmax = 1.05*MAX([Isma,Bsma]) $
                   ELSE xmax = 1.05 *(a_i[jj]*z_f[jj])*as_pix
      xmin = 0.0

      Ir90 = SPLINE(Ifint,Isma,(Istr[jj].flux_t * 0.9))
      Br90 = SPLINE(Bfint,Bsma,(Bstr[jj].flux_t * 0.9))

      IF delflag AND NOT debugflag THEN BEGIN
        Isb = Isb - (delsky[ii]*Istr[jj].flux_scale/(Istr[jj].pixsize^2.0))
        Bsb = Bsb - (delsky[ii]*Bstr[jj].flux_scale/(Bstr[jj].pixsize^2.0))
      ENDIF

      Igood = WHERE(Isb GT 0.0 AND Isma LT xmax,Icount) ; was 1E-21
      Bgood = WHERE(Bsb GT 0.0 AND Bsma LT xmax,Bcount) ; was 1E-21
      Ibad = WHERE(Isb LE 0.0 AND Isma LT xmax AND Ingood GT 0,Ibcount)
      Bbad = WHERE(Bsb LE 0.0 AND Bsma LT xmax AND Bngood GT 0,Bbcount)

;      title = file[ii]+' source #'+STRTRIM(STRING(jj+1),2)
      title = '!3'+STRTRIM(target,2)
      IF numgals GT 1 THEN title = title+':S'+STRTRIM(STRING(jj+1),2)
      ytitle = ytstr[ii]
      IF netflag THEN BEGIN
;        ytitle = 'Surface brightness [erg cm!E-2!N sec!E-1!N arcsec!E-2!N]'
        Iyval = ALOG10(Isb)
        Byval = ALOG10(Bsb)
        Iyerr = Ierr/Isb / al10
        Byerr = Berr/Bsb / al10
        skyval = ALOG10(Istr[jj].skylev / (Istr[jj].pixsize^2.0))
        skyerr = ALOG10(skysig[ii]*Istr[jj].flux_scale / (Istr[jj].pixsize^2.0))
        IF delflag THEN BEGIN
          dsky = ALOG10(delsky[ii]*Istr[jj].flux_scale / (Istr[jj].pixsize^2.0))
          ymax = MAX([Iyval,Byval,skyval,dsky])+0.5 > (-14.0)
          ymin = MIN([Iyval,Byval,skyerr,dsky])-0.5 > (-21.0)
        ENDIF ELSE BEGIN
          ymax = MAX([Iyval,Byval,skyval])+0.5 > (-14.0)
          ymin = MIN([Iyval,Byval,skyerr])-0.5 > (-21.0)
        ENDELSE

        PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],COLOR=!black, $
             CHARSIZE=charsz,TITLE=title,XSTYLE=1,YSTYLE=1, $
             XTITLE='Semi-major axis [arcsec]',YTITLE=ytitle
        OPLOT,[xmin,xmax],[0.0,0.0],COLOR=!black
      ENDIF ELSE BEGIN
;        ytitle = 'Surface brightness [ABmag arcsec!E-2!N]'
        Iyval = magzpt[ii] - 2.5*ALOG10(Isb/Istr[jj].flux_scale) ;;+ 5.0*ALOG10(Istr[jj].pixsize)
        Byval = magzpt[ii] - 2.5*ALOG10(Bsb/Bstr[jj].flux_scale) ;;+ 5.0*ALOG10(Bstr[jj].pixsize)
        Iyerr = 2.5 * Ierr/Isb / al10
        Byerr = 2.5 * Berr/Bsb / al10
        Iyerr2 = 2.5 * Iskyerr/Isb / al10
        Byerr2 = 2.5 * Bskyerr/Bsb / al10
        skyval = magzpt[ii] - 2.5*ALOG10(Istr[jj].skylev/Istr[jj].flux_scale) ;; / (Istr[jj].pixsize^2.0))
        skyerr = magzpt[ii] - 2.5*ALOG10(skysig[ii] / (Istr[jj].pixsize^2.0))
        IF delflag THEN BEGIN
          dsky = magzpt[ii] - 2.5*ALOG10(delsky[ii]) + 5.0*ALOG10(Istr[jj].pixsize)
          ymin = MAX([skyval,skyerr,dsky])+1.0
          ymax = MIN([skyval,skyerr,dsky])-1.0
        ENDIF ELSE BEGIN
          ymin = MAX([skyval,skyerr])+1.0
          ymax = MIN([skyval,skyerr])-1.0
        ENDELSE

        IF Icount GT 0 THEN BEGIN
          ymin = MAX([(ymin-1.0),Iyval[Igood]])+1.0
          ymax = MIN([(ymax+1.0),Iyval[Igood]])-1.0
        ENDIF
        IF Bcount GT 0 THEN BEGIN
          ymin = MAX([(ymin-1.0),Byval[Bgood]])+1.0
          ymax = MIN([(ymax+1.0),Byval[Bgood]])-1.0
        ENDIF

        PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],COLOR=!black, $
             CHARSIZE=charsz,TITLE=title,XSTYLE=1,YSTYLE=1, $
             XTITLE='Semi-major axis [arcsec]',YTITLE=ytitle
        ERRPLOT,Isma,(Iyval-Iyerr2),(Iyval+Iyerr2),COLOR=!dred
        ERRPLOT,Bsma,(Byval-Byerr2),(Byval+Byerr2),COLOR=!dblue
      ENDELSE

      IF Icount GT 0 THEN BEGIN
        ERRPLOT,Isma[Igood],(Iyval[Igood]-Iyerr[Igood]),(Iyval[Igood]+Iyerr[Igood]),COLOR=!dred
        OPLOT,Isma[Igood],Iyval[Igood],COLOR=!red,PSYM=SYM(1),SYMSIZE=symsz
      ENDIF
      IF Bcount GT 0 THEN BEGIN
        ERRPLOT,Bsma[Bgood],(Byval[Bgood]-Byerr[Bgood]),(Byval[Bgood]+Byerr[Bgood]),COLOR=!dblue
        OPLOT,Bsma[Bgood],Byval[Bgood],COLOR=!blue,PSYM=SYM(1),SYMSIZE=symsz
      ENDIF

      IF Ibcount GT 0 THEN OPLOT,Isma[Ibad],skyerr+FLTARR(Ibcount),COLOR=!red,PSYM=SYM(18),SYMSIZE=symsz
      IF Bbcount GT 0 THEN OPLOT,Bsma[Bbad],skyerr+FLTARR(Bbcount),COLOR=!blue,PSYM=SYM(18),SYMSIZE=symsz

      OPLOT,Istr[jj].fluxrad_f*[1.0,1.0],[ymin,ymax],COLOR=!dred,THICK=2.0
      OPLOT,Istr[jj].re_t*[1.0,1.0],[ymin,ymax],COLOR=!dred,LINESTYLE=1
      OPLOT,Ir90*[1.0,1.0],[ymin,ymax],COLOR=!dred,LINESTYLE=2

      OPLOT,Bstr[jj].fluxrad_f*[1.0,1.0],[ymin,ymax],COLOR=!dblue,THICK=2.0
      OPLOT,Bstr[jj].re_t*[1.0,1.0],[ymin,ymax],COLOR=!dblue,LINESTYLE=1
      OPLOT,Br90*[1.0,1.0],[ymin,ymax],COLOR=!dblue,LINESTYLE=2

      IF NOT netflag THEN OPLOT,[xmin,xmax],skyval*[1.0,1.0],COLOR=!black,LINESTYLE=2
      OPLOT,[xmin,xmax],skyerr*[1.0,1.0],COLOR=!black,LINESTYLE=1
      IF delflag AND debugflag THEN OPLOT,[xmin,xmax],dsky*[1.0,1.0],COLOR=!red,LINESTYLE=2

      IF fileflag THEN BEGIN
        CASE ii OF
        0: BEGIN
            sma[0:n_Idists-1] = Isma[0:n_Idists-1]
            sbR[0:n_Idists-1] = Isb[0:n_Idists-1]
            sigR[0:n_Idists-1] = Iskyerr[0:n_Idists-1]
          END
        1: BEGIN
            sbN[0:n_Idists-1] = Isb[0:n_Idists-1]
            sigN[0:n_Idists-1] = Iskyerr[0:n_Idists-1]
          END
        2: BEGIN
            sbS[0:n_Idists-1] = Isb[0:n_Idists-1]
            sigSsky[0:n_Idists-1] = Iskyerr[0:n_Idists-1]
            sigScnt[0:n_Idists-1] = Icnterr[0:n_Idists-1]
          END
        ELSE: BEGIN
            PRINT,'ERROR in sbplot: invalid array index ',ii
            RETURN
          END
        ENDCASE
      ENDIF
    ENDFOR

    pname = STRMID(STRTRIM(Sfile,2),0,STRLEN(Sfile)-8)+'_prof'+STRTRIM(STRING(jj+1),2)
    IF psflag THEN BEGIN
      psend,dir+pname+'.eps',/noprint,/clobber
      DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
             bits_per_pixel=8,/encapsulated
      !P.MULTI=[0,0,3,0,0]
    ENDIF ELSE BEGIN
      im = tvrd(true=3)
      write_jpeg,dir+pname+'.jpg',im,true=3,quality=100
    ENDELSE

    IF fileflag THEN BEGIN
      ind = WHERE(sma GT 0.0001,count)
      FOR kk = 0,count-1 DO BEGIN
        PRINTF,unit,sma[ind[kk]],sbR[ind[kk]],sigR[ind[kk]],sbN[ind[kk]],sigN[ind[kk]],sbS[ind[kk]],sigSsky[ind[kk]],sigScnt[ind[kk]],FORMAT='(F9.3,7E10.3)'
      ENDFOR
    ENDIF

    IF debugflag AND numgals GT 1 THEN BEGIN
      PRINT,'Hit any key for next galaxy'
      key = GET_KBRD(1)
    ENDIF

  ENDFOR

  IF fileflag THEN BEGIN
    CLOSE,unit
    FREE_LUN,unit
;;spawn,'e '+sbfile
  ENDIF

  RETURN

END
