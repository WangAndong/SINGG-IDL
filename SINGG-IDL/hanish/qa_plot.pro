PRO qa_plot,Rfile,Nfile,Sfile,Tfile,Rmaskfile,Smaskfile,Imaskfile,Omask, $
            Dx,Dy,pa,a,b,PLOTSIG=plotsig,INDIR=indir,OUTDIR=outdir, $
            SKY_RAD=sky_rad,BXW=bxw
; Creates the five QA plot files.
; INPUTS
;   Rfile,Nfile,Sfile      The three .fits inputs
;   Tfile                  The check.fits image from SExtractor
;   Rmaskfile              R/Narrow-band exclusion mask file
;   Smaskfile              Net exclusion mask file
;   Imaskfile              Inclusion mask file
;   Omask[x,y]             Override mask
;   Dx,Dy,pa,a,b [ell]     The ellipse variables
; OPTIONAL INPUT
;   plotsig[2]             The minimum and maximum number of sigmas to plot.
;   indir                  Input directory
;   outdir                 Output directory

  IF NOT KEYWORD_SET(plotsig) THEN plotsig = [-4.0,20.0]
  IF NOT KEYWORD_SET(indir) THEN indir = './'
  IF NOT KEYWORD_SET(outdir) THEN outdir = './'
  IF NOT KEYWORD_SET(bxw) THEN bxw = 2
  IF bxw EQ 2 THEN suff = '.jpg' ELSE suff = '_'+STRTRIM(STRING(bxw),2)+'.jpg'
  short = STRMID(STRTRIM(Sfile,2),0,STRLEN(Sfile)-8)

  Oflag = MAX(Omask) ; If ANY pixel is 1b in Omask, use it.

  fits_read,indir+Rfile,Rimg,Rhd
  fits_read,indir+Nfile,Nimg,Nhd
  fits_read,indir+Sfile,Simg,Shd

  Rmask = readfits(outdir+Rmaskfile,/SILENT)
  Smask = readfits(outdir+Smaskfile,/SILENT)
  Imask = readfits(outdir+Imaskfile,/SILENT)

  buffer = SXPAR(Shd,"BUFFER")
  cntrat = SXPAR(Shd,"CNTRAT1")
  sz = SIZE(Rimg)

  fits_read,outdir+Tfile,Timg,Thd
  edgemask = make_array(sz[1],sz[2],/byte,value=1b)
  edgemask[buffer:(sz[1]-buffer-1),buffer:(sz[2]-buffer-1)] = 0b
  skymask = edgemask OR (Timg GT 0)

  mysky,Rimg,Rsky2,Rskysig,mask=skymask,/silent
  mysky,Nimg,Nsky2,Nskysig,mask=skymask,/silent
  mysky,Simg,Ssky2,Sskysig,mask=skymask,/silent

  getrot,Shd,rot,cdelt 
  cdelt  = abs(cdelt)*3600.
  as_pix = cdelt[0]

  Dx = Dx + buffer
  Dy = Dy + buffer

; First, the unmasked version.  We use bxw=2 for our normal QA plots.
  singg_tvplot,Rimg,Nimg,Simg,[Rsky2,Rskysig],[Nsky2,Nskysig],[Ssky2,Sskysig],$
               bxw,cntrat,OUTPUT=tvdisp,PLOTSIG=plotsig,/LINEAR
  im = tvrd(true=3)

  IF KEYWORD_SET(sky_rad) THEN BEGIN
    jpgfile = STRTRIM(short,2)+"_3clr_clean"+suff
    write_jpeg,outdir+jpgfile,im,true=3,quality=100
    ellcolor = [!purple,!green,!magenta,!dorange,!dyellow,!red,!dcyan,!blue]
    a_s = sky_rad
    b_s = (a_s * b/a)
; Outer ring is sky aperture
    plot_ellipse,Dx,Dy,(pa-90.0)*!dtor,a_s,b_s,ellcolor[0:N_ELEMENTS(Dx)-1], $
                 barclr=!gray,symscale=0.6,normal=[sz[1],sz[2]]
; Inner ring is flux aperture
    plot_ellipse,Dx,Dy,(pa-90.0)*!dtor,a,b,ellcolor[0:N_ELEMENTS(Dx)-1], $
                 barclr=!gray,symscale=1.0,normal=[sz[1],sz[2]]
    im = tvrd(true=3)
  ENDIF
  jpgfile = STRTRIM(short,2)+"_3clr"+suff
  write_jpeg,outdir+jpgfile,im,true=3,quality=100

  IF bxw NE 2 THEN RETURN ;; Abort the later steps if we're playing around with the image sizes.

; Now that we have tvdisp, assemble the plot with the masks
  sz2 = SIZE(tvdisp)
  tv_R_good = FLTARR(sz2[1],sz2[2],sz2[3])
  tv_R_bad = FLTARR(sz2[1],sz2[2],sz2[3])
  tv_Ha_good = FLTARR(sz2[1],sz2[2],sz2[3])
  tv_Ha_bad = FLTARR(sz2[1],sz2[2],sz2[3])
  IF Oflag THEN BEGIN
    tv_Ha_O_good = FLTARR(sz2[1],sz2[2],sz2[3])
    tv_Ha_O_bad = FLTARR(sz2[1],sz2[2],sz2[3])
  ENDIF

  ellmask = BYTARR(sz[1],sz[2])
  FOR ii = 0,N_ELEMENTS(Dx)-1 DO BEGIN
    dist_ellipse,tempmask,[sz[1],sz[2]],Dx[ii],Dy[ii],(a[ii]/b[ii]),pa[ii]
    ellmask = ellmask OR (tempmask LT a[ii])
  ENDFOR

  Rgood = (ellmask AND (NOT Rmask)) OR ((NOT ellmask) AND Imask)
  outfile = STRMID(STRTRIM(Rfile,2),0,STRLEN(Rfile)-5)+'_good.fits'
  fits_write,outdir+outfile,Rgood,Rhd
  Sgood = (ellmask AND (NOT Smask)) OR ((NOT ellmask) AND Imask)
  outfile = STRMID(STRTRIM(Sfile,2),0,STRLEN(Sfile)-5)+'_good.fits'
  fits_write,outdir+outfile,Sgood,Shd

  FOR ii = 0,2 DO BEGIN
    tv_R_good[ii,*,*] = (tvdisp[ii,*,*]+100.0) * FLOAT(Rgood) - 100.0
    tv_R_bad[ii,*,*] = (tvdisp[ii,*,*]+100.0) * (1.0 - FLOAT(Rgood)) - 100.0
    tv_Ha_good[ii,*,*] = (tvdisp[ii,*,*]+100.0) * FLOAT(Sgood) - 100.0
    tv_Ha_bad[ii,*,*] = (tvdisp[ii,*,*]+100.0) * (1.0 - FLOAT(Sgood)) - 100.0
    IF Oflag THEN BEGIN
      tv_Ha_O_good[ii,*,*] = (tvdisp[ii,*,*]+100.0) * FLOAT(Omask) - 100.0
      tv_Ha_O_bad[ii,*,*] = (tvdisp[ii,*,*]+100.0) * (1.0 - FLOAT(Omask)) - 100.0
    ENDIF
  ENDFOR

  tvscale,tv_R_good,MAXVALUE=plotsig[1],MINVALUE=plotsig[0],BOTTOM=0,TOP=!ngrays-1
  jpgfile = STRTRIM(short,2)+"_good.jpg"
  im = tvrd(true=3)
  write_jpeg,outdir+jpgfile,im,true=3,quality=100

  tvscale,tv_R_bad,MAXVALUE=plotsig[1],MINVALUE=plotsig[0],BOTTOM=0,TOP=!ngrays-1
  jpgfile = STRTRIM(short,2)+"_bad.jpg"
  im = tvrd(true=3)
  write_jpeg,outdir+jpgfile,im,true=3,quality=100

  tvscale,tv_Ha_good,MAXVALUE=plotsig[1],MINVALUE=plotsig[0],BOTTOM=0,TOP=!ngrays-1
  jpgfile = STRTRIM(short,2)+"_net_good.jpg"
  im = tvrd(true=3)
  write_jpeg,outdir+jpgfile,im,true=3,quality=100

  tvscale,tv_Ha_bad,MAXVALUE=plotsig[1],MINVALUE=plotsig[0],BOTTOM=0,TOP=!ngrays-1
  jpgfile = STRTRIM(short,2)+"_net_bad.jpg"
  im = tvrd(true=3)
  write_jpeg,outdir+jpgfile,im,true=3,quality=100

  IF Oflag THEN BEGIN
    tvscale,tv_Ha_O_good,MAXVALUE=plotsig[1],MINVALUE=plotsig[0],BOTTOM=0,TOP=!ngrays-1
    jpgfile = STRTRIM(short,2)+"_net_O_good.jpg"
    im = tvrd(true=3)
    write_jpeg,outdir+jpgfile,im,true=3,quality=100

    tvscale,tv_Ha_O_bad,MAXVALUE=plotsig[1],MINVALUE=plotsig[0],BOTTOM=0,TOP=!ngrays-1
    jpgfile = STRTRIM(short,2)+"_net_O_bad.jpg"
    im = tvrd(true=3)
    write_jpeg,outdir+jpgfile,im,true=3,quality=100
  ENDIF

  RETURN
END
