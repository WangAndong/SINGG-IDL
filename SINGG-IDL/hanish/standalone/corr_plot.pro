PRO corr_plot

  datafile = !singgdir+"/sdss_gal_binned.dat"
  readcol_new,datafile,mcent,mmin,mmax,ngal,mmid,ebvg,ebvg_sig, $
       nii,nii_sig,FORMAT='F,F,F,I,F,F,F,F,F',COMMENT="#"

  aha = ebvg * 2.547
  aha_sig = ebvg_sig * 2.547

  vegamag = mcent - 0.21

  niicorr = 10.0^(-0.13*vegamag - 3.2)
  niicorrsig = 10.0^(0.035*vegamag + 0.90)*ALOG(10)*niicorr
  dustcorr = 10.0^(-0.12*vegamag - 2.5)
  dustcorrsig = 10.0^(0.048*vegamag + 0.96)*ALOG(10)*dustcorr

  setplotcolors

  dummy = FLTARR(2)
  DEVICE, RETAIN=2, DECOMPOSED=0
  WINDOW,XSIZE=800,YSIZE=1000
  !P.MULTI=[0,0,2,0,0]

  tremonti_clr = !red
  helmboldt_clr = !blue
  fg_clr = 1
  bg_clr = 100
  oldsym = SYM(1)
  errsym = -1
  xmin = -14
  xmax = -24

  PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[0.0,2.0],CHARSIZE=2.0, $
       COLOR=fg_clr,XSTYLE=1,YSTYLE=1,XTITLE="M!Ir'!N [ABmag]", $
       YTITLE="A(H!Dalpha!N) [mag]",CLIP=[xmin,0.0,xmax,2.0], $
       TITLE="SDSS Dust Extinction"
  PLOTS,[xmin,xmax],[0.5,0.5],COLOR=bg_clr
  OPLOT,mcent,aha,PSYM=oldsym,COLOR=tremonti_clr
  ERRPLOT,mcent,(aha-aha_sig),(aha+aha_sig), $
          COLOR=tremonti_clr
  OPLOT,mcent,dustcorr,PSYM=oldsym,COLOR=helmboldt_clr
  ERRPLOT,mcent,(dustcorr-dustcorrsig),(dustcorr+dustcorrsig), $
          COLOR=helmboldt_clr

  PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[0.0,0.6],CHARSIZE=2.0, $
       COLOR=fg_clr,XSTYLE=1,YSTYLE=1,XTITLE="M!Ir'!N [ABmag]", $
       YTITLE="F!D[N!II I!D]6583!N/F!DH!Ialpha!N",CLIP=[xmin,0.0,xmax,2.0], $
       TITLE="SDSS [N!III!N] Correction"
  PLOTS,[xmin,xmax],[0.35,0.35],COLOR=bg_clr
  OPLOT,mcent,nii,PSYM=oldsym,COLOR=tremonti_clr
  ERRPLOT,mcent,(nii-nii_sig),(nii+nii_sig), $
          COLOR=tremonti_clr
  OPLOT,mcent,niicorr,PSYM=oldsym,COLOR=helmboldt_clr
  ERRPLOT,mcent,(niicorr-niicorrsig),(niicorr+niicorrsig), $
          COLOR=helmboldt_clr

END
