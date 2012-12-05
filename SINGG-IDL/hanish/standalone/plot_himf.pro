PRO plot_himf,PS=ps

  readcol_new,!singgdir+"/hipass_himf.dat",oldbin,oldnum,oldtheta,oldplus,oldminus
  readcol_new,!singgdir+"/mould_himf.dat",newbin,newnum,newtheta,newplus,newminus

; Original BGC HIMF
  otheta0 = [8.6D-3,2.1D-3]  ; in Mpc^-3
  orefmass = [9.79D0,0.06] ; log10, in solar masses
  oalpha = [-1.3,0.08] ; unitless

; Revised HICAT HIMF
  xtheta0 = [6.0D-3,0.8D-3] ; in Mpc^-3
  xrefmass = [9.80D0,0.03] ; log10, in solar masses
  xalpha = [-1.37,0.03] ; unitless

; Mould model HIMF
  ntheta0 = [5.372D-3,0.8D-3] ; in Mpc^-3
  nrefmass = [9.805D0,0.03] ; log10, in solar masses
  nalpha = [-1.386,0.03] ; unitless

  dummy = FLTARR(2)
  IF KEYWORD_SET(ps) THEN BEGIN
    set_plot,'PS'
    setplotcolors
    xs = 6.5
    ys = 4.5
    xoff = 1.0
    yoff = 3.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
           bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,0,1,0,0]
    symsz = 0.8
  ENDIF ELSE BEGIN
    set_plot,'X'
    setplotcolors
    setbgfg,!white,!black
    DEVICE, RETAIN=2, DECOMPOSED=0
    WINDOW,XSIZE=1200,YSIZE=1000
    !P.MULTI=[0,0,1,0,0]
    symsz = 2.0
  ENDELSE

  xmin = 6.0
  xmax = 11.0
  ymin = -4.0
  ymax = -0.5
  PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax], $
       XTITLE="log(M!DHI!N / M!Dsolar!N)",PSYM=4,CHARSIZE=2.0,COLOR=!black, $
       YTITLE="log(!7h!6[Mpc!E-3!N])",LINESTYLE=1, $
       TITLE="H!DI!N Mass Function",CLIP=[xmin,ymin,xmax,ymax],NOCLIP=0
;;  OPLOT,oldbin,oldtheta,COLOR=!ddgray,PSYM=SYM(1)
;;  ERRPLOT,oldbin,(oldtheta-oldminus),(oldtheta+oldplus),COLOR=!ddgray
  OPLOT,newbin,newtheta,COLOR=!black,PSYM=SYM(1),SYMSIZE=symsz
  OPLOT,newbin,newtheta,COLOR=!gray,PSYM=SYM(1),SYMSIZE=symsz*0.6
  ERRPLOT,newbin,(newtheta-newminus),(newtheta+newplus),COLOR=!dblue

  Ax = [xtheta0[0],xrefmass[0],xalpha[0]]
  Ao = [otheta0[0],orefmass[0],oalpha[0]]
  An = [ntheta0[0],nrefmass[0],nalpha[0]]

  logmass = FINDGEN(200)*(xmax-xmin)/200.0 + xmin

  Schechter,logmass,Ax,yvalx,dy
  PLOTS,logmass,ALOG10(yvalx),COLOR=!ddgray,LINESTYLE=2,CLIP=[xmin,ymin,xmax,ymax],NOCLIP=0

;  Schechter,logmass,Ao,yvalo,dy
;  PLOTS,logmass,ALOG10(yvalo),COLOR=!ddgray,LINESTYLE=2,CLIP=[xmin,ymin,xmax,ymax],NOCLIP=0

  Schechter,logmass,An,yvaln,dy
  PLOTS,logmass,ALOG10(yvaln),COLOR=!black,LINESTYLE=2,CLIP=[xmin,ymin,xmax,ymax],NOCLIP=0

  IF KEYWORD_SET(ps) THEN BEGIN
    psend,!outdir+'himf.eps',/noprint,/clobber
  ENDIF

  lintheta = 10.d0^newtheta

  weights = 1.d0/lintheta

  fit=CURVEFIT(newbin,lintheta,weights,An,sigA,FUNCTION_NAME='Schechter', $
               /double,TOL=1E-10,iter=iter,status=status,chisq=chisq)

  theta0  = [An[0],sigA[0]]
  refmass = [An[1],sigA[1]]
  alpha   = [An[2],sigA[2]]
  FORPRINT,theta0,refmass,alpha
END
