PRO brinchmann

  xtitle = ["r!D90!N(R)/r!De!N(R)","!6r!De!N(!8R!6) [kpc])", $
            "!6log(M*[M!Dsolar!N])","!6log(!7l!6* [M!Dsolar!N kpc!E-2!N])", $
            "!6log(SFR / <SFR>)","A(H!7a!6) [ABmag]"]
  filename = ["concentration","R50","mstar","mustar", $
              "log_b","TauV"]+".dat"
  n_plots = N_ELEMENTS(filename)
  path = !singgdir+"/brinchmann/"
  dummy = FLTARR(2)

  set_plot,'X'
  setplotcolors
  setbgfg,!white,!black
  DEVICE, RETAIN=2, DECOMPOSED=0
  WINDOW,XSIZE=1500,YSIZE=1100
  !P.MULTI=[0,2,3,0,0]
  charsz = 3.0
  symsz = 0.1
  thick = 1.0
  ymax = 0.2

  FOR ii = 0,n_plots-1 DO BEGIN
    readcol_new,path+filename[ii],xval,num,mass,sfr, $
                FORMAT='(F,F,F,F)',COMMENT='#',/SILENT
    nvals = N_ELEMENTS(xval)
    step = xval[1]-xval[0]
    nfrac = num / TOTAL(num)
    mfrac = mass / TOTAL(mass)
    sfrac = sfr / TOTAL(sfr)
    xmin = xval[0]-step
    xmax = xval[nvals-1]+step

    PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[0.0,ymax], $
         COLOR=!black,XTITLE=xtitle[ii],XSTYLE=1,YSTYLE=1, $
         CHARSIZE=charsz,TITLE=filename[ii]

    ind = WHERE(xval GE xmin AND xval LE xmax,count)

    maxS = count*2+1
    xplot = [xmin,FLTARR(maxS-2),xmax,xmax]
    nplot = [0.0,0.0,FLTARR(maxS-2),0.0]
    mplot = [0.0,0.0,FLTARR(maxS-2),0.0]
    splot = [0.0,0.0,FLTARR(maxS-2),0.0]

    FOR jj = 1,count DO BEGIN
      xplot[MAX([0,2*jj-1]):2*jj] = xval[ind[jj-1]]
      nplot[2*jj:MIN([maxS+1,2*jj+1])] = nfrac[ind[jj-1]]
      mplot[2*jj:MIN([maxS+1,2*jj+1])] = mfrac[ind[jj-1]]
      splot[2*jj:MIN([maxS+1,2*jj+1])] = sfrac[ind[jj-1]]
    ENDFOR


    PLOTS,xplot,nplot,COLOR=!black,THICK=thick,PSYM=-SYM(1), $
          NOCLIP=0,CLIP=[xmin,0.0,xmax,ymax],SYMSIZE=symsz
    PLOTS,xplot,mplot,COLOR=!red,THICK=thick,PSYM=-SYM(1), $
          NOCLIP=0,CLIP=[xmin,0.0,xmax,ymax],SYMSIZE=symsz
    PLOTS,xplot,splot,COLOR=!blue,THICK=thick,PSYM=-SYM(1), $
          NOCLIP=0,CLIP=[xmin,0.0,xmax,ymax],SYMSIZE=symsz

  ENDFOR

  RETURN
END
