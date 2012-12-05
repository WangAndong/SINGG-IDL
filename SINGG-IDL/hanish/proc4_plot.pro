PRO proc4_plot,tvdisp,mainclip,plotsig,dmode,Rid,Timg,dw,masklist,Omask,Oimg,mode, $
               dotclr,dmodelist,dspace,dsize,zoomscale,dsym,normal,n_ellipses,$
               Dx,Dy,Px,Py,theta,a,b,a_f,b_f,ellcolor
; Refreshes the current plot window.  All of the arguments are inputs.

  tvscale,tvdisp,POSITION=mainclip,BOTTOM=0,TOP=!ngrays-1, $
          MAXVALUE=SSQRT(plotsig[1]),MINVALUE=SSQRT(plotsig[0])

  IF dmode GT 0 THEN proc4_dot,Rid,Timg,dw, $
                     masklist,Omask,Oimg,mode,dotclr,ellcolor, $
                     dspace[dmodelist[dmode-1]], $
                     dsize[dmodelist[dmode-1]]*zoomscale, $
                     dsym[dmodelist[dmode-1]],normal,mainclip

  plot_ellipse,(Dx-dw[0]),(Dy-dw[1]),theta,a,b,ellcolor, $
               barclr=!gray,clip=mainclip,normal=normal,symscale=zoomscale
  FOR ell = 0,n_ellipses-1 DO BEGIN
    IF ABS(a_f[ell]/a[ell] - 1.0) GT 0.01 THEN $
       plot_ellipse,Dx[ell]-dw[0],Dy[ell]-dw[1],theta[ell],a_f[ell],b_f[ell], $
                    ellcolor[ell],barclr=!dgray,normal=normal,clip=mainclip, $
                    symscale=zoomscale
  ENDFOR

  PLOTS,(Px-dw[0])/normal[0],(Py-dw[1])/normal[1],$
        PSYM=SYM(13),COLOR=ellcolor,SYMSIZE=2.0*zoomscale, $
        CLIP=mainclip,NOCLIP=0,/NORMAL

  RETURN

END
