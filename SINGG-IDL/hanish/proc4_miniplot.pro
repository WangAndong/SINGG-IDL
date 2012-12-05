PRO proc4_miniplot,xclick,yclick,sz,buffer,minisize,xsep,plotmin,tvbase,dw,zoomscale, $
                   Rimg,Simg,plotsig,boxsize,index,Timg,Rid,xsize,ysize,bsize,$
                   Rx,Ry,Rtheta,Ra,Rb,zoomnormal,normal,Rscale,Sscale,mode,mask,Rsky2

; Creates the four miniaturized plot windows in the upper-right corner
; of the proc4 interface.

  minitvclip = [xsep,plotmin,(0.2 + 0.8*xsep),(1.0+plotmin)/2.0]
  numind = N_ELEMENTS(index)
  IF MAX(index) LT 0 THEN numind = 0
  Tval = Timg[xclick,yclick]

; Start filling the plot windows
; First, the zoomed-in TV display in the upper left.
  cenX = MIN([MAX([FIX(xclick-buffer),minisize]), $
             (sz[1]-2*buffer-1)-minisize])
  cenY = MIN([MAX([FIX(yclick-buffer),minisize]), $
             (sz[2]-2*buffer-1)-minisize])

  tvscale,tvbase[*,cenX-minisize:cenX+minisize, $
                   cenY-minisize:cenY+minisize], $
          POSITION=[xsep,(1.0+plotmin)/2.0,minitvclip[2],1.0],$
          MAXVALUE=plotsig[1],MINVALUE=plotsig[0],BOTTOM=0,TOP=!ngrays-1

  cenX = MIN([MAX([FIX(xclick),boxsize/2]),sz[1]-boxsize/2-1])
  cenY = MIN([MAX([FIX(yclick),boxsize/2]),sz[2]-boxsize/2-1])

  IF mode EQ 1 THEN BEGIN
; If we're in ellipse mode, an inverted R-image with brightness peak
; highlighted is generally more useful than the segmentation image.
    temp = (Rimg[cenX-minisize:cenX+minisize, $
                 cenY-minisize:cenY+minisize] - Rsky2) * $
            FLOAT(mask[cenX-minisize:cenX+minisize, $
                       cenY-minisize:cenY+minisize] le 0b)
    temp = temp^0.25
    tempmin = MIN(temp,/NAN) > 0.0
    tempmax = MAX(temp,val,/NAN)

    tvscale,temp,POSITION=minitvclip, $
            MAXVALUE=tempmax,MINVALUE=tempmin,BOTTOM=0,TOP=!ngrays-1

    peakY = FIX(val / (2*minisize + 1))
    peakX = (cenX-minisize) + val - peakY * FIX(2*minisize + 1)
    peakY = peakY + (cenY-minisize)

;;print,Rimg[peakX,peakY],Simg[peakX,peakY],mask[peakX,peakY]

; Also plot the peak on the main plot.  This is the one time where
; these mini plots affect the main window.
    PLOTS,(peakX-dw[0])/normal[0],(peakY-dw[1])/normal[1], $
          PSYM=SYM(6),COLOR=!red,SYMSIZE=2.0*zoomscale,/NORMAL

    PLOTS,FLOAT(peakX-cenX+minisize)/FLOAT(2*minisize + 1)*(0.2*(1.0-xsep))+xsep, $
          FLOAT(peakY-cenY+minisize)/FLOAT(2*minisize + 1)*(0.5*(1.0-plotmin))+plotmin, $
          PSYM=SYM(6),COLOR=!red,SYMSIZE=1.0,/NORMAL
  ENDIF ELSE BEGIN
; Next, the segmentation image.
    temp = (Timg[cenX-minisize:cenX+minisize,cenY-minisize:cenY+minisize] NE 0)*(!white-!black) + !black
    FOR jj = 0,numind-1 DO BEGIN
      ind2 = WHERE(Timg[cenX-minisize:cenX+minisize, $
                        cenY-minisize:cenY+minisize] EQ Rid[index[jj]],count)
      IF count GT 0 THEN temp[ind2] = !gray
    ENDFOR
    tv,temp,xsize,ysize+bsize-boxsize,xsize=2*minisize,ysize=2*minisize
; Really, this should be a call to tvscale with POSITION=minitvclip.
; But this works, too.

    IF Tval GT 0 THEN BEGIN
      FOR jj = 0,numind-1 DO BEGIN
        zoomX = Rx[index[jj]] - (cenX - minisize)
        zoomY = Ry[index[jj]] - (cenY - minisize)
        plot_ellipse,zoomX+xsep*zoomnormal[0],zoomY+plotmin*zoomnormal[1], $
                     Rtheta[index[jj]]*!dtor,Ra[index[jj]],Rb[index[jj]],$
                     !red,barclr=!blue,normal=zoomnormal,clip=minitvclip
      ENDFOR
    ENDIF
  ENDELSE

; Finally, the R-band and Net image sections.
  tvscale,Rimg[cenX-boxsize/2:cenX+boxsize/2,cenY-boxsize/2:cenY+boxsize/2], $
          POSITION=[minitvclip[2],plotmin,(1.0+minitvclip[2])/2.0,1.0], $
          MAXVALUE=Rscale[0],MINVALUE=Rscale[1],BOTTOM=0,TOP=!ngrays-1

  tvscale,Simg[cenX-boxsize/2:cenX+boxsize/2,cenY-boxsize/2:cenY+boxsize/2], $
          POSITION=[(1.0+minitvclip[2])/2.0,plotmin,1.0,1.0], $
          MAXVALUE=Sscale[0],MINVALUE=Sscale[1],BOTTOM=0,TOP=!ngrays-1

; Now, overlay the crosshairs.
; Really, we should move these to match the position of xclick and
; yclick, but the MAX/MIN caps are never used (profsize would have to
; be 750 for it to hit the edge.)
  PLOTS,[(xsep + 0.2*(1.0-xsep)),1.0],(1.0+plotmin)/2.0*[1.0,1.0], $
        COLOR=!dorange,LINESTYLE=1
  PLOTS,(xsep + 0.4*(1.0-xsep))*[1.0,1.0],[plotmin,1.0], $
        COLOR=!dorange,LINESTYLE=1
  PLOTS,(xsep + 0.8*(1.0-xsep))*[1.0,1.0],[plotmin,1.0], $
        COLOR=!dorange,LINESTYLE=1

  PLOTS,(xsep + 0.1*(1.0-xsep))*[1.0,1.0],[plotmin,1.0], $
        COLOR=!dorange,LINESTYLE=1
  PLOTS,[xsep,xsep+0.2*(1.0-xsep)],plotmin+(1.0-plotmin)*[0.25,0.25], $
        COLOR=!dorange,LINESTYLE=1
  PLOTS,[xsep,xsep+0.2*(1.0-xsep)],plotmin+(1.0-plotmin)*[0.75,0.75], $
        COLOR=!dorange,LINESTYLE=1

  RETURN

END
