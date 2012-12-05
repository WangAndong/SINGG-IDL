PRO proc4_sbplot,binsize,bin_num,bin_Rtot,bin_Ntot,color,Rclip,Nclip,window,charsz, $
                 a,zmax,Rscale,Nscale,Rsky,Nsky,NOERASE=noerase
; Given the arrays from proc4_sbgen, plot the SB profiles.

  xmin = Rclip[0]
  xmax = Rclip[2]
  ymax = Rclip[3]
  ymid = Rclip[1]
  ymin = Nclip[1]
  dotspace = 100.0
  amax = MAX(zmax*a)
  textsize = 0.008*charsz

  sz = SIZE(bin_num)
  n_ellipses = sz[1]

  IF n_ellipses GT 0 THEN BEGIN
    IF N_ELEMENTS(color) GE n_ellipses THEN plotclr = color[0:n_ellipses-1] $
                                       ELSE plotclr = INTARR(n_ellipses)+color[0]
  ENDIF

  IF NOT KEYWORD_SET(noerase) THEN BEGIN
    SBclear = INTARR(LONG((xmax-xmin)*window[0])-2, $
                     LONG((ymax-ymin)*window[1])-2) $
                     +!white
    tv,SBclear,LONG(window[0]*xmin)+1,LONG(window[1]*ymin)+2
    PLOTS,[xmin,xmax],ymid*[1.0,1.0],COLOR=!black,/NORMAL

    FOR ii = 1,LONG(amax/dotspace) DO BEGIN
      PLOTS,(dotspace*ii)*[1.0,1.0]/amax*(xmax - xmin) + xmin, $
            [ymin,ymax],LINESTYLE=1,COLOR=!ddgray,/NORMAL
    ENDFOR

    FOR ii = 0,n_ellipses-1 DO BEGIN
      PLOTS,a[ii]*[1.0,1.0]/amax*(xmax - xmin) + xmin, $
            [ymin,ymax],LINESTYLE=2,COLOR=plotclr[ii],/NORMAL
      PLOTS,zmax[ii]*a[ii]*[1.0,1.0]/amax*(xmax - xmin) + xmin, $
            [ymin,ymax],LINESTYLE=1,COLOR=plotclr[ii],/NORMAL
    ENDFOR

    label = ["R band surface brightness","Narrow band surface brightness"]
    XYOUTS,0.5*(xmax+xmin)*[1.0,1.0],[ymax,ymid]-textsize,label,CHARSIZE=charsz, $
           COLOR=!black,ALIGNMENT=0.5,CHARTHICK=2.0,/NORMAL
  ENDIF

  FOR ii = 0,n_ellipses-1 DO BEGIN
    plotindex = WHERE(bin_num[ii,*] GT 0,count)

    IF count GT 0 THEN BEGIN
      bin_x = ((FLOAT(plotindex)+0.5)*binsize*a[ii])/amax * $
                     (xmax-xmin) + xmin
      PLOTS,bin_x,(bin_Rtot[ii,plotindex]/FLOAT(bin_num[ii,plotindex])+Rsky-Rscale[1])/(Rscale[0]-Rscale[1])*(ymax-ymid) + ymid, $
            COLOR=plotclr[ii],PSYM=-SYM(ii+1),NOCLIP=0,CLIP=Rclip,/NORMAL
      PLOTS,bin_x,(bin_Ntot[ii,plotindex]/FLOAT(bin_num[ii,plotindex])+Nsky-Nscale[1])/(Nscale[0]-Nscale[1])*(ymid-ymin) + ymin, $
            COLOR=plotclr[ii],PSYM=-SYM(ii+1),NOCLIP=0,CLIP=Nclip,/NORMAL
    ENDIF

  ENDFOR

  RETURN
END
