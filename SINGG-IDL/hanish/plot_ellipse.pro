PRO plot_ellipse,Dx,Dy,theta,a,b,ellclr,BARCLR=barclr, $
                 NORMAL=normal,CLIP=clip,SYMSCALE=symscale
; OPTIONAL INPUT
;  normal[2]      [xsize,ysize] of input image to be normalized to.
;  clip[x0,y0,x1,y1]
; Given sets of ellipse coordinates, plot the edges of the ellipses onto the
; current plot window.

; In the /normal setup, we can always use the CLIP flags.
  IF KEYWORD_SET(normal) AND NOT KEYWORD_SET(clip) THEN clip=[0.0,0.0,1.0,1.0]
  IF NOT KEYWORD_SET(symscale) THEN symscale = 1.0

  ss = [0.3,1.0,1.5]*SQRT(symscale)

  n_pts = FIX(2.0*!pi*SQRT(a*b) / 50.0) > 16
  n_ellipses = N_ELEMENTS(Dx)
  
; If only one color is passed in, pad the array with it.  So, store to
; a temporary array to keep from overwriting.
  IF N_ELEMENTS(ellclr) EQ n_ellipses THEN ellclr2 = ellclr $
                                      ELSE ellclr2 = ellclr[0] * (FLTARR(n_ellipses)+1.0)

  FOR ii = 0,n_ellipses-1 DO BEGIN
; Plot the ellipse part first, to make sure it's on bottom.
    phi = FINDGEN(n_pts[ii]+1)/FLOAT(n_pts[ii]) * (!pi/2.0)
    FOR jj = 0,n_pts[ii] DO BEGIN
      x = FLTARR(4)
      y = FLTARR(4)

; Nonrotated del_X and del_Y
      x0 = a[ii]*COS(phi[jj])
      y0 = b[ii]*SIN(phi[jj])

      x[0] = Dx[ii] + x0*COS(theta[ii]) - y0*SIN(theta[ii])
      x[1] = Dx[ii] - x0*COS(theta[ii]) - y0*SIN(theta[ii])
      x[2] = Dx[ii] + x0*COS(theta[ii]) + y0*SIN(theta[ii])
      x[3] = Dx[ii] - x0*COS(theta[ii]) + y0*SIN(theta[ii])

      y[0] = Dy[ii] + y0*COS(theta[ii]) + x0*SIN(theta[ii])
      y[1] = Dy[ii] + y0*COS(theta[ii]) - x0*SIN(theta[ii])
      y[2] = Dy[ii] - y0*COS(theta[ii]) + x0*SIN(theta[ii])
      y[3] = Dy[ii] - y0*COS(theta[ii]) - x0*SIN(theta[ii])

      IF KEYWORD_SET(normal) THEN BEGIN
        PLOTS,x/normal[0],y/normal[1],PSYM=SYM(1),SYMSIZE=symscale*0.3,COLOR=ellclr2[ii],/NORMAL, $
              NOCLIP=0,CLIP=clip
      ENDIF ELSE BEGIN
        IF KEYWORD_SET(clip) THEN $
          PLOTS,x,y,PSYM=SYM(1),SYMSIZE=symscale*0.3,COLOR=ellclr2[ii],CLIP=clip,NOCLIP=0 $
        ELSE PLOTS,x,y,PSYM=SYM(1),SYMSIZE=symscale*0.3,COLOR=ellclr2[ii]
      ENDELSE

    ENDFOR

    IF KEYWORD_SET(barclr) THEN BEGIN
      Ax = Dx[ii] + a[ii]*COS(theta[ii])
      Ay = Dy[ii] + a[ii]*SIN(theta[ii])
      Bx = Dx[ii] - a[ii]*COS(theta[ii])
      By = Dy[ii] - a[ii]*SIN(theta[ii])
      Ex = Dx[ii] + b[ii]*SIN(theta[ii])
      Ey = Dy[ii] - b[ii]*COS(theta[ii])
      Fx = Dx[ii] - b[ii]*SIN(theta[ii])
      Fy = Dy[ii] + b[ii]*COS(theta[ii])

      IF KEYWORD_SET(normal) THEN BEGIN
        PLOTS,[Ax,Bx,Ex,Fx]/normal[0],[Ay,By,Ey,Fy]/normal[1],COLOR=ellclr2[ii], $
              PSYM=SYM(1),SYMSIZE=ss[2],/NORMAL,NOCLIP=0,CLIP=clip
        PLOTS,[Ax,Bx]/normal[0],[Ay,By]/normal[1],COLOR=barclr,PSYM=-SYM(1), $
              SYMSIZE=ss[1],/NORMAL,NOCLIP=0,CLIP=clip,THICK=SQRT(symscale)
        PLOTS,[Ex,Fx]/normal[0],[Ey,Fy]/normal[1],COLOR=barclr,PSYM=-SYM(1), $
              SYMSIZE=ss[1],/NORMAL,NOCLIP=0,CLIP=clip,THICK=SQRT(symscale)
        PLOTS,[Ax,Bx,Ex,Fx]/normal[0],[Ay,By,Ey,Fy]/normal[1],COLOR=ellclr2[ii], $
              PSYM=SYM(1),SYMSIZE=ss[0],/NORMAL,NOCLIP=0,CLIP=clip
      ENDIF ELSE BEGIN
        IF KEYWORD_SET(clip) THEN BEGIN
          PLOTS,[Ax,Bx,Ex,Fx],[Ay,By,Ey,Fy],COLOR=ellclr2[ii],PSYM=SYM(1), $
                SYMSIZE=ss[2],NOCLIP=0,CLIP=clip
          PLOTS,[Ax,Bx],[Ay,By],COLOR=barclr,PSYM=-SYM(1), $
                SYMSIZE=ss[1],NOCLIP=0,CLIP=clip,THICK=SQRT(symscale)
          PLOTS,[Ex,Fx],[Ey,Fy],COLOR=barclr,PSYM=-SYM(1), $
                SYMSIZE=ss[1],NOCLIP=0,CLIP=clip,THICK=SQRT(symscale)
          PLOTS,[Ax,Bx,Ex,Fx],[Ay,By,Ey,Fy],COLOR=ellclr2[ii],PSYM=SYM(1), $
                SYMSIZE=ss[0],NOCLIP=0,CLIP=clip
        ENDIF ELSE BEGIN
          PLOTS,[Ax,Bx,Ex,Fx],[Ay,By,Ey,Fy],COLOR=ellclr2[ii],PSYM=SYM(1),SYMSIZE=ss[2]
          PLOTS,[Ax,Bx],[Ay,By],COLOR=barclr,PSYM=-SYM(1),SYMSIZE=ss[1],THICK=SQRT(symscale)
          PLOTS,[Ex,Fx],[Ey,Fy],COLOR=barclr,PSYM=-SYM(1),SYMSIZE=ss[1],THICK=SQRT(symscale)
          PLOTS,[Ax,Bx,Ex,Fx],[Ay,By,Ey,Fy],COLOR=ellclr2[ii],PSYM=SYM(1),SYMSIZE=ss[0]
        ENDELSE
      ENDELSE
    ENDIF
  ENDFOR

  RETURN
END
