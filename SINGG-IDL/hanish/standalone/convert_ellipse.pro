PRO convert_ellipse
; change from old-style ellipse file to new format.
; WARNING: once this has been run on a file, make sure not to do it again.

  spawn,"ls j*/*ellipse.dat",imlist
;  imlist = ["j2257-41/j2257-41_ellipse.dat"]

  pi = ACOS(-1.0)

  FOR ii = 0,N_ELEMENTS(imlist)-1 DO BEGIN
    ellipse = imlist[ii]

    PRINT,"Converting ellipse file ",ellipse

    spawn,"cp "+ellipse+" "+ellipse+"_bak"
 
; convert all the ellipse files to the new format
    readcol_new,ellipse,refname,Px,Py,Ax,Ay,Bx,By,Cx,Cy,Gx,Gy,Ztemp,$
                format='A,F,F,F,F,F,F,F,F,F,F,F,F',comment='#'
    n_ellipses = N_ELEMENTS(refname)  
    define_ellipse,Ax,Ay,Bx,By,Cx,Cy,Gx,Gy,a,b,theta,Dx,Dy,Ex,Ey,Fx,Fy

    diamaj = a * Ztemp
    diamin = b * Ztemp

    pa = (theta*(180.0/pi))+90.0

; Now, we're ready to write.
    GET_LUN,unit
    OPENW,unit,ellipse

    PRINTF,unit,"# NUMGALS = ",n_ellipses," / Number of galaxies"
    PRINTF,unit,"# POSREF = ",refname[0]," / Positional reference image"
    PRINTF,unit,"# isophx isophy brightx brighty pa diamaj diamin"

    FOR jj = 0,n_ellipses-1 DO BEGIN
      PRINTF,unit,Dx[jj],Dy[jj],Px[jj],Py[jj], $
                  pa[jj],diamaj[jj],diamin[jj], $
                  FORMAT='(4(F8.3," "),F8.4," ",2(F8.3," "))'
    ENDFOR

    CLOSE,unit
    FREE_LUN,unit

  ENDFOR

END
