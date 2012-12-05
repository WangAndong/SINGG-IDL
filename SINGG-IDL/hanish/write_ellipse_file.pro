PRO write_ellipse_file,outfile,ngal,refimage,Dx,Dy,Px,Py,buffer, $
                       pa,a_i,b_i,z_s,z_f,z_c,SILENT=silent
; Since we're going to need to do this often, might as well make a
; separate routine for it.

  OPENW,eunit,outfile,/GET_LUN

  PRINTF,eunit,"# NUMGALS = ",ngal," / Number of galaxies"
  PRINTF,eunit,"# POSREF = ",refimage," / Positional reference image"
  PRINTF,eunit,"# isophx isophy   brightx  brighty   pa       a_i      b_i      z_s     z_f     z_c"
  FOR jj = 0,ngal-1 DO BEGIN
    IF (a_i[jj]/b_i[jj]) GT 1.0 THEN BEGIN
      atemp = a_i[jj]*z_s[jj]
      btemp = b_i[jj]*z_s[jj]
      patemp = pa[jj]
    ENDIF ELSE BEGIN
; We placed the axes in the wrong order.
      atemp = b_i[jj]*z_s[jj]
      btemp = a_i[jj]*z_s[jj]
      patemp = pa[jj] + 90.0
      IF patemp GT 360.0 THEN patemp = patemp - 360.0
    ENDELSE

    IF NOT KEYWORD_SET(silent) THEN BEGIN
      PRINT,"Semi-major axis = ",atemp
      PRINT,"Semi-minor axis = ",btemp
      PRINT,"Axial ratio = ",(atemp/btemp)
      PRINT,"Position angle (deg) = ",patemp
    ENDIF

    IF z_c[jj] GT 0.01 THEN zctemp = (z_c[jj]/z_s[jj] < 1.0) ELSE zctemp = -999.0
    PRINTF,eunit,Dx[jj]-buffer,Dy[jj]-buffer,Px[jj]-buffer,Py[jj]-buffer, $
                 patemp,atemp,btemp,1.0,(z_f[jj]/z_s[jj] < 1.0),zctemp, $
                 FORMAT='(4(F8.3," "),F8.4," ",2(F8.3," "),2(F7.4," "),F9.4)'
  ENDFOR

  CLOSE,eunit
  FREE_LUN,eunit

  RETURN

END
