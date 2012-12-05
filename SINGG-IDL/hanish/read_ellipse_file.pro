PRO read_ellipse_file,ellipse,numgals,refimage,Dx,Dy,Px,Py,pa, $
                      a_i,b_i,z_s,z_f,z_c
; Simple wrapper for reading the ellipse file.  Reason I did it this
; way was to allow for changes in format or future functionality
; without rewriting half a dozen routines to match.

  readcol_new,ellipse,numgals,/SILENT,FORMAT='X,X,X,I,X,X,X,X',NUMLINE=1
  numgals = numgals[0]
  readcol_new,ellipse,refimage,/SILENT,FORMAT='X,X,X,A,X,X,X,X',NUMLINE=1,SKIPLINE=1
  refimage = refimage[0]
  readcol_new,ellipse,Dx,Dy,Px,Py,pa,a_i,b_i,z_s,z_f,z_c, $
                  FORMAT='F,F,F,F,F,F,F,F,F,F',COMMENT='#',/SILENT

  IF numgals EQ 0 OR N_ELEMENTS(Dx) EQ 0 THEN BEGIN
    PRINT,"ERROR in read_ellipse_file: no valid galaxies in file ",ellipse
    RETURN
  ENDIF

  RETURN

END
