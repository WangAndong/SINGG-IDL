PRO patch_ellipse
; Converts old ellipse files to the new format.

  spawn,"ls -d /home/hanish/data/Run??/j*/*ellipse.dat",filelist
  FOR ii = 0,N_ELEMENTS(filelist)-1 DO BEGIN
    outfile = filelist[ii]
    readcol_new,outfile,numgals,/SILENT,FORMAT='X,X,X,I,X,X,X,X',NUMLINE=1
    numgals = numgals[0]
    readcol_new,outfile,refimage,/SILENT,FORMAT='X,X,X,A,X,X,X,X',NUMLINE=1,SKIPLINE=1
    refimage = refimage[0]
    readcol_new,outfile,Dx,Dy,Px,Py,pa,a,b, $
                    FORMAT='F,F,F,F,F,F,F',comment='#',/silent
    PRINT,outfile," ",numgals," ",refimage

    dummy = FLTARR(numgals) + 1.0
    write_ellipse_file,outfile,numgals,refimage,Dx,Dy,Px,Py,0,pa,a,b,dummy,dummy,dummy,/SILENT

  ENDFOR

END
