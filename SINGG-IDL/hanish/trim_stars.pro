PRO trim_stars,starfile,buffer,imsize,NPIX=Npix,MAXPIX=maxpix
; Given a .stars file, open it, parse it, remove any lines that are within
; "buffer" of an edge, write the result to test.stars, and exit.  Also, if the
; npix map is specified, remove any star whose center isn't in all of the
; individual images

  outfile = "Temporary_IDL_file_short.stars"

  IF NOT KEYWORD_SET(maxpix) THEN maxpix = 2 ; Assume it's the Rsub image

  IF NOT FILE_TEST(starfile) THEN BEGIN
    PRINT,"ERROR in trim_stars: .stars file does not exist ",starfile
    RETURN
  ENDIF

  readcol_new,starfile,refnumber,X,Y,Mag,flag,$
              A,B,R,FWHM,$
              FORMAT='(I,F,F,F,A,F,F,F,F,F)',COMMENT="#",/SILENT

  OPENW,unit,outfile,/GET_LUN

  FOR ii = 0,N_ELEMENTS(refnumber)-1 DO BEGIN
    IF X[ii] GT buffer AND X[ii] LT (imsize[0]-buffer) AND $
       Y[ii] GT buffer AND Y[ii] LT (imsize[1]-buffer) THEN BEGIN
      IF KEYWORD_SET(Npix) THEN BEGIN
        IF Npix[ROUND(X[ii]),ROUND(Y[ii])] EQ maxpix THEN BEGIN
;      PRINTF,unit,refnumber[ii],X[ii],Y[ii],Mag[ii],flag[ii], $
;                  A[ii],B[ii],R[ii],FWHM[ii], $
;                  FORMAT='(I5," ",2(F8.3," "),F9.4," ",A3," ",5(F6.3," "))'
          PRINTF,unit,X[ii],Y[ii]
        ENDIF
      ENDIF ELSE BEGIN
        PRINTF,unit,X[ii],Y[ii]
      ENDELSE
    ENDIF

  ENDFOR

  CLOSE,unit
  FREE_LUN,unit

END
