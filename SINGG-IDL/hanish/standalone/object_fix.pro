PRO object_fix,objname,RENAME=rename
; For every fits file within the directory, change the "OBJECT" header line
; to a user-specified value.
; This should only be run in very specific cases.
; Set /rename if you want the files to be renamed as well

  spawn,"ls *.fits",imlist

  FOR ii = 0, N_ELEMENTS(imlist)-1 DO BEGIN
    PRINT,"Fixing image: ",imlist[ii]
    fits_read,imlist[ii],img,hd

    SXADDPAR,hd,"OBJECT",objname,' Name of the object observed'

    fits_write,imlist[ii],img,hd
  ENDFOR

  IF KEYWORD_SET(rename) THEN BEGIN
    PRINT,"Updated headers, now renaming files"

    spawn,"ls J*",imlist2
    objlen = STRLEN(objname)

    FOR jj = 0, N_ELEMENTS(imlist2)-1 DO BEGIN
      newname = objname+STRMID(imlist2[jj],objlen,STRLEN(imlist2[jj])-objlen)
      command = "mv "+imlist2[jj]+" "+newname
      spawn,command
    ENDFOR
  ENDIF

END
