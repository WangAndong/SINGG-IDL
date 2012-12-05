PRO change_obj,filename,objname

  fits_read,filename,img,hd

  oldname = SXPAR(hd,"OBJECT")
  PRINT,oldname

  SXADDPAR,hd,"OBJECT",objname,' Name of the object observed'

  fits_write,filename,img,hd

END
