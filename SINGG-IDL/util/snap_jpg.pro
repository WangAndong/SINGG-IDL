PRO snap_jpg, filo
   ;
   ; snap (grab) tv window and write to a jpg file
   ; 
   ; filo -> name of output jpg file
   ;
   ; G. Meurer 9/2006
   ;
   im = tvrd(true=3)
   WRITE_JPEG,filo,im,TRUE=3,QUALITY=100
END 
