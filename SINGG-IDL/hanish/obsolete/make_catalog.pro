PRO make_catalog
; A stand-alone that makes the basic catalog file.  You'll need to go in
; and change it by hand later.

; First, find where we are.
  spawn,"pwd",currentdir
  run = STRTRIM(STRMID(currentdir[0],STRLEN(currentdir[0])-5,5),2)
  outfile = run+".catalog"

  IF FILE_TEST(outfile) THEN BEGIN
    PRINT,"ERROR in make_catalog: output file already exists.",outfile
    PRINT,"If you want to overwrite this file, remove it manually first."
    RETURN
  ENDIF

; Next, get the list of galaxies
  spawn,"ls -d j???????",dirlist

; NOTE: this won't get galaxies with unusual names, like run02/j0209-10a.
; You'll have to add these by hand.

  max_filt = 3 ; max number of filters per directory

  n_dir = N_ELEMENTS(dirlist)
  n_filters = INTARR(n_dir)
  num_Rfilt = INTARR(n_dir)

  filter = STRARR(n_dir,max_filt)
  Rfile = STRARR(n_dir,2)
  Cfile = STRARR(n_dir,max_filt)
  Sfile = STRARR(n_dir,max_filt,2)
  Elfile = STRARR(n_dir)
  refnum = STRARR(n_dir)

  FOR ii = 0,n_dir-1 DO BEGIN
    CD,dirlist[ii],current=rootdir
    id = "J"+STRMID(dirlist[ii],1,STRLEN(dirlist[ii])-1)

    Rsuffix = STRARR(2)

    IF FILE_TEST("obj_R.lis") THEN BEGIN
      readcol_new,"obj_R.lis",rtemp,format="A",comment='#',/silent
      Rfile[ii,0] = id+"_R.fits"   
      refnum[ii] = STRMID(find_middle(rtemp),3,4)
      Rsuffix[0] = "_Rsub.fits"
      num_Rfilt[ii] = 1
    ENDIF

    IF FILE_TEST("obj_cont.lis") THEN BEGIN
      readcol_new,"obj_cont.lis",rtemp,format="A",comment='#',/silent
      IF num_Rfilt[ii] EQ 1 THEN BEGIN
        Rfile[ii,1] = id+"_C.fits"   
        Rsuffix[1] = "_Csub.fits"
      ENDIF ELSE BEGIN
        Rfile[ii,0] = id+"_C.fits"   
        Rsuffix[0] = "_Csub.fits"
        refnum[ii] = STRMID(find_middle(rtemp),3,4)
      ENDELSE
      num_Rfilt[ii] = num_Rfilt[ii] + 1
    ENDIF

    Elfile[ii] = dirlist[ii]+"_ellipse.dat"

    spawn,"ls obj_6???.lis",narrowlist
    n_filters[ii] = N_ELEMENTS(narrowlist)
    FOR jj = 0,n_filters[ii]-1 DO BEGIN
      filter[ii,jj] = STRMID(narrowlist[jj],4,4) ; 6XXX
      Cfile[ii,jj] = id+"_"+filter[ii,jj]+".fits"
      FOR kk = 0,num_Rfilt[ii]-1 DO BEGIN
        IF n_filters[ii] EQ 1 THEN Sfile[ii,jj,kk] = id+Rsuffix[kk] $
                          ELSE Sfile[ii,jj,kk] = id+"_"+filter[ii,jj]+Rsuffix[kk]
      ENDFOR
    ENDFOR

    CD,rootdir
  ENDFOR

  OPENW,unit,outfile,/GET_LUN

  PRINTF,unit,"# (name) (filter) (R image) (narrow image) (subtracted image) (ellipse file) (reference image number) (R mask) (narrow mask)"
  FOR ii = 0,n_dir-1 DO BEGIN
    FOR jj = 0,n_filters[ii]-1 DO BEGIN
      FOR kk = 0,num_Rfilt[ii]-1 DO BEGIN
        PRINTF,unit,dirlist[ii],filter[ii,jj], $
               Rfile[ii,kk],Cfile[ii,jj],Sfile[ii,jj,kk], $
               Elfile[ii],refnum[ii],"basic_mask.fits","basic_mask.fits", $
               FORMAT='(9(A," "))'
      ENDFOR
    ENDFOR
  ENDFOR

  CLOSE,unit
  FREE_LUN,unit

; While we're at it, let's make the two list files needed for the PHOTFLUX
; measurement later.

  OPENW,unit1,run+".lis",/GET_LUN

  OPENW,unit2,run+"_net.lis",/GET_LUN

  FOR ii = 0,n_dir-1 DO BEGIN
    FOR jj = 0,n_filters[ii]-1 DO BEGIN
      PRINTF,unit1,Rfile[ii,jj]," ",dirlist[ii]," Y"
      PRINTF,unit1,Cfile[ii,jj]," ",dirlist[ii]," Y"

      coeffdat = "/home/data/"+run+"/filtcoeff/"+dirlist[ii]+"_filtcoef.dat"
      PRINTF,unit2,Sfile[ii,jj]," ",coeffdat," Y 0.35"
    ENDFOR
  ENDFOR 

  CLOSE,unit1
  FREE_LUN,unit1
  CLOSE,unit2
  FREE_LUN,unit2

END
