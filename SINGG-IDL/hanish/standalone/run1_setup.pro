PRO run1_setup,FILTFILE=filtfile
; Sets up the directory structure and creates the list files.
; This is often done by hand or by using IRAF, but this script was made just
; for the heck of it.
; OPTIONAL INPUTS
; filtfile         Table of valid filter names.  If not set, it uses a 
;                  hard-coded machine-specific default.

  IF NOT KEYWORD_SET(filtfile) THEN filtfile = !singgdir+"/filtfile.dat"
  IF NOT FILE_TEST(filtfile) THEN BEGIN
    PRINT,"ERROR in singg_setup: filter file not found ",filtfile
    RETURN
  ENDIF
; Load filter table:
  singg_rdfiltfile,filtfile,fnamarr

  spawn,"ls -d j*",dirlist

  num_gals = N_ELEMENTS(dirlist)

  max_filters = 4 ; max filters per run

  filter = STRARR(num_gals,max_filters) ; which filters this galaxy uses
  filtnum = INTARR(num_gals)            ; how many filters this galaxy uses
  filtcnt = INTARR(num_gals,max_filters) ; how many images are in each filter
  filtlist = STRARR(num_gals,max_filters,20) ; name of each image in each filter
  
  FOR ii = 0,num_gals-1 DO BEGIN
    CD,dirlist[ii]

    spawn,"ls obj????tr.fits",imlist
    num_objs = N_ELEMENTS(imlist)
    newobj = "J"+STRMID(dirlist[ii],1,STRLEN(dirlist[ii])-1)
    
    FOR jj = 0,num_objs-1 DO BEGIN
      fits_read,imlist[jj],img,hd
      SXADDPAR,hd,"OBJECT",newobj
      fits_write,imlist[jj],img,hd

      objfilter = STRTRIM(SXPAR(hd,'FILTER1'),2)
      filtername = singg_filtnam(fnamarr,objfilter,pos)
      filtindex = WHERE(filter[ii,*] EQ STRTRIM(filtername[0],2),matches)
      IF matches GT 1 THEN BEGIN
        PRINT,"ERROR in run1_setup: multiple matches on filter ",filtername
        RETURN
      ENDIF

      IF matches LT 1 THEN BEGIN
; new filter
        filtindex = filtnum[ii]
        filter[ii,filtindex] = STRTRIM(filtername[0],2)
        filtnum[ii] = filtnum[ii] + 1
      ENDIF

      filtlist[ii,filtindex,filtcnt[ii,filtindex]] = imlist[jj]
 
      filtcnt[ii,filtindex] = filtcnt[ii,filtindex] + 1
    ENDFOR

; Now, we've sorted everything into some big honkin' arrays.  Time to start 
; creating directories and files.

    FOR jj = 0,filtnum[ii]-1 DO BEGIN
; Create the .lis file for each filter
      tempfiltindex = WHERE(fnamarr[*,0] EQ filter[ii,jj],matches)
      IF matches NE 1 THEN BEGIN
        PRINT,"ERROR in run1_setup: filter table mismatch ",filter[ii,jj],matches
        RETURN
      ENDIF
      filename = 'obj_'+fnamarr[tempfiltindex,1]+'.lis'

      GET_LUN,unit
      OPENW,unit,filename
      
      FOR kk = 0,filtcnt[ii,jj]-1 DO BEGIN
        file = filtlist[ii,jj,kk]
; For each file, copy it here and add it to the .lis file

; Parse the part of the filename that's just its obj???? and add "sh.fits"
        shfile = STRMID(file,0,STRLEN(file)-7)+'sh.fits'
        PRINTF,unit,shfile,FORMAT='(A)'
      ENDFOR

      CLOSE,unit
      FREE_LUN,unit
    ENDFOR

; Step out.
    CD,".."

  ENDFOR

  maskfile = !singgdir+'basic_mask.fits'
  IF FILE_TEST(maskfile) THEN BEGIN
    command = "cp "+maskfile+" ./mask/"
    spawn,command
  ENDIF ELSE BEGIN
    PRINT,"basic_mask.fits not found in SINGG directory.  Please find a copy "
    PRINT,"to be placed in the mask subdirectory before running singg_combine"
  ENDELSE

  PRINT,"SINGG setup complete."

END
