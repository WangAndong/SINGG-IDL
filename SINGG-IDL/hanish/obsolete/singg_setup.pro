PRO singg_setup
; Sets up the directory structure and creates the list files.
; This is often done by hand or by using IRAF, but this script was made just
; for the heck of it.

; Load filter table:
  filt_rddbfnames,"filter",fnamarr

; First, sort the directories to the higher level.
; That is, turn /(N/n)ight1/(O/o)bjs/obj*.fits OR /night1/j*/obj.fits 
; into /j*/obj*.fits

  spawn,"ls ?ight?/?bjs/obj????tr.fits",imlist1
  spawn,"ls ?ight?/j*/obj????tr.fits",imlist2
; Remove the "tr" from these if we don't have transposed images yet.

  len1 = N_ELEMENTS(imlist1)
  len2 = N_ELEMENTS(imlist2)

  IF len1 GT 1 THEN BEGIN
    IF len2 GT 1 THEN BEGIN
; Case 1: both lists are valid.
      imlist = [imlist1,imlist2]
    ENDIF ELSE BEGIN
; Case 2: only list 1 is valid.
      imlist = imlist1
    ENDELSE
  ENDIF ELSE BEGIN
    IF len2 GT 1 THEN BEGIN
; Case 3: only list 2 is valid.
      imlist = imlist2
    ENDIF ELSE BEGIN
; Case 4: neither is valid
      PRINT,"ERROR in singg_setup: no valid image files found."
      RETURN
    ENDELSE
  ENDELSE

  num_objs = N_ELEMENTS(imlist)

  max_gal = 50 ; Maximum galaxies per run
  max_filters = 9 ; max filters per run

  galname = STRARR(max_gal)
  filter = STRARR(max_gal,max_filters) ; which filters this galaxy uses
  filtnum = INTARR(max_gal)            ; how many filters this galaxy uses
  filtcnt = INTARR(max_gal,max_filters) ; how many images are in each filter
  filtlist = STRARR(max_gal,max_filters,20) ; name of each image in each filter
  
; Standards get handled separately for filtlist, because the list is so big
  stdfiltlist = STRARR(10,100)

  galname[0] = "STD"
  num_gal = 1
; if galindex = 0, it's a standard

  FOR ii = 0,num_objs-1 DO BEGIN
    fits_read,imlist[ii],img,hd,/header_only
    objstr = SXPAR(hd,'OBJECT')
    IF STRMID(objstr,0,1) EQ 'J' OR STRMID(objstr,0,1) EQ 'j' THEN BEGIN
      object = "j"+STRMID(objstr,1,STRLEN(objstr)-1)

      galindex = WHERE(galname EQ object,matches)
      IF matches GT 1 THEN BEGIN
        PRINT,"ERROR in singg_setup: multiple matches on object ",object
        RETURN
      ENDIF

      IF matches LT 1 THEN BEGIN
; No matches, so add a new one to the end of the list
        galindex = num_gal
        galname[galindex] = object
        num_gal = num_gal + 1
      ENDIF
; If matches=1, we only need galindex, which has already been set
    ENDIF ELSE BEGIN
; it must be a standard, so stick it in the appropriate array
      object = "STD"
      galindex = 0
    ENDELSE

    objfilter = STRTRIM(SXPAR(hd,'FILTER1'),2)
    IF objfilter EQ "dia" OR objfilter EQ "cb" THEN objfilter = STRTRIM(SXPAR(hd,"FILTER2"),2)
    filtername = singg_filtnam(fnamarr,objfilter,pos)

    filtindex = WHERE(filter[galindex,*] EQ STRTRIM(filtername[0],2),matches)
    IF matches GT 1 THEN BEGIN
      PRINT,"ERROR in singg_setup: multiple matches on filter ",filtername
      RETURN
    ENDIF

    IF matches LT 1 THEN BEGIN
; new filter
      filtindex = filtnum[galindex]
      filter[galindex,filtindex] = STRTRIM(filtername[0],2)
      filtnum[galindex] = filtnum[galindex] + 1
    ENDIF

    IF galindex GT 0 THEN BEGIN
      filtlist[galindex,filtindex,filtcnt[galindex,filtindex]] = imlist[ii]
    ENDIF ELSE BEGIN
      stdfiltlist[filtindex,filtcnt[0,filtindex]] = imlist[ii]
    ENDELSE

    filtcnt[galindex,filtindex] = filtcnt[galindex,filtindex] + 1

  ENDFOR

; Now, we've sorted everything into some big honkin' arrays.  Time to start 
; creating directories and files.

;  FOR ii = 0,num_gal-1 DO BEGIN
;    PRINT,STRING(ii)+" "+galname[ii]
;    PRINT,filtnum[ii],filter[ii,*]
;  ENDFOR

  FOR ii = 0,num_gal-1 DO BEGIN
; First, create the directory:
    command = "mkdir "+galname[ii]
    spawn,command

; Step in.
    CD,galname[ii],current=rootdir

    FOR jj = 0,filtnum[ii]-1 DO BEGIN
; Create the .lis file for each filter
      tempfiltindex = WHERE(fnamarr[*,0] EQ filter[ii,jj],matches)
      IF matches NE 1 THEN BEGIN
        PRINT,"ERROR in singg_setup: filter table mismatch ",filter[ii,jj],matches
        RETURN
      ENDIF
      filename = 'obj_'+fnamarr[tempfiltindex,1]+'.lis'

      OPENW,unit,filename,/GET_LUN

      FOR kk = 0,filtcnt[ii,jj]-1 DO BEGIN
        IF ii GT 0 THEN file = "../"+filtlist[ii,jj,kk] ELSE file = "../"+stdfiltlist[jj,kk]
; For each file, copy it here and add it to the .lis file
        command = "cp "+file+" ."
        spawn,command
        trfile = STRMID(file,0,STRLEN(file)-5)+"tr.fits"
        IF FILE_TEST(trfile) THEN BEGIN
          command = "cp "+trfile+" ."
          spawn,command
        ENDIF

; Parse the part of the filename that's just its obj???? and add "sh.fits"
        shfile = STRMID(file,STRLEN(file)-12,7)+'sh.fits'
        PRINTF,unit,shfile,FORMAT='(A)'
      ENDFOR

      CLOSE,unit
      FREE_LUN,unit
    ENDFOR

; Step out.
    CD,rootdir

  ENDFOR

; Set up the mask directory
  command="mkdir mask"
  spawn,command

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
