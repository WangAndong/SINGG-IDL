PRO singg_header,hd,filename,num_files,exptime,wexptime,buffer,$
                    posref,fluxref,pred,wpred,photqual,date_start,date_end,$
                    pixfile,seeing,datamin,datamax
; Adds a whole bunch of stuff to the three combined-image headers.
; INPUT/OUTPUT:
; hd         Header.  Will be modified by the routine.
; INPUT:
; the rest of the stuff.  Duh.

  SXADDPAR,hd,"FILENAME",filename,' File name',after='OBJECT'

  SXADDPAR,hd,"NCOMBINE",num_files,$
           ' Number of combined images',before='EXPTIME'
  SXADDPAR,hd,"EXPTIME",exptime,' Total exposure time [seconds]'
  SXADDPAR,hd,"WEXPTIME",wexptime,$
           ' Weighted exposure time [seconds]',after='EXPTIME'
  SXADDPAR,hd,"BUNIT",'counts/sec',' Units of image'
  SXADDPAR,hd,"BUFFER",buffer,' Size of edge buffer [pixels]'
  SXADDPAR,hd,"POSREF",posref,' Positional reference image'
  SXADDPAR,hd,"FLUXREF",fluxref,$
           ' Flux reference image',after='POSREF'
  SXADDPAR,hd,"MASKFILE",pixfile,' Pixel count file'
  SXADDPAR,hd,"SEEING",seeing,' Seeing [arcsec]'
  SXADDPAR,hd,"DATAMIN",datamin,' Minimum pixel value'
  SXADDPAR,hd,"DATAMAX",datamax,' Maximum pixel value',after='DATAMIN'
  SXADDPAR,hd,"SKYSUB",'F',' sky subtraction complete?'

  FOR ii=0,num_files-1 DO BEGIN
    indstr = STRTRIM(STRING(ii),2)
    tempstr = "PRED0"+indstr
    SXADDPAR,hd,tempstr,pred[ii],' Input filename #'+indstr
    tempstr = "W"+tempstr
    SXADDPAR,hd,tempstr,wpred[ii],' Scale factor #'+indstr
  ENDFOR

  SXADDPAR,hd,"PHOTQUAL",photqual,' RMS of time-corrected scaling'

  SXADDPAR,hd,"DATE-OBS",date_start,$
           ' Date and time of start of first image'
  SXADDPAR,hd,"DATEEND",date_end,$
           ' Date and time of end of last image',after='DATE-OBS'

  RETURN

END
