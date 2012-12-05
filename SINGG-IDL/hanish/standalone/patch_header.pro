PRO patch_header,fili,filo

; Given input file "fili" and output file "filo", copy certain missing header
; pieces from fili to filo.

  fits_read,fili,Iimg,Ihd
  fits_read,filo,Oimg,Ohd

  Newhd = Ohd

; Now, add the new stuff

  Itemp = SXPAR(Ihd,'OBJECT')
  Otemp = SXPAR(Ohd,'OBJECT',count=count)
  IF count NE 1 THEN SXADDPAR,Newhd,'OBJECT',Itemp,'',before='DATE'

  Itemp = SXPAR(Ihd,'DATE-OBS')
  Otemp = SXPAR(Ohd,'DATE-OBS',count=count)
  IF count NE 1 THEN SXADDPAR,Newhd,'DATE-OBS',Itemp, $
                     'Date of observation start',after='CCDSUM'

  Itemp = SXPAR(Ihd,'UT')
  Otemp = SXPAR(Ohd,'UT',count=count)
  IF count NE 1 THEN SXADDPAR,Newhd,'UT',Itemp, $
                     'UT of TCS coords',after='DATE-OBS'

  Itemp = SXPAR(Ihd,'RA')
  Otemp = SXPAR(Ohd,'RA',count=count)
  IF count NE 1 THEN SXADDPAR,Newhd,'RA',Itemp, $
                     'right ascension (telescope)',after='ASEC21'

  Itemp = SXPAR(Ihd,'DEC')
  Otemp = SXPAR(Ohd,'DEC',count=count)
  IF count NE 1 THEN SXADDPAR,Newhd,'DEC',Itemp, $
                     'declination (telescope)',after='RA'

  Itemp = SXPAR(Ihd,'ZD')
  Otemp = SXPAR(Ohd,'ZD',count=count)
  IF count NE 1 THEN SXADDPAR,Newhd,'ZD',Itemp, $
                     'zenith distance (degrees)',after='DEC'

  Itemp = SXPAR(Ihd,'HA')
  Otemp = SXPAR(Ohd,'HA',count=count)
  IF count NE 1 THEN SXADDPAR,Newhd,'HA',Itemp, $
                     'declination (telescope)',after='ZD'

  Itemp = SXPAR(Ihd,'ST')
  Otemp = SXPAR(Ohd,'ST',count=count)
  IF count NE 1 THEN SXADDPAR,Newhd,'ST',Itemp, $
                     'sidereal time',after='HA'

  Itemp = SXPAR(Ihd,'AIRMASS')
  Otemp = SXPAR(Ohd,'AIRMASS',count=count)
  IF count NE 1 THEN SXADDPAR,Newhd,'AIRMASS',Itemp, $
                     'airmass',after='ST'

  Itemp = SXPAR(Ihd,'EXPTIME')
  Otemp = SXPAR(Ohd,'EXPTIME',count=count)
  IF count NE 1 THEN SXADDPAR,Newhd,'EXPTIME',Itemp, $
                     'Exposure time in secs',after='AIRMASS'

  Itemp = SXPAR(Ihd,'OBSERVER')
  Otemp = SXPAR(Ohd,'OBSERVER',count=count)
  IF count NE 1 THEN SXADDPAR,Newhd,'OBSERVER',Itemp, $
                     'Observers',after='EXPTIME'

  Itemp = SXPAR(Ihd,'INSTRUME')
  Otemp = SXPAR(Ohd,'INSTRUME',count=count)
  IF count NE 1 THEN SXADDPAR,Newhd,'INSTRUME',Itemp, $
                     'cassegrain direct imager',after='OBSERVER'

  Itemp = SXPAR(Ihd,'FILTER1')
  Otemp = SXPAR(Ohd,'FILTER1',count=count)
;  IF count NE 1 THEN SXADDPAR,Newhd,'FILTER1',Itemp, $
;                     'Filter in wheel one',before='RECID'
  SXADDPAR,Newhd,'FILTER1','6618','Filter in wheel one',before='RECID'

  Itemp = SXPAR(Ihd,'FNAME1')
  Otemp = SXPAR(Ohd,'FNAME1',count=count)
;  IF count NE 1 THEN SXADDPAR,Newhd,'FNAME1',Itemp, $
;                     'Full name of filter in wheel1',before='RECID'
  SXADDPAR,Newhd,'FNAME1','6618/74 kp1564 4x4','Full name of filter in wheel1',before='RECID'

  Itemp = SXPAR(Ihd,'FILTER2')
  Otemp = SXPAR(Ohd,'FILTER2',count=count)
  IF count NE 1 THEN SXADDPAR,Newhd,'FILTER2',Itemp, $
                     'Filter in wheel two',before='RECID'

  Itemp = SXPAR(Ihd,'FNAME2')
  Otemp = SXPAR(Ohd,'FNAME2',count=count)
  IF count NE 1 THEN SXADDPAR,Newhd,'FNAME2',Itemp, $
                     'Full name of filter in wheel2',before='RECID'

  Itemp = SXPAR(Ihd,'FILTERS')
  Otemp = SXPAR(Ohd,'FILTERS',count=count)
  IF count NE 1 THEN SXADDPAR,Newhd,'FILTERS',Itemp, $
                     'Filter positions',before='RECID'

  Itemp = SXPAR(Ihd,'XPIXSIZE')
  Otemp = SXPAR(Ohd,'XPIXSIZE',count=count)
  IF count NE 1 THEN SXADDPAR,Newhd,'XPIXSIZE',Itemp, $
                     'Pixel size in X (arcsec/pix)',before='RECID'

  Itemp = SXPAR(Ihd,'YPIXSIZE')
  Otemp = SXPAR(Ohd,'YPIXSIZE',count=count)
  IF count NE 1 THEN SXADDPAR,Newhd,'YPIXSIZE',Itemp, $
                     'Pixel size in Y (arcsec/pix)',before='RECID'

; Don't need UTSHUT, WAVEFILE, NOTE, WAVEMODE, GTRON, GTGAIN, GTINDEX,
; PIXELT, DCS_TIME, EPOCH, COMMENT, TELID, ARCONVER, TELFOCUS

; Now that the new header is fixed, write the file
  fits_write,filo,Oimg,Newhd

END