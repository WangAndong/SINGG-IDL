PRO ssoup_atidyhdr, ll, bname, fnami, fnamo, kfwhm, im, hdin, hdout, astr
  ;
  ; Tidy headers for output from ssoup_align
  ;
  ; ll     -> logical unit of log file
  ; bname  -> name of band (string).
  ; fnami  -> input filename (to be stored in hdout) 
  ; fnamo  -> output filename (also stored in header)
  ; kfwhm  -> FWHM of convolution kernel
  ; im     -> image array, used to create basic header
  ; hdin   -> input header
  ; hdout  <- tidied output header
  ; astr   <> astrometry structure, if this is already set as a 
  ;           structure then this is copied into the new header
  ;           otherwise it is taken from the current header
  ;
  ; G. Meurer 5/2010 ICRAR/UWA
  ; S. Andrews 2/2013 ICRAR/UWA added Wise, refactoring 
  COMMON bands, band, nband, bandnam, bandavail, nbandavail
  findpro,"ssoup_atidyhdr",/noprint,dirlist=temp123 ; directory containing template headers 
  prog      = 'SSOUP_ATIDYHDR: '
  ddir = temp123[0]
  htempl    = ddir+["wx", "wx", "wx", "wx", 'ha', 'r', 'uv', 'uv']+'_templ_hdr.dat'
  pname     = 'SSOUP.PRO'         ; name of main program
  ;
  ; find pointer to apropriate band
  plog,ll,prog,'--------------------- starting '+prog+'---------------------------------'
  jj        = where(bandavail EQ bname, njj)
  IF njj NE 1 THEN BEGIN 
     IF njj EQ 0 THEN plog,ll,prog,'**** ERROR BAND '+bname+' does not match any of BNAME'
     IF njj GT 1 THEN plog,ll,prog,'**** ERROR BAND '+bname+' matches more than one of BNAME (must be a bug...)'
     stop
  ENDIF ELSE BEGIN 
     ptr    = jj[0]
     plog,ll,prog,'Working with band = '+bandavail[ptr]
  ENDELSE 
  ;
  ; make working copy of header to do initial changes in
  hdwk      = hdin
  ;
  ; stuff to store for all images
  get_date, date, /timetag
  fxaddpar, hdwk, 'DATE', date, 'date FITS file was created' ;date of processing
  fxaddpar, hdwk, 'ORIGIN', pname, 'the SINGG/SUNGG pipeline (written in IDL)' 
  fxaddpar, hdwk, 'FILENAME', fnamo, 'File name'             ;name of file that is to be written
  fxaddpar, hdwk, 'PRED00', fnami, 'Input filename #0'       ;predecessor image 
  fxaddpar, hdwk, 'WPRED00', 1.0, 'Scale factor for file #0' ;and its weight
  ;
  ; convolution kernel width
  fxaddpar, hdwk, 'KFWHM', kfwhm, 'Convolution kernel FWHM [pixels]'
  ;
  ; determine if astrometry structure has been passed
  sz         = size(astr)
  nsz        = n_elements(sz)
  typ        = sz[nsz-2]
  IF typ EQ 8 THEN BEGIN 
     plog,ll,prog,'Astrometry structure has been passed'
  ENDIF ELSE BEGIN 
     plog,ll,prog,'deriving astrometry structure from header'
     extast, hdwk, astr, noparams
     ;
     ; determine pixel at center of image
     plog,ll,prog,'determining CRPIX1,2; CRVAL1,2 at image center'
     nx          = sxpar(hdwk, 'NAXIS1')
     ny          = sxpar(hdwk, 'NAXIS2')
     xx          = float(nx)/2.0 - 0.5    ; central 
     yy          = float(ny)/2.0 - 0.5    ; coord 
     xy2ad, xx, yy, astr, ra, dec
     ;
     ; convert center to fits convention as coords
     ; are inserted to astr strcture
     astr.crpix[0] = xx + 1.0
     astr.crpix[1] = yy + 1.0
     astr.crval[0] = ra
     astr.crval[1] = dec
  ENDELSE 
  ;
  ; get x,y pixel sizes from differences at CRPIX1,2 with neighboring pixels
  plog,ll,prog,'determining pixel sizes'
  xx          = astr.crpix[0] - 1.0 + [0.0, 1.0, 0.0]
  yy          = astr.crpix[1] - 1.0 + [0.0, 0.0, 1.0]
  xy2ad, xx, yy, astr, ra, dec
  gcircd, 2, ra[0], dec[0], ra[1], dec[1], xpixsize
  gcircd, 2, ra[0], dec[0], ra[2], dec[2], ypixsize
  xpixsize    = 3600.0*xpixsize
  ypixsize    = 3600.0*ypixsize
  ;
  ; check offset between CRVAL1,2 in header and that computed by xy2ad
  gcircd, 2, astr.crval[0], astr.crval[1], ra[0], dec[0], dis
  dis         = dis*3600.0
  dis2        = dis/sqrt(xpixsize*ypixsize)
  plog,ll,prog,'offset between computed RA,DEC at CRPIX1,2 and CRVAL1,2 [arcsec] : '+numstr(dis)
  if dis2 gt 0.1 then plog,ll,prog,'**** warning this corresponds to an offset of '+numstr(dis2)+' pixels!!!'
  ;
  ; insert astrometry structure in header, and update pixel size
  plog,ll,prog,'updating WCS, and pixel sizes '
  putast, hdwk, astr, cd_type=2
  fxaddpar, hdwk, 'XPIXSIZE', xpixsize, 'Pixel size in X [arcsec]'
  fxaddpar, hdwk, 'YPIXSIZE', ypixsize, 'Pixel size in Y [arcsec]'
  ;
  ; add photometric keywords
  plog,ll,prog,'adding photometric keywords '
  ssoup_addphotkwds, bname, hdwk
  ;
  ; UV specific stuff
  if bname eq band.FUV or bname eq band.NUV then begin 
     plog,ll,prog,'updating UV specific keywords '
     ;
     ; concatenate date and time of observation
     dati = sxpar(hdwk, 'OBS-DATE', count=n1)
     uti  = sxpar(hdwk, 'TIME-OBS', count=n2)
     if n1 eq 1 and n2 eq 1 then begin 
        dstr =  strtrim(dati,2)+'T'+strtrim(uti,2)
        fxaddpar, hdwk, 'DATE-OBS', dstr, 'Date and time of observation'
     endif 
     if n2 eq 1 then fxaddpar, hdwk, 'UT', uti, 'Universal time of observation'
     ;
     ; change NADDED to NCOMBINE
     ncomb = sxpar(hdwk, 'NADDED', count=nn)
     if nn eq 1 then fxaddpar, hdwk, 'NCOMBINE', ncomb, 'Number of images combined'
     ;
     ; change RA_CENT, DEC_CENT to RA,DEC
     ra    = sxpar(hdwk, 'RA_CENT', count=n1)
     dec   = sxpar(hdwk, 'DEC_CENT', count=n2)
     if n1 eq 1 and n2 eq 1 then begin
        rastr  = degsexi(ra/15.0, prec=1)
        decstr = degsexi(dec,prec=0)
        fxaddpar, hdwk, 'RA', rastr[0], 'Right Ascencion of GALEX field center (J2000)'
        fxaddpar, hdwk, 'DEC', decstr[0], 'Declination of GALEX field center (J2000)'
     endif
     fxaddpar, hdwk, 'WCSDIM', 2
     fxaddpar, hdwk, 'WAT0_001', 'system=image'
     fxaddpar, hdwk, 'WAT1_001', 'wtype=tan axtype=ra'
     fxaddpar, hdwk, 'WAT2_001', 'wtype=tan axtype=dec'
  endif 
  ; wise specific stuff
  if bname eq band.mir_W1 or bname eq band.mir_W2 or bname eq band.mir_W3 or bname eq band.mir_W4 then begin
     ; change zero point
     fxaddpar, hdwk, "MAGZPT1", sxpar(hdwk, 'MAGZP', count=n1), "Magnitude zero point"
     fxaddpar, hdwk, "ERRZPT1", sxpar(hdwk, 'MAGZPUNC', count=n2), "Uncertainty in magzpt1"
     fxaddpar, hdwk, "PHOTPLAM", sxpar(hdwk, "WAVELEN", count=n3)*10000, "Filter pivot wavelength
  endif
  ;
  ; apply template header
  plog,ll,prog,'applying template header'
  hdr_template, hdwk, im, htempl[ptr], hdout
  ;
  ; delete blank lines
  plog,ll,prog,'removing blank lines'
  nch         = strlen(strtrim(hdout,2))
  jj          = where(nch gt 0, njj)
  if njj gt 0 then hdout = hdout[jj]
END 
