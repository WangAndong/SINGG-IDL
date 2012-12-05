PRO wfc_ff, infile, outfile, ff, verbose=verbose
   ;
   ; Apply a flatfield to an ACS WFC image
   ;
   ; infile  -> input file name
   ; outfile -> output file name
   ; ff      -> flat field file name.
   ;
   ; This program does minimimum checking flatfielding of images.
   ; The following improvements would result in a more robust 
   ; programming and better documented output products:
   ; * loop through CCDCHIP rather than EXTVER
   ; * properly propogate DQ arrays
   ; * update FILENAME in output primary header
   ; * update PFLTFILE
   ; * add HISTORY lines
   ;
   ; G. Meurer 6/2004
   fits_open, infile, fcbi
   IF fcbi.nextend NE 6 THEN stop, 'infile does not have 6 extensions'
   
   fits_open, ff, fcbf
   IF fcbf.nextend NE 6 THEN stop, 'ff does not have 6 extensions'
   vb = keyword_set(verbose)
   ;
   ; open output file, copy primary header after updating to new FILENAME
   fits_open, outfile, fcbo, /write
   fits_read,  fcbi, dum, hdri, exten_no=0
   sxaddpar, hdri, 'FILENAME', outfile, /pdu
   fits_write, fcbo, dum, hdri
   ;
   ; loop through extension versions
   FOR extver = 1, 2 DO BEGIN 
      IF vb THEN print, 'Working on extension version = ', extver
      IF vb THEN print, 'reading infile,ff sci arrays ... '
      fits_read, fcbi, imi, hsci, extname='SCI', extver=extver
      fits_read, fcbf, imf, hscf, extname='SCI', extver=extver
      IF vb THEN print, 'reading infile,ff err arrays ... '
      fits_read, fcbi, eri, heri, extname='ERR', extver=extver
      fits_read, fcbf, erf, herf, extname='ERR', extver=extver
      IF vb THEN print, 'Processing sci & err arrays ... '
      eri = sqrt((eri/imi)^2 + (erf/imf)^2)
      imi = imi / imf
      eri = eri * imi
      ;
      ; write output sci, err arrays
      IF vb THEN print, 'writing sci & err 1 arrays ... '
      fits_write, fcbo, imi, hsci, extname='SCI', extver=extver
      fits_write, fcbo, eri, heri, extname='ERR', extver=extver
      ;
      ; process DQ arrays
      IF vb THEN print, 'copying DQ array ...'
      fits_read,  fcbi, imi, hdqi, extname='DQ', extver=extver
      fits_write, fcbo, imi, hdqi, extname='DQ', extver=extver
   ENDFOR 
   ;
   ; close fits files
   fits_close, fcbi
   fits_close, fcbf
   fits_close, fcbo
END 
