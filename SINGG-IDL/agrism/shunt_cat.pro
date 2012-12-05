PRO shunt_cat, fsquash, awkstr, fcat, fecat, ffilt, fsegm, fback, verbose=verbose
   ;
   ; catalog images made for shunt using SExtractor
   ;
   ; fsquash -> name of squashed grism image
   ; awkstr  -> awk command string to edit SE catalog 
   ; fcat    <- name of output SE catalog
   ; fecat   <- name of output SE catalog edited to retain
   ;            just the compact sources that are not too elongated.
   ; ffilt   <- name of output filtered image
   ; fsegm   <- name of output segmentation image
   ; fback   <- name of output background image
   ;
   ; G. Meurer 09/2005
   ;
   ; find root of input file name, and derive output files
   kk = strpos(fsquash, '.fits')
   IF kk EQ -1 THEN root = fsquash ELSE root = strmid(fsquash, 0, kk)
   fili     =  root + '.fits'
   fsegm    =  root + '_segm.fits'
   ffilt    =  root + '_filtered.fits'
   fback    =  root + '_back.fits'
   fcat     =  root + '.cat'
   fecat    =  root + '_ed.cat'
   ;
   ; derive the command to spawn
   cmd      =  'sex '+fili+' -c firstord_finder.inpar -checkimage_type FILTERED,SEGMENTATION,BACKGROUND '+$
               ' -checkimage_name '+ffilt+','+fsegm+','+fback+' -catalog_name '+fcat
   IF keyword_set(verbose) THEN BEGIN 
      print, 'SHUNT_CAT: running SE with command: '
      print, cmd
   ENDIF 
   spawn, cmd
   ;
   ; derive awk command for editing catalog to recover just the small things
   cmd = "awk '"+awkstr+"' "+fcat+" > "+fecat
   IF keyword_set(verbose) THEN BEGIN 
      print, 'SHUNT_CAT: editing catalog with awk command: '
      print, cmd
   ENDIF 
   spawn, cmd
END 
