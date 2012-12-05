PRO test_checkblem
   ;
   ;  Check correspondence between objects in BLEM only analysis
   ;  and BLEM + AXE analysis.
   ;
   imdir = '/home/meurer/ACS/Grism/HDFN_jun04/Apsis/Work/'
   fdir  = '/home/meurer/text/ACS/Grism/Figures/'
   imseg = imdir + 'detectionImage_SEGM.fits'
   filbo = fdir + 'hdfn_blem_emsource.dat'
   filba = fdir + 'hdfn_blemaxe_dec04.cat'
   fmtbo = '(x,x,x,x,x,x,x,x,x,x,x,f,f,x,x,x,i)'
   fmtba = '(x,i)'
   ;
   ; read segmentation image
   fits_read, imseg, segm, hdr
   ;
   ; read blemonly file, we only care about x,y positions, old id
   readcol, filbo, ximbo, yimbo, oldid, format=fmtbo
   ;
   ; get unique sexids
   ii    = fix(ximbo - 0.5)
   jj    = fix(yimbo - 0.5)
   idbo  = segm[ii,jj]
   jj    = sort(idbo)
   temp  = idbo[jj]
   oldid = oldid[jj]
   ii    = uniq(temp)
   uidbo = temp[ii]
   oldid = oldid[ii]
   nbo   = n_elements(uidbo)
   ;
   ; get unique blemaxe sexids
   readcol, filba, idba, format=fmtba
   ii    = sort(idba)
   temp  = idba[ii]
   jj    = uniq(temp)
   uidba = temp[jj]
   nba   = n_elements(uidba)
   ;
   print, 'Number of unique sources in blem only  : ', nbo
   print, 'Number of unique sources in blemaxe    : ', nba
   ;
   ; Find which sources are missing from blemaxe
   FOR ii = 0, nbo-1 DO BEGIN 
      jj = where(uidba EQ uidbo[ii], njj)
      IF njj NE 1 THEN print, 'Missing / multiple source in blem only : ', uidbo[ii], oldid[ii], njj
   ENDFOR 
END 
