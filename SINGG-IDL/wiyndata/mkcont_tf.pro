PRO mkcont_tf

   filelist = 'exptimes.dat'
   format1  = '(a,a,a,i,f)'
   ; format2  = '(a,a,a,a,a,a,i,f)'
   ; folders  = ['Run0410_n','Run0503_n','Run0603_n']
   ;
   ; load list of filenames
   readcol, filelist, hipassid, galid, filename, nexp, texp, format=format1
   ;readcol, filelist, temphi, tempga1, tempga2, tempga3, tempga4, $
   ;  tempfile, tempnexp, temptexp, format=format2
   ; hipassid = [hipassid, temphi]
   ; filename = [filename, tempfile]
   nfiles   = n_elements(filename)
   ;
   ; match filenames to folders
   ;fileno   = strmid(filename, 3)
   ;runno    = fix(strmid(fileno, 1, 1))
   ;nightno  = strmid(fileno, 3, 1)
   ;
   ; create filename strings for input and output files
   ;cofile   = '../'+folders(runno-1)+nightno+'/'+filename+'.co.cr.ms.fits'
   ;fullfile = '../'+folders(runno-1)+nightno+'/Redundant/'+filename+'.cr.ms.fits'
   ;contfile = '../'+folders(runno-1)+nightno+'/Redundant/cont'+fileno+'.cr.ms.fits'

   fullfile = filename+'.extr.fits'
   cofile   = filename+'.con.fits'
   contfile = filename+'.cont.fits'

   FOR ii = 0, nfiles-1 DO BEGIN
       obj    = readfits(cofile(ii), header)
       full   = readfits(fullfile(ii))
       cont   = full - obj
       writefits, contfile(ii), cont, header
   ENDFOR

END
