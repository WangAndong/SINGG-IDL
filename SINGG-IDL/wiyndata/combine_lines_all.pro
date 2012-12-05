PRO combine_lines_all
   filelist = 'exptimes.dat'
   format1  = '(a,a,a,a,i,f)'
   format2  = '(a,a,a,a,a,a,i,f)'
   folders  = ['Run0410_n','Run0503_n','Run0603_n']
   ;
   ; load list of filenames
   readcol, filelist, hipassid, galid1, galid2, filename, $
     nexp, texp, format=format1, /silent
   readcol, filelist, temphi, tempga1, tempga2, tempga3, tempga4, $
     tempfile, tempnexp, temptexp, format=format2, /silent
   hipassid = [hipassid, temphi]
   galid    = [galid1+' '+galid2, $
               tempga1+' '+tempga2+' '+tempga3+' '+tempga4]
   filename = [filename, tempfile]
   nfiles   = n_elements(filename)
   ;
   ; match filenames to folders
   fileno   = strmid(filename, 3)
   runno    = fix(strmid(fileno, 1, 1))
   nightno  = strmid(fileno, 3, 1)
   ;
   ; create strings to pass to combine_lines
   fgaus    = '../'+folders(runno-1)+nightno+'/gauss'+fileno+'.dat'
   ;
   ; run combine_lines for each file
   FOR ii=0, n_elements(fgaus)-1 DO BEGIN
       print, hipassid(ii)
       combine_lines, fgaus(ii)
   ENDFOR



END
