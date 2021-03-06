PRO plot_all_fields, ps=ps, folder=folder, $
                     filestart=filestart, drawbg=drawbg, noempty=noempty
   IF keyword_set(ps) THEN BEGIN
       outfolder = 'ps'
       extension = '.ps'
   ENDIF ELSE BEGIN
       outfolder = 'jpg'
       extension = '.jpg'
   ENDELSE
   IF keyword_set(folder) THEN outfolder=folder
   IF NOT(keyword_set(filestart)) THEN filestart='field'
   linenames = ['[NII] 6548','!6H!7a!6 6563','[NII] 6584',$
                'HeI 6678','[SII] 6716','[SII] 6731']
   nlines   = n_elements(linenames)
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
   ; create strings to pass to plot_field
   fgaus    = '../'+folders(runno-1)+nightno+'/gauss'+fileno+'.dat'
   outfile  = make_array(nfiles, nlines, /string)
   title    = outfile
   FOR ii=0, nlines-1 DO BEGIN
       outfile(*,ii) = outfolder+'/'+filestart+fileno+'_'+$
         strcompress(string(ii+1),/remove_all)+extension
       title(*,ii)   = hipassid+' ('+galid+') '+linenames(ii)
   ENDFOR
   ;
   ; run plot_field for each file
   FOR ii=0, n_elements(fgaus)-1 DO BEGIN
       print, hipassid(ii)
       IF keyword_set(noempty) THEN BEGIN
           IF keyword_set(drawbg) THEN BEGIN
               IF keyword_set(ps) THEN BEGIN
                   FOR line=0, nlines-1 DO plot_field, fgaus(ii), line+1, $
                     hardfile=outfile(ii,line), title=title(ii,line), $
                     background=hipassid(ii), /noempty
               ENDIF ELSE BEGIN
                   FOR line=0, nlines-1 DO plot_field, fgaus(ii), line+1, $
                     jpg=outfile(ii,line), title=title(ii,line), $
                     background=hipassid(ii), /noempty
               ENDELSE
           ENDIF ELSE BEGIN
               IF keyword_set(ps) THEN BEGIN
                   FOR line=0, nlines-1 DO plot_field, fgaus(ii), line+1, $
                     hardfile=outfile(ii,line), title=title(ii,line), /noempty
               ENDIF ELSE BEGIN
                   FOR line=0, nlines-1 DO plot_field, fgaus(ii), line+1, $
                     jpg=outfile(ii,line), title=title(ii,line), /noempty
               ENDELSE
           ENDELSE 
       ENDIF ELSE BEGIN
           IF keyword_set(drawbg) THEN BEGIN
               IF keyword_set(ps) THEN BEGIN
                   FOR line=0, nlines-1 DO plot_field, fgaus(ii), line+1, $
                     hardfile=outfile(ii,line), title=title(ii,line), $
                     background=hipassid(ii)
               ENDIF ELSE BEGIN
                   FOR line=0, nlines-1 DO plot_field, fgaus(ii), line+1, $
                     jpg=outfile(ii,line), title=title(ii,line), $
                     background=hipassid(ii)
               ENDELSE
           ENDIF ELSE BEGIN
               IF keyword_set(ps) THEN BEGIN
                   FOR line=0, nlines-1 DO plot_field, fgaus(ii), line+1, $
                     hardfile=outfile(ii,line), title=title(ii,line)
               ENDIF ELSE BEGIN
                   FOR line=0, nlines-1 DO plot_field, fgaus(ii), line+1, $
                     jpg=outfile(ii,line), title=title(ii,line)
               ENDELSE
           ENDELSE 
       ENDELSE 
   ENDFOR
   

END
