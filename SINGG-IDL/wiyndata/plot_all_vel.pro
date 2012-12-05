PRO plot_all_vel, ps=ps, folder=folder, all=all, filestart=filestart, $
                  drawbg=drawbg, noempty=noempty

   IF keyword_set(ps) THEN BEGIN
       outfolder = 'ps'
       extension = '.ps'
   ENDIF ELSE BEGIN
       outfolder = 'jpg'
       extension = '.jpg'
   ENDELSE
   IF keyword_set(folder) THEN outfolder=folder
   IF NOT(keyword_set(filestart)) THEN filestart='vel'
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
   fcomb    = '../'+folders(runno-1)+nightno+'/comb'+fileno+'.dat'
   outfile  = outfolder+'/'+filestart+fileno+extension
   title    = hipassid+' ('+galid+')'
   ;
   ; run plot_field for each file
   FOR ii=0, n_elements(fcomb)-1 DO BEGIN
       print, hipassid(ii)
       IF keyword_set(noempty) THEN BEGIN
           IF keyword_set(drawbg) THEN BEGIN
               IF keyword_set(all) THEN BEGIN
                   IF keyword_set(ps) THEN plot_vel_field, fcomb(ii), /all, $
                     hardfile=outfile(ii), title=title(ii), background=hipassid(ii), $
                     /noempty $
                   ELSE plot_vel_field, fcomb(ii), /all, $
                     jpg=outfile(ii), title=title(ii), background=hipassid(ii), $
                     /noempty
               ENDIF ELSE BEGIN
                   IF keyword_set(ps) THEN plot_vel_field, fcomb(ii), $
                     hardfile=outfile(ii), title=title(ii), background=hipassid(ii), $
                     /noempty $
                   ELSE plot_vel_field, fcomb(ii), $
                     jpg=outfile(ii), title=title(ii), background=hipassid(ii), $
                     /noempty
               ENDELSE 
           ENDIF ELSE BEGIN
               IF keyword_set(all) THEN BEGIN
                   IF keyword_set(ps) THEN plot_vel_field, fcomb(ii), /all, $
                     hardfile=outfile(ii), title=title(ii), /noempty $
                   ELSE plot_vel_field, fcomb(ii), /all, $
                     jpg=outfile(ii), title=title(ii), /noempty
               ENDIF ELSE BEGIN
                   IF keyword_set(ps) THEN plot_vel_field, fcomb(ii), $
                     hardfile=outfile(ii), title=title(ii), /noempty $
                   ELSE plot_vel_field, fcomb(ii), $
                     jpg=outfile(ii), title=title(ii), /noempty
               ENDELSE 
           ENDELSE 
       ENDIF ELSE BEGIN
           IF keyword_set(drawbg) THEN BEGIN
               IF keyword_set(all) THEN BEGIN
                   IF keyword_set(ps) THEN plot_vel_field, fcomb(ii), /all, $
                     hardfile=outfile(ii), title=title(ii), background=hipassid(ii) $
                   ELSE plot_vel_field, fcomb(ii), /all, $
                     jpg=outfile(ii), title=title(ii), background=hipassid(ii)
               ENDIF ELSE BEGIN
                   IF keyword_set(ps) THEN plot_vel_field, fcomb(ii), $
                     hardfile=outfile(ii), title=title(ii), background=hipassid(ii) $
                   ELSE plot_vel_field, fcomb(ii), $
                     jpg=outfile(ii), title=title(ii), background=hipassid(ii)
               ENDELSE 
           ENDIF ELSE BEGIN
               IF keyword_set(all) THEN BEGIN
                   IF keyword_set(ps) THEN plot_vel_field, fcomb(ii), /all, $
                     hardfile=outfile(ii), title=title(ii) $
                   ELSE plot_vel_field, fcomb(ii), /all, $
                     jpg=outfile(ii), title=title(ii)
               ENDIF ELSE BEGIN
                   IF keyword_set(ps) THEN plot_vel_field, fcomb(ii), $
                     hardfile=outfile(ii), title=title(ii) $
                   ELSE plot_vel_field, fcomb(ii), $
                     jpg=outfile(ii), title=title(ii)
               ENDELSE 
           ENDELSE 
       ENDELSE
   ENDFOR


END
