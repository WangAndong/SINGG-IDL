PRO sr2web_objpage, targ, baseout, baseimg, basedata, sufpng, fnet, fcnt, fnb, fcat, $
                    rootlong, rootshrt, runid, filt_cnt, filt_nb, filo
   ;
   ; create a directory and web page for SR2 QA images 
   ; and information.
   ;
   ; targ     -> target name.  This will also determine
   ;             the name of the directory that is made to
   ;             hold the images and web pages.
   ; baseout  -> base directory where all output directories 
   ;             web pages will be based.
   ; baseimg  -> base directory which contains all jpg and png
   ;             images that will be referenced in the web 
   ;             pages.
   ; basedata -> base directory which contains all ascii data files.
   ; sufpng   -> suffix for png images
   ; fnet     -> (array of) net images of the target
   ; fcnt     -> (array of) corresponding continuum images
   ; fnb      -> (array of) corresponding narrow band images
   ; fcat     -> (array of) calibrated catalog file names.
   ; rootlong -> (array of) root name for the aperture jpg, and 
   ;             out files referenced in the page.
   ; rootshrt -> (array of) root names for the png images referenced 
   ;             in the page.
   ; runid    -> (array of) 2-3 digit run identification string
   ; filt_cnt -> (array of) continuum filter names
   ; filt_nb  -> (array of) narrow band filter names
   ; filo     <- Output html file 
   ;
   ; G. Meurer  12/2006
   ;
   ; start up stuff
   nf         = n_elements(fnet)
   prog       = 'sr2web_objpage'
   prog2      = strupcase(prog)+': '
   print, prog2+'starting work on target = '+targ+' Number of image sets = ', nf
   ;
   ; go to base directory
   cd, baseout, current=cwd
   ;
   ; check if target directory is there.  
   ; * if so nuke it
   ; * otherwise or afterwards create clean directory having target name
   ; * then enter directory
   finfo      = file_info(targ)
   IF finfo.exists THEN BEGIN 
      print, prog2+'Deleting old directory'
      file_delete, targ, /recursive
   ENDIF 
   print, prog2+'creating directory, name = '+targ
   file_mkdir, targ
   cd, targ
   ;
   ; make list of files to link
   ns         = n_elements(sufpng)
   nl         = nf*(ns + 2)
   file       = make_array(nl, /string, value='')
   kk         = 0
   FOR jj = 0, nf-1 DO BEGIN 
      ;
      FOR ii = 0, ns-1 DO BEGIN 
         file[kk] = rootshrt[jj]+'_'+sufpng[ii]+'.png'
         kk = kk + 1
      ENDFOR 
      file[kk]    = rootshrt[jj] + '_aps.jpg'
      kk          = kk + 1
      file[kk]    = rootlong[jj] + '_cat_dbcheck.jpg'
      kk          = kk + 1
   ENDFOR 
   nl         = nl + nf
   fili       = [baseimg+file, basedata+fcat]
   file       = [file, fcat]
   ;
   ; do the symbolik links
   FOR ii = 0, nl - 1 DO BEGIN 
      print, prog2+'creating symbolic link from '+fili[ii]+'  to '+file[ii]
      file_link, fili[ii], file[ii]
      finfo   = file_info(file[ii])
      IF finfo.exists THEN BEGIN
         IF NOT finfo.dangling_symlink THEN BEGIN 
            IF finfo.symlink THEN BEGIN 
               print, prog2+'symbolic link created'
            ENDIF ELSE BEGIN
               print, prog2+'ERROR - file not a symbolic link (WTF?) : '+file[ii]
            ENDELSE 
         ENDIF ELSE BEGIN 
            print, prog2+'WARNING - symbolic link is dangling : '+file[ii]
         ENDELSE 
      ENDIF ELSE BEGIN 
         print, prog2+'ERROR - file does not exist : '+file[ii]
      ENDELSE
   ENDFOR
   ;
   ; open web page, write top of it
   filo = targ+'_sr2qa.html'
   print,prog2+'opening file : '+filo
   openw, lu, filo, /get_lun
   singg_pagetop, lu, targ, 'SINGG Release 2 Quality Assurance page'
   ;
   ; (format and write summary table)
   ;
   ; loop through image sets
   ;
   FOR jj = 0, nf-1 DO BEGIN 
      ;
      print,prog2+'marking up image set summary : '+strtrim(string(jj+1),2)
      IF nf GT 0 THEN printf, lu, '<hr>'
      ;
      ; Basic information on this data set
      printf,lu,'<H3>Image set #'+strtrim(string(jj+1),2)+'</H3>'
      printf,lu,'<P>'
      printf,lu,'Target             = '+targ+' <br>'
      printf,lu,'run                = Run'+runid[jj]+' <br>'
      printf,lu,'R/continuum filter = '+filt_cnt[jj]+' <br>'
      printf,lu,'narrow band filter = '+filt_nb[jj] +' <br>'
      printf,lu,'Merged & calibrated SE catalog = <a href="'+fcat[jj]+'">'+fcat[jj]+'</a> <br>'
      printf,lu,'</P>'
      ; 
      ; markup links to png images
      print,prog2+'marking up preview images'
      printf,lu,'<P>'
      str = 'Three color preview images: '
      FOR ii = 0, ns-1 DO BEGIN 
         file = rootshrt[jj]+'_'+sufpng[ii]+'.png'
         IF ii GT 0 THEN str = str + ', '
         str = str + '<a href="'+file+'">'+sufpng[ii]+'</a>'
      ENDFOR
      str = str + ' <br>'
      printf,lu,str
      printf,lu, 'These images use '+ $
                'red=<font color="#FF0000">'+fnet[jj]+'</font>'+ $
                ', green=<font color="#00FF00">'+fnb[jj]+'</font>'+ $
                ', blue=<font color="#0000FF">'+fcnt[jj]+'</font> : '
      printf,lu,'</P>'
      ;
      ; (markup links to gray scale images)
      ;
      ; (markup links to predecessor images)
      ;
      ; markup aperture map and finder chart
      print,prog2+'marking up aperture and dbcheck images'
      printf,lu,'<P><TABLE border=1 cellpadding=3>'
      printf,lu,'<tr>'
      printf,lu,'  <td><img src="'+rootshrt[jj]+'_aps.jpg'+'"></td>'
      printf,lu,'  <td><img src="'+rootlong[jj]+'_cat_dbcheck.jpg'+'"></td>'
      printf,lu,'</tr>'
      printf,lu,'<tr>'
      printf,lu,'  <td align="center">Apertures</td>'
      printf,lu,'  <td align="center">finder chart</td>'
      printf,lu,'</tr>'
      ;
      ; markup dbmatch object table (as a sub-table in page)
      printf,lu,'<tr>'
      printf,lu,'<td align="center" valign="middle">'
      fdbcheck = basedata+rootlong[jj]+'_dbcheck.out'
      sr2web_dbcheck2html, lu, fdbcheck
      printf,lu,'</td>'
      ;
      ; markup aperture match table  (as a sub-table in page)
      printf,lu,'<td align="center" valign="middle">'
      fapmatch = basedata+rootlong[jj]+'_apmatch.out'
      sr2web_apmatch2html, lu, fapmatch
      printf,lu,'</td></tr>'
      printf,lu,'</P></TABLE>'
      ;
      ; (markup links to inclusion masks for each source in this image)
      ;
      ; (markup links to radial profile images and data)
      ;
      ; (markup links to SExtractor products)
   ENDFOR 
   ;
   ; write bottom of file, & close
   print,prog2+'Finishing off html file '
   singg_pagebot, lu, prog=prog
   free_lun, lu
   ;
   ; link this file as index.html for this directory
   print,prog2+'linking '+filo+' to index.html'
   file_link,filo,'index.html'
   ;
   ; append output file name to directory name to give 
   ; final output file name
   filo = targ+'/'+filo
   ;
   ; go back to directory at entry
   print,prog2+'returning to '+cwd
   cd, cwd
   ;
   print,prog2+'finished.'
END 
