PRO sr2web_ddb_objpage, object, baseout, baseimg, basedata, baseplt, filo
   ;
   ; create a directory and web page for SR2 QA images 
   ; and information.  This routine is similar to 
   ; SR2WEB_OBJPAGE except that it works closely with the
   ; singg_derived database to determine file names (using the 
   ; function SINGG_DDB_FNAME).
   ;
   ; The singg_derived database must be opened for this routine to 
   ; work.
   ;
   ; object   -> target name.  This will also determine
   ;             the name of the directory that is made to
   ;             hold the images and web pages.
   ; baseout  -> base directory where all output directories 
   ;             web pages will be based.
   ; baseimg  -> base directory which contains all jpg and png
   ;             images that will be referenced in the web 
   ;             pages.
   ; basedata -> directory which contains all ascii data files.
   ; baseplt  -> directory which contains all line plots 
   ; filo     <- Output html file 
   ;
   ; G. Meurer 08/2007
   ;
   ; start up stuff
   prog       = 'sr2web_ddb_objpage'        ; name of program
   prog2      = strupcase(prog)+': '    ; for messages
   ;
   ; the following are the options for singg_ddb_fname corresponding to
   ; images (opti), line plots (optl), data files (optd), and 
   ; fits files (optf).
   opti       = ['rgood_o', 'rgood_s', 'rbad_o', 'rbad_s', 'netgood_o', 'netgood_s', $
                 'netbad_o', 'netbad_s', 'sqrt1png', 'sqrt3png', 'sqrt8png', $
                 'lin8png', 'log1png', 'apsjpg']
   optl       = ['skyprof_o', 'skyprof_s', 'cog_o', 'cog_s', 'sbprof_o', 'sbprof_s', $
                 'hispecplt', 'calibplt']
   optd       = ['calibcat', 'apmatch_o', 'dbcheck_o']
   optf       = ['rssfits', 'nbssfits', 'netssfits']
   ;
   ; convert these into indecis when option='all' is sent to 
   ; singg_ddb_fname
   option     = singg_ddb_fname(1, 'options')
   noi        = n_elements(opti)
   nol        = n_elements(optl)
   nod        = n_elements(optd)
   nof        = n_elements(optf)
   indi       = make_array(noi, /int, value=-1)
   indl       = make_array(nol, /int, value=-1)
   indd       = make_array(nod, /int, value=-1)
   indf       = make_array(nof, /int, value=-1)
   FOR ii = 0, noi-1 DO BEGIN 
      jj      = where(option EQ opti[ii], njj)
      IF njj EQ 1 THEN indi[ii] = jj[0] ELSE stop
   ENDFOR 
   FOR ii = 0, nol-1 DO BEGIN 
      jj      = where(option EQ optl[ii], njj)
      IF njj EQ 1 THEN indl[ii] = jj[0] ELSE stop
   ENDFOR 
   FOR ii = 0, nod-1 DO BEGIN 
      jj      = where(option EQ optd[ii], njj)
      IF njj EQ 1 THEN indd[ii] = jj[0] ELSE stop
   ENDFOR 
   FOR ii = 0, nof-1 DO BEGIN 
      jj      = where(option EQ optf[ii], njj)
      IF njj EQ 1 THEN indf[ii] = jj[0] ELSE stop
   ENDFOR 
   ;
   ; find entries for this hipass object, get essential items
   dbext, -1, 'entry,object,runid,filter_r,filter_n', entall, objall, runall, frall, fnall
   objall     = strtrim(objall,2)
   runall     = strtrim(runall,2)
   frall      = strtrim(frall,2)
   fnall      = strtrim(fnall,2)
   object     = strtrim(object)
   kk         = where(objall EQ object AND runall NE 'Run04s', nent) 
   ;
   ; If there is nothing to do, just return
   IF nent LE 0 THEN BEGIN 
      filo    = ''
      print, prog2, 'No sources with given object found in database'
      print, prog2, 'WARNING: No web page created'
      return
   ENDIF 
   list       = entall[kk]
   dbext, list, 'entry,object,name,runid,filter_r,filter_n,entry_hdr_s,mult_priority', $
          entry, objdb, name, runid, filtr, filtn, enths, mprior
   objdb      = strtrim(objdb,2)
   name       = strtrim(name,2)
   runid      = strtrim(runid,2)
   filtr      = strtrim(filtr,2)
   filtn      = strtrim(filtn,2)
   forprint, objdb+'  '+name+'  '+runid+'  '+filtr+'  '+filtn+'  ',enths,mprior
   ;
   ; determine and report number of image sets
   jj         = sort(enths)
   kk         = uniq(enths[jj])
   nset       = n_elements(kk)
   IF nset EQ 1l AND max(list[jj[kk]]) LE 0 THEN nset = 0
   pp         = jj[kk]
   enthsu     = enths[pp]
   print, prog2, 'There are '+strtrim(string(nset),2)+' image sets '+$
                 'of HIPASS object '+object
   ;
   ; If there is nothing to do, just return
   IF nset LE 0 THEN BEGIN 
      filo    = ''
      print, prog2, 'WARNING: No web page created'
      return
   ENDIF 
   ;
   ; go to base directory
   cd, baseout, current=cwd
   ;
   ; check if target directory is there.  
   ; * if so nuke it
   ; * otherwise or afterwards create clean directory having target name
   ; * then enter directory
   finfo      = file_info(object)
   IF finfo.exists THEN BEGIN 
      print, prog2+'Deleting old directory'
      file_delete, object, /recursive
   ENDIF 
   print, prog2+'creating directory, name = '+object
   file_mkdir, object
   cd, object
   ;
   ; determine summary items for each set these include:
   ; * mprior: the minimum mult_priority for each unique net image
   ; * nobjset: number of objects in the set 
   ; * runset: runid of thset
   ; * frset: R (continuum) filter for set
   ; * fnset: narowband filter for set
   ; * enthset: entry in header database for this observation set
   minprior = make_array(nset, /int, value=999)
   nobjset  = make_array(nset, /int, value=0)
   runset   = make_array(nset, /string, value='')
   frset    = make_array(nset, /string, value='')
   fnset    = make_array(nset, /string, value='')
   enthset  = make_array(nset, /long, value=-1l)
   FOR ii = 0, nset-1 DO BEGIN
      kk          = where(enths EQ enthsu[ii] AND objdb EQ object, nkk)
      ;print, kk
      IF nkk GT 0 THEN minprior[ii] = min(mprior[kk])
      nobjset[ii] = nkk
      runset[ii]  = runid[kk[0]]
      frset[ii]   = filtr[kk[0]]
      fnset[ii]   = filtn[kk[0]]
      enthset[ii] = enthsu[ii]
   ENDFOR 
   qq        = sort(minprior)
   runset    = temporary(runset[qq])
   minprior  = temporary(minprior[qq])
   nobjset   = temporary(nobjset[qq])
   frset     = temporary(frset[qq])
   fnset     = temporary(fnset[qq])
   enthset   = temporary(enthset[qq])
   ;
   ; create string for internal links
   linkset   = 'set'+strtrim(string(indgen(nset)+1),2)
   ;forprint,runset+'  '+frset+'  '+fnset+'  ', enthset, minprior, nobjset
   ;stop
   ;
   print, prog2+'Order for displaying results: '
   forprint,ljust(runset, 6)+ljust(frset,10)+ljust(fnset,10)+' ', minprior
   ;
   ; make list of files to link
   nott  = noi + nol + nod
   nff   = nott*nent
   fname = make_array(nff, /string, value='')
   pathi = make_array(nff, /string, value='')
   patht = [make_array(noi, /string, value=baseimg), $
            make_array(nol, /string, value=baseplt), $
            make_array(nod, /string, value=basedata)]
   k1    = 0
   FOR ii = 0, nent-1 DO BEGIN 
      ffil          = singg_ddb_fname(entry[ii], 'all', /nopath)
      k2            = k1 + nott - 1
      fname[k1:k2]  = [ffil[indi], ffil[indl], ffil[indd]]
      pathi[k1:k2]  = patht
      k1            = k2 + 1
   ENDFOR 
   ;
   ; find unique elements
   jj    = sort(pathi+fname)
   kk    = uniq(pathi[jj]+fname[jj])
   jj    = jj[kk]
   nfu   = n_elements(jj)
   ;forprint, pathi+fname
   ;
   ; do the symbolik links
   ;
   ; do the symbolik links
   FOR ii = 0, nfu - 1 DO BEGIN 
      ff0 = pathi[jj[ii]]+fname[jj[ii]]
      ff1 = fname[jj[ii]]
      print, prog2+'creating symbolic link from '+ff0+'  to '+ff1
      file_link, ff0, ff1
      finfo   = file_info(ff1)
      IF finfo.exists THEN BEGIN
         IF NOT finfo.dangling_symlink THEN BEGIN 
            IF NOT finfo.symlink THEN print, prog2+'ERROR - file not a symbolic link (WTF?) : '+ff1
         ENDIF ELSE BEGIN 
            print, prog2+'WARNING - symbolic link is dangling : '+ff1
         ENDELSE 
      ENDIF ELSE BEGIN 
         print, prog2+'ERROR - file does not exist : '+ff1
      ENDELSE
   ENDFOR
   ;
   ; open web page, write top of it
   filo = object+'_sr2qa.html'
   print,prog2+'opening file : '+filo
   openw, lu, filo, /get_lun
   print,prog2+'starting to write markup'
   singg_pagetop, lu, object, 'SINGG Release 2 Quality Assurance page'
   printf,lu,'<hr>'
   printf,lu,'<h3><a name="summary">Object Summary</a></h3>'
   ;
   ; (markup gsc annotated image)
   ;
   ; write markup for HI spectrum
   fhispec = singg_ddb_fname(entry[0], 'hispecplt', /nopath)
   print,prog2+'marking up HI spectrum'
   printf,lu,'<p>HI spectrum:<br>'
   printf,lu,'<img src="'+fhispec+'"></p>'
   ;
   ; format and write summary table
   print,prog2+'marking up summary table'
   printf,lu,'<P><TABLE border=1 cellpadding=3>'
   printf,lu,'<tr>'
   printf,lu,'  <th>set</th>'
   printf,lu,'  <th>Run</th>'
   printf,lu,'  <th>Continuum<br>filter</th>'
   printf,lu,'  <th>Narrow Band<br>filter</th>'
   printf,lu,'  <th>Number of<br>SINGG sources</th>'
   printf,lu,'  <th>priority<br>code</th>'
   printf,lu,'</tr>'
   ;
   FOR kk = 0, nset-1 DO BEGIN 
      printf,lu,'<tr>'
      printf,lu,'  <td align="center"><a href="#'+linkset[kk]+'">'+strtrim(string(kk+1),2)+'</a></td>'
      printf,lu,'  <td align="center">'+runset[kk]+'</td>'
      printf,lu,'  <td align="center">'+frset[kk]+'</td>'
      printf,lu,'  <td align="center">'+fnset[kk]+'</td>'
      printf,lu,'  <td align="center">'+strtrim(nobjset[kk],2)+'</td>'
      printf,lu,'  <td align="center">'+strtrim(minprior[kk],2)+'</td>'
      printf,lu,'</tr>'
   ENDFOR 
   printf,lu,'</table></p>'
   ;
   ; loop through image sets
   FOR kk = 0, nset-1 DO BEGIN 
      sset = strtrim(string(kk+1),2)
      print,prog2+'marking up set #'+sset
      ;
      ; write hrule, internal link, start of set
      printf, lu, '<hr>'
      printf, lu, '<p><a name='+linkset[kk]+'>Set #'+sset+'</a>: '+runset[kk]+'<br>'
      printf, lu, 'R/Continuum filter = '+frset[kk]+'<br>'
      printf, lu, 'Narrow band filter = '+fnset[kk]+'<br>'
      printf, lu, 'Priority code      = '+strtrim(minprior[kk],2)+'</br></p>'
      ;
      ; find database entries corresponding to this set
      ii   =  where(objall EQ object AND runall EQ runset[kk] AND frall EQ frset[kk] AND fnall EQ fnset[kk], nentset)
      IF nentset LE 0 THEN stop, prog2+"ERROR: can't find anything to markup"
      entset = entall[ii]
      ;
      ; start table
      printf, lu, '<p><table border=1 celpadding=3>'
      ;printf, lu, '<tr><td align=center>Hi</td><td align=center>guy</td></tr>'  ; place holder...
      ;
      ; markup aperture images, and links to images at different stretches
      faps   = singg_ddb_fname(entset[0], 'apsjpg', /nopath)
      fsqrt1 = singg_ddb_fname(entset[0], 'sqrt1png', /nopath)
      fsqrt3 = singg_ddb_fname(entset[0], 'sqrt3png', /nopath)
      fsqrt8 = singg_ddb_fname(entset[0], 'sqrt8png', /nopath)
      flin8  = singg_ddb_fname(entset[0], 'lin8png', /nopath)
      flog1  = singg_ddb_fname(entset[0], 'log1png', /nopath)
      printf, lu, '<tr><td align=center colspan=2>aperture plot:<br>'
      printf, lu, '    <a href="'+faps+'"><img src="'+faps+'"></a><br>'
      printf, lu, '     PNG images: <a href="'+fsqrt1+'">sqrt1</a>, <a href="'+fsqrt3+'">sqrt3</a>, '+$
                  ' <a href="'+fsqrt8+'">sqrt8</a>,<a href="'+flin8+'">lin8</a>, <a href="'+flog1+'">log1</a> </td></tr>'
      ;
      ; markup catalog map and calibrated catalog
      fpltcal = singg_ddb_fname(entset[0], 'calibplt', /nopath)
      fcatcal = singg_ddb_fname(entset[0], 'calibcat', /nopath)
      printf, lu, '<tr><td align=center colspan=2>SE catalog map:<br>'
      printf, lu, '    <a href="'+fpltcal+'"><img src="'+fpltcal+'"></a><br>'
      printf, lu, '     Calibrated SE catalog: <a href="'+fcatcal+'">fcatcal</a></td></tr>'
      ;
      ; markup apmatch table, as a table within a table
      fapmatch = singg_ddb_fname(entset[0], 'apmatch_o', /nopath)
      fdbmatch = singg_ddb_fname(entset[0], 'dbcheck_o', /nopath)
      printf, lu, '<tr><td align=center colspan=2>Aperture match results:<br>'
      sr2web_apmatch2html, lu, fapmatch
      printf, lu, '<br>ASCII files- Aperture match: <a href="'+fapmatch+'">'+fapmatch+$
                  '</a> Database check: <a href="'+fdbmatch+'">'+fdbmatch+'</a></td></tr>'
      ;
      ; markup good/bad pixel thumbnails/links
      frgoodo  = singg_ddb_fname(entset[0], 'rgood_o', /nopath)
      frbado   = singg_ddb_fname(entset[0], 'rbad_o', /nopath)
      frgoods  = singg_ddb_fname(entset[0], 'rgood_s', /nopath)
      frbads   = singg_ddb_fname(entset[0], 'rbad_s', /nopath)
      fngoodo  = singg_ddb_fname(entset[0], 'netgood_o', /nopath)
      fnbado   = singg_ddb_fname(entset[0], 'netbad_o', /nopath)
      fngoods  = singg_ddb_fname(entset[0], 'netgood_s', /nopath)
      fnbads   = singg_ddb_fname(entset[0], 'netbad_s', /nopath)
      printf, lu, '<tr><td align=center>R included pixel map:<br>'
      printf, lu, '    <a href="'+frgoodo+'"><img src="'+frgoods+'"></a></td>'
      printf, lu, '    <td align=center>R excluded pixel map:<br>'
      printf, lu, '    <a href="'+frbado+'"><img src="'+frbads+'"></a></td></tr>'
      printf, lu, '<tr><td align=center>Net included pixel map:<br>'
      printf, lu, '    <a href="'+fngoodo+'"><img src="'+fngoods+'"></a></td>'
      printf, lu, '    <td align=center>Net excluded pixel map:<br>'
      printf, lu, '    <a href="'+fnbado+'"><img src="'+fnbads+'"></a></td></tr>'
      ;
      ; markup sky profile
      fskyprof = singg_ddb_fname(entset[0], 'skyprof_o', /nopath)
      fskystmp = singg_ddb_fname(entset[0], 'skyprof_s', /nopath)
      printf, lu, '<tr><td align=center colspan=2>Sky profile:<br>'
      printf, lu, '    <a href="'+fskyprof+'"><img src="'+fskystmp+'"></a><br></td></tr>'
      ;
      ; loop through objects in this dataset
      FOR jj = 0, nentset-1 DO BEGIN 
         ;
         ; get source name
         dbext, entset[jj], 'name', sname
         sname    = strtrim(sname,2)
         ;
         ; markup cog and surface brightness profile stamps with links
         fcogo    = singg_ddb_fname(entset[jj], 'cog_o', /nopath)
         fcogs    = singg_ddb_fname(entset[jj], 'cog_s', /nopath)
         fsbprofo = singg_ddb_fname(entset[jj], 'sbprof_o', /nopath)
         fsbprofs = singg_ddb_fname(entset[jj], 'sbprof_s', /nopath)
         printf, lu, '<tr><td align=center>'+sname+' curves of growth:<br>'
         printf, lu, '    <a href="'+fcogo+'"><img src="'+fcogs+'"></a></td>'
         printf, lu, '    <td align=center>'+sname+' surface brightness profiles:<br>'
         printf, lu, '    <a href="'+fsbprofo+'"><img src="'+fsbprofs+'"></a></td></tr>'
      ENDFOR 
      ;
      ; End table
      printf, lu, '</table></p>'
   ENDFOR 
   ;
   ;
   ; write bottom of file, & close
   print,prog2+'Finishing off html file '
   printf, lu, '<hr>'
   singg_pagebot, lu, prog=prog
   free_lun, lu
   ;
   ; link this file as index.html for this directory
   print,prog2+'linking '+filo+' to index.html'
   file_link,filo,'index.html'
   ;
   ; append output file name to directory name to give 
   ; final output file name
   filo = object+'/'+filo
   ;
   ; go back to directory at entry
   print,prog2+'returning to '+cwd
   cd, cwd
   ;
END 
