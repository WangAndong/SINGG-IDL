PRO test_singg_ddb_fname, optcheck=optcheck
   ;
   ; test singg_ddb_fname by checking the existence of all input 
   ; file names
   ;
   ; optcheck -> if set a string array of options or substrings
   ;             of options to check file names
   ;
   ; G. Meurer 07/2007
   ;
   ddb     = 'singg_derived'
   prog    = 'test_singg_ddb_fname'
   ;
   pstr    = strupcase(prog)+': '
   ostr    = ['cog_o']
   ostr    = ['skyprof_i', 'cog_i', 'sbprof_i', 'haps_i', 'rgood_i', 'rbad_i', 'netgood_i', 'netbad_i', $
              'skyprof_o', 'cog_o', 'sbprof_o', 'haps_o', 'rgood_o', 'rbad_o', 'netgood_o', 'netbad_o', $
              'skyprof_s', 'cog_s', 'sbprof_s', 'haps_s', 'rgood_s', 'rbad_s', 'netgood_s', 'netbad_s', $
              'rssfits', 'nbssfits', 'netssfits', 'rplfits', 'nbplfits', 'netplfits', 'rplfitsgz', $
              'nbplfitsgz', 'netplfitsgz', 'rootpng', 'rsecat', 'nbsecat', 'netsecat', 'calibcat', $
              'calibplt', 'hispecplt', 'apsjpg', 'sqrt1png', 'sqrt3png', 'sqrt8png', 'lin1png', $ 
              'lin3png', 'lin8png', 'log1png', 'log3png', 'log8png', 'apmatch_i', 'dbcheck_i', $
              'apmatch_o', 'dbcheck_o']
   IF keyword_set(optcheck) THEN ostr = optcheck
   ;
   ; open database, get all entries
   dbopen, ddb
   dbext, -1, 'entry', entry
   IF entry[0] LE 0 THEN stop, pstr+'There are no entries in the database'
   nent    = n_elements(entry)
   ;
   ; find out which options refer to input files
   opt     = singg_ddb_fname(entry[0], 'options')
   nopt    = n_elements(opt)
   nch     = n_elements(ostr)
   pp      = make_array(nch, nopt, /long, value=-1l)
   FOR ii = 0, nch-1 DO BEGIN
      qq       = strpos(opt, ostr[ii])
      pp[ii,*] = qq
   ENDFOR 
   ;
   FOR ii = 0, nopt-1 DO qq[ii] = max(pp[*,ii])
   kk      = where(qq GE 0, nkk)
   forprint, opt[kk]
   ;
   ; create array for file names
   nf      = nent*nkk
   fili    = make_array(nf, /string, value='')
   print, pstr+'will generate a total of '+strtrim(string(nf),2)+' file names'
   ;
   ; loop through entries and build up fili array
   FOR ii = 0, nent-1 DO BEGIN 
      file        = singg_ddb_fname(entry[ii], 'all')
      k1          = ii*nkk
      k2          = (ii+1l)*nkk-1l
      fili[k1:k2] = file[kk]
   ENDFOR 
   ;
   ; determine where input files are unique and trim fili array
   kk      = sort(fili)
   jj      = uniq(fili[kk])
   kk      = kk[jj]
   fili    = fili[kk]
   nu      = n_elements(fili)
   stati   = make_array(nu, /byte, value=0b) ; for status of input files
   print, pstr+'will check the status of '+strtrim(string(nu),2)+' unique files'
   ;
   ; check existence of input files
   FOR ii = 0l, nu-1l DO BEGIN
      finfo = file_info(fili[ii])
      stati[ii] = finfo.exists
   ENDFOR 
   ;
   ; report stats of existence
   kg      = where(stati EQ 1b, nkg)
   kb      = where(stati NE 1b, nkb)
   print, pstr+'Number of files that exist = '+strtrim(string(nkg),2)
   print, pstr+'Number of files not found  = '+strtrim(string(nkb),2)
   ;
   ; report non-existent files (if any)
   IF nkb GT 0 THEN BEGIN 
      print, pstr+'The following files could not be found: '
      forprint, fili[kb]
   ENDIF 
   ;
   dbclose
END 
