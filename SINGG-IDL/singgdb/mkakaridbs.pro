PRO mkakaridbs
   ;
   ; Make Akari FIS and IRC databases
   ;
   ; G. Meurer (ICRAR/UWA) 10/2011
   logstr    = 'MKAKARIDBS: '                   ; start of all log messages
   wd        = getenv('SHOME')+'Akari/'         ; working directory
   updatestr = 'V1 as ingested 24 Oct, 2011'    ; * make sure to update with each ingest or SW change
   zdbase    = getenv('ZDBASE')                 ; ZDBASE is where databases are kept
   fisfile   = 'AKARI-FIS_BSC_V1.fits'          ; name of FITS file containing FIS catalogue
   ircfile   = 'AKARI-IRC_PSC_V1.fits'          ; name of FITS file containing IRC catalogue
   fisext    = 1                                ; extension of fits binary table
   ircext    = 1                                ; extension of fits binary table
   fisfilo   = 'akari_fis'                      ; outfile name (base) for FIS database
   ircfilo   = 'akari_irc'                      ; outfile name (base) for IRC database
   nbyte     = [-1, 1, 2, 4, 4, 8, 8, 0, -1, 16, -1, -1, 2, 4, 8, 8]        ; number of bytes for IDL types
   dbtype    = ['','B','I','I','R','R','','C','','','','','U','U','I','U']  ; IDL database type strings
   ;
   ; Go to working directory
   cd, wd, current=cwd
   ;
   ; setup things for databases
   ;
   ; start with FIS catalogue -------------------------------
   print, logstr+'working on AKARI far infrared database' 
   title     = 'AKARI FIS Catalogue: '+updatestr
   ;
   ; read in table and header
   print, logstr+'Reading binary fits file: '+fisfile
   cstruct   = mrdfits(fisfile,fisext,chdr)
   nent      = sxpar(chdr,'naxis2')          ; number of entries
   nitem     = sxpar(chdr,'tfields')         ; number of items in database
   ;
   ;  setup arrays for database header
   print, logstr+'setting up items for database'
   item      = make_array(nitem, /string, value='')
   descript  = make_array(nitem, /string, value='')
   tout      = make_array(nitem, /string, value='')
   tlen      = make_array(nitem, /int, value=0)
   indblk    = ['ra', 'dec', 'objid', 'flux90', 'flux65', 'flux140', 'flux160']
   ;
   ; get item names & descriptions from header 
   FOR ii = 0, nitem-1 DO BEGIN 
      inam         = 'TTYPE'+strtrim(ii+1,2)
      item[ii]     = strtrim(sxpar(chdr,inam,comment=comm),2)
      descript[ii] = strtrim(comm,2)
      siz          = size(cstruct.(ii))
      dtyp         = siz[n_elements(siz)-2]
      tlen[ii]     = nbyte[dtyp]
      tout[ii]     = dbtype[dtyp]
   ENDFOR
   ;
   ; find items that are invalid
   jj        = where(tlen LT 0 OR tout EQ '', njj)
   IF njj GT 0 THEN stop, logstr+'**** error there are unsupported datatypes in fits table'
   ;
   ; find length of character arrays in structures
   jj        = where(tout EQ 'C', njj)
   IF njj GT 0 THEN FOR ii = 0, njj-1 DO tlen[jj[ii]] = max(strlen(strtrim(cstruct.(jj[ii]),2)))
   ;
   ; open dbd file, write top of the file
   fildbd  = fisfilo + '.dbd'
   print, logstr+'Writing DBD file: '+fildbd
   openw, lu, fildbd, /get_lun
   printf, lu, '#title'
   printf, lu, title
   printf, lu, ''
   printf, lu, '#maxentries'
   printf, lu, strtrim(string(nent),2)
   printf, lu, ''
   printf, lu, '#items'
   ;
   ; write item lines of dbd file
   mc     = max(strlen(item))
   pritem = ljust(strupcase(item), mc)
   nit    = n_elements(item)
   FOR ii = 0, nit-1 DO BEGIN 
      typstr = ljust(tout[ii]+'*'+strtrim(string(tlen[ii]),2),5)
      printf,lu,pritem[ii]+'   '+typstr+'   "'+descript[ii]+'"'
   ENDFOR 
   ;
   ; write index blocks of dbd file and close
   printf,lu,'  '
   printf,lu,'#index'
   ni = n_elements(indblk)
   FOR ii = 0, ni-1 DO BEGIN 
      printf,lu,ljust(strupcase(strtrim(indblk[ii],2)),mc)+'   sort'
   ENDFOR 
   free_lun,lu   
   ;
   ; breakout structure into arrays
   print, logstr+'breaking out arrays from structure'
   buildcmd = 'dbbuild_new'
   FOR ii = 0, nit-1 DO BEGIN 
      cmd      = item[ii]+' = cstruct.(ii)'
      buildcmd = buildcmd+','+item[ii]
      print, logstr+'executing: '+cmd
      res = execute(cmd)
      print, logstr+'result of command = ',res
   ENDFOR
   ;
   ; build database with dbbuild
   print, logstr+'Creating database: '+fisfilo
   !PRIV = 2
   dbcreate, fisfilo, 1, 1, /external
   dbopen, fisfilo, 1
   print, logstr+'executing: '+buildcmd
   res = execute(buildcmd)
   print, logstr+'result of command = ',res
   dbclose,dummy
   print, logstr+'FIS database created: '+fisfilo
   ;
   ; create database with dbbuildstruct (very slow)
   ;print, logstr+'Creating database: '+fisfilo
   ;!PRIV = 2
   ;dbcreate, fisfilo, 1, 1, /external
   ;dbopen, fisfilo, 1
   ;dbbuildstruct, cstruct
   ;dbclose,dummy
   ;
   ;
   ; Now do IRC catalogue -------------------------------
   print, logstr+'working on AKARI mid-infrared (IRC) database' 
   title     = 'AKARI IRC Catalogue: '+updatestr
   ;
   ; read in table and header
   print, logstr+'Reading binary fits file: '+ircfile
   cstruct   = mrdfits(ircfile,ircext,chdr)
   nent      = sxpar(chdr,'naxis2')          ; number of entries
   nitem     = sxpar(chdr,'tfields')         ; number of items in database
   ;
   ;  setup arrays for database header
   print, logstr+'setting up items for database'
   item      = make_array(nitem, /string, value='')
   descript  = make_array(nitem, /string, value='')
   tout      = make_array(nitem, /string, value='')
   tlen      = make_array(nitem, /int, value=0)
   indblk    = ['ra', 'dec', 'objid', 'flux09', 'flux18', 'mean_ab09', 'mean_ab18']
   ;
   ; get item names & descriptions from header 
   FOR ii = 0, nitem-1 DO BEGIN 
      inam         = 'TTYPE'+strtrim(ii+1,2)
      item[ii]     = strtrim(sxpar(chdr,inam,comment=comm),2)
      descript[ii] = strtrim(comm,2)
      siz          = size(cstruct.(ii))
      dtyp         = siz[n_elements(siz)-2]
      tlen[ii]     = nbyte[dtyp]
      tout[ii]     = dbtype[dtyp]
   ENDFOR
   ;
   ; find items that are invalid
   jj        = where(tlen LT 0 OR tout EQ '', njj)
   IF njj GT 0 THEN stop, logstr+'**** error there are unsupported datatypes in fits table'
   ;
   ; find length of character arrays in structures
   jj        = where(tout EQ 'C', njj)
   IF njj GT 0 THEN FOR ii = 0, njj-1 DO tlen[jj[ii]] = max(strlen(strtrim(cstruct.(jj[ii]),2)))
   ;
   ; open dbd file, write top of the file
   fildbd  = ircfilo + '.dbd'
   print, logstr+'Writing DBD file: '+fildbd
   openw, lu, fildbd, /get_lun
   printf, lu, '#title'
   printf, lu, title
   printf, lu, ''
   printf, lu, '#maxentries'
   printf, lu, strtrim(string(nent),2)
   printf, lu, ''
   printf, lu, '#items'
   ;
   ; write item lines of dbd file
   mc     = max(strlen(item))
   pritem = ljust(strupcase(item), mc)
   nit    = n_elements(item)
   FOR ii = 0, nit-1 DO BEGIN 
      typstr = ljust(tout[ii]+'*'+strtrim(string(tlen[ii]),2),5)
      printf,lu,pritem[ii]+'   '+typstr+'   "'+descript[ii]+'"'
   ENDFOR 
   ;
   ; write index blocks of dbd file and close
   printf,lu,'  '
   printf,lu,'#index'
   ni = n_elements(indblk)
   FOR ii = 0, ni-1 DO BEGIN 
      printf,lu,ljust(strupcase(strtrim(indblk[ii],2)),mc)+'   sort'
   ENDFOR 
   free_lun,lu   
   ;
   ; breakout structure into arrays
   print, logstr+'breaking out arrays from structure'
   buildcmd = 'dbbuild_new'
   FOR ii = 0, nit-1 DO BEGIN 
      cmd      = item[ii]+' = cstruct.(ii)'
      buildcmd = buildcmd+','+item[ii]
      print, logstr+'executing: '+cmd
      res = execute(cmd)
      print, logstr+'result of command = ',res
   ENDFOR
   ;
   ; build database with dbbuild
   print, logstr+'Creating database: '+ircfilo
   !PRIV = 2
   dbcreate, ircfilo, 1, 1, /external
   dbopen, ircfilo, 1
   print, logstr+'executing: '+buildcmd
   res = execute(buildcmd)
   print, logstr+'result of command = ',res
   dbclose,dummy
   print, logstr+'IRC database created: '+ircfilo
   ;
   ; check if the databases (the dbd file) exists already at $ZDBASE
   ; * if so use update_idldb
   ; * otherwise do a straight copy into $ZDBASE
   print, logstr+'going to database directory to update or move the databaases'
   cd, zdbase
   stat     = file_info(fisfilo+'.dbd')
   IF stat.exists THEN BEGIN 
      print, logstr+' updating FIS database with update_idldb'
      update_idldb, fisfilo, dbdir=wd
   ENDIF ELSE BEGIN 
      print, logstr+' moving FIS database to ZDBASE'
      spawn,'/bin/mv -f '+wd+fisfilo+'.db? .'
   ENDELSE 
   stat     = file_info(ircfilo+'.dbd')
   IF stat.exists THEN BEGIN 
      print, logstr+' updating IRC database with update_idldb'
      update_idldb, ircfilo, dbdir=wd
   ENDIF ELSE BEGIN 
      print, logstr+' moving IRC database to ZDBASE'
      spawn,'/bin/mv -f '+wd+ircfilo+'.db? .'
   ENDELSE 
   ;
   ; return to starting directory and end
   print, logstr+'going back to start directory: '+cwd
   cd, cwd
   print, logstr+'done!'
END 
