PRO update_idldb, dbname, dbdir=dbdir, backdir=backdir, $
                     pubdir=pubdir, export=export, usecdate=usecdate
   ;
   ; Update an IDL database on $ZDBASE with a new version
   ; found in some other directory.
   ; * Check if new file exists
   ; * copy old version to backup directory
   ; * overwrite new version to $ZDBASE
   ; * tar gzip & export database to public directory
   ;
   ; dbname  -> name of database to update
   ; dbdir   -> directory that contains new version of database 
   ;            default is /home/hanish/singg/
   ; backdir -> directory to backup old databse to
   ;            default is $ZDBASE/Old
   ; pubdir  -> directory for public copy of gzipped tar file
   ;            default is '/home/meurer/public_html/research/singg/DB' 
   ; export  -> set if the new database is to be tarred, gzipped and 
   ;            copied to pubdir.
   ; usecdate -> use creation date of previous file to generate name 
   ;             of old output database name.  This might be nec.
   ;             if the UPDATE entries are compromised, or 
   ;             inaccurate.
   ;
   ; G. Meurer 3/2005   (JHU) written
   ;           11/2005  (JHU) now explicitly spawns /bin/cp -f and /bin/mv -f
   ;                    to force the copy and command without user 
   ;                    interaction
   ;           10/2010  (ICRAR/UWA) fixed bug when testing for '/' at ends 
   ;                    of various directory names
   ;           08/2012  (ICRAR/UWA) added usecdate option, to handle 
   ;                    super_sungg_derived
   ;
   dbnam = strtrim(dbname,2)
   ;
   zdbase = strtrim(getenv('ZDBASE'),2)
   IF strmid(zdbase, strlen(zdbase)-1) NE '/' THEN zdbase = zdbase + '/'
   IF zdbase EQ '' THEN BEGIN 
      print, '**** ERROR in UPDATE_IDLDB : $ZDBASE not set'
      return 
   ENDIF 
   ;
   IF NOT keyword_set(dbdir) THEN dbdir = '/home/hanish/singg/'
   IF NOT keyword_set(backdir) THEN backdir = zdbase + 'Old/'
   IF NOT keyword_set(pubdir) THEN pubdir = '/home/meurer/public_html/research/singg/DB' 
   IF strmid(dbdir, strlen(dbdir)-1) NE '/' THEN dbdir = dbdir + '/'
   IF strmid(backdir, strlen(backdir)-1) NE '/' THEN backdir = backdir + '/'
   IF strmid(pubdir, strlen(pubdir)-1) NE '/' THEN pubdir = pubdir + '/'
   ;
   ; check if the dbd file is there
   file = dbdir + dbnam + '.dbf'
   result = file_info(file)
   IF NOT result.exists THEN BEGIN 
      print, '**** ERROR in UPDATE_IDLDB : file '+dbnam+' does not exist'
      return 
   ENDIF 
   ;
   ; Check if an old version exists, 
   ; if so this will need to be backed up
   file_old = zdbase + dbnam + '.dbf'
   result_old = file_info(file_old)
   IF result_old.exists THEN BEGIN 
      ;
      ; get date string to use for the backup copy naming
      dbopen, dbnam
      check = db_item_info('name')
      kk    = where(strpos(check,'UPDATE') EQ 0, nkk)
      IF (NOT keyword_set(usecdate)) AND (nkk GT 0) THEN BEGIN 
         ;
         ; If an update keyword exists, use the most recent update
         ; as a string
         nn   = db_info('entries')
         list = lindgen(nn)+1L
         dbext,list,'UPDATE',upd
         dstrmax = '0'
         FOR ii = 0, nn[0]-1 DO BEGIN 
            stop
            IF strmid(upd[ii],22,1) EQ 'T' THEN $
                str = strmid(upd[ii],0,20) + strmid(upd[ii],24)  ELSE $
                str = upd[ii]
            bd = bin_date(str)
            dstr = string(bd,format='(i4,i2,i2,i2,i2,i2)')
            jj   = strpos(dstr,' ') 
            WHILE jj GE 0 DO BEGIN 
               strput,dstr, '0', jj
               jj   = strpos(dstr,' ') 
            ENDWHILE 
            IF double(dstr) GT double(dstrmax) THEN dstrmax = dstr
         ENDFOR 
      ENDIF ELSE BEGIN 
         ;
         ; use creation time of previous file
         bd = bin_date(systime(0,result_old.ctime,/utc))
         dstr = string(bd,format='(i4,i2,i2,i2,i2,i2)')
         jj   = strpos(dstr,' ') 
         WHILE jj GE 0 DO BEGIN 
            strput,dstr, '0', jj
            jj   = strpos(dstr,' ') 
         ENDWHILE 
      ENDELSE 
      ;
      ; now do the backup copying
      dstr = '_'+dstr
      old1 = zdbase + dbnam + '.dbd'
      old2 = zdbase + dbnam + '.dbf'
      old3 = zdbase + dbnam + '.dbh'
      old4 = zdbase + dbnam + '.dbx'
      new1 = backdir + dbnam + dstr + '.dbd'
      new2 = backdir + dbnam + dstr + '.dbf'
      new3 = backdir + dbnam + dstr + '.dbh'
      new4 = backdir + dbnam + dstr + '.dbx'
      ;
      print, 'UPDATE_IDLDB : making backup copies'
      print, '   '+'/bin/cp -f '+old1+' '+new1
      spawn,'/bin/cp -f '+old1+' '+new1
      print, '   '+'/bin/cp -f '+old2+' '+new2
      spawn,'/bin/cp -f '+old2+' '+new2
      print, '   '+'/bin/cp -f '+old3+' '+new3
      spawn,'/bin/cp -f '+old3+' '+new3
      print, '   '+'/bin/cp -f '+old4+' '+new4
      spawn,'/bin/cp -f '+old4+' '+new4
   ENDIF 
   ;
   ; copy the files from dbdir 
   cmd = '/bin/cp -f '+dbdir+dbnam+'.* '+zdbase
   print, 'UPDATE_IDLDB: copying files to ZDBASE '
   print, '   '+cmd
   spawn, cmd
   ;
   ; tar gzip and export new database if /export is set
   IF keyword_set(export) THEN BEGIN
      print,'UPDATE_IDLDB: exporting compressed tar-ed database to public directory'
      cd,zdbase,current=cwd
      print, '    '+'tar cvf '+dbnam+'.tar '+dbnam+'.db?'
      spawn,'tar cvf '+dbnam+'.tar '+dbnam+'.db?'
      print, '    '+'gzip -v '+dbnam+'.tar'
      spawn,'gzip -v '+dbnam+'.tar'
      print, '    '+'mv -f '+dbnam+'.tar.gz '+pubdir
      spawn,'/bin/mv -f '+dbnam+'.tar.gz '+pubdir
      cd,cwd
   ENDIF 
END 
