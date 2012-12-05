PRO mksinggrejdb
   ;
   ; Make DB for sources in previous versions of
   ; SINGG sample, but now rejected.
   ;
   ; G. Meurer 08/2004
   ; G. Meurer 12/2004 - adjusted to insert E(B-V) and flag value from 
   ;                     Schlegel et al. (1998, ApJ, 500, 525) into
   ;                     database.  Databse now has E(b-V) in its sort 
   ;                     block
   ; G. Meurer 07/2006 - update sdir to current correct directory
   ;                   - don't make db in zdbase, don't want to 
   ;                     overwrite old version, Make in current directory
   ;                     instead
   ;
   ; setup
   shome    = getenv('SHOME')
   sdir     = shome+'/Sample/V04b_dec04/'  ; directory for singg sample
   fil      = 'singg_reject.dat'          ; reject sample file name
   invers   = 4                           ; singg sample version
   sfil     = sdir+fil
   fmti     = '(a13,a11,1x,a11,f10.1,2x,a30)'
   skipline = 6
   ;
   ; definition of fields for structure and items for db
   item     = ['name','ra','dec','vhel','reason']
   tout     = ['C','R','R','R','C']
   tlen     = [0,8,8,4,0]
   dstring  = 'a,d,d,f,a'
   descript = ['Name', 'Right Ascension (J2000) [deg]', 'Declination (J2000) [deg]', $
               'Heliocentric radial velocity [km/s]', $
               'Reason rejected from SINGG sample ']
   title    = '(Spurious) Sources rejected from SINGG  selection'
   filo     = 'singg_reject'
   indblk   = ['ra','dec','vhel']
   ;
   ; read in sampleo
   readfmt, sfil, fmti, name, rastr, decstr, vhel, reason, skipline=skipline
   ns       = n_elements(name)
   ra       = 15.0d0*sexideg(rastr)
   dec      = sexideg(decstr)
   ;
   ; set lengths of character items
   k       = where(tlen EQ 0,nk)
   IF nk GT 0 THEN FOR i = 0,nk-1 DO result = execute('tlen[k[i]] = max(strlen('+item[k[i]]+'))') $
              ELSE stop
   ;
   ; go to ZDBASE
   ;zdbase = getenv('ZDBASE')
   ;cd, zdbase, current=cwd
   ;
   ; create structure
   create_struct2, singgrej, 'SINGGREJ', item, dstring, dimen=ns
   ;
   ; open dbd file, write top of the file
   fildbd  = filo + '.dbd'
   openw, lu, fildbd, /get_lun
   printf, lu, '#title'
   printf, lu, title
   printf, lu, ''
   printf, lu, '#maxentries'
   printf, lu, strtrim(string(ns),2)
   printf, lu, ''
   printf, lu, '#items'
   ;
   ; Fill singg structure, write item lines of dbd file
   mc     = max(strlen(item))
   pritem = ljust(strupcase(item), mc)
   ni     = n_elements(item)
   FOR i = 0, ni-1 DO BEGIN 
      cmd = "singgrej."+strtrim(item[i],2)+" = "+strtrim(item[i],2)
      result = execute(cmd)
      typstr = ljust(tout[i]+'*'+strtrim(string(tlen[i]),2),5)
      printf,lu,pritem[i]+'   '+typstr+'   "'+descript[i]+'"'
   ENDFOR 
   ;
   ; write index blocks of dbd file and close
   printf,lu,'  '
   printf,lu,'#index'
   ni = n_elements(indblk)
   FOR i = 0, ni-1 DO BEGIN 
      printf,lu,ljust(strupcase(strtrim(indblk[i],2)),mc)+'   sort'
   ENDFOR 
   free_lun,lu   
   ;
   ; write database
   !PRIV = 2
   dbcreate, filo, 1, 1, /external
   dbopen, filo, 1
   dbbuildstruct, singgrej
   dbclose,dummy
   ; cd, cwd
   ;
END 
