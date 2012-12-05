PRO dbexample2
   ;
   ; Example program to get R band mag and luminosity from
   ; the singg_derived database.
   ;
   ; program written to help David Nichols @ RSAA
   ;
   ;  To see what databases are available do
   ;
   ;  dbhelp, -1       ; before opening any databases
   ;
   ;  To see what items are in a database do
   ;
   ;  dbhelp, -1       ; after opening that database
   ;
   ; The following routines are called (singg programs marked with "*"):
   ; DBOPEN
   ; ZPARCHECK
   ; DBCLOSE
   ; FIND_WITH_DEF
   ; FIND_ALL_DIR
   ; BREAK_PATH
   ; IEEE_TO_HOST
   ; DB_INFO
   ; GOOD_DERIVED3 *
   ; DBFIND
   ; DBFPARSE
   ; DB_ITEM
   ; DB_ITEM_INFO
   ; IS_IEEE_BIG
   ; DBRD
   ; DBSEARCH
   ; DBEXT
   ; DBEXT_DBF
   ; DBXVAL
   ; DEGSEXI *
   ; LJUST *
   ;
   ; G. Meurer 10/2009 (original dbexample.pro)
   ; G. Meuerer 10/2010 (converted for DN)
   ;
   ; define some constants
   mabsr_max = -16.7             ; Absolute apparent R mag upper limit (in ABmag)
   filo      = 'example2.out'    ; output file name
   ;
   dbopen, 'singg_derived'      ; open the database
   list      = good_derived3()  ; return only the best entries for each galaxy
   ;
   ; create string to define search and print it
   sstr      = 'mabs_r0_t > '+strtrim(string(min(mabsr_max)),2)
   print, 'search string = "'+sstr+'"'
   list      = dbfind(sstr, list) ; get entries that meet search crit (sstr), and previous selection (list)
   ;
   ; get quantities that we are interested in.  Here we will get
   ;   name (SINGG name), optid (optical name), ra,dec (position), 
   ;   mabs_r0_t (total absolute mag), mapp_r_t, err_mag_r_t 
   ;   (apparent mag and error), logf_ha_t, err_logf_ha_t 
   ;   (total log Halpha flux and error), re_ha_t, err_re_ha_t
   ;   (Halpha half light radius and error)
   dbext, list, 'name,optid,ra,dec,mabs_r0_t,mapp_r_t,err_mag_r_t,logf_ha_t,err_logf_ha_t,re_ha_t,err_re_ha_t', $
                 name,optid,ra,dec,mabsr,mappr,emagr,lfha,elfha,reha,ereha
   ; 
   rastr     = degsexi(ra,prec=1,/ra) ; convert right ascension to sexigessimal string
   decstr    = degsexi(dec,prec=0)    ; convert declination to sexigessimal string
   str       = ljust(name,13)+ljust(optid,15)+'  '+rastr+' '+decstr+' '  ; concatenate strings arrays for printing
   ;
   ; write data to output file in ra order
   openw, lu, filo, /get_lun          ; open file, then write header line
   printf, lu, '# SINGG_name Optid               RA          Dec        RA       Dec       M_r      m_r    e_m_r  lg(Fha)   elg(Fha)   r_e  e_r_e'
   jj        = sort(ra)               ; sort by right ascension
   FOR ii = 0, n_elements(name)-1 DO BEGIN 
      kk     = jj[ii]
      printf, lu, str[kk], ra[kk], dec[kk], mabsr[kk], mappr[kk], emagr[kk], lfha[kk], elfha[kk], reha[kk], ereha[kk], $
       format='(a,f10.4,f10.4,f9.3,f9.3,f7.3,f9.3,f9.3,f8.1,f7.1)'
   ENDFOR 
   free_lun, lu     ; remember to free up logical unit
END 
