PRO sr2web_ddb_driver, computer=computer
   ;
   ; Driver program to generate QA web pages for SINGG Release 2.
   ;
   ; G. Meurer 12/2006
   ;
   ; setup stuff
   ddb        = 'singg_derived'
   baseout    = '/Users/meurer/SINGG/SR2_QA/'
   baseimg    = '/Users/meurer/SINGG/SR2_QA/Images/'
   basedata   = '/Users/meurer/SINGG/SR2_QA/Data/'
   baseplt    = '/Users/meurer/SINGG/SR2_QA/Plots/'
   fhtml      = 'index.html'
   prog       = 'sr2web_ddb_driver.pro'
   prog2      = strupcase(prog)+': '    ; for messages
   title      = 'SINGG Release 2 Quality Assurance'
   subtitle   = 'Main page '
   ncol       = 8
   ;
   ; reset directories according to user
   ; switch computer
   IF keyword_set(computer) THEN BEGIN 
      CASE computer OF 
         'acs34'    : BEGIN 
                         baseout  = '/data2/acs34/meurer/SINGG/SR2QA/'
                         baseimg  = '/data2/acs34/meurer/SINGG/SR2QA/Images/'
                         basedata = '/data2/acs34/meurer/SINGG/SR2QA/Data/'
                         baseplt  = '/data2/acs34/meurer/SINGG/SR2QA/Plots/'
                      END
         'singgpb1' : BEGIN
                         baseout  = '/Users/meurer/SINGG/SR2_QA/'
                         baseimg  = '/Users/meurer/SINGG/SR2_QA/Images/'
                         basedata = '/Users/meurer/SINGG/SR2_QA/Data/'
                         baseplt  = '/Users/meurer/SINGG/SR2_QA/Plots/'
                      END
         'sungg'    : BEGIN 
                         baseout  = '/data1/sungg/www/protected/SINGG/SR2QA/'
                         baseimg  = '/data1/sungg/www/protected/SINGG/SR2QA/Images/'
                         basedata = '/data1/sungg/www/protected/SINGG/SR2QA/Data/'
                         baseplt  = '/data1/sungg/www/protected/SINGG/SR2QA/Plots/'
                      END
         ELSE       : BEGIN
                         baseout  = '/Users/meurer/SINGG/SR2_QA/'
                         baseimg  = '/Users/meurer/SINGG/SR2_QA/Images/'
                         basedata = '/Users/meurer/SINGG/SR2_QA/Data/'
                         baseplt  = '/Users/meurer/SINGG/SR2_QA/Plots/'
                      END
      ENDCASE 
   ENDIF 
   ;
   ; open database
   print, prog2+'opening IDL database '+ddb
   dbopen, ddb
   ;
   ; extract all HIPASS object names in database
   print, prog2+'determining objects to mark-up '
   dbext, -1, 'entry,object,runid',entall,objall,runall
   ;
   ; exclude Run04s, and then sort
   kk        = where(runall NE 'Run04s', nkk)
   jj        = sort(objall[kk])
   ;
   ; find unique objects
   uu        = uniq(objall[kk[jj]])
   nuu       = n_elements(uu)
   uu        = kk[jj[uu]]
   ;
   ; winnow down objall array & summarize what will be done
   objall    = objall[uu]
   print, prog2+'will make web pages for '+strtrim(string(nuu),2)+' unique HIPASS targets'
   ;
   ; go to base directory
   print,prog2+'going to base directory : '+baseout
   cd, baseout, current=cwd
   ;
   ; Open index file
   print,prog2+'opening file : '+fhtml
   openw, lu, fhtml, /get_lun
   singg_pagetop, lu, title, subtitle
   ;
   ; write top of table of links to object web pages
   printf,lu,'<P><TABLE border=1 cellpadding=3>'
   printf,lu,'<TR bgcolor=#707070>'
   printf,lu,'   <TH align="center" colspan='+strtrim(string(ncol,2))+'>Objects in SR2</TH>'
   printf,lu,'</TR>'
   ;
   ; loop through target IDs
   FOR ii = 0, nuu-1 DO BEGIN 
      sr2web_ddb_objpage, objall[ii], baseout, baseimg, basedata, baseplt, filo
      ;
      ; start a new table row if need be
      kk      = ii MOD ncol
      IF kk EQ 0 THEN printf,lu,'<TR bgcolor=#ffffff>'
      ;
      ; markup link to object page
      printf,lu,'   <TD align="center"><a href="'+filo+'">'+objall[ii]+'</a></TD>'
      ;
      ; finish up row if need be
      IF kk EQ ncol-1 THEN printf,lu,'</TR>'
   ENDFOR 
   ;
   ; finish off table
   print,prog2+'finishing off HTML file'
   printf,lu,'</P></TABLE>'
   ;
   ; finish off web page
   singg_pagebot, lu, prog=prog
   free_lun, lu
   ;
   ; go back to directory at entry
   print,prog2+'returning to '+cwd
   cd, cwd
   ;
   dbclose, ddb
   print,prog2+'database closed, finished.'
END 
