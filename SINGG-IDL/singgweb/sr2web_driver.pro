PRO sr2web_driver, computer=computer
   ;
   ; Driver program to generate QA web pages for SINGG Release 2.
   ;
   ; G. Meurer 12/2006
   ;
   ; setup stuff
   hdb      = 'proc3_header'
   sufpng   = ['sqrt1', 'sqrt3', 'sqrt8', 'lin8', 'log1']
   baseout  = '/Users/meurer/SINGG/SR2_QA/'
   baseimg  = '/Users/meurer/SINGG/SR2_QA/Images/'
   basedata = '/Users/meurer/SINGG/SR2_QA/Data/'
   fhtml    = 'index.html'
   prog     = 'sr2web_driver.pro'
   prog2    = 'SR2WEB_DRIVER: '
   title    = 'SINGG Release 2 Quality Assurance'
   subtitle = 'Main page '
   ncol     = 8
   ;
   ; reset directories according to user
   ; switch computer
   IF keyword_set(computer) THEN BEGIN 
      CASE computer OF 
         'acs34'    : BEGIN 
                         baseout  = '/data2/acs34/meurer/SINGG/SR2QA/'
                         baseimg  = '/data2/acs34/meurer/SINGG/SR2QA/Images/'
                         basedata = '/data2/acs34/meurer/SINGG/SR2QA/Data/'
                      END
         'singgpb1' : BEGIN
                         baseout  = '/Users/meurer/SINGG/SR2_QA/'
                         baseimg  = '/Users/meurer/SINGG/SR2_QA/Images/'
                         basedata = '/Users/meurer/SINGG/SR2_QA/Data/'
                      END
         'sungg'    : BEGIN 
                         baseout  = '/data1/sungg/www/protected/SINGG/SR2QA/'
                         baseimg  = '/data1/sungg/www/protected/SINGG/SR2QA/Images/'
                         basedata = '/data1/sungg/www/protected/SINGG/SR2QA/Data/'
                      END
         ELSE       : BEGIN
                         baseout  = '/Users/meurer/SINGG/SR2_QA/'
                         baseimg  = '/Users/meurer/SINGG/SR2_QA/Images/'
                         basedata = '/Users/meurer/SINGG/SR2_QA/Data/'
                      END
      ENDCASE 
   ENDIF 
   ;
   ; open database
   print, prog2+'opening IDL database '+hdb
   dbopen, hdb
   ;
   ; find all net images
   print, prog2+'querying for net Halpha images '
   list_net = dbfind('filename = sub_ss.fits')
   ;
   ; extract info on these images
   print, prog2+'getting information on net Halpha images '
   dbext, list_net, 'filename,pred00,pred01,runid,target',$
    fnet,fcnt,fnb,runid,target
   print, prog2+'deriving stuff... '
   nl       = n_elements(list_net)
   fnet     = strtrim(fnet,2)
   fcnt     = strtrim(fcnt,2)
   fnb      = strtrim(fnb,2)
   runid    = strtrim(runid,2)
   target   = strtrim(target,2)
   k1       = strpos(fcnt,'.fits')
   k2       = strpos(fnb,'.fits')
   k3       = strpos(fnet, '.fits')
   ;
   ; note code for generating rootshrt was hacked from 
   ; setup_colorlists.pro.  Should really write a function
   ; that is called from both programs
   p0       = strpos(fnet, 'sub')-2
   p1       = strpos(fnet, '_')
   p2       = strpos(fnet, '_6')
   p3       = strpos(fnet, '_Csub')
   rootlong = make_array(nl, /string, value='')
   rootshrt = make_array(nl, /string, value='')
   add      = make_array(nl, /string, value='')
   FOR ii = 0, nl-1 DO BEGIN 
      fcnt[ii]     = strmid(fcnt[ii], 0, k1[ii])+'_ss.fits'
      fnb[ii]      = strmid(fnb[ii], 0, k2[ii])+'_ss.fits'
      rootlong[ii] = strmid(fnet[ii], 0, k3[ii])+'_r'+runid[ii]
      rootshrt[ii] = strmid(fnet[ii], 0, p1[ii])
   ENDFOR 
   qq       = where(p2 GT 0, nqq)
   IF nqq GT 0 THEN BEGIN 
      FOR jj = 0, nqq-1 DO BEGIN 
         ii       = qq[jj]
         str      = fnet[ii]
         p4       = p2[ii]+1
         p5       = p2[ii]+6
         add[ii]  = '_'+strmid(str,p4,4)+strmid(str,p5,1)
      ENDFOR 
   ENDIF 
   qq       = where(p3 GT 0 AND p2 LT 0, nqq)
   IF nqq GT 0 THEN add[qq] = '_C'
   rootshrt = rootshrt+add+'_r'+runid
   list_cnt = dbmatch('filename', fcnt)
   list_nb  = dbmatch('filename', fnb)
   dbext, list_cnt, 'filtname', filt_cnt
   dbext, list_nb,  'filtname', filt_nb
   filt_cnt = strtrim(filt_cnt, 2)
   filt_nb  = strtrim(filt_nb, 2)
   dbclose
   ;
   ; derive some file names
   ;ffinder   = rootlong+'_dbcheck.jpg'
   ;fdbout    = rootlong+'_dbcheck.out'
   ;
   ; Do some sorting
   print,prog2+'sorting...'
   k1        = sort(filt_cnt)
   k2        = sort(filt_nb[k1])
   k3        = sort(target[k1[k2]])
   k3        = k1[k2[k3]]
   ;
   fnet      = fnet[k3]
   fcnt      = fcnt[k3]
   fnb       = fnb[k3]
   runid     = runid[k3]
   target    = target[k3]
   rootlong  = rootlong[k3]
   rootshrt  = rootshrt[k3]
   filt_cnt  = filt_cnt[k3]
   filt_nb   = filt_nb[k3]
   fcat      = rootlong + '_catcalib.dat'
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
   ;
   ; find unique target IDs
   uu        = uniq(target)
   nuu       = n_elements(uu)
   ;
   ; loop through target IDs
   FOR ii = 0, nuu-1 DO BEGIN 
      targ   = target[uu[ii]]
      jj     = where(target EQ targ, njj)
      sr2web_objpage, targ, baseout, baseimg, basedata, sufpng, $
                      fnet[jj], fcnt[jj], fnb[jj], fcat[jj], $
                      rootlong[jj], rootshrt[jj], runid[jj], $
                      filt_cnt[jj], filt_nb[jj], filo
      ;
      ; start a new table row if need be
      kk      = ii MOD ncol
      IF kk EQ 0 THEN printf,lu,'<TR bgcolor=#ffffff>'
      ;
      ; markup link to object page
      printf,lu,'   <TD align="center"><a href="'+filo+'">'+targ+'</a></TD>'
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
   print,prog2+'finished.'
END 
