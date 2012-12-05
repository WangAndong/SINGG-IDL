PRO setup_colorlists, runstr=runstr
   ;
   ; Prepare for making 3 color images used for QA work.
   ; - setup directories to hold (links to) images
   ; - make symbolic links to fits images
   ; - make input list file for generating 3 color images
   ;
   ; G. Meurer 12/2006 : originally written
   ;           03/2007 : better doc.
   ;
   ; set-up stuff, I put this at the top of program 
   hdb   = 'proc3_header'                   ; header database
   rstr  = ['01', '02', '03', '04', '05',  $
            '06', '07', '08', '09', '10',  $
            '11', '12', '13', '15', '17']   ; runs
   base  = '/data2/acs36/meurer/SINGG/'     ; base directory for output
   sbase = '/data1/acs22/hanish/'           ; base directory for input
   fact  = [1,3,8]                        ; rebin factors for color images
   ;
   ; use default rstr if runstr is not set
   IF keyword_set(runstr) THEN rstr = runstr 
   ;
   nruns = n_elements(rstr)
   ;
   ; go to base directory, save current
   cd, base, current=cwd
   ;
   ; open header database, get runids because querying it is hard
   dbopen, hdb
   dbext, -1, 'entry,runid',entry,runid
   runid = strtrim(runid, 2)
   FOR kk = 0, nruns-1 DO BEGIN 
      ;
      ; find net image entries for this run
      pp   = where(runid EQ rstr[kk], npp)
      IF npp GT 0 THEN BEGIN 
         list = entry[pp]
         list = dbfind('filename = sub_ss.fits', list)
         ;
         ; query filenames and predecessors
         dbext, list, 'filename,pred00,pred01', fname, pred0, pred1
         nf   = n_elements(fname)
         ;
         ; Check for directory, make if need be
         dname = 'Run'+rstr[kk]+'/Proc4'
         finfo = file_info(dname)
         IF NOT finfo.exists THEN BEGIN 
            file_mkdir, dname
            print, 'Created directory: '+dname
         ENDIF ELSE BEGIN 
            IF finfo.directory THEN print,'Directory already exists: '+dname $
               ELSE stop,'ERROR**** file exists but not directory: '+dname
         ENDELSE 
         ;
         ; make links
         cd, dname
         FOR jj = 0, nf-1 DO BEGIN 
            fnet   = strtrim(fname[jj],2)
            fnb    = pred1[jj]
            fcn    = pred0[jj]
            pp     = strpos(fnet, '_')
            dirnam = strmid(fnet, 0, pp)
            pp     = strpos(fnb, '.fits')
            fnb    = strmid(fnb, 0, pp)+'_ss.fits'
            pp     = strpos(fcn, '.fits')
            fcn    = strmid(fcn, 0, pp)+'_ss.fits'
            sdname = sbase + 'Run'+rstr[kk]+'/Proc4/'+dirnam+'/'
            finfo  = file_info(fnet)
            IF NOT finfo.exists THEN file_link, sdname+fnet, fnet, /verbose
            finfo  = file_info(fnb)
            IF NOT finfo.exists THEN file_link, sdname+fnb, fnb, /verbose
            finfo  = file_info(fcn)
            IF NOT finfo.exists THEN file_link, sdname+fcn, fcn, /verbose
            fname[jj] = fnet
            pred1[jj] = fnb
            pred0[jj] = fcn
         ENDFOR 
         ;
         ; calculate output root name of jpg images
         p1   = strpos(fname, '_')
         p2   = strpos(fname, '_6')
         p3   = strpos(fname, '_Csub')
         root = make_array(nf, /string, value='')
         FOR jj = 0, nf-1 DO BEGIN 
            str      = fname[jj]
            p4       = p1[jj]
            root[jj] = strmid(str, 0, p4)
         ENDFOR 
         add  = make_array(nf, /string, value='')
         qq   = where(p2 GT 0, nqq)
         IF nqq GT 0 THEN BEGIN 
            FOR jj = 0, nqq-1 DO BEGIN 
               ii      = qq[jj]
               str     = fname[ii]
               p4      = p2[ii]+1
               p5      = p2[ii]+6
               add[ii] = '_'+strmid(str, p4, 4)+strmid(str,p5,1)
            ENDFOR 
         ENDIF 
         qq   = where(p3 GT 0 AND p2 LT 0, nqq)
         IF nqq GT 0 THEN add[qq] = '_C'
         root = root+add+'_r'+rstr[kk]
         ;
         ; make file list
         flis = 'run'+rstr[kk]+'_3col.in'
         openw, lu, flis, /get_lun
         FOR ii = 0, nf-1 DO $
          printf, lu, ljust(fname[ii],30)+ljust(pred1[ii],25)+ljust(pred0[ii],25)+ljust(root[ii],25)
         free_lun, lu
         print, 'Wrote 3 color input list file : '+flis
         ;
         ; oh, what the heck might as well and go ahead and make 
         ; the color images while we are at it
         singg_3colorrun, flis, fact=fact
         ;
         ; go back to base directory
         cd, base
      ENDIF 
   ENDFOR 
   ;
   ; close database, go to starting directory
   dbclose
   print, 'Returning to directory: '+cwd
END 
