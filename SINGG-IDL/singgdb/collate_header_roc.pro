PRO collate_header_roc
   ; 
   ; collate header database with SST ROC database
   ;
   ; G. Meurer, Feb 2004
   ;
   srcrad  = 12.0  ;search radius in arcmin
   ;filo    = '/home/meurer/teltime/SST/singg_mips_collate.dat'
   ;fstr    = 'MIPS'
   fstr    = 'IRAC'
   filo    = '/home/meurer/teltime/SST/singg_irac_collate.dat'
   ;
   ; Open header database, extract & convert 
   ; file (Rsub), object, RA, DEC.
   dbopen, 'header'
   list    = dbfind('FILENAME = rsub')
   dbext,list,'runname,object,filename,ra,dec',run,obj,filen,rastr,decstr
   ra      = 15.0d0*sexideg(rastr)
   dec     = sexideg(decstr)
   nh      = n_elements(list)
   nfound  = make_array(nh, /int)
   dbclose
   ;
   ; open ROC, find targets with mips observations
   dbopen, 'roc'
   ; rlist   = dbfind('aot = mops') this does not work
   rlist = dbfind('name')      ; since the above line does not work
   dbext,rlist,'aot',aotall    ; do it the hard way
   good = where(strpos(strupcase(aotall),fstr) ge 0)
   rlist = temporary(rlist[good])
   ;
   ; open output file
   openw, lu, filo, /get_lun
   FOR i = 0, nh-1 DO BEGIN 
      dis   = float(rlist)*0.0
      list  = dbcircled(ra[i], dec[i], srcrad, dis, rlist, /silent)
      IF list[0] GE 0 THEN nfound[i] = n_elements(list) ELSE nfound[i] = 0
      IF nfound[i] GT 0 THEN BEGIN 
         dbext, list, 'name,ra,dec,aot,arrayid,pid', $
          name,rasst,decsst,aot,arrayid,pid
         FOR j = 0, nfound[i]-1 DO BEGIN 
            print, obj[i], run[i], rastr[i], decstr[i], $
             ' | ', name[j], dis[j]/60.0, $
             aot[j], arrayid[j], pid[j], $
             format='(a15,2x,a5,2x,a16,2x,a16,a3,a15,f10.4,2x,a8,2x,a4,i6)'
            printf, lu, obj[i], run[i], rastr[i], decstr[i], $
             ' | ', name[j], dis[j]/60.0, $
             aot[j], arrayid[j], pid[j], $
             format='(a15,2x,a5,2x,a16,2x,a16,a3,a15,f10.4,2x,a8,2x,a4,i6)'
         ENDFOR 
      ENDIF ELSE BEGIN 
         print, obj[i], run[i], rastr[i], decstr[i], $
          ' | No_matches', $
          format='(a15,2x,a5,2x,a16,2x,a16,a13)'
         printf, lu, obj[i], run[i], rastr[i], decstr[i], $
          ' | No_matches', $
          format='(a15,2x,a5,2x,a16,2x,a16,a13)'
      ENDELSE 
   ENDFOR 
   free_lun,lu
END 
