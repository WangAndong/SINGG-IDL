PRO cpgsamp_web_db, nocopy=nocopy
   ;
   ; compile sources web pages, gif, jpg, fits files for potential 
   ; galex sample.
   ;
   wdir     = '/data5/acs27/meurer/SINGG/Sample/V04a_mar04/WWW/'
   imdir    = '/data5/acs27/meurer/SINGG/Sample/V04a_mar04/CADC/'
   ;odir     = '~/public_html/research/singg/sample/gsamp/'
   odir     = '~/public_html/research/singg/sample/gsamp_extend/'
   grej     = '~/teltime/Galex/sungg_rej.dat'
   osamp    = 'sungg_sample.dat'
   opage    = 'sungg_sample.html'
   ofil     = odir + 'index.html'
   outsamp  = odir + osamp
   outpage  = odir + opage
   db       = 'singg_sample'
   title    = 'GALEX sample'
   subtitle = '(First pass)' 
   lm0      = 6.8
   lm1      = 11.0
   dlm      = 0.2
   ;maxnbin  = 12
   maxnbin  = 16
   ;
   ; open database
   dbopen,db
   ;
   ; find low extinction sources, extract relevant quantities
   list = dbfind('ebv < 0.07', count=nl)
   IF nl EQ 0 THEN stop
   dbext,list,'name,distance,logmhi,observed',name,distance,logmhi,observed
   name = strtrim(name,2)
   ;
   ; read reject list, reject matches
   good = make_array(nl, value=1b)
   readcol, grej, bgal, format='(a)'
   bgal = strtrim(bgal,2)
   nb   = n_elements(bgal)
   FOR i = 0, nb-1 DO BEGIN 
      k = where(name EQ bgal[i], nk)
      IF nk EQ 0 THEN print, 'Warning no match for bad gal = '+bgal[i]
      IF nk GT 1 THEN print, 'Warning multiple matches for bad gal = '+bgal[i]
      IF nk GT 0 THEN good[k] = 0b 
   ENDFOR 
   k        = where(good EQ 1b,ng)
   list     = temporary(list[k])
   name     = temporary(name[k])
   distance = temporary(distance[k])
   logmhi   = temporary(logmhi[k])
   ;
   ; open output web page, print title
   openw, lu, ofil, /get_lun
   singg_pagetop, lu, title, subtitle
   ;
   ; Make links to html and text tables
   printf, lu, '<hr>'
   printf, lu, '<p>Full data table on selected sources: '+$
               '<a href="'+osamp+'">ASCII</a> , <a href="'+opage+'">HTML</a></p>'
   printf, lu, '<hr>'
   ;
   ; open table
   printf, lu, '<P><TABLE border=1 cellpadding=3>'
   ;
   ; reset good, now use to make sure that only nearest
   ; maxnbin galaxies are selected.
   glist    = make_array(ng, value=-1)
   ng       = -1
   ;
   ; loop through mass bins
   FOR lm = lm0, lm1, dlm DO BEGIN 
      j = where(logmhi GE lm AND logmhi LT lm+dlm,nj)
      IF nj GT 0 THEN BEGIN 
         ;
         ; There are sources in this mass bin print a sub table head
         printf, lu, '<tr>'
         printf, lu, '<th colspan=3>Mass bin '+string(lm,format='(f4.1)')+$
                     ' - '+string(lm+dlm,format='(f4.1)')
         printf, lu, '</tr>'
         printf, lu, '<tr>'
         printf, lu, '<th>Target</th><th>distance</th><th>log(M<sub>HI</sub>)</th>'
         printf, lu, '</tr>'
         ;
         ; Sort the sources in this bin by distance
         kk = sort(distance[j])
         ;
         ; loop through sources in bin
         FOR k = 0, nj-1 DO BEGIN 
            IF k LT maxnbin THEN BEGIN 
               ;
               ng        = ng + 1
               glist[ng] = list[j[kk[k]]]
               ;
               ; write entry for source
               printf, lu, '<tr>'
               printf, lu, '<td><a href="'+name[j[kk[k]]]+'.html">'+name[j[kk[k]]]+'</a></td>'
               printf, lu, '<td>'+string(distance[j[kk[k]]],format='(f5.1)')+'</td>'
               printf, lu, '<td>'+string(logmhi[j[kk[k]]],format='(f5.2)')+'</td>'
               printf, lu, '</tr>'
               ;
               ; copyfiles
               IF NOT keyword_set(nocopy) THEN BEGIN 
                  spawn, 'cp '+wdir+name[j[kk[k]]]+'.html '+odir
                  spawn, 'cp '+wdir+name[j[kk[k]]]+'.jpg '+odir
                  spawn, 'cp '+wdir+name[j[kk[k]]]+'.gif '+odir
                  spawn, 'cp '+imdir+name[j[kk[k]]]+'_*.fits '+odir
               ENDIF 
            ENDIF ELSE BEGIN 
               good[kk[k]] = 0b
            ENDELSE 
         ENDFOR 
      ENDIF 
   ENDFOR 
   ;
   ; close table
   printf, lu, '</TABLE></p>'
   ;
   ; finish page 
   date=systime()
   printf,lu,'<p><small>Page generated automatically ' $
    +' on '$
    +date+'<br>'
   printf,lu,'</BODY>'
   printf,lu,'</HTML>'
   close, lu
   ;
   ; Make text file in SINNG v4 format
   print, 'Number of galaxies in sample = ', ng+1
   glist = temporary(glist[0:ng])
   dbext, glist, 'name,optid,catalog,ra,dec,glat,glong,vhel,vlg,vcmb', $
    objname,nednam,cat,ra,dec,l,b,vhel,vlg,vcmb
   dbext, glist, 'vtonry,vshap,w50,sp,sint,logmhi,observed,ebv', $
    vtonry,vshap,w50,peak,sdv,logmhi,obstr,ebv
   peak   = 0.001*peak
   rastr  = degsexi(ra, prec=1, /ra)
   decstr = degsexi(dec, prec=0)
   singg_writesamp,outsamp,objname,cat,rastr,decstr,nednam,peak,sdv,w50,vhel,vshap,obstr,$
                    logmhi,l,b,vlg,vcmb,vtonry,ebv
   ;
   ; convert text file to html
   singg_general_table, outsamp, outpage, title, subtitle
   ;
   ; close data base
   dbclose, dummy
   ;
END 
