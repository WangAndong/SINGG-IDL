PRO singg_runsel,rarange,vrange,specdir,sampdir,sampfil,$
                 rundir,runname,xdir,startdate,xduration,dstflag,$
                 jpgflag=jpgflag,observatory=observatory,version=version
           
;
; Routine to select objects over an RA range, and write appropriate
; webpages
;
   IF keyword_set(jpgflag) THEN usejpg = jpgflag ELSE usejpg = 0b 
   IF usejpg THEN print,'using jpg' ELSE print,'using gif'
   IF keyword_set(observatory) THEN obsv = observatory ELSE obsv = 'ctio'
;
   ramin = rarange[0]
   ramax = rarange[1] 
;
   step = 15        ; minutes
;
   IF ramin LE ramax THEN BEGIN 
      ramin1 = ramin 
      ramax1 = ramax
      ramin2 = -100
      ramax2 = ramin2
   ENDIF ELSE BEGIN 
      ramin1 = ramin
      ramax1 = 24.00
      ramin2 =  0.00
      ramax2 = ramax
   ENDELSE
;
; read sample
;
   command1 = 'cp '+sampdir+sampfil+' '+rundir
   spawn,command1
   singg_readsamp,sampdir+sampfil,objname,cat,rastr,decstr,nednam,peak,sdv,w50,vhel,vshap,obstr,$
    logmhi,l,b,d,ra,dec,obs,vlg,vcmb,vtonry,ebv,version=version
   obstr = strupcase(strtrim(obstr,2))
;
; Pick sources to observe
;
   keep = where((ra GE ramin1 AND ra LE ramax1) OR (ra GE ramin2 AND ra LE ramax2) $
                AND (obstr NE 'Y') AND (vhel GE vrange[0] AND vhel LE vrange[1]))
;
   singg_def_filts,vfilt1,vfilt2,filtnam,colorcode,vhel,filtid
;
; make summary table
;
   title = 'SINGG: selected objects' 
   tabpage = rundir+'index.html'
   openw, tabunit, tabpage, /GET_LUN, /MORE
   subtitle = runname
   singg_pagetop, tabunit, title, subtitle
   singg_runhead, tabunit, title, subtitle, sampfil

   raord = sort(ra[keep])
   
   FOR i = 0, n_elements(keep)-1 DO BEGIN 
      j = keep[raord[i]]
;
; assign best filter, color
;
      filter = 'unknown'
      colrow = '#ffffff'
      IF filtid[j] GE 0 THEN BEGIN 
         filter = filtnam[filtid[j]]
         colrow = colorcode[filtid[j]]
      ENDIF 
;
; create individual web page
;
      ; command1 = 'cp '+sampdir+objname[j]+'.html '+rundir+objname[j]+'.html'
      fndpage=rundir+objname[j]+'.html'
      OPENW, fndunit, fndpage, /GET_LUN, /MORE  
      singg_objpage, fndunit, colrow, objname[j], rastr[j], decstr[j], l[j], b[j], $
       vhel[j], w50[j], peak[j], sdv[j], d[j], logmhi[j], filter, obstr[j], $
       cat[j], nednam[j], imgfound, jpgflag=usejpg
      singg_pagebot, fndunit
      FREE_LUN,fndunit
      IF usejpg THEN BEGIN 
         command2 = 'cp '+sampdir+objname[j]+'.jpg '+rundir+objname[j]+'.jpg'
      ENDIF ELSE BEGIN 
         command2 = 'cp '+sampdir+objname[j]+'.gif '+rundir+objname[j]+'.gif'
      ENDELSE 
      ; spawn,command1
      spawn,command2
      IF (dstflag GT 0) THEN BEGIN
         airmassplot,obsv,startdate,xduration,step,objname[j],15.0*ra[j],dec[j],/dst
      ENDIF ELSE BEGIN
         airmassplot,obsv,startdate,xduration,step,objname[j],15.0*ra[j],dec[j]
      ENDELSE
      airpng = objname[j]+'_airmass.png'
      makepng,rundir+airpng,/color
      ;
      ; look for HIspec
      ;
      hispec   = objname[j]+'_HIspec.png'
      junkstr  = findfile(specdir+hispec, count=nfound)
      IF (nfound EQ 1) THEN BEGIN 
         command1 = 'cp '+specdir+hispec+' '+rundir+hispec
         spawn,command1
      ENDIF ELSE BEGIN 
         hispec = ''
      ENDELSE 
      ;
      ; look for extra jpg finders
      ;
      IF usejpg THEN BEGIN 
         xdssb = objname[j]+'_xdss_b.jpg'
         xdssr = objname[j]+'_xdss_r.jpg'
         xdssi = objname[j]+'_xdss_i.jpg'
      ENDIF ELSE BEGIN 
         xdssb = objname[j]+'_xdss_b.gif'
         xdssr = objname[j]+'_xdss_r.gif'
         xdssi = objname[j]+'_xdss_i.gif'
      ENDELSE 
      junkstr = findfile(xdir+xdssb, count=nfound)
      IF (nfound EQ 1) THEN BEGIN
         command1 = 'cp '+xdir+xdssb+' '+rundir+xdssb
         spawn,command1
      ENDIF ELSE BEGIN
         xdssb = ''
      ENDELSE 
      junkstr = findfile(xdir+xdssr, count=nfound)
      IF (nfound EQ 1) THEN BEGIN
         command1 = 'cp '+xdir+xdssr+' '+rundir+xdssr
         spawn,command1
      ENDIF ELSE BEGIN
         xdssr = ''
      ENDELSE 
      junkstr = findfile(xdir+xdssi, count=nfound)
      IF (nfound EQ 1) THEN BEGIN
         command1 = 'cp '+xdir+xdssi+' '+rundir+xdssi
         spawn,command1
      ENDIF ELSE BEGIN
         xdssi = ''
      ENDELSE 
      singg_runrow, tabunit, colrow, objname[j], rastr[j], decstr[j], $ 
       l[j], b[j], vhel[j], w50[j], peak[j], sdv[j], $
       d[j], logmhi[j], filter, obstr[j], cat[j], nednam[j], $
       airpng, hispec, xdssb, xdssr, xdssi
      
   ENDFOR 

   singg_tfoot, tabunit
   singg_pagebot, tabunit
   FREE_LUN, tabunit

END 

