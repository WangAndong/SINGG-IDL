PRO singg_readsamp,sampfil,objname,cat,rastr,decstr,nednam,peak,sdv,w50,vhel,vshap,obstr,$
                   logmhi,l,b,d,ra,dec,obs
   h0 = 70.0
   nskip=2
   readcol,sampfil,format='A,A,A,A,A,F,F,F,F,F,A,F,F,F',$
    objname,cat,rastr,decstr,nednam,peak,sdv,w50,vhel,vshap,obstr,logmhi,l,b,$
    skipline=nskip
;
; Convert ra,dec from sexigesimal strings to floating point
;
   ra = 0.0*vhel
   dec = ra
   coords = [0, 0]
   peak = 1000.0*peak
   d = vshap/h0
   obs = make_array(n_elements(objname),/int)
   FOR i = 0, n_elements(objname)-1 DO BEGIN 
      coordstr = rastr[i]+' '+decstr[i]
      get_coords,coords,Instring=coordstr,/QUIET
      ra[i] = coords[0]
      dec[i] = coords[1]
      obs[i] = -1*(strtrim(obstr[i],1) eq 'Y')
   ENDFOR  
END 

; PRO singg_def_filts,vfilt1,vfilt2,filtnam,colorcode,vhel,filtid
;    vfilt1 = [-50.0,1100.0,2275.0,3300.0]
;    vfilt2 = [1100.0,2275.0,3300.0,140000.0]
;    filtnam = ['6568/28', '6605/32', '6628/33', 'other']
;    colorcode = ['#c0c0ff', '#c0ffff', '#c0ffc0', '#ffffc0']
;    filtid = fix(0.0*vhel - 1.0)
;    FOR i = 0, n_elements(filtnam)-1 DO BEGIN
;       j = where((vhel GT vfilt1[i] AND vhel LE vfilt2[i]), count)
;       IF count NE 0 THEN filtid[j] = i
;    ENDFOR 
; END 

PRO singg_def_filts_old,vfilt1,vfilt2,filtnam,colorcode,vhel,filtid
   vfilt1 = [-50.0,900.0,1250,2275.0,3300.0,5100.0,5500.0,7545.0,9053.0,11748.0,13850.0]
   vfilt2 = [900.0,1250,2275.0,3300.0,5100.0,5500.0,7545.0,9053.0,11748.0,13850.0,140000.0]
   filtnam = ['6568/28', 'GAP', '6605/32', '6628/33', 'KP1565', 'GAP', 'KP1566','6738/50','6781/78', '6826/78', 'other']
   colorcode = ['#c0c0ff', '#ffc0c0', '#c0ffff', '#c0ffc0', '#eeeec0', '#ffc0c0', '#ffffc0', '#ffd8c0', '#d0d0d0', '#c0c0c0', '#ffc0c0']
   filtid = fix(0.0*vhel - 1.0)
   FOR i = 0, n_elements(filtnam)-1 DO BEGIN
      j = where((vhel GT vfilt1[i] AND vhel LE vfilt2[i]), count)
      IF count NE 0 THEN filtid[j] = i
   ENDFOR 
END 

PRO singg_def_filts,vfilt1,vfilt2,filtnam,colorcode,vhel,filtid
   vfilt1 = [-50.0,830.0,1320,2275.0,3300.0,5100.0,5500.0,7545.0,9053.0,11748.0,13850.0]
   vfilt2 = [830.0,1320,2275.0,3300.0,5100.0,5500.0,7545.0,9053.0,11748.0,13850.0,140000.0]
   filtnam =   ['6568/28', '6596/35', '6605/32', '6628/33', 'KP1565',  'GAP',     'KP1566',  '6738/50', '6781/78', '6826/78', 'other']
   colorcode = ['#c0c0ff', '#ffc0ff', '#c0ffff', '#c0ffc0', '#eeeec0', '#ffffff', '#ffffc0', '#ffc0c0', '#d0d0d0', '#c0c0c0', '#ffffff']
   filtid = fix(0.0*vhel - 1.0)
   FOR i = 0, n_elements(filtnam)-1 DO BEGIN
      j = where((vhel GT vfilt1[i] AND vhel LE vfilt2[i]), count)
      IF count NE 0 THEN filtid[j] = i
   ENDFOR 
END 

PRO singg_pagetop, unit, title, subtitle
   printf,unit,'<HTML>'
   printf,unit,'<HEAD>'
   printf,unit,'<TITLE>'+title+' '+subtitle+'</TITLE>'
   printf,unit,'</HEAD>'
   printf,unit,'<BODY bgcolor="#c0c0c0" text="#000000" link="#400080" vlink="#C40000" alink="#00A000">'
   printf,unit,'<H1>'
   printf,unit,title
   printf,unit,'</H1>'
   printf,unit,'<H2>'
   printf,unit,subtitle
   printf,unit,'</H2>'
END

PRO singg_indhead, unit, nminicol, title, sourcefile
   printf,unit,'<P><TABLE border=1 cellpadding=5>'
   printf,unit,'<th colspan='+strn(nminicol+1)+' bgcolor=a8a8a8><center>'
   printf,unit,title+'<br><a href="'+sourcefile+'">Text file (all sources)</a><br>'
   printf,unit,"<small>Click on the object Name for a 20'x20' DSS finder chart.<br>"
   printf,unit,"Use link in first column for summary table.</small>"
   printf,unit,'</center></th>'
   printf,unit,'<tr bgcolor="#a8a8a8">'
   printf,unit,'<td><center><b>Summary<br>Table</b></center></td>'
   printf,unit,'<td colspan='+strn(nminicol)+'><center><b>Objects</b><center></td>'
   printf,unit,'</tr>'
END 

PRO singg_indsumcol, unit, nminirow, tabnum, tabpage
   printf,unit,'<TD ROWSPAN='+strn(nminirow)+' ALIGN=center VALIGN=middle bgcolor=#fffff>' $
    +'<a href="'+tabpage+'">'+strn(tabnum)+'</a></TD>'
END 

PRO singg_indminicell, unit, cellno, nminicol, object, color, link
   mcolno = cellno MOD nminicol
   IF (cellno GT 0 AND mcolno EQ 0) THEN printf,unit,'<tr>' 
   IF (link) THEN printf,unit,'<TD BGCOLOR='+color+'><a href="'+object+'.html">'+object+'</a></TD>' $
    ELSE printf,unit,'<TD BGCOLOR='+color+'>'+object+'</TD>' 
   IF (mcolno EQ nminicol-1) THEN printf,unit,'</tr>' 
END 

PRO singg_tnavbar, unit, tableno, maxtabs, sourcefile
   navbar = '<p>Pages: '
   IF tableno GT 1 THEN BEGIN 
      fname = 'sample'+strn(tableno-1,length=3,padchar='0')+'.html'
      navbar = navbar+'[<a href="'+fname+'">&lt;prev ('+strn(tableno-1)+')</a>] '
   ENDIF 
   navbar = navbar+'[<a href="index.html">Index</a>] '
   IF tableno LT maxtabs THEN BEGIN 
      fname = 'sample'+strn(tableno+1,length=3,padchar='0')+'.html'
      navbar = navbar+'[<a href="'+fname+'">next ('+strn(tableno+1)+')&gt;</a>] '
   ENDIF 
   FOR i = 1, maxtabs DO BEGIN 
      fname = 'sample'+strn(i,length=3,padchar='0')+'.html'
      navbar = navbar+'[<a href="'+fname+'">'+strn(i)+'</a>] '
      ENDFOR 
   navbar = navbar+'[<a href="'+sourcefile+'">Text file (all sources)</a>]</p>'
   printf,unit,navbar
END 

PRO singg_observed_link, unit, obstab
   printf,unit,'<p><a href="'+obstab+'">Observed sources table</a></p>'
END 

PRO singg_histpl_links, unit, samppl, pfile=pfile
   tablnk = ' ' 
   IF keyword_set(pfile) THEN tablnk = ' , <a href="'+pfile+'">Table</a>'
   printf,unit,'<p>HI mass histogram: <a href="'+samppl+'">Plot</a>'+tablnk+' </p>'
END 

PRO singg_sample_link, unit, samptab
   printf,unit,'<p><a href="'+samptab+'">Total sample</a></p>'
END 

PRO singg_thead, unit, title, subtitle
   printf,unit,'<P><TABLE border=1 cellpadding=3>'

   printf,unit,'<th colspan=15 bgcolor=a8a8a8><center>'
   printf,unit,title+'<br>'+subtitle+'<br>'
   printf,unit,"<small>Click on the object Name for a 20'x20' DSS finder chart."
   printf,unit,"The coordinates are from HIPASS.</small>"

   printf,unit,'</center></th>'

   printf,unit,'<tr bgcolor="#a8a8a8">'
   printf,unit,'<td>Object Name</td>'
   printf,unit,'<td><center>RA<br>[J2000]</center></td>'
   printf,unit,'<td><center>DEC<br>[J2000]</center></td>'
   printf,unit,'<td><center>Gal long<br>[<sup>o</sup>]</center></td>'
   printf,unit,'<td><center>Gal lat<br>[<sup>o</sup>]</center></td>'
   printf,unit,'<td><center>V<sub>hel</sub><br>[km/s]</center></td>'
   printf,unit,'<td><center>W<sub>50</sub><br>[km/s]</center></td>' 
   printf,unit,'<td><center>S<sub>peak</sub><br>[mJy]</center></td>'
   printf,unit,'<td><center>Int(S dv)<br>[Jy km/s]</center></td>'
   printf,unit,'<td><center>D<br>[Mpc]</center></center></td>'
   printf,unit,'<td><center>log(M<sub>HI</sub>)<br>[log(M<sub>sun</sub>)]</center></td>'
   printf,unit,'<td><center>best<br>filter</center></td>'
   printf,unit,'<td><center>Obs<br>?</center></td>'
   printf,unit,'<td><center>Source</center></td>'
   printf,unit,'<td><center>id</center></td>'
   printf,unit,'</tr>'
END 

PRO singg_tabrow, unit, colrow, objname, rastr, decstr, l, b, vhel, w50, peak, sdv, $
                  d, logmhi, filter, obs, cat, nednam
   printf,unit,'<tr bgcolor="'+colrow+'">'
   printf,unit,'<td><a href="'+objname+'.html">'+objname+'</a></td>'
   printf,unit,'<td>'+rastr+'</td>'
   printf,unit,'<td>'+decstr+'</td>'
   printf,unit,'<td>',l,'</td>',format="(A4,I4,A5)"
   printf,unit,'<td>',b,'</td>',format="(A4,I4,A5)"
   printf,unit,'<td>',vhel,'</td>',format="(A4,F7.1,A5)"
   printf,unit,'<td>',w50,'</td>',format="(A4,F5.1,A5)"
   printf,unit,'<td>',peak,'</td>',format="(A4,F6.1,A5)"
   printf,unit,'<td>',sdv,'</td>',format="(A4,F6.1,A5)"
   printf,unit,'<td><center>',d,'</center></td>',format="(A12,F5.1,A14)"
   printf,unit,'<td><center>',logmhi,'</center></td>',format="(A12,F5.2,A14)"
   printf,unit,'<td>'+filter+'</td>'
   printf,unit,'<td>'+obs+'</td>'
   printf,unit,'<td>'+cat+'</td>'
   printf,unit,'<td>'+nednam+'</td>'
   printf,unit,'</tr>'
END

PRO singg_tfoot, unit
   printf,unit,'</TABLE></P>'
END  

PRO singg_runhead, unit, title, subtitle, sourcefile
   printf,unit,'<P><TABLE border=1 cellpadding=3>'

   printf,unit,'<th colspan=12 bgcolor=a8a8a8><center>'
   printf,unit,title+'<br>'+subtitle+'<br>'
   printf,unit,'<a href="'+sourcefile+'">data file</a><br>'
   printf,unit,"<small>Click on the object Name for a 20'x20' DSS finder chart."
   printf,unit,"The coordinates are from HIPASS.</small>"

   printf,unit,'</center></th>'

   printf,unit,'<tr bgcolor="#a8a8a8">'
   printf,unit,'<td>Object Name</td>'
   printf,unit,'<td><center>Source</center></td>'
   printf,unit,'<td><center>RA<br>[J2000]</center></td>'
   printf,unit,'<td><center>DEC<br>[J2000]</center></td>'
   printf,unit,'<td><center>Gal (l,b)<br>[<sup>o</sup>,<sup>o</sup>]</center></td>'
   printf,unit,'<td><center>V<sub>hel</sub><br>[km/s]</center></td>'
   printf,unit,'<td><center>W<sub>50</sub><br>[km/s]</center></td>' 
   printf,unit,'<td><center>S<sub>peak</sub><br>[mJy]</center></td>'
   printf,unit,'<td><center>log(M<sub>HI</sub>)<br>[log(M<sub>sun</sub>)]</center></td>'
   printf,unit,'<td><center>best<br>filter</center></td>'
   printf,unit,'<td><center>Obser<br>ved?</center></td>'
   printf,unit,'<td>Plots</td>'
   printf,unit,'</tr>'
END 

PRO singg_runrow, unit, colrow, objname, rastr, decstr, l, b, vhel, w50, peak, sdv, $
                  d, logmhi, filter, obs, cat, nednam, airplt, hispec, xdssb, xdssr, xdssi
   lbstr = strn(fix(l+0.5))+','+strn(fix(b+0.5))
   printf,unit,'<tr bgcolor="'+colrow+'">'
   printf,unit,'<td><a href="'+objname+'.html">'+objname+'</a></td>'
   printf,unit,'<td>'+cat+'</td>'
   printf,unit,'<td>'+rastr+'</td>'
   printf,unit,'<td>'+decstr+'</td>'
   printf,unit,'<td>'+lbstr+'</td>'
   printf,unit,'<td>',vhel,'</td>',format="(A4,F7.1,A5)"
   printf,unit,'<td>',w50,'</td>',format="(A4,F5.1,A5)"
   printf,unit,'<td>',peak,'</td>',format="(A4,F6.1,A5)"
   printf,unit,'<td><center>',logmhi,'</center></td>',format="(A12,F5.2,A14)"
   printf,unit,'<td>'+filter+'</td>'
   printf,unit,'<td>'+obs+'</td>'
   plts = '<a href="'+airplt+'">X(t)</a>'
   IF (hispec NE '') THEN plts = plts+' <a href="'+hispec+'">HI spectrum</a>' 
   IF (xdssb NE '') THEN plts = plts+' <a href="'+xdssb+'">XDSS(B)</a>'
   IF (xdssr NE '') THEN plts = plts+' <a href="'+xdssr+'">XDSS(R)</a>'
   IF (xdssi NE '') THEN plts = plts+' <a href="'+xdssi+'">XDSS(I)</a>'
   printf,unit,'<td>'+plts+'</td>'
   printf,unit,'</tr>'
END

PRO singg_pagebot, unit
   date=systime()
   printf,unit,'<p><small>Page generated automatically with ' $
    +'<a href="singg_genweb.pro">singg_genweb.pro</a> on '$
    +date+'<br>'
   printf,unit,'Page maintanined by <a href=mailto:meurer@pha.jhu.edu>Gerhardt Meurer</a></small>'
   printf,unit,'</BODY>'
   printf,unit,'</HTML>'
END 

PRO singg_objpage, unit, colrow, objname, rastr, decstr, l, b, vhel, w50, peak, sdv, $
                   d, logmhi, filter, obs, cat, nednam, imgfound, jpgflag=jpgflag
   IF keyword_set(jpgflag) THEN usejpg = jpgflag ELSE usejpg = 0b 
   IF usejpg THEN imgnam = objname+'.jpg' ELSE imgnam = objname+'.gif' 
   found = findfile(imgnam,count=nfound)
   IF (nfound EQ 1) THEN BEGIN 
      imgfound = 1b
   ENDIF ELSE BEGIN
      imgfound = 0b
      print,' Could not find : ',imgnam
   ENDELSE 
   printf,unit,'<HTML>'
   printf,unit,'<HEAD>'
   printf,unit,'<TITLE>Finder Chart: '+objname+'</TITLE>'
   printf,unit,'</HEAD>'
   printf,unit,'<BODY BGCOLOR="#FFFFFF">'
   printf,unit,'<P>'
;   printf,unit,'<pre>'
   printf,unit,'<img src='+imgnam+'></a>'
   printf,unit,'</P>'
   printf,unit,'<br>'
   printf,unit,'<P><TABLE border=1 cellpadding=3 bgcolor='+colrow+'>'
   printf,unit,'<tr><td><b>Property</b></td><td><b>Value</b></td><td><b>Property</b></td><td><b>Value</b></td></tr>' 
   printf,unit,'<tr><td>Name</td>               <td>'+objname+'</td>     '
   printf,unit,'    <td>NED id</td>             <td>'+nednam+'</td></tr>'  
   printf,unit,'<tr><td>Cat. source</td>        <td>'+cat+'</td>     '
   printf,unit,'    <td>Gal (l,b)</td>          <td>'+strn(fix(l+0.5))+','+strn(fix(b+0.5))+'</td></tr>'
   printf,unit,'<tr><td>RA(2000)</td>           <td>'+rastr+'</td>     '
   printf,unit,'    <td>DEC(2000)</td>          <td>'+decstr+'</td></tr>'
   printf,unit,'<tr><td>V<sub>hel</sub></td>    <td>', vhel, ' km/s </td>     ',format="(A36,F7.1,A16)"
   printf,unit,'    <td>W<sub>50</sub></td>     <td>', w50, ' km/s </td></tr>',format="(A36,F5.1,A16)"
   printf,unit,'<tr><td>S<sub>peak</sub></td>   <td>', peak, ' mJy </td>     ',format="(A36,F7.1,A15)"
   printf,unit,'    <td>Int(Sdv)</td>           <td>', sdv, ' Jy km/s </td></tr>',format="(A36,F7.1,A19)"
   printf,unit,'<tr><td>Dist</td>               <td>', d, ' Mpc </td>     ',format="(A36,F5.1,A15)"
   printf,unit,'    <td>log(M<sub>HI</sub>)</td><td>', logmhi, ' (solar units) </td></tr>',format="(A36,F6.2,A25)"
   printf,unit,'<tr><td>Best filter</td>        <td>'+filter+'</td>     '
   printf,unit,'    <td>Observed?</td>          <td>'+obs+'</td></tr>'
;   printf,unit,'<tr><td>Name</td>               <td>'+objname+'</td></tr>'
;   printf,unit,'<tr><td>NED id</td>             <td>'+nednam+'</td></tr>'  
;   printf,unit,'<tr><td>Cat. source</td>        <td>'+cat+'</td></tr>'
;   printf,unit,'<tr><td>Observed?</td>          <td>'+obs+'</td></tr>'
;   printf,unit,'<tr><td>RA(2000)</td>           <td>'+rastr+'</td></tr>'
;   printf,unit,'<tr><td>DEC(2000)</td>          <td>'+decstr+'</td></tr>'
;   printf,unit,'<tr><td>V<sub>hel</sub></td>    <td>', vhel, ' km/s </td></tr>',format="(A36,F7.1,A16)"
;   printf,unit,'<tr><td>W<sub>50</sub></td>     <td>', w50, ' km/s </td></tr>',format="(A36,F5.1,A16)"
;   printf,unit,'<tr><td>S<sub>peak</sub></td>   <td>', peak, ' mJy </td></tr>',format="(A36,F7.1,A15)"
;   printf,unit,'<tr><td>Int(Sdv)</td>           <td>', sdv, ' Jy km/s </td></tr>',format="(A36,F7.1,A19)"
;   printf,unit,'<tr><td>Dist</td>               <td>', d, ' Mpc </td></tr>',format="(A36,F6.2,A15)"
;   printf,unit,'<tr><td>log(M<sub>HI</sub>)</td><td>', logmhi, ' (solar units) </td></tr>',format="(A36,F6.2,A25)"
;   printf,unit,'<tr><td>Best filter</td>        <td>'+filter+'</td></tr>'
   printf,unit,'</TABLE></P>'
   printf,unit,''
   ;printf,fndunit,'</pre>'

end

; singg_genweb,'sample2_ravsort.dat','observed.html','masshist.png','masshist.dat',jpgflag=1b

PRO singg_genweb,sampfil,obspage,samppl,samptab,jpgflag=jpgflag
   IF keyword_set(jpgflag) THEN usejpg = jpgflag ELSE usejpg = 0b 

   ; title='SINGG: Working Sample (7 May 2001)'
   title='SINGG: Semi-final Sample (15 Oct 2001)'
   subtitle=''

;
; read sample
;
   singg_readsamp,sampfil,objname,cat,rastr,decstr,nednam,peak,sdv,w50,vhel,vshap,obstr,$
    logmhi,l,b,d,ra,dec,obs
  
   nminicol = 5
   nminirow = 4
   nminitab = nminicol*nminirow
   ntabs    = fix((n_elements(objname) + nminitab - 1)/nminitab)
   nobjcell = ntabs*nminitab

   singg_def_filts,vfilt1,vfilt2,filtnam,colorcode,vhel,filtid
;
; Open index web page.  
; Write index page table header
;
   indpage='index.html'
   OPENW, indunit, indpage, /GET_LUN, /MORE  

   singg_pagetop, indunit, title, subtitle
   singg_observed_link,indunit,obspage
   singg_histpl_links,indunit,samppl,pfile=samptab
   singg_indhead, indunit, nminicol, title, sampfil

   i = -1
   FOR k = 1, ntabs DO BEGIN 
      tabpage = 'sample'+strn(k,length=3,padchar='0')+'.html'
      openw, tabunit, tabpage, /GET_LUN, /MORE
      subtitle = 'Page '+strn(k)
      singg_pagetop, tabunit, title, subtitle
      singg_tnavbar, tabunit, k, ntabs, sampfil
      singg_observed_link,tabunit,obspage
      singg_histpl_links,tabunit,samppl,pfile=samptab
      singg_thead, tabunit, title, subtitle
;
      singg_indsumcol, indunit, nminirow, k, tabpage
      FOR j = 0, nminitab-1 DO BEGIN
         i = i + 1
         IF (i LE n_elements(objname)-1) THEN BEGIN 
;
; assign best filter, color
;
            filter = 'unknown'
            colrow = '#ffffff'
            IF filtid[i] GE 0 THEN BEGIN 
               filter = filtnam[filtid[i]]
               colrow = colorcode[filtid[i]]
            ENDIF 
;
; table entry
;
            singg_tabrow, tabunit, colrow, objname[i], rastr[i], decstr[i], l[i], b[i], $
             vhel[i], w50[i], peak[i], sdv[i], d[i], logmhi[i], filter, obstr[i], $
             cat[i], nednam[i]
;
; create individual web page
;
            fndpage=objname[i]+'.html'
            OPENW, fndunit, fndpage, /GET_LUN, /MORE  
            singg_objpage, fndunit, colrow, objname[i], rastr[i], decstr[i], l[i], b[i], $
             vhel[i], w50[j], peak[i], sdv[i], d[i], logmhi[i], filter, obstr[i], $
             cat[i], nednam[i], imgfound, jpgflag=usejpg
            singg_pagebot, fndunit
            FREE_LUN,fndunit
;
; Make entry in "minitable"
;
            singg_indminicell, indunit, j, nminicol, objname[i], colrow, 1 
         ENDIF ELSE BEGIN 
            singg_indminicell, indunit, j, nminicol, '&nbsp;', '#fffff', 0 
         ENDELSE 
      ENDFOR 
;
      singg_tfoot, tabunit
      singg_pagebot, tabunit
      FREE_LUN, tabunit

   ENDFOR 

   singg_tfoot, indunit

   singg_observed_link,indunit,obspage
   singg_histpl_links,indunit,samppl

   singg_pagebot, indunit
   FREE_LUN, indunit

end

PRO singg_run04

   rarange = [8.15, 22.75]
   vrange  = [0.0, 3500.0]
   specdir = '/home/meurer/public_html/research/singg/HIspec/'
   sampdir = '/home/meurer/public_html/research/singg/sample/'
   sampfil = 'sample_ravsort_temp.dat'
   rundir = '/home/meurer/public_html/research/singg/run04/sources/'
   runname = 'Run 4 (May 2001)'
   xdir = '/data1/acs6/meurer/singg/sample_selection/cadc/xdss/'
   startdate = [5.0, 14.0, 2001.0, 18.1] ; month, day year, hour
   xduration = 13.11
   dstflag = 0

   singg_runsel,rarange,vrange,specdir,sampdir,sampfil,$
     rundir,runname,xdir,startdate,xduration,dstflag

END 

PRO singg_run05

   rarange = [10.0, 01.0]
   vrange  = [0.0, 3500.0]
   specdir = '/home/meurer/public_html/research/singg/HIspec/'
   sampdir = '/home/meurer/public_html/research/singg/sample/'
   sampfil = 'sample_ravsort.dat'
   rundir = '/home/meurer/public_html/research/singg/run05/sources/'
   runname = 'Run 5 (June 2001)'
   xdir = '/data1/acs6/meurer/singg/sample_selection/cadc/xdss/'
   startdate = [6.0, 10.0, 2001.0, 17.97] ; month, day year, hour
   xduration = 13.50
   dstflag = 0

   singg_runsel,rarange,vrange,specdir,sampdir,sampfil,$
     rundir,runname,xdir,startdate,xduration,dstflag

END 

PRO singg_run06

   rarange = [16.75, 4.25]
   vrange  = [0.0, 3500.0]
   specdir = '/home/meurer/public_html/research/singg/HIspec/'
   sampdir = '/home/meurer/public_html/research/singg/sample/'
   sampfil = 'sample_ravsort.dat'
   rundir = '/home/meurer/public_html/research/singg/run06/sources/'
   runname = 'Run 6 (September 2001)'
   xdir = '/data1/acs6/meurer/singg/sample_selection/cadc/xdss/'
   startdate = [09.0, 13.0, 2001.0, 18.7] ; month, day year, hour
   xduration = 11.88
   dstflag = 0

   singg_runsel,rarange,vrange,specdir,sampdir,sampfil,$
    rundir,runname,xdir,startdate,xduration,dstflag

END 

PRO singg_run07

   rarange = [19.95, 8.15]
   vrange  = [0.0, 3500.0]
   specdir = '/home/meurer/public_html/research/singg/HIspec/'
   sampdir = '/home/meurer/public_html/research/singg/sample/'
   sampfil = 'sample2_ravsort.dat'
   rundir = '/home/meurer/public_html/research/singg/run07/sources/'
   runname = 'Run 7 (October 2001)'
   xdir = '/data1/acs6/meurer/singg/sample_selection/from_cadc/xdss/'
   startdate = [10.0, 25.0, 2001.0, 20.13] ; month, day year, hour
   xduration = 10.6
   dstflag = 1
   usejpg = 1b

   singg_runsel,rarange,vrange,specdir,sampdir,sampfil,$
    rundir,runname,xdir,startdate,xduration,dstflag,jpgflag=usejpg

END 

PRO singg_run08

   rarange = [03.95, 16.23]
   vrange  = [0.0, 14000.0]
   specdir = '/home/meurer/public_html/research/singg/HIspec/'
   sampdir = '/home/meurer/public_html/research/singg/sample/'
   sampfil = 'sample2_ravsort.dat'
   rundir = '/home/meurer/public_html/research/singg/run08/sources/'
   runname = 'Run 8 (February 2002)'
   xdir = '/data2/acs27/meurer/singg/sample_selection/from_cadc/xdss/'
   startdate = [2.0, 16.0, 2002.0, 20.6] ; month, day year, hour
   xduration = 10.667
   dstflag = 1
   usejpg = 1b

   singg_runsel,rarange,vrange,specdir,sampdir,sampfil,$
    rundir,runname,xdir,startdate,xduration,dstflag,jpgflag=usejpg

END 

PRO singg_run09

   rarange = [06.53, 20.67]
   vrange  = [0.0, 14000.0]
   specdir = '/home/meurer/public_html/research/singg/HIspec/'
   sampdir = '/home/meurer/public_html/research/singg/sample/'
   sampfil = 'sample2_ravsort.dat'
   rundir = '/home/meurer/public_html/research/singg/run09/sources/'
   runname = 'Run 9 (April 2002)'
   xdir = '/data2/acs27/meurer/singg/sample_selection/from_cadc/xdss/'
   startdate = [4, 14.0, 2002.0, 18.5] ; month, day year, hour
   xduration = 12.4333
   dstflag = 0
   usejpg = 1b

   singg_runsel,rarange,vrange,specdir,sampdir,sampfil,$
    rundir,runname,xdir,startdate,xduration,dstflag,jpgflag=usejpg

END 

PRO singg_run10

   rarange = [08.5, 23.43]
   vrange  = [0.0, 14000.0]
   specdir = '/home/meurer/public_html/research/singg/HIspec/'
   sampdir = '/home/meurer/public_html/research/singg/sample/'
   sampfil = 'sample2_ravsort.dat'
   rundir = '/home/meurer/public_html/research/singg/run10/sources/'
   runname = 'Run 10 (DD, May 2002)'
   xdir = '/data2/acs27/meurer/singg/sample_selection/from_cadc/xdss/'
   startdate = [5, 22.0, 2002.0, 18.0333] ; month, day year, hour
   xduration = 13.2667
   dstflag = 0
   usejpg = 1b

   singg_runsel,rarange,vrange,specdir,sampdir,sampfil,$
    rundir,runname,xdir,startdate,xduration,dstflag,jpgflag=usejpg

END 

PRO singg_run11

   rarange = [13.5, 04.25]
   vrange  = [0.0, 14000.0]
   specdir = '/home/meurer/public_html/research/singg/HIspec/'
   sampdir = '/home/meurer/public_html/research/singg/sample/'
   sampfil = 'sample2_ravsort.dat'
   rundir = '/home/meurer/public_html/research/singg/run11/sources/'
   runname = 'Run 11 (Aug 2002)'
   xdir = '/data2/acs27/meurer/singg/sample_selection/from_cadc/xdss/'
   startdate = [8, 2.0, 2002.0, 18.58] ; month, day year, hour
   xduration = 13.02
   dstflag = 0
   usejpg = 1b

   singg_runsel,rarange,vrange,specdir,sampdir,sampfil,$
    rundir,runname,xdir,startdate,xduration,dstflag,jpgflag=usejpg

END 

PRO singg_run12

   rarange = [21.52, 08.92]
   vrange  = [0.0, 14000.0]
   specdir = '/home/meurer/public_html/research/singg/HIspec/'
   sampdir = '/home/meurer/public_html/research/singg/sample/'
   sampfil = 'sample2_ravsort.dat'
   rundir = '/home/meurer/public_html/research/singg/run12/sources/'
   runname = 'Run 12 (Aug 2002)'
   xdir = '/data2/acs27/meurer/singg/sample_selection/from_cadc/xdss/'
   startdate = [11, 10.0, 2002.0, 20.33] ; month, day year, hour
   xduration = 10.23
   dstflag = 1b
   usejpg = 1b

   singg_runsel,rarange,vrange,specdir,sampdir,sampfil,$
    rundir,runname,xdir,startdate,xduration,dstflag,jpgflag=usejpg

END 

PRO singg_runsel,rarange,vrange,specdir,sampdir,sampfil,$
rundir,runname,xdir,startdate,xduration,dstflag,jpgflag=jpgflag
;
; Routine to select objects over an RA range, and write appropriate
; webpages
;
   IF keyword_set(jpgflag) THEN usejpg = jpgflag ELSE usejpg = 0b 
   IF usejpg THEN print,'using jpg' ELSE print,'using gif'
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
    logmhi,l,b,d,ra,dec,obs
;
; Pick sources to observe
;
   
   keep = where((ra GE ramin1 AND ra LE ramax1) OR (ra GE ramin2 AND ra LE ramax2) $
                AND (obs GE 0) AND (vhel GE vrange[0] AND vhel LE vrange[1]))
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
         airmassplot,'ctio',startdate,xduration,step,objname[j],15.0*ra[j],dec[j],/dst
      ENDIF ELSE BEGIN
         airmassplot,'ctio',startdate,xduration,step,objname[j],15.0*ra[j],dec[j]
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

PRO singg_make_observed, sampfil, nhd_samp, colobs, $
                         obslist, obsfil, update_fil
; PRO singg_make_observed, sampfil, nhd_samp, colobs, extrafil, nhd_extra, $
;                          obslist, obsfil, update_fil
;
; From a list of observed galaxies & the sample file, make
; observed.dat file containing all the info in the sample file for the
; observed galaxies.  Also update the sample file to indicate which
; galaxies were observed.
; 
   ; read input file, first the header, then the data
   readfmt,sampfil,'A120',head,numline=nhd_samp
   readfmt,sampfil,'A120',datline,skipline=nhd_samp
   samp_matched  = make_array(n_elements(datline), /INT, VALUE=0)
   goodlin       = make_array(n_elements(datline), /BYTE, VALUE=1b)
   FOR i = 0, n_elements(datline)-1 DO IF (strmid(datline[i],0,1) EQ '#') THEN goodlin[i] = 0b
   ;
   ; reset all observations to N
   FOR i = 0, n_elements(datline)-1 DO IF (goodlin[i]) THEN $
    datline[i]   = strtrim(strmid(datline[i],0,colobs-1)+'N'+strmid(datline[i],colobs))
   FOR i = 0, n_elements(datline)-1 DO IF (strmid(datline[i],0,1) EQ '#') THEN goodlin[i] = 0b
   goodind       = where(goodlin,ngoodlin)
   namsamp       = strtrim(strmid(datline,0,10),2)

   ; read observed galaxies list
   readfmt,obslist,'A10,A3',namobs,statin
   goodnam       = make_array(n_elements(namobs), /BYTE, VALUE=1b)
   ; strip out comments
   FOR i = 0, n_elements(namobs)-1 DO IF (strmid(namobs[i],0,1) EQ '#') THEN goodnam[i] = 0b
   ;
   ; I probably should have some condition for no good names...
   namobs        = strtrim(namobs[where(goodnam)],2)
   stat          = statin[where(goodnam)]
   obs_matched   = make_array(n_elements(namobs), /INT, VALUE=0)
   ;
   ; compare names; loop over sample
   openw,luno,obsfil,/get_lun
   FOR i = n_elements(head)-2, n_elements(head)-1 DO printf,luno,strtrim(head[i])
   nobserved = 0
   FOR i = 0, ngoodlin-1 DO BEGIN 
      j               = goodind[i]
      matches         = STRCMP(namsamp[j],namobs,/FOLD_CASE)
      samp_matched[j] = total(matches)
      IF (samp_matched[j] GT 0 ) THEN BEGIN
         im           = where(matches GT 0,ncount)
         lasto        = im[ncount-1]
         ;datline[j]   = strtrim(strmid(datline[j],0,colobs-1)+'Y'+strmid(datline[j],colobs))
         ;
         ; Make sure that if any of the observations are with stat = 'Y'
         ; that the output status also equals Y.  Otherwise use last 
         ; last stat.
         ;
         wherey       = where(strupcase(strtrim(stat[im],2)) EQ 'Y', ny)
         IF ny GE 1 THEN usestat = 'Y  ' ELSE usestat = stat[im[ncount-1]]
         ;
         datline[j]   = strtrim(strmid(datline[j],0,colobs-1)+usestat+strmid(datline[j],colobs+2))
         ; datline[j]   = strtrim(strmid(datline[j],0,colobs-1)+stat[lasto]+strmid(datline[j],colobs+2))
         ; print,datline[j]
         printf,luno,datline[j]
         IF (samp_matched(goodind[j]) GT 1) $ 
          THEN print,'**** Warning: ',namsamp[j],' matched ',samp_matched[j],' times'
         nobserved = nobserved + 1
      ENDIF 
      obs_matched = obs_matched + matches
   ENDFOR 
   ;
   ; note objects in observed.lis without matches
   notmatched = where(obs_matched EQ 0,count)
   IF (count GT 0) THEN BEGIN
      print,'**** Warning - observed sources not found in sample:'
      FOR i = 0,count-1 DO print,'  ',namobs[notmatched[i]]
   ENDIF 
   ;
   ; write update file
   openw,lunu,update_fil,/get_lun
   FOR i = 0, n_elements(head)-1 DO printf,lunu,strtrim(head[i])
   FOR i = 0, n_elements(datline)-1 DO printf,lunu,datline[i]
   close,lunu
   ;
   ; Read in extras, append to observed list
   ;
   ; This is no longer needed extras are now in sample2_ravsort.dat
   ;readfmt,extrafil,'A120',datline,skipline=nhd_extra
   ;FOR i = 0, n_elements(datline)-1 DO BEGIN
   ;   datline[i]   = strtrim(strmid(datline[i],0,colobs-1)+'Y'+strmid(datline[i],colobs))
   ;   ; print,datline[i]
   ;   printf,luno,datline[i]
   ;   nobserved = nobserved + 1
   ;ENDFOR 
   
   close,luno
   print, 'From singg_make_observed : galaxies observed = ', nobserved

   ; FOR i = 0, n_elements(namobs)-1 DO print,namobs[i],'  ',obs_matched[i]
END 

;singg_make_observed,'sample_ravsort.dat',5,89,'extras.dat',4,'observed.lis', $
; 'observed.dat','sample_ravsort.update'

PRO singg_observed_table,obsfil,samptab,outpage

   date=systime()

   title='SINGG: observed galaxies'
   subtitle='updated on ' + date

;
; read sample
;
   singg_readsamp,obsfil,objname,cat,rastr,decstr,nednam,peak,sdv,w50,vhel,vshap,obstr,$
    logmhi,l,b,d,ra,dec,obs

   singg_def_filts,vfilt1,vfilt2,filtnam,colorcode,vhel,filtid

   openw, outunit, outpage, /GET_LUN, /MORE
   singg_pagetop, outunit, title, subtitle
   singg_sample_link,outunit,samptab
   singg_thead, outunit, title, subtitle

   FOR i = 0, n_elements(objname)-1 DO BEGIN 
;
; assign best filter, color
;
      filter = 'unknown'
      colrow = '#ffffff'
      IF filtid[i] GE 0 THEN BEGIN 
         filter = filtnam[filtid[i]]
         colrow = colorcode[filtid[i]]
      ENDIF 
;
; table entry
;
      singg_tabrow, outunit, colrow, objname[i], rastr[i], decstr[i], l[i], b[i], $
       vhel[i], w50[i], peak[i], sdv[i], d[i], logmhi[i], filter, obstr[i], $
       cat[i], nednam[i]
;

   ENDFOR 

   singg_tfoot, outunit

   singg_sample_link,outunit,samptab

   singg_pagebot, outunit
   FREE_LUN, outunit

END

PRO singg_lmbound, lmbound
   ;
   ; Return default singg log HI mass bin boundaries
   ;
   lmbound = [7.0, 7.8, 8.2, 8.6, 8.8, 9.0, 9.2, 9.4, 9.6, 9.8, 10.0, 10.2, 10.4, 10.6, 11.0, 11.4]
   return
END 

PRO singg_masshist, file, lmbin, lmmin, lmmax, nmin, nmax, pfile=pfile
   ;
   ; Calculate mass histograms for 
   ;   1. All objects selected (white)
   ;   2. Objects observed (yellow)
   ;   3. Objects observed in photometric conditions (green)
   ;
   ; file  -> file to read from
   ; lmbin -> default bin size
   ; lmmin -> min log mass of window
   ; lmmax -> max log mass of window
   ; nmin  -> min number (y) to plot
   ; nmax  -> max number to plot
   ; pfile -> File to print histogram
   ;
   forward_FUNCTION singg_lmbound
   ;
   singg_readsamp,file,objname,cat,rastr,decstr,nednam,peak,sdv,w50,vhel,vshap,obstr,$
    logmhi,l,b,d,ra,dec,obs
   ostr = strupcase(strtrim(obstr,2))
   ;
   binid    = make_array(n_elements(objname),/int)
   ;
   singg_lmbound, lmbound
   nbin     = n_elements(lmbound)-1
   lm0      = lmbound[0:nbin-1]
   lm1      = lmbound[1:nbin]
   lmhist1  = make_array(nbin,/int,value=0)
   lmhist2  = lmhist1
   lmhist3  = lmhist1
   lmhistn1 = make_array(nbin,/float,value=0.0)
   lmhistn2 = lmhistn1
   lmhistn3 = lmhistn1
   ;
   ; make plot window
   ;
   xrng    = [lmmin, lmmax]
   yrng    = [nmin, nmax]
   xtitle  = "log(M!DHI!N/M!D!9n!X!N)"
   ytitle  = string(format='("Number per ", F3.1, " dex of mass")', lmbin)
   x       = lmbound
   y       = 0.0*lmbound

   setplotcolors
   setbgfg,!white,!black
   plot, x, y, xrange=xrng, yrange=yrng, linestyle=0, xstyle=1, ystyle=1, $
    xtitle=xtitle, ytitle=ytitle, thick=2.0
   ;
   ; Calculate number in each bin and oplot box
   ;
   FOR i = 0, nbin-1 DO BEGIN 
      j1          = where((logmhi GE lm0[i]) AND (logmhi LT lm1[i]), count)
      lmhist1[i]  = count
      IF lmhist1[i] GE 1 THEN BEGIN 
         binid[j1] = i
      ENDIF 
      j2          = where((logmhi GE lm0[i]) AND (logmhi LT lm1[i]) AND (strmid(ostr,0,1) EQ 'Y'), count)
      lmhist2[i]  = count
      j3          = where((logmhi GE lm0[i]) AND (logmhi LT lm1[i]) AND (ostr EQ 'Y'), count)
      lmhist3[i]  = count
      lmhistn1[i] = float(lmhist1[i])*lmbin/(lm1[i] - lm0[i])
      lmhistn2[i] = float(lmhist2[i])*lmbin/(lm1[i] - lm0[i])
      lmhistn3[i] = float(lmhist3[i])*lmbin/(lm1[i] - lm0[i])
      x           = [lm0[i], lm0[i], lm1[i], lm1[i], lm1[i]]
      y           = lmhistn2[i]*[0.0, 1.0, 1.0, 0.0, 0.0]
      polyfill, x, y, color=!yellow
      oplot, x, y, linestyle=0, thick=2.0, color=!brown
      y           = lmhistn3[i]*[0.0, 1.0, 1.0, 0.0, 0.0]
      polyfill, x, y, color=!green
      oplot, x, y, linestyle=0, thick=2.0, color=!dgreen
      y           = lmhistn1[i]*[0.0, 1.0, 1.0, 0.0, 0.0]
      oplot, x, y, linestyle=0, thick=2.0, color=!black
   ENDFOR 
   ;
   ; Print results
   ;
   print, '# bin limits  Number in bin  ',ytitle
   print, '#  min   max  sel  obs  phot  sel    obs    phot'
   IF keyword_set(pfile) THEN BEGIN
     openw,luno,pfile,/get_lun
     printf, luno, '# bin limits  Number in bin    ',ytitle
     printf, luno, '#  min   max  sel  obs  phot  sel    obs    phot'
   ENDIF  
   FOR i = 0, nbin-1 DO BEGIN 
      print, lm0[i], lm1[i], lmhist1[i], lmhist2[i], lmhist3[i], $
       lmhistn1[i], lmhistn2[i], lmhistn3[i], $
       format='(f6.1, f6.1, i5, i5, i5, f7.2, f7.2, f7.2)'
      IF  keyword_set(pfile) THEN printf, luno, lm0[i], lm1[i], lmhist1[i], lmhist2[i], lmhist3[i], $
       lmhistn1[i], lmhistn2[i], lmhistn3[i], $
       format='(f6.1, f6.1, i5, i5, i5, f7.2, f7.2, f7.2)'
   ENDFOR 
   print, '# total :   ', total(lmhist1), total(lmhist2), total(lmhist3), format='(a, i5, i5, i5)'
   IF  keyword_set(pfile) THEN BEGIN 
      printf, luno, '# total :   ', $
       total(lmhist1), total(lmhist2), total(lmhist3), format='(a, i5, i5, i5)'
      close,luno
   ENDIF 
END 

; singg_masshist, 'sample2_ravsort.dat', 0.5, 6.0, 11.5, 0, 90

PRO singg_oldmasshist, file, lmbin, lmmin, lmmax, nmin, nmax
; Make a mass histogram plot out of either the sample or observed data file. 
   singg_readsamp,file,objname,cat,rastr,decstr,nednam,peak,sdv,w50,vhel,vshap,obstr,$
    logmhi,l,b,d,ra,dec,obs

   xrng = [lmmin, lmmax]
   yrng = [nmin, nmax]

   lm     = lmmin + 0.5*lmbin + lmbin*indgen(float((lmmax - lmmin)/lmbin), /float)
   lmhist = histogram(logmhi, binsize=lmbin, min=lmmin, max=lmmax)
   
   plot, lm, lmhist, xrange=xrng, yrange=yrng, psym=10, xstyle=1, ystyle=1, $
    xtitle="log(M!DHI!N/M!D!9n!X!N)", ytitle="Number", thick=2.0

   ;plothist, logmhi, lm, lmhist, bin=lmbin, xrange=xrng, yrange=yrng, /fil, $
   ; xstyle=1, ystyle=1, xtitle="log(M!DHI!N/M!D!9n!X!N)", ytitle="Number"
END 

PRO singg_genweb_all
   ;sampfil  = 'sample_ravsort.dat'
   sampfil   = 'sample2_ravsort.dat'
   nhd_samp  = 5
   nhd_ext   = 4
   colobs    = 89
   nmax      = 45
   exfile    = 'extras.dat'
   obslis    = 'observed.lis'
   obsfil    = 'observed.dat'
   shistpl   = 'masshist.png'
   shisttab  = 'masshist.dat'
   obspage   = 'observed.html'
   samptab   = 'index.html'
   updatefil = 'sample2_ravsort.update'
;
; update observed galaxies.
;
   print,' Running singg_make_observed '
   singg_make_observed,sampfil,nhd_samp,colobs,obslis,obsfil,updatefil
   ; singg_make_observed,sampfil,nhd_samp,colobs,exfile,nhd_ext,obslis,obsfil,updatefil
   print,' Running singg_observed_table '
   singg_observed_table,obsfil,samptab,obspage
;
; make mass histograms
;
   print,' making mass histograms '
   setplotcolors
   setbgfg,!white,!black

   singg_masshist, updatefil, 0.2, 6.0, 11.5, 0, nmax, pfile=shisttab
   makepng,shistpl,/color
;
; Update web pages
;
   print,' Running singg_genweb '
   singg_genweb,updatefil,obspage,shistpl,shisttab
;
; update the observed galaxies page
;
END 



