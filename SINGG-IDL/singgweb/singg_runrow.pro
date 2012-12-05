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

