PRO grp_emhead, unit, title, filcat
   ;
   ; Make html header for emission line catalog
   ; unit    -> logical unit to write to
   ; title   -> title of table
   ; filcat  -> name of ascii catalog
   ;
   ; G. Meurer 11/2004 - doc & adjusted to accomodate 
   ;                     continuum & ew
   printf,unit,'<P>The spectrum plots on this page show the spectrum (black line), spectrum +/- error '
   printf,unit,'(gray lines), emission line fit (red line), continuum (blue line, over-plotted).</P>'
   printf,unit,'<P><a href="'+filcat+'">ASCII catalog</a>'
   printf,unit,'<P><TABLE border=1 cellpadding=3>'

   printf,unit,'<th colspan=10 bgcolor=a8a8a8><center>'
   printf,unit,title+'<br>'

   printf,unit,'</center></th>'

   printf,unit,'<tr bgcolor="#a8a8a8">'
   printf,unit,'<td rowspan=2>ID</td>'
   printf,unit,'<td>X<sub>image</sub><br>Y<sub>image</sub></td>'
   printf,unit,'<td>A<sub>image</sub><br>B<sub>image</sub></td>'
   printf,unit,'<td>theta<sub>image</sub><br>FWHM<sub>image</sub></td>'
   printf,unit,'<td>MAG<sub>AUTO</sub><br>Class</td>'
   printf,unit,'<td rowspan=2>Stamp<br>Ribbon</td>'
   printf,unit,'<td rowspan=2>Spectrum</td>'
   printf,unit,'<td rowspan=2>Center<br>Width<br>Flux</td>'
   printf,unit,'<td rowspan=2>Continuum<br>EW</td>'
   printf,unit,'<td rowspan=2>more<br>stuff</td>'
   printf,unit,'</tr>'
   printf,unit,'<tr bgcolor="#a8a8a8">'
   printf,unit,'<td>RA(deg)</td>'
   printf,unit,'<td>Dec(deg)</td>'
   printf,unit,'<td colspan=2>RA,Dec (sexigessimal)</td>'
END 

