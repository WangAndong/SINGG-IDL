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

