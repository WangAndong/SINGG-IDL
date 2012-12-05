PRO grp_trow, unit, id, posim, aim, bim, thetaim, fwhm, magauto, class, fstmp, fribn, fspec
   printf,unit,'<tr bgcolor="#eeeec0">'
   printf,unit,'<td>',id,'</td>'
   printf,unit,'<td>',posim[0],'<br>',posim[1],'</td>'
   printf,unit,'<td>',aim,'<br>',bim,'</td>'
   printf,unit,'<td>',thetaim,'<br>',fwhm,'</td>'
   printf,unit,'<td>',magauto,'<br>',class,'</td>'
   printf,unit,'<td><img src="'+fstmp+'">','<br>&nbsp;<br>','<img src="'+fribn+'"></td>'
   printf,unit,'<td><img src="'+fspec+'"></td>'
   printf,unit,'</tr>'
END 

