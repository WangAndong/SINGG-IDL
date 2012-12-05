PRO grp_trow_sep02, unit, id, posim, aim, bim, thetaim, fwhm, magauto, class, $
                    fstmp, fribn, fspc1, fspc2, fscii
   printf,unit,'<tr bgcolor="#eeeec0">'
   printf,unit,'<td>',id,'</td>'
   printf,unit,'<td>',posim[0],'<br>',posim[1],'</td>'
   printf,unit,'<td>',aim,'<br>',bim,'</td>'
   printf,unit,'<td>',thetaim,'<br>',fwhm,'</td>'
   printf,unit,'<td>',magauto,'<br>',class,'</td>'
   printf,unit,'<td><img src="'+fstmp+'">','<br>&nbsp;<br>','<img src="'+fribn+'"></td>'
   printf,unit,'<td><img src="'+fspc1+'"></td>'
   printf,unit,'<td><a href="'+fspc2+'">alt plot</a> <br><a href="'+fscii+'">data</a></td>'
   printf,unit,'</tr>'
END 

