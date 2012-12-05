PRO grp_logrow, unit, id, posim, aim, bim, thetaim, fwhm, magauto, class, ra, dec, rdstr, $
                    fstmp, fribn, fspc1, fspc2, fscii
   printf,unit,'<tr bgcolor="#eed0d0">'
   printf,unit,'<td rowspan=2>',id,'</td>'
   printf,unit,'<td>',posim[0],'<br>',posim[1],'</td>'
   printf,unit,'<td>',aim,'<br>',bim,'</td>'
   printf,unit,'<td>',thetaim,'<br>',fwhm,'</td>'
   printf,unit,'<td>',magauto,'<br>',class,'</td>'
   printf,unit,'<td rowspan=2><img src="'+fstmp+'">','<br>&nbsp;<br>','<img src="'+fribn+'"></td>'
   printf,unit,'<td rowspan=2><img src="'+fspc2+'"></td>'
   printf,unit,'<td rowspan=2><a href="'+fspc1+'">plot1</a><br><a href="'+fscii+'">data</a></td>'
   printf,unit,'</tr>'
   printf,unit,'<tr bgcolor="#eed0d0">'
   printf,unit,'<td>',ra,'</td>'
   printf,unit,'<td>',dec,'</td>'
   printf,unit,'<td colspan=2>'+rdstr+'</td>'
   printf,unit,'</tr>'
END 

