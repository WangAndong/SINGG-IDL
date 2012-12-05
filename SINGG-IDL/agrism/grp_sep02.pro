PRO grp_thead_sep02, unit, title
   printf,unit,'<P><TABLE border=1 cellpadding=3>'

   printf,unit,'<th colspan=8 bgcolor=a8a8a8><center>'
   printf,unit,title+'<br>'

   printf,unit,'</center></th>'

   printf,unit,'<tr bgcolor="#a8a8a8">'
   printf,unit,'<td>ID</td>'
   printf,unit,'<td>X<sub>image</sub><br>Y<sub>image</sub></td>'
   printf,unit,'<td>A<sub>image</sub><br>B<sub>image</sub></td>'
   printf,unit,'<td>theta<sub>image</sub><br>FWHM<sub>image</sub></td>'
   printf,unit,'<td>MAG<sub>AUTO</sub><br>Class</td>'
   printf,unit,'<td>Stamp<br>Ribbon</td>'
   printf,unit,'<td>Spectrum</td>'
   printf,unit,'<td>more<br>stuff</td>'
   printf,unit,'</tr>'
END 

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

PRO grism_page_sep02, filhtml, title, elm, id, pos, aim, bim, thetaim, $
                      w50, magauto, class, pstmp, pspcc, pspcf, pribn, pscii
   ;
   ; make a page of grism data in tabular form
   ;
   openw, unitp, filhtml, /get_lun, /more
   grp_pagetop, unitp, title
   grp_thead_sep02, unitp, title
   ;
   FOR i = 0, n_elements(elm)-1 DO BEGIN
      grp_trow_sep02, unitp, id[elm[i]], [pos[elm[i],0],pos[elm[i],1]], aim[elm[i]], bim[elm[i]], $
       thetaim[elm[i]], w50[elm[i]], magauto[elm[i]], class[elm[i]], $
       nampng(pstmp,id[elm[i]]), nampng(pribn,id[elm[i]]), nampng(pspcc,id[elm[i]]), $
       nampng(pspcf,id[elm[i]]), namdat(pscii,id[elm[i]])
   ENDFOR
   ;
   grp_tfoot, unitp
   grp_pagebot, unitp
   free_lun, unitp
   
END

;PRO grp_trow_sep02, unit, id, posim, aim, bim, thetaim, fwhm, magauto,
;class, fstmp, fribn, fspcf, fspcc, fscii

