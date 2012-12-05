PRO grp_emrow, unit, id, posim, aim, bim, thetaim, fwhm, magauto, class, ra, dec, rdstr, cen, width, flux, $
               continuum, ew, fstmp, fribn, ffit, fspc1, fspc2, fscii
   ;
   ; write a table row to the emission line html catalog file
   ;
   ; unit      -> logical unit to write to.
   ; id        -> id # odf object
   ; posim     -> x,y position of obj (2 elem array).
   ; aim       -> semi major axis size
   ; bim       -> semi minor axis size
   ; thetaim   -> PA of sma on image
   ; fwhm      -> full width at half max
   ; magauto   -> magnitude
   ; class     -> star / gal classifier
   ; ra        -> RA in decimal degree
   ; dec       -> Declination in decimal degree
   ; rdstr     -> sexigessimal RA & DEC
   ; cen       -> center of line
   ; width     -> width of line
   ; flux      -> flux of line
   ; continuum -> continuum level at line center
   ; ew        -> equivalent width of line
   ; fstmp     -> name of stamp file
   ; fribn     -> name of ribbon file
   ; ffit      -> name of fit file
   ; fspc1     -> name of spec 1
   ; fspc2     -> name of spec 2
   ; fscii     -> ascii file name
   ;
   ; G. Meurer 11/2004 - documented & adjusted to include continuum 
   ;                     and EW.
   printf,unit,'<tr bgcolor="#d0d0ee">'
   printf,unit,'<td rowspan=2>',id,'</td>'
   printf,unit,'<td>',posim[0],'<br>',posim[1],'</td>'
   printf,unit,'<td>',aim,'<br>',bim,'</td>'
   printf,unit,'<td>',thetaim,'<br>',fwhm,'</td>'
   printf,unit,'<td>',magauto,'<br>',class,'</td>'
   printf,unit,'<td rowspan=2><img src="'+fstmp+'">','<br>&nbsp;<br>','<img src="'+fribn+'"></td>'
   printf,unit,'<td rowspan=2><img src="'+ffit+'"></td>'
   printf,unit,'<td rowspan=2>',cen,'<br>',width,'<br>',flux,'</td>'
   printf,unit,'<td rowspan=2>',continuum,'<br>',ew,'</td>'
   printf,unit,'<td rowspan=2><a href="'+fspc1+'">plot1</a><br><a href="'+fspc2+'">plot2</a><br><a href="'+fscii+'">data</a></td>'
   printf,unit,'</tr>'
   printf,unit,'<tr bgcolor="#d0d0ee">'
   printf,unit,'<td>',ra,'</td>'
   printf,unit,'<td>',dec,'</td>'
   printf,unit,'<td colspan=2>'+rdstr+'</td>'
   printf,unit,'</tr>'
END 

