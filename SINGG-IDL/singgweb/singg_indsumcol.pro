PRO singg_indsumcol, unit, nminirow, tabnum, tabpage
   printf,unit,'<TD ROWSPAN='+strn(nminirow)+' ALIGN=center VALIGN=middle bgcolor=#fffff>' $
    +'<a href="'+tabpage+'">'+strn(tabnum)+'</a></TD>'
END 

