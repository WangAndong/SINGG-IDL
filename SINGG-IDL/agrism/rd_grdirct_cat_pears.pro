PRO rd_grdirct_cat, file, id, xim, yim, magauto, aim, bim, thetaim, w50, class
   ;
   ; Version of rd_grdirct_cat that works for pears catalogs
   readcol, file, xim, yim, id, d1, d2, magauto, aim, bim, thetaim, $
    d3, d4, d5, d6, w50, class, comment="#", format='(i,f,f,f,f,f,f,f,f,f,f,f,f,f,f)'
END
