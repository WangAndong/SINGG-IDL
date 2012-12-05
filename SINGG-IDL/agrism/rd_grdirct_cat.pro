PRO rd_grdirct_cat, file, id, xim, yim, magauto, aim, bim, thetaim, w50, class, $
                    type=type
   ;
   ; Read a direct image catalog of the type that is used to drive aXe.
   ;
   ; G. Meurer ~2003 (originally written)
   ;           1/2007 make robust to work with either the catalogs used
   ;                  by PEARS or the GTO groups
   ; 
   IF NOT keyword_set(type) THEN ttyp = 'PEARS' ELSE ttyp = type
   CASE ttyp OF 
      'PEARS': readcol, file, xim, yim, id, d1, d2, magauto, aim, bim, thetaim, $
                        d3, d4, d5, d6, w50, class, comment="#", $
                        format='(f,f,l,f,f,f,f,f,f,f,f,f,f,f,f)'
      'GTO':   readcol, file, id, xim, yim, d1, d2, magauto, aim, bim, thetaim, $
                        d3, d4, d5, w50, d6, class, comment="#", $
                        format='(l,f,f,f,f,f,f,f,f,f,f,f,f,f,f)'
   ENDCASE 
END
