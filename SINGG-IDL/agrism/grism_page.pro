PRO grism_page, filhtml, title, elm, id, pos, aim, bim, thetaim, $
                w50, magauto, class, pstmp, pspec, pribn
   ;
   ; make a page of grism data in tabular form
   ;
   openw, unitp, filhtml, /get_lun, /more
   grp_pagetop, unitp, title
   grp_thead, unitp, title
   ;
   FOR i = 0, n_elements(elm)-1 DO BEGIN
      grp_trow, unitp, id[elm[i]], [pos[elm[i],0],pos[elm[i],1]], aim[elm[i]], bim[elm[i]], $
       thetaim[elm[i]], w50[elm[i]], magauto[elm[i]], class[elm[i]], $
       namstmp(pstmp,id[elm[i]]), namribn(pribn,id[elm[i]]), namspec(pspec,id[elm[i]])
   ENDFOR
   ;
   grp_tfoot, unitp
   grp_pagebot, unitp
   free_lun, unitp
   
END

