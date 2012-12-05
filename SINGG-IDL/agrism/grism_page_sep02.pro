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
       namjpg(pstmp,id[elm[i]]), namjpg(pribn,id[elm[i]]), namjpg(pspcc,id[elm[i]]), $
       namjpg(pspcf,id[elm[i]]), namdat(pscii,id[elm[i]])
;      grp_trow_sep02, unitp, id[elm[i]], [pos[elm[i],0],pos[elm[i],1]], aim[elm[i]], bim[elm[i]], $
;       thetaim[elm[i]], w50[elm[i]], magauto[elm[i]], class[elm[i]], $
;       nampng(pstmp,id[elm[i]]), nampng(pribn,id[elm[i]]), nampng(pspcc,id[elm[i]]), $
;       nampng(pspcf,id[elm[i]]), namdat(pscii,id[elm[i]])
   ENDFOR
   ;
   grp_tfoot, unitp
   grp_pagebot, unitp
   free_lun, unitp
   
END

;PRO grp_trow_sep02, unit, id, posim, aim, bim, thetaim, fwhm, magauto,
;class, fstmp, fribn, fspcf, fspcc, fscii

