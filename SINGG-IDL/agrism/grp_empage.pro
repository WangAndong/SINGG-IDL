PRO grp_empage, filcat, filhtml, title, id, xim, yim, aim, bim, thetaim, $
               w50, magauto, class, ra, dec, rdstr, cen, width, flux, $
               pstmp, pfit, pspcc, pspcf, pribn, pscii
   ;
   ; Mark up emission line catalog
   ;
   openw, unitp, filhtml, /get_lun, /more
   grp_pagetop, unitp, title
   grp_emhead, unitp, title, filcat
   ;
   FOR i = 0, n_elements(id)-1 DO BEGIN
      _id      = id[i]
      _pos     = [xim[i], yim[i]]
      _aim     = aim[i]
      _bim     = bim[i]
      _thetaim = thetaim[i]
      _w50     = w50[i]
      _magauto = magauto[i]
      _class   = class[i]
      _ra      = ra[i]
      _dec     = dec[i]
      _rdstr   = rdstr[i]
      _cen     = cen[i]
      _width   = width[i]
      _flux    = flux[i]
      grp_emrow, unitp, _id, _pos, _aim, _bim, _thetaim, _w50, _magauto, _class, _ra, _dec, _rdstr, _cen, _width, _flux, $
       nampng(pstmp,_id), nampng(pribn,_id), nampng(pfit,_id), nampng(pspcc,_id), nampng(pspcf,_id), namdat(pscii,_id)
   ENDFOR
   ;
   grp_tfoot, unitp
   grp_pagebot, unitp
   free_lun, unitp
END

