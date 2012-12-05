PRO grp_logcat2html, filcat, filhtml, title, comment, pstmp, pspcc, pspcf, pribn, pscii
   ;
   ; Read in emission line catalog
   ;
   fmt = '(i,f,f,f,f,f,f,f,f,f,f,f,f,i,i,i)'
   readcol,filcat,id,xim,yim,magauto,aim,bim,thetaim,w50,class,ra,dec,$
    minsn,maxsn,nord0,lin,qual,format=fmt
   rdstr = adstring(ra,dec,2)
   ;
   ; Mark up emission line catalog
   ;
   openw, unitp, filhtml, /get_lun, /more
   grp_pagetop, unitp, title
   grp_loghead, unitp, title, filcat, comment
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
      grp_logrow, unitp, _id, _pos, _aim, _bim, _thetaim, _w50, _magauto, _class, _ra, _dec, _rdstr, $
       nampng(pstmp,_id), nampng(pribn,_id), nampng(pspcc,_id), nampng(pspcf,_id), namdat(pscii,_id)
   ENDFOR
   ;
   grp_tfoot, unitp
   grp_pagebot, unitp
   free_lun, unitp
END 
