PRO grp_emcat2html, filcat, filhtml, title, pstmp, pfit, pspcc, pspcf, pribn, pscii
   ;
   ; filcat  -> name of ascii catalog file containing gaussian fit results.
   ; filhtml -> name of html file for output
   ; title   -> title for html file.
   ; pstmp   -> pstmp stamp file prefix
   ; pfit    -> fit file prefix
   ; pspcc   -> cross correlation func png file prefix
   ; pspcf   -> ?
   ; pribn   -> ribbon png file prefix
   ; pscii   -> ascii spectrum file prefix.
   ;
   ; Read in emission line catalog
   ;
   ; G. Meurer 11/2004 - tidied up & adjusted to handle 
   ;                     continuum & EW measurements
   ;
   fmt = '(i,f,f,f,f,f,f,f,f,f,f,f,f,i,i,i,f,f,f,f,f)'
   readcol,filcat,id,xim,yim,magauto,aim,bim,thetaim,w50,class,ra,dec,$
    minsn,maxsn,nord0,lin,qual,cen,width,flux,continuum,ew,format=fmt
   rdstr = adstring(ra,dec,2)
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
      _cont    = continuum[i]
      _ew      = ew[i]
;      grp_emrow, unitp, _id, _pos, _aim, _bim, _thetaim, _w50, _magauto, _class, _ra, _dec, _rdstr, _cen, _width, _flux, $
;       _cont, _ew, nampng(pstmp,_id), nampng(pribn,_id), nampng(pfit,_id), nampng(pspcc,_id), nampng(pspcf,_id), namdat(pscii,_id)
      grp_emrow, unitp, _id, _pos, _aim, _bim, _thetaim, _w50, _magauto, _class, _ra, _dec, _rdstr, _cen, _width, _flux, $
       _cont, _ew, namjpg(pstmp,_id), namjpg(pribn,_id), nampng(pfit,_id), namjpg(pspcc,_id), namjpg(pspcf,_id), namdat(pscii,_id)
   ENDFOR
   ;
   grp_tfoot, unitp
   grp_pagebot, unitp
   free_lun, unitp
END 

