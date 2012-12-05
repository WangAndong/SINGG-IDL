PRO plot_matchqa, ima, imb, cata, catb
   ;
   ;  Make QA plots for mrmatch:
   ;  * grayscale stamp of object in catalog A
   ;  * grayscale stamp of object in catalog B
   ;    - calculated pos of A in B
   ;
   expand = 2
   szstmp = 251
   dza    = [-200, 1000]
   dzb    = [-100, 500]
   buf    = 20
   xwin   = expand*(3*buf + 2*szstmp)
   ywin   = expand*(2*buf + szstmp)
   ;
   ; read in matched image pairs
   readcol, 'matched.mtA', idma, xfa, yfa, mga
   readcol, 'matched.mtB', idmb, xxb, yyb, mgb
   nm = n_elements(idma)
   ;
   ; get catalog positions from A (, B)
   readcol, cata, ida, xa, ya, mga, format='(i,f,f,f)'
   readcol, catb, idb, xb, yb, mgb, format='(i,f,f,f)'
   ;
   ; open images
   fits_read, ima, imga, hdra
   fits_read, imb, imgb, hdrb
   sza = size(imga)
   nxa = sza[1]
   nya = sza[2]
   szb = size(imgb)
   nxb = szb[1]
   nyb = szb[2]
   loadct, 0
   window, 0, xsize=xwin, ysize=ywin
   ;
   ; loop through sources
   FOR ii = 0, nm-1 DO BEGIN 
      ka = where(ida EQ idma[ii], nka)
      kb = where(idb EQ idmb[ii], nkb)
      ia1    = min([max([fix(xa[ka] - 0.5*szstmp + 0.5) - 1, 0]), nxa-1])
      ia2    = min([max([fix(xa[ka] + 0.5*szstmp + 0.5) - 1, 0]), nxa-1])
      ja1    = min([max([fix(ya[ka] - 0.5*szstmp + 0.5) - 1, 0]), nya-1])
      ja2    = min([max([fix(ya[ka] + 0.5*szstmp + 0.5) - 1, 0]), nya-1])
      nia    = ia2 - ia1 + 1
      nja    = ja2 - ja1 + 1
      nia2   = expand*nia
      nja2   = expand*nja

      ib1    = min([max([fix(xb[kb] - 0.5*szstmp + 0.5) - 1, 0]), nxb-1])
      ib2    = min([max([fix(xb[kb] + 0.5*szstmp + 0.5) - 1, 0]), nxb-1])
      jb1    = min([max([fix(yb[kb] - 0.5*szstmp + 0.5) - 1, 0]), nyb-1])
      jb2    = min([max([fix(yb[kb] + 0.5*szstmp + 0.5) - 1, 0]), nyb-1])
      nib    = ib2 - ib1 + 1
      njb    = jb2 - jb1 + 1
      nib2   = expand*nib
      njb2   = expand*njb
   
      stmpa  = bytscl(-1.0*imga[ia1:ia2,ja1:ja2], min=min(-1.0*dza), max=max(-1.0*dza))
      stmpb  = bytscl(-1.0*imgb[ib1:ib2,jb1:jb2], min=min(-1.0*dzb), max=max(-1.0*dzb))
      stmpa2 = rebin(stmpa, nia2, nja2, /sample)
      stmpb2 = rebin(stmpb, nib2, njb2, /sample)

      ;
      ; plot grayscale
      iposa = expand*buf - 1
      jposa = expand*buf - 1
      iposb = 2*expand*buf + expand*szstmp
      jposb = jposa
      keywait, 'type anything for plot'
      tv, stmpa2, iposa, jposa
      tv, stmpb2, iposb, jposb
   ENDFOR 


END 
