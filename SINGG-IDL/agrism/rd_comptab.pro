PRO rd_comptab, file, iau, sexid, grid, rastr, decstr, line, knt, cene, fluxe, ew, bmag, vmag, imag, zmag, $
                zspec, zcapak, zfly, zbpz1, zbpz1a, zbpz1b, obpz1, zbpz2, zbpz2a, zbpz2b, obpz2, zbpz3, zbpz3a, zbpz3b, obpz3, zgrism, $
                linid, lam0, nmatch, status, flag
   ;
   ; Read a atable of compiled axe or BLEM results
   ;
   ; G. Meurer 09/04
   ;
   fmt1       = '(a,i,i,a,a,i,i,f,f,f,f,f,f,f)'
   fmt2       = '(x,x,x,x,x,x,x,x,x,x,x,x,x,x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f)'
   fmt3       = '(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,a,f,i,i,i,i,i,i)'
   ;
   readcol, file, iau, sexid, grid, rastr, decstr, line, knt, cene, fluxe, ew, bmag, vmag, imag, zmag, $
            format=fmt1, comment='#'
   readcol, file, zspec, zcapak, zfly, zbpz1, zbpz1a, zbpz1b, obpz1, zbpz2, zbpz2a, zbpz2b, obpz2, zbpz3, zbpz3a, zbpz3b, obpz3, zgrism, $
            format=fmt2, comment='#'
   readcol, file, linid, lam0, nmatch, status, f1, f2, f3, f4, format=fmt3, comment='#'
   ns         = n_elements(iau)
   ;
   flag       = make_array(4, ns, /int, value=-1)
   FOR i = 0, ns-1 DO BEGIN 
      flag[0,*] = f1
      flag[1,*] = f2
      flag[2,*] = f3
      flag[3,*] = f4
   ENDFOR 
END 
