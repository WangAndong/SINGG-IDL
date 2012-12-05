PRO read_cbhcs04, ra, dec, hkmag, zmag, imag, rmag, vmag, bmag, umag, $
                  zsp, flag_zsp, src_zsp, zph, flag_zph, zphodds
   ;
   ; read data on hawaii sample from Cowie et al (2004), table 2
   ;
   wd       = '/home/meurer/ACS/Grism/HDFN_jun04/Apsis/Work/Match/'
   fili     = 'cbhcs04_table2.dat'
   fmti     = '(f10.6,1x,f8.5,1x,a5,1x,a4,1x,a4,1x,a4,1x,a5,1x,a5,1x,a5,1x,a6,1x,a1,1x,a1,1x,a4,1x,a1,1x,a4)'
   defmag   = -99.0   ; set undefined magnitudes to this
   defz     =  -1.0   ; set undefined redshifts to this
   deff     =   0b    ; set undefined flags to this
   defs     =  -1     ; set undefined sources to this
   skipline =  37     ; lines to skip in fili
   ;
   cd, wd, current=cwd
   readfmt, fili, fmti, ra, dec, hkmags, zmags, imags, rmags, vmags, bmags, umags, $
    zsps, flag_zsps, src_zsps, zphs, flag_zphs, zphoddss, skipline=skipline
   nz       = n_elements(ra)
   cd, cwd
   ;
   ; set blank strings to default values, decode evrything else
   hkmag    = make_array(nz, /float, value=defmag)
   zmag     = make_array(nz, /float, value=defmag)
   imag     = make_array(nz, /float, value=defmag)
   rmag     = make_array(nz, /float, value=defmag)
   vmag     = make_array(nz, /float, value=defmag)
   bmag     = make_array(nz, /float, value=defmag)
   umag     = make_array(nz, /float, value=defmag)
   zsp      = make_array(nz, /float, value=defz)
   zph      = make_array(nz, /float, value=defz)
   zphodds  = make_array(nz, /float, value=defz)
   src_zsp  = make_array(nz, /int, value=defs)
   flag_zsp = make_array(nz, /byte, value=deff)
   flag_zph = make_array(nz, /byte, value=deff)
   k        = where(strtrim(hkmags,2) NE '',nk)
   IF nk GT 0 THEN hkmag[k] = float(hkmags[k])
   k        = where(strtrim(zmags,2) NE '',nk)
   IF nk GT 0 THEN zmag[k] = float(zmags[k])
   k        = where(strtrim(imags,2) NE '',nk)
   IF nk GT 0 THEN imag[k] = float(imags[k])
   k        = where(strtrim(rmags,2) NE '',nk)
   IF nk GT 0 THEN rmag[k] = float(rmags[k])
   k        = where(strtrim(vmags,2) NE '',nk)
   IF nk GT 0 THEN vmag[k] = float(vmags[k])
   k        = where(strtrim(bmags,2) NE '',nk)
   IF nk GT 0 THEN bmag[k] = float(bmags[k])
   k        = where(strtrim(umags,2) NE '',nk)
   IF nk GT 0 THEN umag[k] = float(umags[k])
   k        = where(strtrim(zsps,2) NE '',nk)
   IF nk GT 0 THEN zsp[k] = float(zsps[k])
   k        = where(strtrim(zphs,2) NE '',nk)
   IF nk GT 0 THEN zph[k] = float(zphs[k])
   k        = where(strtrim(flag_zsps,2) NE '',nk)
   IF nk GT 0 THEN flag_zsp[k] = 1b
   k        = where(strtrim(flag_zphs,2) NE '',nk)
   IF nk GT 0 THEN flag_zph[k] = 1b
   k        = where(strtrim(zphoddss,2) NE '',nk)
   IF nk GT 0 THEN zphodds[k] = float(zphoddss[k])
   k        = where(strtrim(src_zsps,2) NE '',nk)
   IF nk GT 0 THEN src_zsp[k] = float(src_zsps[k])
END 

