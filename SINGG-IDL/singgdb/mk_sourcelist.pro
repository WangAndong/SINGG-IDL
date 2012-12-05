PRO mk_sourcelist
   ;
   ; create a list of singg/sungg measured objects their positions
   ; and some basic fluxes to facility position matching to other 
   ; samples.
   ;
   ; G. Meurer 05/2009
   ;
   mdb       = 'supersingg_master'
   hdb       = 'singg_derived'
   udb       = 'sungg_derived'
   filo      = 'singg_sungg_positions.dat'
   fmto      = '(a14,1x,f10.4,f10.4,f9.3,f9.3,f9.3,f9.3,f9.3)'
   lfflag    = -99.99
   ;
   ; open master db, get sources and positions
   dbopen, mdb
   list      = dbfind('possrc > 1')
   dbext, list, 'sname,ra,dec,lf90ha,lf90uv,entoptphot,entnuvphot',sname,ra,dec,lf90ha,lf90uv,eopt,enuv
   ns        = n_elements(sname)
   dbclose
   ;
   ; get Halpha fluxes and R band magnitudes
   dbopen, hdb
   dbext, eopt, 'name,mapp_r_t,logf_ha_t',sname1,magr,lfha
   dbclose
   ;
   ; get NUV magnitudes
   dbopen, udb
   dbext, enuv, 'sname,mag_brt',sname2,magnuv
   dbclose
   ;
   ; make sure optical & uv fluxes are flagged consistently
   jj        = where(lf90ha LE lfflag, njj)
   IF njj GT 0 THEN BEGIN 
      magr[jj] = lfflag
      lfha[jj] = lfflag
   ENDIF 
   jj        = where(lf90uv LE lfflag, njj)
   IF njj GT 0 THEN magnuv[jj] = lfflag
   ;
   ; write output
   openw, lu, filo, /get_lun
   printf, lu, '# --- Results from mk_sourcelist.pro --- '
   printf, lu, '# 1 HIPASS:SINGG name'
   printf, lu, '# 2 RA [J2000]'
   printf, lu, '# 3 Dec [J2000]'
   printf, lu, '# 4 log(f_90micron [Jy]) - estimated from Halpha/FIR versus M_R' 
   printf, lu, '# 5 log(f_90micron [Jy]) - estimated from GALEX (FUV-NUV)'
   printf, lu, '# 6 log(f_Halpha [erg/cm^2/s]) - from SINGG'
   printf, lu, '# 7 m_R [ABmag] - from SINGG'
   printf, lu, '# 8 m_NUV [ABmag] - from SUNGG'
   FOR ii = 0, ns-1 DO $ 
      printf, lu, ljust(sname[ii],14), ra[ii], dec[ii], lf90ha[ii], lf90uv[ii], lfha[ii], magr[ii], magnuv[ii], format=fmto
   free_lun, lu
END 
