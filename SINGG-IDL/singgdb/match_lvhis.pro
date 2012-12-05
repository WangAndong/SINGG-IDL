pro match_lvhis
   ;
   ; Find matches with LVHIS, as marked in supersingg_master database
   ; listthem and whether they have Halpha, and UV data
   ;
   ; G. Meurer, 10/2011
   ;
   wd      = getenv('SHOME')+'Sample'
   sdb     = 'supersingg_master'
   filo    = 'lvhis_matches.dat'
   hdro    = '#SINGG name     esingg   efuv   enuv egnear egdeep'
   fmto    = '(a15,i7,i7,i7,i7,i7)'
   ;
   ; go to working directory
   cd, wd, current=cwd
   ;
   ; find lvhis matches
   dbopen, sdb
   list    = dbfind('lvhis > 1')
   nlvhis  = n_elements(list)
   if nlvhis eq 1 and list[0] eq -1 then nlvhis = 0
   ;
   ; get relevant quantities
   dbext, list, 'sname,uvflag,entsamp,entuvsamp,entoptphot,entfuvphot,entnuvphot,entgalexa,entgalexb', $
          sname,uvflag,esamp,euvsamp,esingg,esunggf,esunggn,eglxa,eglxb
   ;
   ; print and write to file
   openw, lu, filo, /get_lun
   printf, -1, hdro
   printf, lu, hdro
   for ii = 0, nlvhis-1 do begin
      printf, -1, ljust(sname[ii],15), esingg[ii], esunggf[ii], esunggn[ii], eglxa[ii], eglxb[ii], format=fmto
      printf, lu, ljust(sname[ii],15), esingg[ii], esunggf[ii], esunggn[ii], eglxa[ii], eglxb[ii], format=fmto
   endfor 
   ;
   ; close and return to initial directory
   free_lun, lu
   cd, cwd
   ;stop
end
