PRO query_dubious_rexam
   ;
   ; query singg_sample for dubious cases to rexamine.  Cull out the 
   ; quantities that Martin, M. needs.
   ;
   db = 'singg_sample'
   fili = 'dubious_rexamine.in'
   filo = 'dubious_rexamine.dat'
   srad = 15.0
   c    = 2.9978e5
   ;
   readcol, fili, name, rastr, decstr, velin, format='(a,a,a,f)'
   ns = n_elements(name)
   rain = sexideg(rastr)
   decin = sexideg(decstr)
   ;
   raout  = rain
   decout = decin
   velout = velin
   wout   = 0.0*rain + 50.0
   ;
   dbopen, db
   FOR i = 0, ns-1 DO BEGIN 
      list = dbcircle(rain[i], decin[i], srad, dis)
      IF list[0] GT -1 THEN BEGIN 
         dbext, list, 'ra,dec,vhel,w50',ra,dec,vhel,w50
         dv = abs(vhel - velin[i])
         k  = sort(dv)
         velout[i] = vhel[k[0]]
         wout[i] = w50[k[0]]
      ENDIF 
   ENDFOR 
   dbclose
   ;
   fact = 1.0/(1.0 + velout/c)
   velout = fact*velout
   wout   = fact*wout
   k      = where(wout LE 50., nk)
   IF nk GT 0 THEN wout[k] = 50.0 
   ;
   openw, lu, filo, /get_lun
   FOR i = 0, ns-1 DO printf, lu, ljust(name[i],15)+ljust(rastr[i],15)+ljust(decstr[i],15), velout[i], wout[i], format='(a45,f10.1,f8.1)'
   free_lun, lu
END 
