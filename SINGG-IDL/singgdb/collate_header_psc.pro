PRO collate_header_psc
   ; 
   ; collate header database with IRAS PSC
   ;
   ; G. Meurer, Feb 2004
   ;
   srcrad  = 12.0  ;search radius in arcmin
   filo    = '/home/meurer/teltime/SST/singg_iras_collate.dat'
   ;
   ; Open header database, extract & convert 
   ; file (Rsub), object, RA, DEC.
   dbopen, 'header'
   list    = dbfind('FILENAME = rsub')
   dbext,list,'runname,object,filename,ra,dec',run,obj,filen,rastr,decstr
   ra      = sexideg(rastr)
   dec     = sexideg(decstr)
   nh      = n_elements(list)
   nfound  = make_array(nh, /int)
   dbclose
   ;
   ; open IRAS PSC
   dbopen, 'iras_psc'
   dum = dbfind('entry')      
   ;
   ; open output file
   openw, lu, filo, /get_lun
   FOR i = 0, nh-1 DO BEGIN 
      dis   = float(dum)*0.0
      list  = dbcircle(ra[i], dec[i], srcrad, dis, /silent, /to_b1950)
      IF list[0] GE 0 THEN nfound[i] = n_elements(list) ELSE nfound[i] = 0
      IF nfound[i] GT 0 THEN BEGIN 
;
;        Extract name, fluxes, uncertainties, qualities, position
;         dbext, list, 'name,12_flux,25_flux,60_flux,100_flux,12_tsnr,25_tsnr,60_tsnr,100_tsnr,ra,dec', $
         dbext, list, 'name,12_flux,25_flux,60_flux,100_flux,12_unc,25_unc,60_unc,100_unc,ra,dec', $
          nampsc,f12,f25,f60,f100,ef12,ef25,ef60,ef100,rapscb,decpscb
         dbext, list, '12_fqual,25_fqual,60_fqual,100_fqual', qf12,qf25,qf60,qf100
         ;
         ; precess to J2000, convert to string
;         stop
         rapscb = rapscb * 15.0d0
         jprecess, rapscb, decpscb, rapsc, decpsc
         radecstr = adstring(rapsc,decpsc,0)
         rapsc  = rapsc / 15.0d0
         ;
         ; hardwire colons as field separators
         p = [3,6,16,19]
         FOR j = 0, nfound[i]-1 DO BEGIN 
            str = radecstr[j]
            FOR k = 0, 3 DO strput, str, ':', p[k]
            radecstr[j] = str
         ENDFOR
         ;
         ; change uncertainties to absolute uncertainties 
         ; instead of percentages
         ef12  = 0.01*float(temporary(ef12))*f12
         ef25  = 0.01*float(temporary(ef25))*f25
         ef60  = 0.01*float(temporary(ef60))*f60
         ef100 = 0.01*float(temporary(ef100))*f100
         ;
         ; encode upper limits with error = -9.99
;         k = where(qf12 EQ 3,nk)
;         IF nk GT 0 THEN ef12[k] = -9.99
;         k = where(qf25 EQ 3,nk)
;         IF nk GT 0 THEN ef25[k] = -9.99
;         k = where(qf60 EQ 3,nk)
;         IF nk GT 0 THEN ef60[k] = -9.99
;         k = where(qf100 EQ 3,nk)
;         IF nk GT 0 THEN ef100[k] = -9.99
         FOR j = 0, nfound[i]-1 DO BEGIN 
            print, obj[i], run[i], rastr[i], decstr[i], $
             ' | ', j+1, dis[j], nampsc[j], f12[j], f25[j], f60[j], f100[j],$
             ef12[j], ef25[j], ef60[j], ef100[j], $
             qf12[j], qf25[j], qf60[j], qf100[j], radecstr[j], $
             format='(a15,1x,a5,1x,a16,a16,a3,i2,f7.2,1x,a11,f7.2,f7.2,f7.2,f7.2,f6.2,f6.2,f6.2,f6.2,i2,i2,i2,i2,a25)'
            printf, lu, obj[i], run[i], rastr[i], decstr[i], $
             ' | ', j+1, dis[j], nampsc[j], f12[j], f25[j],f60[j], f100[j],$
             ef12[j], ef25[j], ef60[j], ef100[j], $
             qf12[j], qf25[j], qf60[j], qf100[j], radecstr[j], $
             format='(a15,1x,a5,1x,a16,a16,a3,i2,f7.2,1x,a11,f7.2,f7.2,f7.2,f7.2,f6.2,f6.2,f6.2,f6.2,i2,i2,i2,i2,a25)'
         ENDFOR 
      ENDIF ELSE BEGIN 
         print, obj[i], run[i], rastr[i], decstr[i], $
          ' | No_matches', $
          format='(a15,1x,a5,1x,a16,a16,a13)'
         printf, lu, obj[i], run[i], rastr[i], decstr[i], $
          ' | No_matches', $
          format='(a15,1x,a5,1x,a16,a16,a13)'
      ENDELSE 
   ENDFOR 
   free_lun,lu
END 
