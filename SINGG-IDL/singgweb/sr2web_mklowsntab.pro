PRO sr2web_mklowsntab
   ;
   ; Make a table of SR2 sources with low S/N in H-alpha or
   ; continuum flux.
   ;
   ; G. Meurer 08/2007
   ;
   prog       = 'sr2web_mklowsntab'
   snlim      = 3.0
   ddb        = 'singg_derived'
   fmto       = '(a11,a14,f9.3,f6.3,f6.3,f6.3,f9.2,f5.2,2x,a11)'
   hdr0       = '# object   name           log(FHa)  etot  esky  ecnt     Rmag  eR'
   fout       = 'lowsn_cases.dat'
   fhtml      = 'lowsn_cases.html'
   title      = 'SINGG Release 2 Quality Assurance (Subsample)'
   subtitle   = 'Low S/N Cases'
   ncolumns   = 10
   ;
   prog2      = strupcase(prog)+': '    ; for messages
   ;
   ; open database
   dbopen, ddb
   ;
   ; We only care about galaxies with mult_priority = 1
   list       = dbfind('mult_priority = 1')
   ;
   ; convert S/N limit to error limits
   elflim     = alog10(1.0 + 1.0/snlim)
   emglim     = 2.5*elflim
   elflims    = strtrim(string(elflim),2)
   emglims    = strtrim(string(emglim),2)
   print, prog2+'will select sources with ERR_LOGF_HA_T > '+elflims+' or ERR_MAG_R_T > '+emglims
   ;
   ; find the low S/N cases in H-alpha and R separately
   listh      = dbfind('ERR_LOGF_HA_T > '+elflims, list)
   listr      = dbfind('ERR_MAG_R_T > '+emglims, list)
   nh         = n_elements(listh)
   nr         = n_elements(listr)
   IF nh EQ 1 AND listh[0] LE 0 THEN nh = 0
   IF nr EQ 1 AND listr[0] LE 0 THEN nr = 0
   print, prog2+'there are '+strtrim(string(nh),2)+' low H-alpha S/N cases '
   print, prog2+'there are '+strtrim(string(nr),2)+' low continuum S/N cases '
   ;
   ; merge the lists, take only valid elements, and find unique
   ; valid elements
   list       = [listr, listh]
   kk         = sort(list)
   jj         = uniq(list[kk])
   uu         = where(list[kk[jj]] GE 1, nuu)
   print, prog2+'there are '+strtrim(string(nuu),2)+' unique low S/N cases'
   IF nuu LE 0 THEN BEGIN 
      print, prog2+'great! there are no low S/N cases. No web page written - finished'
      dbclose
      return
   ENDIF 
   list       = list[uu]
   ;
   ; extract relevant quantities from database
   print, prog2+'Extracting relevant quantities from DB'
   dbext, list, 'name,object,mapp_r_t,err_mag_r_t,logf_ha_t,err_logf_ha_t_sky,err_logf_ha_t_cont,err_logf_ha_t', $
                name,object,magr,emagr,lfha,elfhas,elfhac,elfha
   ;
   ; calculate s/n values and a string to succinctly list S/N
   snha       = 1.0/(10.0^elfha - 1.0)
   snr        = 1.0/(10.0^(0.4*emagr)-1.0)
   minsn      = make_array(nuu, /float, value=0.0)
   snstr      = '('+string(snha,format='(f4.1)')+','+string(snr,format='(f4.1)')+')'
   FOR ii = 0, nuu-1 DO minsn[ii] = min([snha[ii], snr[ii]])
   ;
   ; print table of results to ascii file and screen
   print, prog2+'Writing sorted results to '+fout
   kk         = reverse(sort(minsn))
   openw, lu, fout, /get_lun
   printf, -1, hdr0
   printf, lu, hdr0
   FOR jj = 0, nuu-1 DO BEGIN
      ii      = kk[jj]
      printf,-1, ljust(object[ii],11),ljust(name[ii],14),lfha[ii],elfha[ii],elfhas[ii],elfhac[ii],magr[ii],emagr[ii],snstr[ii],$
                 format=fmto
      printf,lu, ljust(object[ii],11),ljust(name[ii],14),lfha[ii],elfha[ii],elfhas[ii],elfhac[ii],magr[ii],emagr[ii],snstr[ii],$
                 format=fmto
   ENDFOR 
   free_lun, lu
   ;
   ; write HTML table
   namedisplay = strtrim(name,2)+'<br>'+snstr
   object      = strtrim(object,2)
   print, prog2+'Calling SR2WEB_OBJTAB to write '+fhtml
   sr2web_objtab, fhtml, object[kk], namelis=namedisplay[kk], title=title, subtitle=subtitle, ncolumns=ncolumns
   ;
   ; calculate unique HIPASS targets
   jj         = sort(object)
   qq         = uniq(object[jj])
   print, prog2+'note there are '+strtrim(string(n_elements(qq)),2)+' unique HIPASS targets with low S/N measurements'
   ;
   ; close database
   dbclose
END 
