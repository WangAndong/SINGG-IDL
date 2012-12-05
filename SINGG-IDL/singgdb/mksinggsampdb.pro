PRO mksinggsampdb, usedir=usedir, calcflow=calcflow
   ;
   ; Make database for SINGG sample.  The IDL database is written
   ; to the files singg_sample.db? in the directory sdir.  sdir is
   ; currently hard wired but can be overwridden with the usedir
   ; keyword.
   ;
   ;   usedir   -> If set this is the directory where data files are 
   ;               read from and the directory that the database is 
   ;               written to.
   ;   calcflow -> If set, the multi-attractor flow model JPHFLOW is
   ;               run on the final positions and velocities.  This
   ;               results in new distances.
   ;
   ; G. Meurer 08/2004 - or earlier... program originally written.
   ; G. Meurer 12/2004 - adjusted to insert E(B-V) and flag value from 
   ;                     Schlegel et al. (1998, ApJ, 500, 525) into
   ;                     database.  Databse now has E(b-V) in its sort 
   ;                     block.
   ;                   - Added calcflow keyword
   ; G. Meurer 10/2005 - bug found where same entry (J2215-45a) was 
   ;                     listed twice.  This is because closest entry 
   ;                     in angular and velocity was not adopted.  This
   ;                     is now fixed.  Also changed the "hardwiring"
   ;                     in prep for V05 selection.
   ;
   ; set parameters
   ;
   c        = 2.9978d5                    ; speed of light
   h0       = 70.0                        ; km/s/Mpc
   distmin  = 1.0                         ; minimum distance allowed from vshap
   invrad   = 3.141592654/180.      ; radians / degree
   vmin     = -999.9
   ;
   ; input file
   IF keyword_set(usedir) THEN sdir = usedir $
                          ELSE sdir = '/data1/acs7/meurer/SINGG/Sample/V05a_oct05/' ; directory for singg sample
;                          ELSE sdir = '/data1/acs7/meurer/SINGG/Sample/V04b_dec04/' ; directory for singg sample
;                          ELSE sdir = '/home/meurer/singg/sample/V04b_jun04/' ; directory for singg sample
   fil      =  'sample5_ravsort.update'    ; singg sample name
   fild     = sdir+'distance_overide.dat' ; over-ride distances
   fill     = sdir+'mksinggsampdb.log'    ; report results here
   febv1    = sdir+'ebv1.log'              ; log file for Schlegel code
   febv2    = sdir+'ebv2.log'              ; log file for Schlegel code
   invers   = 4                           ; singg sample version
   sfil     = sdir+fil
   ;
   ;  Databases to match.  Order given is inverted priority
   ;  (last match is used for optical ID if relevant).
   dblis    = ['hipass_sccc', 'hipass_avcc', 'hipass_bgc', $
               'hicat_feb04', 'hipass_remeasure']
   dbnam    = ['SCCC', 'AVCC', 'BGC', 'HICAT', 'REMEASURE']
   shnam    = ['SCC', 'AVC', 'BGC', 'HCT', 'REM']
   vnam     = ['vhel', 'vlsr', 'vhel', 'vel_50max', 'vhel']
   wnam     = ['w50', 'w50', 'w50', 'width_50max', 'w50']
   vsnam    = ['', '', 'vshap', 'vshap', 'vshap']
   pknam    = ['sp', '', 'sp', 'sp', 'sp']
   flxnam   = ['sint', 'sint', 'sint', 'sint', 'sint']
   ebvnam   = ['', '', '', '', '']
   onam     = ['optid', 'optid', 'optid', '', 'optid']
   nnam     = ['', '', 'NAME', 'HIPASS_NAME', 'NAME']
   updpar   = [0b, 0b, 1b, 1b, 1b]
   ;
   tolv     = 0.5
   tolpk    = 0.0005
   tolflx   = 0.1
   tolebv   = 0.001
   tolp     = 5.0/3600.0
   tollm    = 0.005
   ;
   ndb      = n_elements(dblis)
   ;
   ; database matching criteria
   rmatch   = 4.0                         ; arcmin
   vmatch   = 400.0                       ; km/s
   ;
   ; definition of fields for structure and items for db
   item     = ['name','optid','catalog','ra','dec','glat','glong',$
               'vhel','vlg','vcmb','vtonry','vshap','w50','sp',$
               'sint','distance','logmhi','observed','ebv','mask_ebv','entry_'+dbnam]
   tout     = ['C','C','C','R','R','R','R',$
               'R','R','R','R','R','R','R',$
               'R','R','R','C','R','B',make_array(ndb,/string,value='I')]
   tlen     = [0,0,0,8,8,4,4,$
               4,4,4,4,4,4,4,$
               4,4,4,0,4,1,make_array(ndb,/int,value=2)]
   dstring  = 'a,a,a,d,d,f,f,f,f,f,f,f,f,f,f,f,f,a,f,b'
   FOR ii = 0,ndb-1 DO dstring = dstring+',i'
   descript = ['HIPASS name', 'Optical ID', 'HI source catalog', $
               'Right Ascension (J2000) [deg]', 'Declination (J2000) [deg]', $
               'Galactic latitude [deg]', 'Galactic longitude [deg]', $
               'HI Heliocentric radial velocity [km/s]', $
               'Local group corrected radial velocity', $
               'Cosmic Microwave Background corrected radial velocity', $
               'Tonry flow model corrected radial velocity', $
               'Huchra flow model corrected radial velocity', $
               'HI FWHM velocity width [km/s]', 'HI peak flux density [Jy]', $
               'HI integrated flux [Jy km/s]', $
               'Distance - multipole attractor model [Mpc]', $
               'Logarithmic HI mass [Log(M_sun)]', 'Observed status', $
               'E(B-V)g Gal. foreground reddening (Schlegel et al) [mag]',$
               'Byte mask from Schlegel et al',$
               'Entry number in '+dblis+' database']
   title    = 'SINGG sample selection'
   filo     = 'singg_sample'
   indblk   = ['ra','dec','vhel','logmhi','sp','sint','glat','glong','ebv']
   ;
   ; open log file
   openw, ll, fill, /get_lun
   ;
   ; read in sample
   singg_readsamp, sfil, name, catalog, rastr, decstr, optid, sp, $
    sint, w50, vhel, vshap, observed, logmhi, l, b, d, ra, dec, obflag, $
    vlg, vcmb, vtonry, ebv, version=invers
   ra       =  15.0*ra      ; convert RA from hours to deg
   sp       =  0.001*sp     ; convert sp from mJy to Jy
   ns       =  n_elements(name)
   ;
   ; log number of entries read in
   printf, ll, 'MKSINGGSAMPDB: Number of entries read with singg_readsamp: ', ns
   ;
   ; make arrays to store old quantities
   oldcat    =  catalog
   catalog   =  make_array(ns,/string,value='---')
   oldra     =  ra
   olddec    =  dec
   oldvhel   =  vhel
   oldw50    =  w50
   oldvshap  =  vshap
   oldsp     =  sp
   oldsint   =  sint
   oldebv    =  ebv
   oldname   =  name
   oldlogmhi =  logmhi
   ;
   ; Loop through databases and record matches
   FOR jj = 0, ndb-1 DO BEGIN 
      print, 'Opening database : '+dblis[jj]
      printf, ll, 'MKSINGGSAMPDB: Opening database : '+dblis[jj]
      dbopen, dblis[jj]
      entry = make_array(ns, /int, value=-1)
      ;
      ; loop through database entries
      FOR ii = 0, ns -1 DO BEGIN 
         list = dbcircled(oldra[ii], olddec[ii], rmatch, dis)
         nl   = n_elements(list)
         ;
         ; proceed if there is at least one position match
         ; fixed bug (10/2005) so that now closest match is adopted.
         ; closeness now measured by angular distance normalized by
         ; match radius, and vel difference normalized by match
         ; vel, both added in quadrature (rr below).
         IF nl GE 1 AND list[0] NE -1 THEN BEGIN 
            dbext, list, vnam[jj]+',ra,dec', vdb, radb, decdb
            dr   = 60.0*dis/rmatch
            dv   = (vdb - oldvhel[ii])/vmatch
            rr   = sqrt(dr*dr+dv*dv)
            kk   = sort(rr)
            good = where(abs(dv[kk]) LE 1.0, ngood)
            ;
            ; store closest entry if there is at least one vel match
            IF ngood GE 1 THEN entry[ii] = list[kk[good[0]]]
         ENDIF 
      ENDFOR 
      qq     = where(entry NE -1, nent)
      print, '   Number of matched entries in database '+dblis[jj]+' : '+strtrim(string(nent),2)
      printf, ll, 'MKSINGGSAMPDB:  Number of matched entries in database '+dblis[jj]+' : '+strtrim(string(nent),2)
      ;
      ; Things to do with matching entries...
      IF nent GT 0 THEN BEGIN 
         ;
         ; reset catalog
         catalog[qq] = shnam[jj]
         IF updpar[jj] THEN BEGIN 
            ;
            ; reset ra,dec
            dbext, entry[qq], 'ra,dec',newra,newdec
            ra[qq]   =  newra
            dec[qq]  =  newdec
            ;
            ; reset velocities
            dbext, entry[qq], vnam[jj], newvhel
            dbext, entry[qq], vsnam[jj], newvshap
            vhel[qq]  =  newvhel
            vshap[qq] =  newvshap
            ;
            ; reset widths
            dbext, entry[qq], wnam[jj], neww50
            w50[qq]   =  neww50
            ;
            ; reset sp,sint
            dbext, entry[qq], pknam[jj], newsp
            dbext, entry[qq], flxnam[jj], newsint
            sp[qq]    =  newsp
            sint[qq]  =  newsint
            ;
            ; reset E(B-V)
            IF strlen(strtrim(ebvnam[jj],2)) GT 0 THEN BEGIN 
               dbext, entry[qq], ebvnam[jj], newebv
               ebv[qq]   =  newebv
            ENDIF 
            ;
            ; reset names
            dbext, entry[qq], nnam[jj], newname
            name[qq]  =  newname
         ENDIF 
         ;
         ; get optical ids if they are supported in the database
         IF onam[jj] NE '' THEN BEGIN
            oldid  = optid[qq]
            oldnam = name[qq]
            dbext, entry[qq], 'name,'+onam[jj], newnam, newid
            newid  = strcompress(newid, /remove_all)
            oldid  = strcompress(oldid, /remove_all)
            kk     = where(strlen(newid) GT 1)
            kj     = where(strlen(newid) GT 1 AND strlen(oldid) GT 1 $
                           AND strupcase(newid) NE strupcase(oldid), nrep)
            ;
            ; print cases where updated names differ
            print, 'Optical IDs do not agree in '+strtrim(string(nrep),2)+' cases'
            printf, ll, 'MKSINGGSAMPDB: Optical IDs do not agree in '+strtrim(string(nrep),2)+' cases'
            IF nrep GT 0 THEN BEGIN 
               FOR ii =0, nrep-1 DO printf, ll, ljust(oldnam[kj[ii]],15), ljust(oldid[kj[ii]],20), $
                ' | ', ljust(newnam[kj[ii]],15), ljust(newid[kj[ii]],20)
            ENDIF 
            ;
            ; update optical IDs
            optid[qq[kk]] = newid[kk]
         ENDIF 
      ENDIF 
      ;
      ; set pointer to database
      cmd = 'entry_'+dbnam[jj]+' = entry'
      result = execute(cmd)
      dbclose
   ENDFOR 
   ;
   ; now prepare quantities for database ...
   printf, ll, 'MKSINGGSAMPDB: recalculating GLONG, GLAT, EBV, MASK_EBV, VLG.'
   ;
   ; calculate galactic coords
   glactc, ra, dec, 2000.0, glong, glat, 1, /degree
   ;
   ; get new reddening values
   ebv       = dust_getval(glong, glat, /interp, outfile=febv1)
   mask_ebv  = dust_getval(glong, glat, map='mask', outfile=febv2)
   ;
   ; recompute local group velocity
   vlg       = float(vhel) - 79.0*cos(invrad*glong)*cos(invrad*glat) + $
               296.0*sin(invrad*glong)*cos(invrad*glat) - 36.0*sin(invrad*glat)
   IF keyword_set(calcflow) THEN BEGIN 
      ;
      ; compute flow model velocities
      filvo  = sdir+'flow_'+strtrim(filo,2)+'.out'
      filvs  = sdir+'flow_'+strtrim(filo,2)+'.simple'
      vuse   = vhel
      badv   = where(vuse LT vmin, nb)
      IF nb GT 0 THEN vuse[badv] = vmin
      printf, ll, 'MKSINGGSAMPDB: preparing data for distance calculation using JPHFLOW'
      printf, ll, '      Output files: '+filvo+'  ,  '+filvs
      mkjphflowin, name, ra, dec, vuse
      spawn, '/home/meurer/bin/jphflow'
      printf, ll, 'MKSINGGSAMPDB: spawn of JPHFLOW completed'
      spawn, 'mv flow.out '+filvo
      spawn, 'mv flow.simple '+filvs
      ;
      ; read in derived velocities
      readcol,filvs,dum1,dum2,dum3,vhin,vlg,vcmb,vvir,vga,vshap,sty,iau,dum4,dum5,vtonry,$
       format='(a12,f,f,f,f,f,f,f,f,f,f,f,f,f)'
      printf, ll, 'MKSINGGSAMPDB: velocities from JPHFLOW read in'
   ENDIF 
   ;
   ; calculate distances
   printf, ll, 'MKSINGGSAMPDB: recalculating distances'
   distance  =  vshap / h0
   kk        =  where(distance LT distmin,nk)
   IF nk GT 0 THEN distance[kk] = distmin
   ;
   printf, ll, 'MKSINGGSAMPDB: number of sources with distances set to distmin: '+strtrim(string(nk),2)
   ;
   ; Override distances (perhaps I should do this only when making sample...?)
   get_distoverride, fild, namd, newd, logu=ll
   distoverride, namd, newd, oldname, distance, logu=ll
   ;
   ; Recalculate log(M_HI)
   printf, ll, 'MKSINGGSAMPDB: recalculating LOGMHI'
   logmhi   = alog10(2.36e5*sint*distance*distance)
   ;
   ; report places where arrays have changed significantly
   report_diff, ll, tolp, oldra, ra, quant='RA', name=name, /detail
   report_diff, ll, tolp, olddec, dec, quant='DEC', name=name, /detail
   report_diff, ll, tolv, oldvhel, vhel, quant='VHEL', name=name, /detail
   report_diff, ll, tolv, oldw50, w50, quant='W50', name=name, /detail
   report_diff, ll, tolv, oldvshap, vshap, quant='VSHAP', name=name, /detail
   report_diff, ll, tolpk, oldsp, sp, quant='SP', name=name, /detail
   report_diff, ll, tolflx, oldsint, sint, quant='SINT', name=name, /detail
   report_diff, ll, tolebv, oldebv, ebv, quant='EBV', name=name, /detail
   report_diff, ll, tollm, oldlogmhi, logmhi, quant='LOGMHI', name=name, /detail
   ;
   ; find entries that have not been matched
   qq  = where(catalog EQ '---', nqq)
   printf, ll, 'MKSINGGSAMPDB: number of sources not matched to any database : '+strtrim(string(nqq),2)
   IF nqq GT 0 THEN FOR jj = 0, nqq-1 DO printf, ll, '    '+ljust(name[qq[jj]], 20)
   ;
   ; find entries that have not been matched to any of the 
   ; key databases
   mtch = make_array(ns, /byte, value=0b)
   FOR jj = 0, ndb-1 DO BEGIN 
      IF updpar[jj] THEN BEGIN 
         result = execute('mtch = (mtch EQ 1b OR entry_'+dbnam[jj]+' NE -1)')
      ENDIF 
   ENDFOR 
   qq   = where(mtch EQ 0b, nqq)
   printf, ll, 'MKSINGGSAMPDB: number of sources not matched to the "good" databases : '+strtrim(string(nqq),2)
   IF nqq GT 0 THEN FOR jj = 0, nqq-1 DO printf, ll, '    '+ljust(name[qq[jj]], 20)
   ;
   ; Find entries with HIPASS name changes
   qq = where(strtrim(name,2) NE strtrim(oldname,2), nqq)
   printf, ll, 'MKSINGGSAMPDB: number of sources with HIPASS name changes : '+strtrim(string(nqq),2)
   IF nqq GT 0 THEN FOR jj = 0, nqq-1 DO printf, ll, '    old : '+ljust(oldname[qq[jj]], 20)+' | new : '+ljust(name[qq[jj]], 20)
   ;
   ; Kludge name of J1339-31b to remain the same
   jj = strpos(oldname, 'J1339-31b')
   qq = where(jj GE 0, nqq)
   IF nqq EQ 1 THEN BEGIN 
      name[qq] = oldname[qq]
      printf, ll, 'MKSINGGSAMPDB: kludging name of J1339-31b back to old value...'
   ENDIF 
   ;
   ; now do database creation stuff ...
   ;
   ; set lengths of character items
   kk       = where(tlen EQ 0,nk)
   IF nk GT 0 THEN FOR ii = 0,nk-1 DO result = execute('tlen[kk[ii]] = max(strlen('+item[kk[ii]]+'))') $
              ELSE stop
   ;
   ; create structure
   create_struct2, singg, 'SINGG', item, dstring, dimen=ns
   ;
   ; open dbd file, write top of the file
   fildbd  = filo + '.dbd'
   openw, lu, fildbd, /get_lun
   printf, lu, '#title'
   printf, lu, title
   printf, lu, ''
   printf, lu, '#maxentries'
   printf, lu, strtrim(string(ns),2)
   printf, lu, ''
   printf, lu, '#items'
   ;
   ; Fill singg structure, write item lines of dbd file
   mc     = max(strlen(item))
   pritem = ljust(strupcase(item), mc)
   ni     = n_elements(item)
   FOR ii = 0, ni-1 DO BEGIN 
      cmd = "singg."+strtrim(item[ii],2)+" = "+strtrim(item[ii],2)
      result = execute(cmd)
      typstr = ljust(tout[ii]+'*'+strtrim(string(tlen[ii]),2),5)
      printf,lu,pritem[ii]+'   '+typstr+'   "'+descript[ii]+'"'
;      print,cmd
;      print,pritem[ii]+'   '+typstr+'   "'+descript[ii]+'"'
   ENDFOR 
   ;
   ; write index blocks of dbd file and close
   printf,lu,'  '
   printf,lu,'#index'
   ni = n_elements(indblk)
   FOR ii = 0, ni-1 DO BEGIN 
      printf,lu,ljust(strupcase(strtrim(indblk[ii],2)),mc)+'   sort'
   ENDFOR 
   free_lun,lu   
   ;
   ; write database
   !PRIV = 2
   dbcreate, filo, 1, 1, /external
   dbopen, filo, 1
   dbbuildstruct, singg
   dbclose,dummy
   ;
   free_lun, ll
END 
